#[cfg(feature = "trace")]
use super::VmObserver;
use super::dispatch::STAGE_LOOKUP;
use super::error::{BadGraphKind, ParseError, no_match};
use super::flags::{flag_is_set, restore_snapshot};
use super::frames::{Frame, SnapEntry};
use super::utf8::decode_utf8;
use crate::diagnostics::error::{ErrorContext, Expected};
use crate::parse::context::FlagMask;
use crate::parse::insn::{Insn, ParseGraph};
#[cfg(feature = "std")]
use crate::parse::memo::{MemoReplay, MemoTable};
use crate::parse::simd;
use crate::types::{CaptureEvent, Pos, SyntaxKind, TreeEvent};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// Mutable VM buffers threaded through the parse VM (`run` and failure handling).
pub(super) struct VmState<'a> {
    pub(super) stack: &'a mut Vec<Frame>,
    pub(super) events: &'a mut Vec<CaptureEvent>,
    pub(super) tree_events: &'a mut Vec<TreeEvent>,
    pub(super) open_tokens: &'a mut Vec<(SyntaxKind, bool, Pos)>,
    #[cfg(feature = "std")]
    pub(super) memo: &'a mut Option<MemoTable>,
    pub(super) error_ctx: &'a mut ErrorContext,
    pub(super) context_stack: &'a mut Vec<u32>,
    pub(super) flags: &'a mut Vec<u64>,
    pub(super) ctx_snapshots: &'a mut Vec<SnapEntry>,
    pub(super) multi_errors: &'a mut Option<&'a mut Vec<ParseError>>,
    pub(super) max_errors: usize,
    #[cfg(feature = "trace")]
    pub(super) observer: Option<&'a mut dyn VmObserver>,
    #[cfg(feature = "trace")]
    pub(super) tracer: Option<&'a mut dyn super::ParseTracer>,
    /// Rule call stack for `ParseTracer` (one entry per pushed Return/MemoReturn frame).
    #[cfg(feature = "trace")]
    pub(super) rule_stack: &'a mut Vec<crate::types::RuleId>,
}

#[inline(never)]
pub(super) fn run(
    graph: &ParseGraph<'_>,
    input: &[u8],
    vm: &mut VmState<'_>,
) -> Result<Pos, ParseError> {
    run_from_impl(graph, input, vm, graph.start(), 0)
}

#[inline]
fn pos_from_len(len: usize) -> Result<Pos, ParseError> {
    Pos::try_from(len).map_err(|_| ParseError::BadGraph(BadGraphKind::PosOutOfRange { len }))
}

/// Like [`run`], but start at a specific instruction pointer and byte offset.
///
/// This is used by both partial reparsing and "parse from rule" entrypoints.
pub(super) fn run_from(
    graph: &ParseGraph<'_>,
    input: &[u8],
    vm: &mut VmState<'_>,
    start_ip: u32,
    start_pos: Pos,
) -> Result<Pos, ParseError> {
    run_from_impl(graph, input, vm, start_ip, start_pos)
}

/// Single bounds-checked byte read for the parse loop (`pos` in range of `input_len`).
#[inline(always)]
fn byte_at(input_base: *const u8, input_len: usize, pos: Pos) -> Option<u8> {
    let i = pos as usize;
    if i < input_len {
        Some(unsafe { *input_base.add(i) })
    } else {
        None
    }
}

fn run_from_impl(
    graph: &ParseGraph<'_>,
    input: &[u8],
    vm: &mut VmState<'_>,
    start_ip: u32,
    start_pos: Pos,
) -> Result<Pos, ParseError> {
    let mut ip: u32 = start_ip;
    let mut pos: Pos = start_pos;
    let input_base = input.as_ptr();
    let input_len = input.len();

    loop {
        let insn = graph.insn(ip);

        #[cfg(feature = "trace")]
        if let Some(obs) = vm.observer.as_deref_mut() {
            obs.before_insn(ip, pos, insn);
        }

        macro_rules! fail_jump {
            ($on_fail:expr) => {
                fail_or_jump($on_fail, graph, input, &mut ip, &mut pos, vm)?
            };
        }

        // `STAGE_LOOKUP` is built from `is_hot_scan_opcode`, same as `Insn::opcode_u8()`.
        match STAGE_LOOKUP[insn.opcode_u8() as usize] {
            0 => match insn {
                // ── Byte terminals ────────────────────────────────────────────────
                Insn::Byte { byte, on_fail } => {
                    if byte_at(input_base, input_len, pos) == Some(byte) {
                        pos += 1;
                        ip += 1;
                    } else {
                        vm.error_ctx
                            .record(pos, vm.context_stack.as_slice(), Expected::Byte(byte));
                        fail_jump!(on_fail);
                    }
                }

                Insn::ByteEither { a, b, on_fail } => {
                    if let Some(x) = byte_at(input_base, input_len, pos) {
                        if x == a || x == b {
                            pos += 1;
                            ip += 1;
                            continue;
                        }
                    }
                    // Record both possibilities for better errors.
                    vm.error_ctx
                        .record(pos, vm.context_stack.as_slice(), Expected::Byte(a));
                    vm.error_ctx
                        .record(pos, vm.context_stack.as_slice(), Expected::Byte(b));
                    fail_jump!(on_fail);
                }

                Insn::ByteIn3 { a, b, c, on_fail } => {
                    if let Some(x) = byte_at(input_base, input_len, pos) {
                        if x == a || x == b || x == c {
                            pos += 1;
                            ip += 1;
                            continue;
                        }
                    }
                    vm.error_ctx
                        .record(pos, vm.context_stack.as_slice(), Expected::Byte(a));
                    vm.error_ctx
                        .record(pos, vm.context_stack.as_slice(), Expected::Byte(b));
                    vm.error_ctx
                        .record(pos, vm.context_stack.as_slice(), Expected::Byte(c));
                    fail_jump!(on_fail);
                }

                Insn::ByteRange { lo, hi, on_fail } => {
                    if let Some(b) = byte_at(input_base, input_len, pos) {
                        if b >= lo && b <= hi {
                            pos += 1;
                            ip += 1;
                            continue;
                        }
                    }
                    vm.error_ctx.record(
                        pos,
                        vm.context_stack.as_slice(),
                        Expected::ByteRange(lo, hi),
                    );
                    fail_jump!(on_fail);
                }

                Insn::Class {
                    class,
                    label_id,
                    on_fail,
                } => {
                    if let Some(b) = byte_at(input_base, input_len, pos) {
                        if class.contains(b) {
                            pos += 1;
                            ip += 1;
                            continue;
                        }
                    }
                    vm.error_ctx.record(
                        pos,
                        vm.context_stack.as_slice(),
                        Expected::ClassLabel(label_id),
                    );
                    fail_jump!(on_fail);
                }

                Insn::Literal { lit_id, on_fail } => {
                    let lit = graph.literals.get(lit_id);
                    if simd::literal_eq(input, pos, lit) {
                        pos += pos_from_len(lit.len())?;
                        ip += 1;
                    } else {
                        vm.error_ctx.record(
                            pos,
                            vm.context_stack.as_slice(),
                            Expected::Literal(lit_id),
                        );
                        fail_jump!(on_fail);
                    }
                }

                Insn::LiteralSmall {
                    len,
                    bytes,
                    on_fail,
                } => {
                    let n = len as usize;
                    if n > bytes.len() {
                        return Err(ParseError::BadGraph(
                            BadGraphKind::LiteralSmallLenOutOfRange {
                                len,
                                bytes_len: u8::try_from(bytes.len()).unwrap_or(u8::MAX),
                            },
                        ));
                    }
                    if n == 0 {
                        ip += 1;
                        continue;
                    }
                    if simd::literal_small_eq(input, pos as usize, n, &bytes) {
                        pos += pos_from_len(n)?;
                        ip += 1;
                    } else {
                        // We don't have a literal-table id; just record the first byte as a hint.
                        if let Some(&b0) = bytes.first() {
                            vm.error_ctx.record(
                                pos,
                                vm.context_stack.as_slice(),
                                Expected::Byte(b0),
                            );
                        }
                        fail_jump!(on_fail);
                    }
                }

                Insn::Jump { target } => {
                    ip = target;
                }
                Insn::ByteDispatch {
                    table_id,
                    fallback,
                    push_choice_backtrack,
                } => {
                    if let Some(b) = byte_at(input_base, input_len, pos) {
                        let target = graph.dispatch(table_id, b);
                        if target != u32::MAX {
                            if push_choice_backtrack && fallback != u32::MAX {
                                vm.stack.push(Frame::Backtrack {
                                    alt: fallback,
                                    saved_pos: pos,
                                    capture_mark: u32::try_from(vm.events.len()).unwrap_or(0),
                                    tree_mark: u32::try_from(vm.tree_events.len()).unwrap_or(0),
                                    open_tokens_mark: u32::try_from(vm.open_tokens.len())
                                        .unwrap_or(0),
                                    context_mark: u32::try_from(vm.context_stack.len())
                                        .unwrap_or(0),
                                });
                            }
                            ip = target;
                            continue;
                        }
                    }
                    if fallback != u32::MAX {
                        ip = fallback;
                    } else {
                        do_fail(graph, input, &mut ip, &mut pos, vm)?;
                    }
                }
                Insn::ConsumeWhileClass {
                    class,
                    label_id,
                    min,
                    on_fail,
                } => {
                    let start = pos;
                    let mut i = pos as usize;
                    while i < input_len {
                        let b = unsafe { *input_base.add(i) };
                        if !class.contains(b) {
                            break;
                        }
                        i += 1;
                    }
                    pos = pos_from_len(i)?;
                    let consumed = pos.saturating_sub(start) as u32;
                    if consumed >= min {
                        ip += 1;
                    } else {
                        // Failure is at the start position (where the run was required).
                        vm.error_ctx.record(
                            start,
                            vm.context_stack.as_slice(),
                            Expected::ClassLabel(label_id),
                        );
                        // Restore pos so backtracking sees the pre-attempt state.
                        pos = start;
                        fail_jump!(on_fail);
                    }
                }
                _ => {
                    debug_assert!(
                        false,
                        "STAGE_LOOKUP disagrees with Insn::opcode_u8 (expected scan-hot)"
                    );
                    unsafe { core::hint::unreachable_unchecked() }
                }
            },
            _ => match insn {
                Insn::EndOfInput { on_fail } => {
                    if pos as usize == input_len {
                        ip += 1;
                    } else {
                        vm.error_ctx
                            .record(pos, vm.context_stack.as_slice(), Expected::EndOfInput);
                        fail_jump!(on_fail);
                    }
                }

                Insn::Fail => {
                    do_fail(graph, input, &mut ip, &mut pos, vm)?;
                }

                // ── Unicode codepoint terminals ───────────────────────────────────
                Insn::AnyChar { on_fail } => {
                    if let Some((_cp, len)) = decode_utf8(input, pos as usize) {
                        pos += pos_from_len(len)?;
                        ip += 1;
                    } else {
                        vm.error_ctx
                            .record(pos, vm.context_stack.as_slice(), Expected::AnyChar);
                        fail_jump!(on_fail);
                    }
                }

                Insn::Char { codepoint, on_fail } => {
                    if let Some((cp, len)) = decode_utf8(input, pos as usize) {
                        if cp == codepoint {
                            pos += pos_from_len(len)?;
                            ip += 1;
                            continue;
                        }
                    }
                    vm.error_ctx.record(
                        pos,
                        vm.context_stack.as_slice(),
                        Expected::Char(codepoint),
                    );
                    fail_jump!(on_fail);
                }

                Insn::CharRange { lo, hi, on_fail } => {
                    if let Some((cp, len)) = decode_utf8(input, pos as usize) {
                        if cp >= lo && cp <= hi {
                            pos += pos_from_len(len)?;
                            ip += 1;
                            continue;
                        }
                    }
                    vm.error_ctx.record(
                        pos,
                        vm.context_stack.as_slice(),
                        Expected::CharRange(lo, hi),
                    );
                    fail_jump!(on_fail);
                }

                Insn::Call { rule } => {
                    let entry = graph.rule_entry.get(rule as usize).copied().ok_or(
                        ParseError::BadGraph(BadGraphKind::RuleEntryOutOfRange { rule }),
                    )?;

                    // Record that we're trying this rule so diagnostics can show "expected <rule>".
                    vm.error_ctx
                        .record(pos, vm.context_stack.as_slice(), Expected::Rule(rule));

                    #[cfg(feature = "trace")]
                    if let Some(obs) = vm.observer.as_deref_mut() {
                        obs.on_call(rule, pos);
                    }
                    #[cfg(feature = "trace")]
                    if let Some(t) = vm.tracer.as_deref_mut() {
                        let name = graph.rule_name(rule).unwrap_or("?");
                        t.on_call(name, pos);
                    }

                    #[cfg(feature = "std")]
                    {
                        if let Some(mt) = vm.memo {
                            match mt.query_replay(rule, pos, vm.events, vm.tree_events) {
                                MemoReplay::Hit { end_pos } => {
                                    pos = end_pos;
                                    ip += 1;
                                }
                                MemoReplay::Miss { furthest } => {
                                    vm.error_ctx.record(
                                        furthest,
                                        vm.context_stack.as_slice(),
                                        Expected::Rule(rule),
                                    );
                                    do_fail(graph, input, &mut ip, &mut pos, vm)?;
                                }
                                MemoReplay::Unknown => {
                                    vm.stack.push(Frame::MemoReturn {
                                        ret_ip: ip + 1,
                                        rule,
                                        start_pos: pos,
                                        events_mark: u32::try_from(vm.events.len()).unwrap_or(0),
                                        tree_mark: u32::try_from(vm.tree_events.len()).unwrap_or(0),
                                    });
                                    #[cfg(feature = "trace")]
                                    vm.rule_stack.push(rule);
                                    ip = entry;
                                }
                            }
                        } else {
                            vm.stack.push(Frame::Return { ret_ip: ip + 1 });
                            #[cfg(feature = "trace")]
                            vm.rule_stack.push(rule);
                            ip = entry;
                        }
                    }
                    #[cfg(not(feature = "std"))]
                    {
                        vm.stack.push(Frame::Return { ret_ip: ip + 1 });
                        #[cfg(feature = "trace")]
                        vm.rule_stack.push(rule);
                        ip = entry;
                    }
                }

                Insn::Return => {
                    loop {
                        match vm.stack.pop() {
                            Some(Frame::Return { ret_ip }) => {
                                #[cfg(feature = "trace")]
                                if let Some(t) = vm.tracer.as_deref_mut() {
                                    if let Some(r) = vm.rule_stack.pop() {
                                        let name = graph.rule_name(r).unwrap_or("?");
                                        t.on_return(name, pos, true);
                                    }
                                }
                                if ret_ip == u32::MAX {
                                    return Ok(pos);
                                }
                                ip = ret_ip;
                                break;
                            }
                            #[cfg(feature = "std")]
                            Some(Frame::MemoReturn {
                                ret_ip,
                                rule,
                                start_pos,
                                events_mark,
                                tree_mark,
                            }) => {
                                #[cfg(feature = "std")]
                                if let Some(m) = vm.memo {
                                    let em = events_mark as usize;
                                    let tm = tree_mark as usize;
                                    let captures = vm.events.get(em..).unwrap_or(&[]);
                                    let tree = vm.tree_events.get(tm..).unwrap_or(&[]);
                                    m.insert_hit(rule, start_pos, pos, captures, tree);
                                }
                                if ret_ip == u32::MAX {
                                    return Ok(pos);
                                }
                                ip = ret_ip;
                                break;
                            }
                            Some(Frame::ContextSave { snapshot_mark }) => {
                                restore_snapshot(vm.flags, vm.ctx_snapshots, snapshot_mark)?;
                            }
                            Some(Frame::Backtrack { .. }) => { /* defensive */ }
                            Some(f @ (Frame::Recover { .. } | Frame::RecoverSync { .. })) => {
                                vm.stack.push(f);
                                return Err(ParseError::BadGraph(
                                    BadGraphKind::ReturnWithRecoverFrameOnStack,
                                ));
                            }
                            Some(Frame::_Reserved { .. }) => {
                                return Err(ParseError::BadGraph(BadGraphKind::ReservedFrame));
                            }
                            None => {
                                return Err(ParseError::BadGraph(
                                    BadGraphKind::UnexpectedFrameDuringReturn,
                                ));
                            }
                        }
                    }
                }

                // ── Backtracking ──────────────────────────────────────────────────
                Insn::Choice { alt } => {
                    vm.stack.push(Frame::Backtrack {
                        alt,
                        saved_pos: pos,
                        capture_mark: u32::try_from(vm.events.len()).unwrap_or(0),
                        tree_mark: u32::try_from(vm.tree_events.len()).unwrap_or(0),
                        open_tokens_mark: u32::try_from(vm.open_tokens.len()).unwrap_or(0),
                        context_mark: u32::try_from(vm.context_stack.len()).unwrap_or(0),
                    });
                    ip += 1;
                }

                Insn::Commit { target } => {
                    pop_backtrack(vm.stack)?;
                    ip = target;
                }

                Insn::BackCommit { target } => {
                    pop_backtrack_commit(
                        vm.stack,
                        &mut pos,
                        vm.events,
                        vm.tree_events,
                        vm.open_tokens,
                        vm.context_stack,
                    )?;
                    ip = target;
                }

                Insn::NegBackCommit { target } => {
                    pop_backtrack_commit(
                        vm.stack,
                        &mut pos,
                        vm.events,
                        vm.tree_events,
                        vm.open_tokens,
                        vm.context_stack,
                    )?;
                    ip = target;
                    do_fail(graph, input, &mut ip, &mut pos, vm)?;
                }

                // Fix: PartialCommit must update ALL backtrack marks so that a
                // failed later iteration does not discard events from already-committed
                // earlier iterations.
                Insn::PartialCommit { target } => {
                    update_backtrack_marks(
                        vm.stack,
                        pos,
                        u32::try_from(vm.events.len()).unwrap_or(0),
                        u32::try_from(vm.tree_events.len()).unwrap_or(0),
                        u32::try_from(vm.open_tokens.len()).unwrap_or(0),
                        u32::try_from(vm.context_stack.len()).unwrap_or(0),
                    )?;
                    ip = target;
                }
                Insn::IfFlag { flag_id, on_fail } => {
                    if flag_is_set(vm.flags, flag_id) {
                        ip += 1;
                    } else {
                        vm.error_ctx.record(
                            pos,
                            vm.context_stack.as_slice(),
                            Expected::Flag {
                                id: flag_id,
                                required: true,
                            },
                        );
                        fail_jump!(on_fail);
                    }
                }

                Insn::IfNotFlag { flag_id, on_fail } => {
                    if flag_is_set(vm.flags, flag_id) {
                        vm.error_ctx.record(
                            pos,
                            vm.context_stack.as_slice(),
                            Expected::Flag {
                                id: flag_id,
                                required: false,
                            },
                        );
                        fail_jump!(on_fail);
                    } else {
                        ip += 1;
                    }
                }

                Insn::PushFlags { mask_id } => {
                    let entries = graph.flag_masks.get(mask_id);
                    let mark = u32::try_from(vm.ctx_snapshots.len()).unwrap_or(0);
                    for e in entries {
                        let w = e.word as usize;
                        let old = if w < vm.flags.len() { vm.flags[w] } else { 0 };
                        vm.ctx_snapshots.push(SnapEntry {
                            word: e.word,
                            val: old,
                        });
                    }
                    FlagMask { entries }.apply(vm.flags);
                    vm.stack.push(Frame::ContextSave {
                        snapshot_mark: mark,
                    });
                    ip += 1;
                }

                Insn::PopFlags => {
                    let mark = pop_context_save(vm.stack)?;
                    restore_snapshot(vm.flags, vm.ctx_snapshots, mark)?;
                    ip += 1;
                }

                // ── Legacy captures ───────────────────────────────────────────────
                Insn::CaptureBegin { tag } => {
                    vm.events.push(CaptureEvent::Open { tag, pos });
                    ip += 1;
                }
                Insn::CaptureEnd { tag } => {
                    vm.events.push(CaptureEvent::Close { tag, pos });
                    ip += 1;
                }

                // ── Green/Red tree ────────────────────────────────────────────────
                Insn::NodeBegin { kind, field } => {
                    vm.tree_events
                        .push(TreeEvent::NodeOpen { kind, field, pos });
                    ip += 1;
                }

                Insn::NodeEnd => {
                    vm.tree_events.push(TreeEvent::NodeClose { pos });
                    ip += 1;
                }

                Insn::TokenBegin { kind, is_trivia } => {
                    vm.open_tokens.push((kind, is_trivia, pos));
                    ip += 1;
                }

                Insn::TokenEnd => {
                    let (kind, is_trivia, start) = vm
                        .open_tokens
                        .pop()
                        .ok_or(ParseError::BadGraph(BadGraphKind::TokenStackUnderflow))?;
                    vm.tree_events.push(TreeEvent::Token {
                        kind,
                        start,
                        end: pos,
                        is_trivia,
                    });
                    ip += 1;
                }

                Insn::RecordExpectedLabel { label_id } => {
                    vm.error_ctx.record(
                        pos,
                        vm.context_stack.as_slice(),
                        Expected::Label(label_id),
                    );
                    ip += 1;
                }

                Insn::PushDiagnosticContext { label_id } => {
                    vm.context_stack.push(label_id);
                    ip += 1;
                }

                Insn::PopDiagnosticContext => {
                    vm.context_stack.pop();
                    ip += 1;
                }

                Insn::SetHint { hint_id } => {
                    vm.error_ctx
                        .record_hint(pos, vm.context_stack.as_slice(), hint_id);
                    ip += 1;
                }

                Insn::TracePoint { .. } => {
                    #[cfg(feature = "trace")]
                    if let Some(t) = vm.tracer.as_deref_mut() {
                        if let Insn::TracePoint { label_id } = insn {
                            let label = graph.strings.resolve(label_id);
                            t.on_trace(label, pos, ip);
                        }
                    }
                    ip += 1;
                }

                // ── Error recovery ────────────────────────────────────────────────
                Insn::RecoverUntil { sync_rule, resume } => {
                    vm.stack.push(Frame::Recover {
                        sync_rule,
                        resume,
                        capture_mark: u32::try_from(vm.events.len()).unwrap_or(0),
                        tree_mark: u32::try_from(vm.tree_events.len()).unwrap_or(0),
                        open_tokens_mark: u32::try_from(vm.open_tokens.len()).unwrap_or(0),
                        context_mark: u32::try_from(vm.context_stack.len()).unwrap_or(0),
                    });
                    ip += 1;
                }

                Insn::RecoveryResume => {
                    // Body succeeded or sync rule matched; pop Recover or RecoverSync and continue.
                    let mut put_back = Vec::new();
                    let mut found = false;
                    while let Some(f) = vm.stack.pop() {
                        if matches!(f, Frame::RecoverSync { .. } | Frame::Recover { .. }) {
                            found = true;
                            break;
                        }
                        put_back.push(f);
                    }
                    for f in put_back.into_iter().rev() {
                        vm.stack.push(f);
                    }
                    if !found {
                        return Err(ParseError::BadGraph(
                            BadGraphKind::RecoveryResumeWithoutRecoverFrame,
                        ));
                    }
                    ip += 1;
                }

                Insn::Accept => return Ok(pos),
                _ => {
                    debug_assert!(
                        false,
                        "STAGE_LOOKUP disagrees with Insn::opcode_u8 (expected general)"
                    );
                    unsafe { core::hint::unreachable_unchecked() }
                }
            },
        }
    }
}

// ─── Frame-stack helpers ──────────────────────────────────────────────────────

#[inline]
fn fail_or_jump(
    on_fail: u32,
    graph: &ParseGraph<'_>,
    input: &[u8],
    ip: &mut u32,
    pos: &mut Pos,
    vm: &mut VmState<'_>,
) -> Result<(), ParseError> {
    if on_fail == u32::MAX {
        do_fail(graph, input, ip, pos, vm)
    } else {
        *ip = on_fail;
        Ok(())
    }
}

/// Advance `pos` by one byte (or to end of input). Used during error recovery skip.
#[inline]
const fn recovery_advance(pos: &mut Pos, input: &[u8]) {
    if (*pos as usize) < input.len() {
        *pos += 1;
    }
}

#[inline]
fn do_fail(
    graph: &ParseGraph<'_>,
    input: &[u8],
    ip: &mut u32,
    pos: &mut Pos,
    vm: &mut VmState<'_>,
) -> Result<(), ParseError> {
    loop {
        match vm.stack.pop() {
            Some(Frame::Backtrack {
                alt,
                saved_pos,
                capture_mark,
                tree_mark,
                open_tokens_mark,
                context_mark,
            }) => {
                *ip = alt;
                *pos = saved_pos;
                vm.events.truncate(capture_mark as usize);
                vm.tree_events.truncate(tree_mark as usize);
                vm.open_tokens.truncate(open_tokens_mark as usize);
                vm.context_stack.truncate(context_mark as usize);
                return Ok(());
            }
            Some(Frame::ContextSave { snapshot_mark }) => {
                restore_snapshot(vm.flags, vm.ctx_snapshots, snapshot_mark)?;
            }
            Some(Frame::Return { .. }) =>
            {
                #[cfg(feature = "trace")]
                if let Some(t) = vm.tracer.as_deref_mut() {
                    if let Some(r) = vm.rule_stack.pop() {
                        let name = graph.rule_name(r).unwrap_or("?");
                        t.on_return(name, *pos, false);
                    }
                }
            }
            #[cfg(feature = "std")]
            Some(Frame::MemoReturn {
                rule, start_pos, ..
            }) => {
                if let Some(m) = vm.memo {
                    m.insert_miss(rule, start_pos, vm.error_ctx.furthest);
                }
                #[cfg(feature = "trace")]
                if let Some(t) = vm.tracer.as_deref_mut() {
                    if let Some(r) = vm.rule_stack.pop() {
                        let name = graph.rule_name(r).unwrap_or("?");
                        t.on_return(name, *pos, false);
                    }
                }
            }
            Some(Frame::Recover {
                sync_rule,
                resume,
                capture_mark,
                tree_mark,
                open_tokens_mark,
                context_mark,
            }) => {
                vm.events.truncate(capture_mark as usize);
                vm.tree_events.truncate(tree_mark as usize);
                vm.open_tokens.truncate(open_tokens_mark as usize);
                vm.context_stack.truncate(context_mark as usize);
                if let Some(errs) = vm.multi_errors {
                    errs.push(ParseError::NoMatch(vm.error_ctx.to_diagnostic()));
                    if errs.len() >= vm.max_errors {
                        return Err(ParseError::NoMatch(vm.error_ctx.to_diagnostic()));
                    }
                }
                recovery_advance(pos, input);
                if (*pos as usize) >= input.len() {
                    // Synced to EOI; don't jump back into the recovery body — keep popping.
                } else {
                    let entry =
                        *graph
                            .rule_entry
                            .get(sync_rule as usize)
                            .ok_or(ParseError::BadGraph(
                                BadGraphKind::SyncRuleEntryOutOfRange { rule: sync_rule },
                            ))?;
                    vm.stack.push(Frame::RecoverSync { sync_rule, resume });
                    vm.stack.push(Frame::Return { ret_ip: resume });
                    *ip = entry;
                    return Ok(());
                }
            }
            Some(Frame::RecoverSync { sync_rule, resume }) => {
                // Sync probing: advance without recording another diagnostic — the statement error was
                // already collected when we first popped [`Frame::Recover`].
                recovery_advance(pos, input);
                if (*pos as usize) >= input.len() {
                    // Synced to EOI; don't jump back into the recovery body — keep popping.
                } else {
                    let entry =
                        *graph
                            .rule_entry
                            .get(sync_rule as usize)
                            .ok_or(ParseError::BadGraph(
                                BadGraphKind::SyncRuleEntryOutOfRange { rule: sync_rule },
                            ))?;
                    vm.stack.push(Frame::RecoverSync { sync_rule, resume });
                    vm.stack.push(Frame::Return { ret_ip: resume });
                    *ip = entry;
                    return Ok(());
                }
            }
            Some(Frame::_Reserved { .. }) => {
                return Err(ParseError::BadGraph(BadGraphKind::ReservedFrame));
            }
            None => return Err(no_match(&*vm.error_ctx)),
        }
    }
}

#[inline]
fn pop_backtrack(stack: &mut Vec<Frame>) -> Result<(), ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { .. }) => return Ok(()),
            Some(_) => {}
            None => {
                return Err(ParseError::BadGraph(BadGraphKind::BacktrackStackUnderflow));
            }
        }
    }
}

/// Pop the nearest `Backtrack` frame and restore position, captures, tree, and open-token state.
///
/// Used by [`Insn::BackCommit`] / [`Insn::NegBackCommit`] so positive/negative lookahead probes
/// do not leave synthesized tree events behind when input is rewound.
#[inline]
fn pop_backtrack_commit(
    stack: &mut Vec<Frame>,
    pos: &mut Pos,
    events: &mut Vec<CaptureEvent>,
    tree_events: &mut Vec<TreeEvent>,
    open_tokens: &mut Vec<(SyntaxKind, bool, Pos)>,
    context_stack: &mut Vec<u32>,
) -> Result<(), ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack {
                saved_pos,
                capture_mark,
                tree_mark,
                open_tokens_mark,
                context_mark,
                ..
            }) => {
                *pos = saved_pos;
                events.truncate(capture_mark as usize);
                tree_events.truncate(tree_mark as usize);
                open_tokens.truncate(open_tokens_mark as usize);
                context_stack.truncate(context_mark as usize);
                return Ok(());
            }
            Some(_) => {}
            None => {
                return Err(ParseError::BadGraph(BadGraphKind::BacktrackStackUnderflow));
            }
        }
    }
}

/// Update ALL marks in the nearest `Backtrack` frame (used by `PartialCommit`).
#[inline]
fn update_backtrack_marks(
    stack: &mut [Frame],
    new_pos: Pos,
    new_capture_mark: u32,
    new_tree_mark: u32,
    new_open_mark: u32,
    new_context_mark: u32,
) -> Result<(), ParseError> {
    for frame in stack.iter_mut().rev() {
        if let Frame::Backtrack {
            saved_pos,
            capture_mark,
            tree_mark,
            open_tokens_mark,
            context_mark,
            ..
        } = frame
        {
            *saved_pos = new_pos;
            *capture_mark = new_capture_mark;
            *tree_mark = new_tree_mark;
            *open_tokens_mark = new_open_mark;
            *context_mark = new_context_mark;
            return Ok(());
        }
    }
    Err(ParseError::BadGraph(
        BadGraphKind::PartialCommitWithoutBacktrack,
    ))
}

#[inline]
fn pop_context_save(stack: &mut Vec<Frame>) -> Result<u32, ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::ContextSave { snapshot_mark }) => return Ok(snapshot_mark),
            Some(_) => {}
            None => {
                return Err(ParseError::BadGraph(BadGraphKind::PopFlagsStackUnderflow));
            }
        }
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;
    use crate::parse::builder::GrammarBuilder;
    use crate::types::classes;

    fn minimal_graph() -> crate::parse::builder::BuiltGraph {
        let mut g = GrammarBuilder::new();
        g.rule("start", |g| {
            g.byte(b'a');
            g.end_of_input();
            g.accept();
        });
        g.finish().expect("minimal graph valid")
    }

    #[test]
    fn parse_success() {
        let built = minimal_graph();
        let graph = built.as_graph();
        let mut engine = crate::parse::engine::Engine::new();
        let out = engine.parse(&graph, b"a").expect("parse ok");
        assert_eq!(out.consumed, 1);
    }

    #[test]
    fn parse_fail_no_match_diagnostic_has_expected() {
        let built = minimal_graph();
        let graph = built.as_graph();
        let mut engine = crate::parse::engine::Engine::new();
        let err = engine.parse(&graph, b"b").unwrap_err();
        let crate::parse::engine::ParseError::NoMatch(diag) = err else {
            panic!("expected NoMatch");
        };
        assert_eq!(diag.furthest, 0);
        assert!(
            !diag.expected.is_empty(),
            "diagnostic should list expected tokens"
        );
        assert!(
            diag.expected
                .iter()
                .any(|e| matches!(e, Expected::Byte(0x61)))
        );
    }

    #[test]
    fn parse_fail_empty_input() {
        let built = minimal_graph();
        let graph = built.as_graph();
        let mut engine = crate::parse::engine::Engine::new();
        let err = engine.parse(&graph, b"").unwrap_err();
        let crate::parse::engine::ParseError::NoMatch(diag) = err else {
            panic!("expected NoMatch");
        };
        assert_eq!(diag.furthest, 0);
        assert!(!diag.expected.is_empty());
    }

    #[test]
    fn recover_until_skips_to_sync_then_continues() {
        let mut g = GrammarBuilder::new();
        g.rule("start", |g| {
            g.zero_or_more(|g| {
                g.recover_until("semi", |g| {
                    g.call("statement");
                });
            });
            g.end_of_input();
            g.accept();
        });
        g.rule("semi", |g| {
            g.literal(b";");
        });
        g.rule("statement", |g| {
            g.class(classes::LOWER); // one letter
        });
        let built = g.finish().expect("grammar valid");
        let graph = built.as_graph();
        let mut engine = crate::parse::engine::Engine::new();
        let out = engine.parse(&graph, b"a;b;").expect("parse ok");
        assert_eq!(out.consumed, 4);
        let out = engine.parse(&graph, b"a x ; b ;").expect("parse ok");
        assert_eq!(out.consumed, 9);
    }

    #[test]
    fn consume_while_class_min_enforced() {
        let mut g = GrammarBuilder::new();
        g.rule("start", |g| {
            g.consume_while_class(classes::DIGIT, 1);
            g.end_of_input();
            g.accept();
        });
        let built = g.finish().unwrap();
        let graph = built.as_graph();
        let mut engine = crate::parse::engine::Engine::new();
        assert!(engine.parse(&graph, b"123").is_ok());
        assert!(engine.parse(&graph, b"").is_err());
        assert!(engine.parse(&graph, b"a").is_err());
    }

    #[test]
    fn set_hint_attaches_to_furthest_position() {
        let mut g = GrammarBuilder::new();
        g.rule("start", |g| {
            g.hint("did you mean 'a'?");
            g.byte(b'a');
            g.end_of_input();
            g.accept();
        });
        let built = g.finish().unwrap();
        let graph = built.as_graph();
        let mut engine = crate::parse::engine::Engine::new();
        let err = engine.parse(&graph, b"b").unwrap_err();
        let crate::parse::engine::ParseError::NoMatch(diag) = err else {
            panic!("expected NoMatch");
        };
        let msg = diag.message(Some(&graph.literals), Some(&graph));
        assert!(msg.contains("hint: did you mean 'a'?"), "{msg}");
    }
}
