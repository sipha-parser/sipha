use super::error::{no_match, BadGraphKind, ParseError};
use super::flags::{flag_is_set, restore_snapshot};
use super::frames::{Frame, SnapEntry};
use super::utf8::decode_utf8;
use crate::diagnostics::error::{ErrorContext, Expected};
use crate::parse::context::FlagMask;
use crate::parse::insn::{Insn, ParseGraph};
use crate::parse::memo::{MemoReplay, MemoTable};
use crate::parse::simd;
use crate::types::{CaptureEvent, Pos, SyntaxKind, TreeEvent};

/// Mutable VM buffers threaded through the parse VM (`run` and failure handling).
pub(super) struct VmState<'a> {
    pub(super) stack: &'a mut Vec<Frame>,
    pub(super) events: &'a mut Vec<CaptureEvent>,
    pub(super) tree_events: &'a mut Vec<TreeEvent>,
    pub(super) open_tokens: &'a mut Vec<(SyntaxKind, bool, Pos)>,
    pub(super) memo: &'a mut Option<MemoTable>,
    pub(super) error_ctx: &'a mut ErrorContext,
    pub(super) flags: &'a mut Vec<u64>,
    pub(super) ctx_snapshots: &'a mut Vec<SnapEntry>,
    pub(super) multi_errors: &'a mut Option<&'a mut Vec<ParseError>>,
    pub(super) max_errors: usize,
}

#[inline(never)]
#[allow(clippy::too_many_lines)] // VM dispatch loop; splitting would obscure control flow
pub(super) fn run(
    graph: &ParseGraph<'_>,
    input: &[u8],
    vm: &mut VmState<'_>,
) -> Result<Pos, ParseError> {
    let mut ip: u32 = graph.start();
    let mut pos: Pos = 0;

    loop {
        let insn = graph.insn(ip);

        macro_rules! fail_jump {
            ($on_fail:expr) => {
                fail_or_jump($on_fail, graph, input, &mut ip, &mut pos, vm)?
            };
        }

        match insn {
            // ── Byte terminals ────────────────────────────────────────────────
            Insn::Byte { byte, on_fail } => {
                if input.get(pos as usize).copied() == Some(byte) {
                    pos += 1;
                    ip += 1;
                } else {
                    vm.error_ctx.record(pos, Expected::Byte(byte));
                    fail_jump!(on_fail);
                }
            }

            Insn::ByteRange { lo, hi, on_fail } => {
                if let Some(b) = input.get(pos as usize).copied() {
                    if b >= lo && b <= hi {
                        pos += 1;
                        ip += 1;
                        continue;
                    }
                }
                vm.error_ctx.record(pos, Expected::ByteRange(lo, hi));
                fail_jump!(on_fail);
            }

            Insn::Class {
                class,
                label_id,
                on_fail,
            } => {
                if let Some(b) = input.get(pos as usize).copied() {
                    if class.contains(b) {
                        pos += 1;
                        ip += 1;
                        continue;
                    }
                }
                vm.error_ctx.record(pos, Expected::ClassLabel(label_id));
                fail_jump!(on_fail);
            }

            Insn::Literal { lit_id, on_fail } => {
                let lit = graph.literals.get(lit_id);
                if simd::literal_eq(input, pos, lit) {
                    pos += Pos::try_from(lit.len()).unwrap_or(0);
                    ip += 1;
                } else {
                    vm.error_ctx.record(pos, Expected::Literal(lit_id));
                    fail_jump!(on_fail);
                }
            }

            Insn::EndOfInput { on_fail } => {
                if pos as usize == input.len() {
                    ip += 1;
                } else {
                    vm.error_ctx.record(pos, Expected::EndOfInput);
                    fail_jump!(on_fail);
                }
            }

            Insn::Fail => {
                do_fail(graph, input, &mut ip, &mut pos, vm)?;
            }

            // ── Unicode codepoint terminals ───────────────────────────────────
            Insn::AnyChar { on_fail } => {
                if let Some((_cp, len)) = decode_utf8(input, pos as usize) {
                    pos += Pos::try_from(len).unwrap_or(0);
                    ip += 1;
                } else {
                    vm.error_ctx.record(pos, Expected::AnyChar);
                    fail_jump!(on_fail);
                }
            }

            Insn::Char { codepoint, on_fail } => {
                if let Some((cp, len)) = decode_utf8(input, pos as usize) {
                    if cp == codepoint {
                        pos += Pos::try_from(len).unwrap_or(0);
                        ip += 1;
                        continue;
                    }
                }
                vm.error_ctx.record(pos, Expected::Char(codepoint));
                fail_jump!(on_fail);
            }

            Insn::CharRange { lo, hi, on_fail } => {
                if let Some((cp, len)) = decode_utf8(input, pos as usize) {
                    if cp >= lo && cp <= hi {
                        pos += Pos::try_from(len).unwrap_or(0);
                        ip += 1;
                        continue;
                    }
                }
                vm.error_ctx.record(pos, Expected::CharRange(lo, hi));
                fail_jump!(on_fail);
            }

            // ── Control flow ──────────────────────────────────────────────────
            Insn::Jump { target } => {
                ip = target;
            }

            Insn::Call { rule } => {
                let entry =
                    graph
                        .rule_entry
                        .get(rule as usize)
                        .copied()
                        .ok_or(ParseError::BadGraph(BadGraphKind::RuleEntryOutOfRange {
                            rule,
                        }))?;

                // Record that we're trying this rule so diagnostics can show "expected <rule>".
                vm.error_ctx.record(pos, Expected::Rule(rule));

                if let Some(mt) = vm.memo {
                    match mt.query_replay(rule, pos, vm.events, vm.tree_events) {
                        MemoReplay::Hit { end_pos } => {
                            pos = end_pos;
                            ip += 1;
                        }
                        MemoReplay::Miss { furthest } => {
                            vm.error_ctx.record(furthest, Expected::Rule(rule));
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
                            ip = entry;
                        }
                    }
                } else {
                    vm.stack.push(Frame::Return { ret_ip: ip + 1 });
                    ip = entry;
                }
            }

            Insn::Return => {
                loop {
                    match vm.stack.pop() {
                        Some(Frame::Return { ret_ip }) => {
                            ip = ret_ip;
                            break;
                        }
                        Some(Frame::MemoReturn {
                            ret_ip,
                            rule,
                            start_pos,
                            events_mark,
                            tree_mark,
                        }) => {
                            if let Some(m) = vm.memo {
                                let stored_events = vm.events[events_mark as usize..]
                                    .to_vec()
                                    .into_boxed_slice();
                                let stored_tree = vm.tree_events[tree_mark as usize..]
                                    .to_vec()
                                    .into_boxed_slice();
                                m.insert_hit(rule, start_pos, pos, stored_events, stored_tree);
                            }
                            ip = ret_ip;
                            break;
                        }
                        Some(Frame::ContextSave { snapshot_mark }) => {
                            restore_snapshot(vm.flags, vm.ctx_snapshots, snapshot_mark);
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
                });
                ip += 1;
            }

            Insn::Commit { target } => {
                pop_backtrack(vm.stack)?;
                ip = target;
            }

            Insn::BackCommit { target } => {
                pos = pop_backtrack_pos(vm.stack)?;
                ip = target;
            }

            Insn::NegBackCommit { target } => {
                pos = pop_backtrack_pos(vm.stack)?;
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
                )?;
                ip = target;
            }

            // ── O(1) dispatch ────────────────────────────────────────────────
            Insn::ByteDispatch { table_id } => {
                if let Some(b) = input.get(pos as usize).copied() {
                    let target = graph.dispatch(table_id, b);
                    if target != u32::MAX {
                        ip = target;
                        continue;
                    }
                }
                do_fail(graph, input, &mut ip, &mut pos, vm)?;
            }

            // ── Context flags ─────────────────────────────────────────────────
            Insn::IfFlag { flag_id, on_fail } => {
                if flag_is_set(vm.flags, flag_id) {
                    ip += 1;
                } else {
                    vm.error_ctx.record(
                        pos,
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
                restore_snapshot(vm.flags, vm.ctx_snapshots, mark);
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
                vm.error_ctx.record(pos, Expected::Label(label_id));
                ip += 1;
            }

            // ── Error recovery ────────────────────────────────────────────────
            Insn::RecoverUntil { sync_rule, resume } => {
                vm.stack.push(Frame::Recover { sync_rule, resume });
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
            }) => {
                *ip = alt;
                *pos = saved_pos;
                vm.events.truncate(capture_mark as usize);
                vm.tree_events.truncate(tree_mark as usize);
                vm.open_tokens.truncate(open_tokens_mark as usize);
                return Ok(());
            }
            Some(Frame::ContextSave { snapshot_mark }) => {
                restore_snapshot(vm.flags, vm.ctx_snapshots, snapshot_mark);
            }
            Some(Frame::Return { .. }) => {}
            Some(Frame::MemoReturn {
                rule, start_pos, ..
            }) => {
                if let Some(m) = vm.memo {
                    m.insert_miss(rule, start_pos, vm.error_ctx.furthest);
                }
            }
            Some(
                Frame::Recover { sync_rule, resume } | Frame::RecoverSync { sync_rule, resume },
            ) => {
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

#[inline]
fn pop_backtrack_pos(stack: &mut Vec<Frame>) -> Result<Pos, ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { saved_pos, .. }) => return Ok(saved_pos),
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
) -> Result<(), ParseError> {
    for frame in stack.iter_mut().rev() {
        if let Frame::Backtrack {
            saved_pos,
            capture_mark,
            tree_mark,
            open_tokens_mark,
            ..
        } = frame
        {
            *saved_pos = new_pos;
            *capture_mark = new_capture_mark;
            *tree_mark = new_tree_mark;
            *open_tokens_mark = new_open_mark;
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

#[cfg(test)]
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
        assert!(diag
            .expected
            .iter()
            .any(|e| matches!(e, Expected::Byte(0x61))));
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
}
