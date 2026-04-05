//! Post-build optimization of parse bytecode.
//!
//! ## Choice fusion
//!
//! A right-nested spine of [`Insn::Choice`](crate::parse::insn::Insn::Choice) /
//! [`Insn::Commit`](crate::parse::insn::Insn::Commit) (from
//! [`GrammarBuilder::choice`](super::builder::GrammarBuilder::choice) /
//! [`choices`](super::builder::GrammarBuilder::choices)) can become
//! [`Insn::ByteDispatch`](crate::parse::insn::Insn::ByteDispatch) when every arm’s leading
//! discriminant is byte-level (after an optional trivia [`Insn::Call`]).
//!
//! - **Flat dispatch** is used only when pairwise first-byte [`CharClass`] sets are disjoint, so
//!   each input byte selects at most one arm (PEG order preserved). Unmapped bytes and the original
//!   [`Insn::Choice::alt`](Insn::Choice) are wired as [`Insn::ByteDispatch::fallback`](Insn::ByteDispatch)
//!   so later alternatives still run.
//! - When classes overlap, arms are fused into a **prefix trie**: each collision on the next
//!   input byte becomes a shared [`Insn::Byte`] consume plus an inner [`Insn::ByteDispatch`]
//!   (`push_choice_backtrack: false`), repeated for long shared prefixes (depth-capped). The trie
//!   root dispatch uses `push_choice_backtrack: true` so shared-prefix consumption still
//!   backtracks like a leading [`Insn::Choice`]. Discriminators may be
//!   [`Insn::Byte`], [`Insn::ByteEither`], [`Insn::ByteIn3`], [`Insn::ByteRange`], or
//!   single-byte [`Insn::LiteralSmall`]. Multi-byte `LiteralSmall` participates only in
//!   single-arm buckets (cannot split inside the insn).
//!
//! ## Compaction and peepholes
//!
//! After fusion, [`compact_graph`] removes unreachable insns (reachable from any rule entry),
//! remaps all [`InsnId`] edges and jump-table cells, and runs **jump-chain folding**
//! ([`Insn::Jump`] → final non-`Jump` target). [`gc_unused_jump_tables`] then drops unreferenced
//! tables; [`dedup_jump_tables`] merges identical 256-way tables.

use std::collections::HashMap;

use crate::parse::insn::Insn;
use crate::types::{CharClass, InsnId, RuleId};

/// Last [`Insn::Commit`] in `insns[start..end)` (end exclusive), scanning backwards.
fn last_commit_target_in_range(insns: &[Insn], start: usize, end: usize) -> Option<InsnId> {
    if start >= end || end > insns.len() {
        return None;
    }
    insns[start..end].iter().rev().find_map(|i| match i {
        Insn::Commit { target } => Some(*target),
        _ => None,
    })
}

/// Split a right-nested ordered-choice spine starting at `start` (must be [`Insn::Choice`]).
/// Returns each arm’s half-open `[entry, end)` insn range and the shared merge point.
fn split_ordered_choice_spine(
    insns: &[Insn],
    start: usize,
) -> Option<(Vec<(usize, usize)>, usize)> {
    let mut segments: Vec<(usize, usize)> = Vec::new();
    let mut cur = start;
    loop {
        let Insn::Choice { alt } = insns.get(cur)? else {
            return None;
        };
        let alt = *alt as usize;
        let arm_begin = cur + 1;
        if alt <= arm_begin {
            return None;
        }
        segments.push((arm_begin, alt));
        match insns.get(alt) {
            Some(Insn::Choice { .. }) => cur = alt,
            _ => {
                let merge = last_commit_target_in_range(insns, segments[0].0, segments[0].1)?;
                let m = merge as usize;
                if m > insns.len() {
                    return None;
                }
                for &(s, e) in &segments[1..] {
                    let t = last_commit_target_in_range(insns, s, e)?;
                    if t != merge {
                        return None;
                    }
                }
                if alt > m {
                    return None;
                }
                segments.push((alt, m));
                return Some((segments, m));
            }
        }
    }
}

/// Byte set used to pick the first matching arm (PEG order: smallest index wins).
fn arm_discriminant_class(
    insns: &[Insn],
    range: (usize, usize),
    trivia_rule: Option<RuleId>,
) -> Option<CharClass> {
    let (mut i, end) = range;
    if i >= end || end > insns.len() {
        return None;
    }
    while i < end {
        match insns[i] {
            Insn::Call { rule } if Some(rule) == trivia_rule => {
                i += 1;
            }
            Insn::Choice { .. } | Insn::IfFlag { .. } | Insn::IfNotFlag { .. } => return None,
            Insn::Byte { byte, .. } => return Some(CharClass::from_byte(byte)),
            Insn::ByteEither { a, b, .. } => {
                return Some(CharClass::from_byte(a).union(CharClass::from_byte(b)));
            }
            Insn::ByteIn3 { a, b, c, .. } => {
                return Some(
                    CharClass::from_byte(a)
                        .union(CharClass::from_byte(b))
                        .union(CharClass::from_byte(c)),
                );
            }
            Insn::ByteRange { lo, hi, .. } => return Some(CharClass::EMPTY.with_range(lo, hi)),
            Insn::LiteralSmall { len, bytes, .. } if len >= 1 => {
                return Some(CharClass::from_byte(bytes[0]));
            }
            _ => return None,
        }
    }
    None
}

#[inline]
fn char_classes_disjoint(a: CharClass, b: CharClass) -> bool {
    (a.0[0] & b.0[0] | a.0[1] & b.0[1] | a.0[2] & b.0[2] | a.0[3] & b.0[3]) == 0
}

fn classes_pairwise_disjoint(classes: &[CharClass]) -> bool {
    for i in 0..classes.len() {
        for j in (i + 1)..classes.len() {
            if !char_classes_disjoint(classes[i], classes[j]) {
                return false;
            }
        }
    }
    true
}

/// Maximum trie depth (shared-prefix levels) to avoid pathological stack or bytecode growth.
const MAX_PREFIX_TRIE_DEPTH: usize = 48;

fn skip_arm_trivia(
    insns: &[Insn],
    mut ip: usize,
    end: usize,
    trivia_rule: Option<RuleId>,
) -> usize {
    while ip < end {
        match insns[ip] {
            Insn::Call { rule } if Some(rule) == trivia_rule => ip += 1,
            _ => break,
        }
    }
    ip
}

/// `true` when `ip` is not at a byte-level trie discriminator (end of literal prefix).
fn cursor_past_discriminators(
    insns: &[Insn],
    arm: (usize, usize),
    ip: usize,
    trivia_rule: Option<RuleId>,
) -> bool {
    let end = arm.1;
    let i = skip_arm_trivia(insns, ip, end, trivia_rule);
    if i >= end {
        return true;
    }
    !matches!(
        insns[i],
        Insn::Byte { .. }
            | Insn::ByteEither { .. }
            | Insn::ByteIn3 { .. }
            | Insn::ByteRange { .. }
            | Insn::LiteralSmall { .. }
    )
}

/// If the next insn at `ip` accepts `b`, return `(match_ip, ip after that insn)`.
/// `multi_byte_literal` is set when the insn is [`Insn::LiteralSmall`] with len > 1 (cannot share
/// a trie bucket with another arm on that first byte).
fn try_match_one_byte(
    insns: &[Insn],
    arm: (usize, usize),
    ip: usize,
    b: u8,
) -> Option<(usize, usize, bool)> {
    let end = arm.1;
    if ip >= end || ip >= insns.len() {
        return None;
    }
    let match_ip = ip;
    match insns[ip] {
        Insn::Byte { byte, .. } if byte == b => Some((match_ip, ip + 1, false)),
        Insn::ByteEither { a, b: b2, .. } if b == a || b == b2 => Some((match_ip, ip + 1, false)),
        Insn::ByteIn3 { a, b: b2, c, .. } if b == a || b == b2 || b == c => {
            Some((match_ip, ip + 1, false))
        }
        Insn::ByteRange { lo, hi, .. } if b >= lo && b <= hi => Some((match_ip, ip + 1, false)),
        Insn::LiteralSmall { len, bytes, .. } if len >= 1 && bytes[0] == b => {
            let multi = len > 1;
            Some((match_ip, ip + 1, multi))
        }
        _ => None,
    }
}

/// Jump target for [`Insn::ByteDispatch`]: trivia at the arm head must run first, so use
/// `arm.0` when the match is on the first discriminating insn after trivia.
fn trie_dispatch_target(
    insns: &[Insn],
    arm: (usize, usize),
    match_ip: usize,
    trivia_rule: Option<RuleId>,
) -> u32 {
    let first = skip_arm_trivia(insns, arm.0, arm.1, trivia_rule);
    if match_ip == first {
        arm.0 as u32
    } else {
        match_ip as u32
    }
}

fn patch_commit_to_jump(insns: &mut [Insn], arms: &[(usize, usize)]) -> Option<()> {
    for &(s, e) in &arms[..arms.len() - 1] {
        if s >= e {
            return None;
        }
        let last = e - 1;
        match insns[last] {
            Insn::Commit { target } => {
                insns[last] = Insn::Jump { target };
            }
            _ => return None,
        }
    }
    Some(())
}

fn try_fuse_flat_byte_dispatch(
    insns: &mut [Insn],
    jump_tables: &mut Vec<[u32; 256]>,
    arms: &[(usize, usize)],
    classes: &[CharClass],
    merge: usize,
    start: usize,
    peg_fallback: InsnId,
) -> Option<usize> {
    let mut table = [u32::MAX; 256];
    for b in 0u16..=255 {
        let byte = b as u8;
        let mut chosen: Option<usize> = None;
        for (arm_idx, cls) in classes.iter().enumerate() {
            if cls.contains(byte) {
                chosen = Some(arm_idx);
                break;
            }
        }
        if let Some(idx) = chosen {
            table[byte as usize] = arms[idx].0 as u32;
        }
    }
    patch_commit_to_jump(insns, arms)?;
    let table_id = u32::try_from(jump_tables.len()).ok()?;
    jump_tables.push(table);
    insns[start] = Insn::ByteDispatch {
        table_id,
        fallback: peg_fallback,
        push_choice_backtrack: false,
    };
    Some(merge)
}

/// `states`: `(arm_index, insn_ip)` cursors inside each arm after any already-matched prefix.
fn emit_cursor_prefix_trie(
    insns: &mut Vec<Insn>,
    jump_tables: &mut Vec<[u32; 256]>,
    arms: &[(usize, usize)],
    states: &[(usize, usize)],
    trivia_rule: Option<RuleId>,
    depth: usize,
) -> Option<u32> {
    if depth > MAX_PREFIX_TRIE_DEPTH {
        return None;
    }
    if states.is_empty() {
        return None;
    }
    let ins0 = insns.as_slice();
    for &(ai, ip) in states {
        let arm = *arms.get(ai)?;
        if ip >= arm.1 || cursor_past_discriminators(ins0, arm, ip, trivia_rule) {
            return None;
        }
    }

    let mut table = [u32::MAX; 256];
    for b in 0u16..=255 {
        let ins = insns.as_slice();
        let byte = b as u8;
        let mut hits: Vec<(usize, usize, usize, bool)> = Vec::new();
        for &(arm_idx, ip) in states {
            let arm = arms[arm_idx];
            if let Some((m, n, multi_lit)) = try_match_one_byte(ins, arm, ip, byte) {
                if n > arm.1 {
                    return None;
                }
                hits.push((arm_idx, m, n, multi_lit));
            }
        }
        if hits.is_empty() {
            continue;
        }
        if hits.len() > 1 && hits.iter().any(|(_, _, _, multi)| *multi) {
            return None;
        }

        let mut any_past = false;
        let mut any_not_past = false;
        for &(arm_idx, _, next_ip, _) in &hits {
            let arm = arms[arm_idx];
            if cursor_past_discriminators(ins, arm, next_ip, trivia_rule) {
                any_past = true;
            } else {
                any_not_past = true;
            }
        }
        if any_past && any_not_past {
            return None;
        }
        if hits.len() > 1 && any_past {
            return None;
        }

        if hits.len() == 1 {
            let (arm_idx, match_ip, _, _) = hits[0];
            table[byte as usize] =
                trie_dispatch_target(insns.as_slice(), arms[arm_idx], match_ip, trivia_rule);
            continue;
        }

        let base = u32::try_from(insns.len()).ok()?;
        insns.push(Insn::Byte {
            byte,
            on_fail: InsnId::MAX,
        });
        let inner_states: Vec<(usize, usize)> =
            hits.iter().map(|(ai, _, n, _)| (*ai, *n)).collect();
        let inner_tid = emit_cursor_prefix_trie(
            insns,
            jump_tables,
            arms,
            &inner_states,
            trivia_rule,
            depth + 1,
        )?;
        insns.push(Insn::ByteDispatch {
            table_id: inner_tid,
            fallback: InsnId::MAX,
            push_choice_backtrack: false,
        });
        table[byte as usize] = base;
    }

    let tid = u32::try_from(jump_tables.len()).ok()?;
    jump_tables.push(table);
    Some(tid)
}

fn try_fuse_prefix_trie(
    insns: &mut Vec<Insn>,
    jump_tables: &mut Vec<[u32; 256]>,
    arms: &[(usize, usize)],
    merge: usize,
    start: usize,
    trivia_rule: Option<RuleId>,
    peg_fallback: InsnId,
) -> Option<usize> {
    let mut states: Vec<(usize, usize)> = Vec::with_capacity(arms.len());
    for (i, &arm) in arms.iter().enumerate() {
        let ip = skip_arm_trivia(insns.as_slice(), arm.0, arm.1, trivia_rule);
        if ip >= arm.1 || cursor_past_discriminators(insns.as_slice(), arm, ip, trivia_rule) {
            return None;
        }
        states.push((i, ip));
    }

    let root_tid = emit_cursor_prefix_trie(insns, jump_tables, arms, &states, trivia_rule, 0)?;
    insns[start] = Insn::ByteDispatch {
        table_id: root_tid,
        fallback: peg_fallback,
        push_choice_backtrack: true,
    };
    Some(merge)
}

/// Try to replace the choice spine at `start` with [`Insn::ByteDispatch`]. Returns `Some(merge_ip)`
/// when fused so callers can skip scanning the interior of the old spine.
pub(crate) fn try_fuse_choice_spine(
    insns: &mut Vec<Insn>,
    jump_tables: &mut Vec<[u32; 256]>,
    start: usize,
    trivia_rule: Option<RuleId>,
) -> Option<usize> {
    let peg_fallback = match insns.get(start)? {
        Insn::Choice { alt } => *alt,
        _ => return None,
    };
    let (arms, merge) = split_ordered_choice_spine(insns, start)?;
    if arms.len() < 2 {
        return None;
    }

    let mut classes: Vec<CharClass> = Vec::with_capacity(arms.len());
    for &arm in &arms {
        classes.push(arm_discriminant_class(insns, arm, trivia_rule)?);
    }

    if classes_pairwise_disjoint(&classes) {
        try_fuse_flat_byte_dispatch(
            insns,
            jump_tables,
            &arms,
            &classes,
            merge,
            start,
            peg_fallback,
        )
    } else {
        try_fuse_prefix_trie(
            insns,
            jump_tables,
            &arms,
            merge,
            start,
            trivia_rule,
            peg_fallback,
        )
    }
}

fn fold_jump_chains(insns: &mut [Insn]) {
    let len = insns.len();
    if len == 0 {
        return;
    }
    for i in (0..len).rev() {
        let Insn::Jump { target } = insns[i] else {
            continue;
        };
        let mut t = target;
        let mut guard = 0usize;
        while guard <= len {
            guard += 1;
            match insns.get(t as usize) {
                Some(Insn::Jump { target: t2 }) if *t2 != t => t = *t2,
                _ => break,
            }
        }
        insns[i] = Insn::Jump { target: t };
    }
}

fn push_ip(out: &mut Vec<usize>, ip: usize, insn_len: usize) {
    if ip < insn_len {
        out.push(ip);
    }
}

fn push_target(out: &mut Vec<usize>, id: InsnId, insn_len: usize) {
    if id != InsnId::MAX {
        push_ip(out, id as usize, insn_len);
    }
}

fn append_successors(
    insns: &[Insn],
    jump_tables: &[[u32; 256]],
    rule_entry: &[InsnId],
    ip: usize,
    out: &mut Vec<usize>,
) {
    let len = insns.len();
    let insn = match insns.get(ip) {
        Some(i) => i,
        None => return,
    };
    use Insn::*;
    match *insn {
        Jump { target } => push_target(out, target, len),
        Call { rule } => {
            if let Some(&e) = rule_entry.get(rule as usize) {
                push_target(out, e, len);
            }
            push_ip(out, ip + 1, len);
        }
        Return | Fail | Accept | NegBackCommit { .. } => {}
        Choice { alt } => {
            push_ip(out, ip + 1, len);
            push_target(out, alt, len);
        }
        Commit { target } | BackCommit { target } | PartialCommit { target } => {
            push_target(out, target, len);
        }
        ByteDispatch {
            table_id, fallback, ..
        } => {
            if let Some(t) = jump_tables.get(table_id as usize) {
                for &cell in t.iter() {
                    if cell != u32::MAX {
                        push_ip(out, cell as usize, len);
                    }
                }
            }
            push_target(out, fallback, len);
        }
        RecoverUntil { resume, .. } => {
            push_ip(out, ip + 1, len);
            push_target(out, resume, len);
        }
        Byte { on_fail, .. }
        | ByteEither { on_fail, .. }
        | ByteIn3 { on_fail, .. }
        | ByteRange { on_fail, .. }
        | Class { on_fail, .. }
        | Literal { on_fail, .. }
        | LiteralSmall { on_fail, .. }
        | EndOfInput { on_fail, .. }
        | AnyChar { on_fail, .. }
        | Char { on_fail, .. }
        | CharRange { on_fail, .. }
        | ConsumeWhileClass { on_fail, .. }
        | IfFlag { on_fail, .. }
        | IfNotFlag { on_fail, .. } => {
            push_ip(out, ip + 1, len);
            push_target(out, on_fail, len);
        }
        PushFlags { .. }
        | PopFlags
        | CaptureBegin { .. }
        | CaptureEnd { .. }
        | NodeBegin { .. }
        | NodeEnd
        | TokenBegin { .. }
        | TokenEnd
        | RecordExpectedLabel { .. }
        | PushDiagnosticContext { .. }
        | PopDiagnosticContext
        | SetHint { .. }
        | TracePoint { .. }
        | RecoveryResume => push_ip(out, ip + 1, len),
    }
}

fn compute_reachable(
    insns: &[Insn],
    jump_tables: &[[u32; 256]],
    rule_entry: &[InsnId],
) -> Vec<bool> {
    let n = insns.len();
    let mut vis = vec![false; n];
    let mut stack: Vec<usize> = Vec::new();
    for &e in rule_entry {
        push_ip(&mut stack, e as usize, n);
    }
    let mut succ = Vec::new();
    while let Some(ip) = stack.pop() {
        if ip >= n || vis[ip] {
            continue;
        }
        vis[ip] = true;
        succ.clear();
        append_successors(insns, jump_tables, rule_entry, ip, &mut succ);
        for &s in &succ {
            if s < n && !vis[s] {
                stack.push(s);
            }
        }
    }
    vis
}

fn map_insn_id(m: &[u32], id: InsnId) -> InsnId {
    if id == InsnId::MAX {
        return InsnId::MAX;
    }
    m[id as usize]
}

fn remap_insn(insn: &Insn, m: &[u32]) -> Insn {
    use Insn::*;
    match *insn {
        Byte { byte, on_fail } => Byte {
            byte,
            on_fail: map_insn_id(m, on_fail),
        },
        ByteEither { a, b, on_fail } => ByteEither {
            a,
            b,
            on_fail: map_insn_id(m, on_fail),
        },
        ByteIn3 { a, b, c, on_fail } => ByteIn3 {
            a,
            b,
            c,
            on_fail: map_insn_id(m, on_fail),
        },
        ByteRange { lo, hi, on_fail } => ByteRange {
            lo,
            hi,
            on_fail: map_insn_id(m, on_fail),
        },
        Class {
            class,
            label_id,
            on_fail,
        } => Class {
            class,
            label_id,
            on_fail: map_insn_id(m, on_fail),
        },
        Literal { lit_id, on_fail } => Literal {
            lit_id,
            on_fail: map_insn_id(m, on_fail),
        },
        LiteralSmall {
            len,
            bytes,
            on_fail,
        } => LiteralSmall {
            len,
            bytes,
            on_fail: map_insn_id(m, on_fail),
        },
        EndOfInput { on_fail } => EndOfInput {
            on_fail: map_insn_id(m, on_fail),
        },
        Fail => Fail,
        AnyChar { on_fail } => AnyChar {
            on_fail: map_insn_id(m, on_fail),
        },
        Char { codepoint, on_fail } => Char {
            codepoint,
            on_fail: map_insn_id(m, on_fail),
        },
        CharRange { lo, hi, on_fail } => CharRange {
            lo,
            hi,
            on_fail: map_insn_id(m, on_fail),
        },
        Jump { target } => Jump {
            target: map_insn_id(m, target),
        },
        Call { rule } => Call { rule },
        Return => Return,
        Choice { alt } => Choice {
            alt: map_insn_id(m, alt),
        },
        Commit { target } => Commit {
            target: map_insn_id(m, target),
        },
        BackCommit { target } => BackCommit {
            target: map_insn_id(m, target),
        },
        NegBackCommit { target } => NegBackCommit {
            target: map_insn_id(m, target),
        },
        PartialCommit { target } => PartialCommit {
            target: map_insn_id(m, target),
        },
        ByteDispatch {
            table_id,
            fallback,
            push_choice_backtrack,
        } => ByteDispatch {
            table_id,
            fallback: map_insn_id(m, fallback),
            push_choice_backtrack,
        },
        ConsumeWhileClass {
            class,
            label_id,
            min,
            on_fail,
        } => ConsumeWhileClass {
            class,
            label_id,
            min,
            on_fail: map_insn_id(m, on_fail),
        },
        IfFlag { flag_id, on_fail } => IfFlag {
            flag_id,
            on_fail: map_insn_id(m, on_fail),
        },
        IfNotFlag { flag_id, on_fail } => IfNotFlag {
            flag_id,
            on_fail: map_insn_id(m, on_fail),
        },
        PushFlags { mask_id } => PushFlags { mask_id },
        PopFlags => PopFlags,
        CaptureBegin { tag } => CaptureBegin { tag },
        CaptureEnd { tag } => CaptureEnd { tag },
        NodeBegin { kind, field } => NodeBegin { kind, field },
        NodeEnd => NodeEnd,
        TokenBegin { kind, is_trivia } => TokenBegin { kind, is_trivia },
        TokenEnd => TokenEnd,
        RecordExpectedLabel { label_id } => RecordExpectedLabel { label_id },
        PushDiagnosticContext { label_id } => PushDiagnosticContext { label_id },
        PopDiagnosticContext => PopDiagnosticContext,
        SetHint { hint_id } => SetHint { hint_id },
        TracePoint { label_id } => TracePoint { label_id },
        RecoverUntil { sync_rule, resume } => RecoverUntil {
            sync_rule,
            resume: map_insn_id(m, resume),
        },
        RecoveryResume => RecoveryResume,
        Accept => Accept,
    }
}

fn remap_jump_table(table: &mut [u32; 256], m: &[u32]) {
    for cell in table.iter_mut() {
        if *cell != u32::MAX {
            *cell = map_insn_id(m, *cell);
        }
    }
}

#[allow(clippy::ptr_arg)] // `*insns = new_insns` requires owning reassignment on the vec.
fn compact_graph(insns: &mut Vec<Insn>, jump_tables: &mut [[u32; 256]], rule_entry: &mut [InsnId]) {
    fold_jump_chains(insns.as_mut_slice());
    let vis = compute_reachable(insns, jump_tables, rule_entry);
    if vis.iter().all(|&v| v) {
        fold_jump_chains(insns.as_mut_slice());
        return;
    }
    let n = insns.len();
    let mut m = vec![u32::MAX; n];
    let mut next = 0u32;
    for i in 0..n {
        if vis[i] {
            m[i] = next;
            next += 1;
        }
    }
    let new_insns: Vec<Insn> = (0..n)
        .filter(|&i| vis[i])
        .map(|i| remap_insn(&insns[i], &m))
        .collect();
    *insns = new_insns;
    for t in jump_tables.iter_mut() {
        remap_jump_table(t, &m);
    }
    for e in rule_entry.iter_mut() {
        *e = map_insn_id(&m, *e);
    }
    fold_jump_chains(insns.as_mut_slice());
}

/// Drop jump tables not referenced by any [`Insn::ByteDispatch`] (after compaction, dead tables
/// from old fusion attempts may remain).
fn gc_unused_jump_tables(insns: &mut [Insn], jump_tables: &mut Vec<[u32; 256]>) {
    let n = jump_tables.len();
    if n == 0 {
        return;
    }
    let mut used = vec![false; n];
    for insn in insns.iter() {
        if let Insn::ByteDispatch { table_id, .. } = insn {
            let t = *table_id as usize;
            if t < n {
                used[t] = true;
            }
        }
    }
    if used.iter().all(|&u| u) {
        return;
    }
    let mut map = vec![u32::MAX; n];
    let mut new_tables: Vec<[u32; 256]> = Vec::with_capacity(n);
    for i in 0..n {
        if used[i] {
            map[i] = new_tables.len() as u32;
            new_tables.push(jump_tables[i]);
        }
    }
    *jump_tables = new_tables;
    for insn in insns.iter_mut() {
        if let Insn::ByteDispatch { table_id, .. } = insn {
            let o = *table_id as usize;
            if o < map.len() && map[o] != u32::MAX {
                *table_id = map[o];
            }
        }
    }
}

/// FNV-1a over the raw table bytes — used only as a hint; [`dedup_jump_tables`] always verifies
/// with full equality so hash collisions cannot merge distinct tables.
#[inline]
fn jump_table_fingerprint(t: &[u32; 256]) -> u64 {
    const OFFSET: u64 = 14695981039346656037;
    const PRIME: u64 = 1099511628211;
    let mut h = OFFSET;
    for &w in t {
        for b in w.to_le_bytes() {
            h ^= u64::from(b);
            h = h.wrapping_mul(PRIME);
        }
    }
    h
}

/// Merge identical 256-entry dispatch tables and rewrite [`Insn::ByteDispatch`] indices.
fn dedup_jump_tables(insns: &mut [Insn], jump_tables: &mut Vec<[u32; 256]>) {
    let n = jump_tables.len();
    if n <= 1 {
        return;
    }
    let mut canon: HashMap<u64, u32> = HashMap::new();
    let mut new_tables: Vec<[u32; 256]> = Vec::with_capacity(n);
    let mut old_to_new: Vec<u32> = vec![0; n];
    for (i, t) in jump_tables.iter().enumerate() {
        let fp = jump_table_fingerprint(t);
        let mut id = None;
        if let Some(&cand) = canon.get(&fp) {
            let ci = cand as usize;
            if ci < new_tables.len() && new_tables[ci] == *t {
                id = Some(cand);
            }
        }
        if id.is_none() {
            id = new_tables.iter().position(|x| *x == *t).map(|j| j as u32);
        }
        let id = match id {
            Some(id) => id,
            None => {
                let new_id = u32::try_from(new_tables.len()).expect("jump table count fits in u32");
                new_tables.push(*t);
                canon.insert(fp, new_id);
                new_id
            }
        };
        old_to_new[i] = id;
    }
    if new_tables.len() == n {
        return;
    }
    *jump_tables = new_tables;
    for insn in insns.iter_mut() {
        if let Insn::ByteDispatch { table_id, .. } = insn {
            let o = *table_id as usize;
            if o < old_to_new.len() {
                *table_id = old_to_new[o];
            }
        }
    }
}

/// Scan bytecode and fuse eligible choice spines, then compact unreachable insns.
pub(crate) fn optimize_graph(
    insns: &mut Vec<Insn>,
    jump_tables: &mut Vec<[u32; 256]>,
    rule_entry: &mut [InsnId],
    trivia_rule: Option<RuleId>,
) {
    let mut i = 0usize;
    while i < insns.len() {
        if let Insn::Choice { .. } = insns[i] {
            if let Some(merge) = try_fuse_choice_spine(insns, jump_tables, i, trivia_rule) {
                i = merge.max(i + 1);
                continue;
            }
        }
        i += 1;
    }
    compact_graph(insns, jump_tables, rule_entry);
    gc_unused_jump_tables(insns.as_mut_slice(), jump_tables);
    dedup_jump_tables(insns.as_mut_slice(), jump_tables);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::builder::GrammarBuilder;

    #[test]
    fn fuses_three_way_byte_choice() {
        let mut g = GrammarBuilder::new();
        g.parser_rule("r", |g| {
            g.choice(
                |g| {
                    g.byte(b'a');
                },
                |g| {
                    g.choice(
                        |g| {
                            g.byte(b'b');
                        },
                        |g| {
                            g.byte(b'c');
                        },
                    );
                },
            );
        });
        let mut built = g.finish().expect("finish");
        optimize_graph(
            &mut built.insns,
            &mut built.jump_tables,
            &mut built.rule_entry,
            None,
        );
        let entry = built.rule_entry[0] as usize;
        assert!(
            matches!(built.insns[entry], Insn::ByteDispatch { .. }),
            "expected ByteDispatch, got {:?}",
            built.insns[entry]
        );
    }

    #[test]
    fn fuses_shared_first_byte_two_level_trie() {
        use crate::parse::engine::Engine;

        let mut g = GrammarBuilder::new();
        g.parser_rule("start", |g| {
            g.choice(
                |g| {
                    g.byte(b'i');
                    g.byte(b'f');
                },
                |g| {
                    g.choice(
                        |g| {
                            g.byte(b'i');
                            g.byte(b'n');
                        },
                        |g| {
                            g.byte(b'i');
                            g.byte(b'z');
                        },
                    );
                },
            );
            g.end_of_input();
            g.accept();
        });
        let mut built = g.finish().expect("finish");
        optimize_graph(
            &mut built.insns,
            &mut built.jump_tables,
            &mut built.rule_entry,
            None,
        );
        let entry = built.rule_entry[0] as usize;
        assert!(
            matches!(built.insns[entry], Insn::ByteDispatch { .. }),
            "expected root ByteDispatch, got {:?}",
            built.insns[entry]
        );
        let graph = built.as_graph();
        let mut engine = Engine::new();
        assert!(engine.parse(&graph, b"if").is_ok());
        assert!(engine.parse(&graph, b"in").is_ok());
        assert!(engine.parse(&graph, b"iz").is_ok());
        assert!(engine.parse(&graph, b"ix").is_err());
    }

    #[test]
    fn fuses_deep_shared_prefix_trie_foo_vs_for() {
        use crate::parse::engine::Engine;

        let mut g = GrammarBuilder::new();
        g.parser_rule("start", |g| {
            g.choice(
                |g| {
                    g.byte(b'f');
                    g.byte(b'o');
                    g.byte(b'o');
                },
                |g| {
                    g.byte(b'f');
                    g.byte(b'o');
                    g.byte(b'r');
                },
            );
            g.end_of_input();
            g.accept();
        });
        let mut built = g.finish().expect("finish");
        optimize_graph(
            &mut built.insns,
            &mut built.jump_tables,
            &mut built.rule_entry,
            None,
        );
        let entry = built.rule_entry[0] as usize;
        assert!(
            matches!(built.insns[entry], Insn::ByteDispatch { .. }),
            "expected root ByteDispatch, got {:?}",
            built.insns[entry]
        );
        let graph = built.as_graph();
        let mut engine = Engine::new();
        assert!(engine.parse(&graph, b"foo").is_ok());
        assert!(engine.parse(&graph, b"for").is_ok());
        assert!(engine.parse(&graph, b"fo").is_err());
        assert!(engine.parse(&graph, b"fox").is_err());
    }

    #[test]
    fn trie_byte_either_overlaps_byte_then_inner_dispatch() {
        use crate::parse::engine::Engine;

        let mut g = GrammarBuilder::new();
        g.parser_rule("start", |g| {
            g.choice(
                |g| {
                    g.byte_either(b'a', b'b');
                    g.byte(b'x');
                },
                |g| {
                    g.byte(b'a');
                    g.byte(b'y');
                },
            );
            g.end_of_input();
            g.accept();
        });
        let mut built = g.finish().expect("finish");
        optimize_graph(
            &mut built.insns,
            &mut built.jump_tables,
            &mut built.rule_entry,
            None,
        );
        let graph = built.as_graph();
        let mut engine = Engine::new();
        assert!(engine.parse(&graph, b"ax").is_ok());
        assert!(engine.parse(&graph, b"bx").is_ok());
        assert!(engine.parse(&graph, b"ay").is_ok());
        assert!(engine.parse(&graph, b"by").is_err());
    }

    #[test]
    fn dedup_jump_tables_merges_identical() {
        let mut t = [u32::MAX; 256];
        t[b'x' as usize] = 7;
        let mut tables = vec![t, t];
        let mut insns = vec![
            Insn::ByteDispatch {
                table_id: 0,
                fallback: InsnId::MAX,
                push_choice_backtrack: false,
            },
            Insn::ByteDispatch {
                table_id: 1,
                fallback: InsnId::MAX,
                push_choice_backtrack: false,
            },
        ];
        dedup_jump_tables(&mut insns, &mut tables);
        assert_eq!(tables.len(), 1);
        let Insn::ByteDispatch { table_id: id0, .. } = insns[0] else {
            panic!("expected ByteDispatch");
        };
        let Insn::ByteDispatch { table_id: id1, .. } = insns[1] else {
            panic!("expected ByteDispatch");
        };
        assert_eq!(id0, 0);
        assert_eq!(id1, 0);
    }
}
