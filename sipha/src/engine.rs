//! # Parse Engine
//!
//! Iterative, stack-based VM producing both a legacy [`CaptureEvent`] log and
//! a [`TreeEvent`] log for the green/red tree.
//!
//! ## Green/Red tree integration
//!
//! Two new side-buffers exist alongside the capture event log:
//!
//! - `tree_events: Vec<TreeEvent>` — receives `NodeOpen`, `NodeClose`, and
//!   `Token` events emitted by `NodeBegin`/`NodeEnd`/`TokenEnd`.
//! - `open_tokens: Vec<(SyntaxKind, bool, Pos)>` — a stack for in-progress
//!   token spans (`TokenBegin` saves here; `TokenEnd` pops and emits).
//!
//! Both buffers are truncated on backtracking using marks saved in
//! `Frame::Backtrack`, just like the capture event log.
//!
//! ## PartialCommit (zero_or_more) mark update
//!
//! `PartialCommit` updates **all** backtrack marks — `saved_pos`,
//! `capture_mark`, `tree_mark`, and `open_tokens_mark` — so that a failed
//! subsequent iteration does not discard events from earlier completed
//! iterations.

use crate::{
    context::{FlagMask, ParseContext},
    error::{ErrorContext, Expected},
    insn::{Insn, ParseGraph},
    memo::{MemoReplay, MemoTable},
    simd,
    types::{CaptureEvent, Pos, RuleId, SyntaxKind, TreeEvent},
};

// ─── Snapshot entry ───────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct SnapEntry { word: u32, val: u64 }

// ─── Frame ────────────────────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug)]
enum Frame {
    Backtrack {
        alt:              u32,
        saved_pos:        Pos,
        /// Mark into `engine.events`      (legacy captures).
        capture_mark:     u32,
        /// Mark into `engine.tree_events` (green/red tree events).
        tree_mark:        u32,
        /// Mark into `engine.open_tokens` (in-progress token spans).
        open_tokens_mark: u32,
    },
    Return { ret_ip: u32 },
    MemoReturn {
        ret_ip:      u32,
        rule:        RuleId,
        start_pos:   Pos,
        events_mark: u32,
    },
    ContextSave { snapshot_mark: u32 },
}

// ─── Error ────────────────────────────────────────────────────────────────────

#[derive(Clone, Debug)]
pub enum ParseError {
    NoMatch(crate::error::Diagnostic),
    BadGraph,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoMatch(d) => write!(f, "{d}"),
            ParseError::BadGraph   => write!(f, "malformed parse graph"),
        }
    }
}
impl std::error::Error for ParseError {}

#[cfg(feature = "miette")]
impl ParseError {
    /// If this is a parse failure ([`NoMatch`](ParseError::NoMatch)), convert it
    /// into a [`miette::Report`] with source and optional literal table for
    /// pretty-printed diagnostics. Returns `None` for [`BadGraph`](ParseError::BadGraph).
    ///
    /// Requires the `miette` feature. Example:
    ///
    /// ```ignore
    /// if let Err(e) = engine.parse(&graph, &source) {
    ///     if let Some(report) = e.to_miette_report(&source, "file.txt", Some(&graph.literals)) {
    ///         eprintln!("{:?}", report);
    ///     }
    /// }
    /// ```
    pub fn to_miette_report(
        &self,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&crate::insn::LiteralTable>,
    ) -> Option<miette::Report> {
        match self {
            ParseError::NoMatch(diag) => {
                let m = diag.into_miette(source, name, literals);
                Some(miette::Report::new(m))
            }
            ParseError::BadGraph => None,
        }
    }
}

// ─── Output ───────────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct ParseOutput {
    /// Number of input bytes consumed (always equals `input.len()` if the
    /// grammar ends with `end_of_input`).
    pub consumed:    Pos,
    /// Legacy flat capture events.  Use `CaptureNode::build_forest` to get
    /// the capture tree.
    pub events:      Vec<CaptureEvent>,
    /// Green/red tree events.  Pass to `green::build_green_tree` and wrap
    /// the result in `red::SyntaxNode::new_root`.
    pub tree_events: Vec<TreeEvent>,
}

impl ParseOutput {
    /// Build the green tree from this parse output.
    ///
    /// `input` is the raw source bytes used during parsing — token texts are
    /// sliced from it and stored inside each [`crate::green::GreenToken`].
    ///
    /// Returns `None` if `tree_events` is empty or malformed.
    pub fn build_green_tree(&self, input: &[u8]) -> Option<std::sync::Arc<crate::green::GreenNode>> {
        crate::green::build_green_tree(input, &self.tree_events)
    }

    /// Build the green tree and wrap it in a [`crate::red::SyntaxNode`] root.
    ///
    /// This is the most common entry-point for working with the syntax tree
    /// after a successful parse:
    ///
    /// ```rust,ignore
    /// let output = engine.parse(&graph, input)?;
    /// let root   = output.syntax_root(input).unwrap();
    /// for tok in root.descendant_semantic_tokens() {
    ///     println!("{:?} {:?}", tok.kind(), tok.text());
    /// }
    /// ```
    pub fn syntax_root(&self, input: &[u8]) -> Option<crate::red::SyntaxNode> {
        self.build_green_tree(input).map(crate::red::SyntaxNode::new_root)
    }
}

// ─── Engine ───────────────────────────────────────────────────────────────────

pub struct Engine {
    stack:         Vec<Frame>,
    events:        Vec<CaptureEvent>,
    tree_events:   Vec<TreeEvent>,
    /// In-progress token spans: `(kind, is_trivia, start_pos)`.
    /// Pushed by `TokenBegin`, popped by `TokenEnd`.
    open_tokens:   Vec<(SyntaxKind, bool, Pos)>,
    memo:          Option<MemoTable>,
    error_ctx:     ErrorContext,
    flags:         Vec<u64>,
    ctx_snapshots: Vec<SnapEntry>,
}

impl Engine {
    pub fn new() -> Self { Self::with_capacity(1024, 256) }

    pub fn with_capacity(stack_cap: usize, capture_cap: usize) -> Self {
        Self {
            stack:         Vec::with_capacity(stack_cap),
            events:        Vec::with_capacity(capture_cap),
            tree_events:   Vec::with_capacity(capture_cap),
            open_tokens:   Vec::with_capacity(16),
            memo:          None,
            error_ctx:     ErrorContext::new(),
            flags:         Vec::with_capacity(4),
            ctx_snapshots: Vec::with_capacity(64),
        }
    }

    pub fn with_memo(mut self) -> Self {
        self.memo = Some(MemoTable::new());
        self
    }

    pub fn parse(
        &mut self,
        graph: &ParseGraph,
        input: &[u8],
    ) -> Result<ParseOutput, ParseError> {
        self.parse_with_context(graph, input, &ParseContext::new())
    }

    pub fn parse_with_context(
        &mut self,
        graph:   &ParseGraph,
        input:   &[u8],
        context: &ParseContext,
    ) -> Result<ParseOutput, ParseError> {
        self.stack.clear();
        self.events.clear();
        self.tree_events.clear();
        self.open_tokens.clear();
        self.error_ctx.clear();
        self.ctx_snapshots.clear();
        if let Some(m) = &mut self.memo { m.clear(); }

        self.flags.clear();
        self.flags.extend_from_slice(context.words());

        match run(
            graph,
            input,
            &mut self.stack,
            &mut self.events,
            &mut self.tree_events,
            &mut self.open_tokens,
            &mut self.memo,
            &mut self.error_ctx,
            &mut self.flags,
            &mut self.ctx_snapshots,
        ) {
            Ok(consumed) => Ok(ParseOutput {
                consumed,
                events:      std::mem::take(&mut self.events),
                tree_events: std::mem::take(&mut self.tree_events),
            }),
            Err(e) => Err(e),
        }
    }

    pub fn error_context(&self) -> &ErrorContext { &self.error_ctx }
    pub fn memo_len(&self) -> Option<usize>      { self.memo.as_ref().map(|m| m.len()) }
    pub fn flag_words(&self) -> &[u64]            { &self.flags }
}

impl Default for Engine { fn default() -> Self { Self::new() } }

// ─── VM loop ─────────────────────────────────────────────────────────────────

#[inline(never)]
#[allow(clippy::too_many_arguments)]
fn run(
    graph:         &ParseGraph,
    input:         &[u8],
    stack:         &mut Vec<Frame>,
    events:        &mut Vec<CaptureEvent>,
    tree_events:   &mut Vec<TreeEvent>,
    open_tokens:   &mut Vec<(SyntaxKind, bool, Pos)>,
    memo:          &mut Option<MemoTable>,
    error_ctx:     &mut ErrorContext,
    flags:         &mut Vec<u64>,
    ctx_snapshots: &mut Vec<SnapEntry>,
) -> Result<Pos, ParseError> {
    let mut ip:  u32 = graph.start();
    let mut pos: Pos = 0;

    loop {
        let insn = graph.insn(ip);

        macro_rules! fail_jump {
            ($on_fail:expr) => {
                fail_or_jump($on_fail, stack, &mut ip, &mut pos,
                    flags, ctx_snapshots, events, tree_events, open_tokens,
                    memo, error_ctx)?
            };
        }

        match insn {
            // ── Byte terminals ────────────────────────────────────────────────

            Insn::Byte { byte, on_fail } => {
                if input.get(pos as usize).copied() == Some(byte) {
                    pos += 1; ip += 1;
                } else {
                    error_ctx.record(pos, Expected::Byte(byte));
                    fail_jump!(on_fail);
                }
            }

            Insn::ByteRange { lo, hi, on_fail } => {
                if let Some(b) = input.get(pos as usize).copied() {
                    if b >= lo && b <= hi { pos += 1; ip += 1; continue; }
                }
                error_ctx.record(pos, Expected::ByteRange(lo, hi));
                fail_jump!(on_fail);
            }

            Insn::Class { class, on_fail } => {
                if let Some(b) = input.get(pos as usize).copied() {
                    if class.contains(b) { pos += 1; ip += 1; continue; }
                }
                // No per-class label in the instruction; use a generic description.
                error_ctx.record(pos, Expected::Class("character class"));
                fail_jump!(on_fail);
            }

            Insn::Literal { lit_id, on_fail } => {
                let lit = graph.literals.get(lit_id);
                if simd::literal_eq(input, pos, lit) {
                    pos += lit.len() as Pos;
                    ip  += 1;
                } else {
                    error_ctx.record(pos, Expected::Literal(lit_id));
                    fail_jump!(on_fail);
                }
            }

            Insn::EndOfInput { on_fail } => {
                if pos as usize == input.len() {
                    ip += 1;
                } else {
                    error_ctx.record(pos, Expected::EndOfInput);
                    fail_jump!(on_fail);
                }
            }

            Insn::Fail => {
                do_fail(stack, &mut ip, &mut pos, flags, ctx_snapshots,
                        events, tree_events, open_tokens, memo, error_ctx)?;
            }

            // ── Unicode codepoint terminals ───────────────────────────────────

            Insn::AnyChar { on_fail } => {
                if let Some((_cp, len)) = decode_utf8(input, pos as usize) {
                    pos += len as Pos; ip += 1;
                } else {
                    error_ctx.record(pos, Expected::AnyChar);
                    fail_jump!(on_fail);
                }
            }

            Insn::Char { codepoint, on_fail } => {
                if let Some((cp, len)) = decode_utf8(input, pos as usize) {
                    if cp == codepoint { pos += len as Pos; ip += 1; continue; }
                }
                error_ctx.record(pos, Expected::Char(codepoint));
                fail_jump!(on_fail);
            }

            Insn::CharRange { lo, hi, on_fail } => {
                if let Some((cp, len)) = decode_utf8(input, pos as usize) {
                    if cp >= lo && cp <= hi { pos += len as Pos; ip += 1; continue; }
                }
                error_ctx.record(pos, Expected::CharRange(lo, hi));
                fail_jump!(on_fail);
            }

            // ── Control flow ──────────────────────────────────────────────────

            Insn::Jump   { target } => { ip = target; }

            Insn::Call { rule } => {
                let entry = graph.rule_entry.get(rule as usize).copied()
                    .ok_or(ParseError::BadGraph)?;

                if let Some(mt) = memo {
                    match mt.query_replay(rule, pos, events) {
                        MemoReplay::Hit { end_pos } => {
                            pos = end_pos;
                            ip  += 1;
                            continue;
                        }
                        MemoReplay::Miss { furthest } => {
                            error_ctx.record(furthest, Expected::EndOfInput);
                            do_fail(stack, &mut ip, &mut pos, flags, ctx_snapshots,
                                    events, tree_events, open_tokens, memo, error_ctx)?;
                            continue;
                        }
                        MemoReplay::Unknown => {
                            stack.push(Frame::MemoReturn {
                                ret_ip:      ip + 1,
                                rule,
                                start_pos:   pos,
                                events_mark: events.len() as u32,
                            });
                            ip = entry;
                        }
                    }
                } else {
                    stack.push(Frame::Return { ret_ip: ip + 1 });
                    ip = entry;
                }
            }

            Insn::Return => {
                loop {
                    match stack.pop() {
                        Some(Frame::Return { ret_ip }) => { ip = ret_ip; break; }
                        Some(Frame::MemoReturn { ret_ip, rule, start_pos, events_mark }) => {
                            if let Some(m) = memo {
                                let stored = events[events_mark as usize..].to_vec().into_boxed_slice();
                                m.insert_hit(rule, start_pos, pos, stored);
                            }
                            ip = ret_ip;
                            break;
                        }
                        Some(Frame::ContextSave { snapshot_mark }) => {
                            restore_snapshot(flags, ctx_snapshots, snapshot_mark);
                        }
                        Some(Frame::Backtrack { .. }) => { /* defensive */ }
                        None => return Err(ParseError::BadGraph),
                    }
                }
            }

            // ── Backtracking ──────────────────────────────────────────────────

            Insn::Choice { alt } => {
                stack.push(Frame::Backtrack {
                    alt,
                    saved_pos:        pos,
                    capture_mark:     events.len()      as u32,
                    tree_mark:        tree_events.len() as u32,
                    open_tokens_mark: open_tokens.len() as u32,
                });
                ip += 1;
            }

            Insn::Commit { target } => {
                pop_backtrack(stack)?;
                ip = target;
            }

            Insn::BackCommit { target } => {
                pos = pop_backtrack_pos(stack)?;
                ip  = target;
            }

            Insn::NegBackCommit { target } => {
                pos = pop_backtrack_pos(stack)?;
                ip  = target;
                do_fail(stack, &mut ip, &mut pos, flags, ctx_snapshots,
                        events, tree_events, open_tokens, memo, error_ctx)?;
            }

            // Fix: PartialCommit must update ALL backtrack marks so that a
            // failed later iteration does not discard events from already-committed
            // earlier iterations.
            Insn::PartialCommit { target } => {
                update_backtrack_marks(
                    stack,
                    pos,
                    events.len()      as u32,
                    tree_events.len() as u32,
                    open_tokens.len() as u32,
                )?;
                ip = target;
            }

            // ── O(1) dispatch ────────────────────────────────────────────────

            Insn::ByteDispatch { table_id } => {
                if let Some(b) = input.get(pos as usize).copied() {
                    let target = graph.dispatch(table_id, b);
                    if target != u32::MAX { ip = target; continue; }
                }
                do_fail(stack, &mut ip, &mut pos, flags, ctx_snapshots,
                        events, tree_events, open_tokens, memo, error_ctx)?;
            }

            // ── Context flags ─────────────────────────────────────────────────

            Insn::IfFlag { flag_id, on_fail } => {
                if flag_is_set(flags, flag_id) {
                    ip += 1;
                } else {
                    error_ctx.record(pos, Expected::Flag { id: flag_id, required: true });
                    fail_jump!(on_fail);
                }
            }

            Insn::IfNotFlag { flag_id, on_fail } => {
                if !flag_is_set(flags, flag_id) {
                    ip += 1;
                } else {
                    error_ctx.record(pos, Expected::Flag { id: flag_id, required: false });
                    fail_jump!(on_fail);
                }
            }

            Insn::PushFlags { mask_id } => {
                let entries = graph.flag_masks.get(mask_id);
                let mark    = ctx_snapshots.len() as u32;
                for e in entries {
                    let w = e.word as usize;
                    let old = if w < flags.len() { flags[w] } else { 0 };
                    ctx_snapshots.push(SnapEntry { word: e.word, val: old });
                }
                FlagMask { entries }.apply(flags);
                stack.push(Frame::ContextSave { snapshot_mark: mark });
                ip += 1;
            }

            Insn::PopFlags => {
                let mark = pop_context_save(stack)?;
                restore_snapshot(flags, ctx_snapshots, mark);
                ip += 1;
            }

            // ── Legacy captures ───────────────────────────────────────────────

            Insn::CaptureBegin { tag } => {
                events.push(CaptureEvent::Open  { tag, pos });
                ip += 1;
            }
            Insn::CaptureEnd { tag } => {
                events.push(CaptureEvent::Close { tag, pos });
                ip += 1;
            }

            // ── Green/Red tree ────────────────────────────────────────────────

            // Open a syntax node: push NodeOpen event immediately.
            // On backtracking, tree_events is truncated to the saved mark.
            Insn::NodeBegin { kind } => {
                tree_events.push(TreeEvent::NodeOpen { kind, pos });
                ip += 1;
            }

            // Close a syntax node.
            Insn::NodeEnd => {
                tree_events.push(TreeEvent::NodeClose { pos });
                ip += 1;
            }

            // Begin a leaf token: save (kind, is_trivia, start_pos) on the
            // open-tokens stack.  No TreeEvent yet — we don't know the end.
            Insn::TokenBegin { kind, is_trivia } => {
                open_tokens.push((kind, is_trivia, pos));
                ip += 1;
            }

            // End a leaf token: pop the open-tokens stack, emit TreeEvent::Token.
            Insn::TokenEnd => {
                let (kind, is_trivia, start) = open_tokens.pop()
                    .ok_or(ParseError::BadGraph)?;
                tree_events.push(TreeEvent::Token { kind, start, end: pos, is_trivia });
                ip += 1;
            }

            Insn::Accept => return Ok(pos),
        }
    }
}

// ─── UTF-8 decoder ───────────────────────────────────────────────────────────

#[inline(always)]
fn decode_utf8(bytes: &[u8], pos: usize) -> Option<(u32, usize)> {
    let b0 = *bytes.get(pos)?;
    if b0 < 0x80 { return Some((b0 as u32, 1)); }
    if b0 < 0xC2 { return None; }
    if b0 < 0xE0 {
        let b1 = *bytes.get(pos + 1)?;
        if b1 & 0xC0 != 0x80 { return None; }
        return Some((((b0 as u32 & 0x1F) << 6) | (b1 as u32 & 0x3F), 2));
    }
    if b0 < 0xF0 {
        let b1 = *bytes.get(pos + 1)?;
        let b2 = *bytes.get(pos + 2)?;
        if b1 & 0xC0 != 0x80 || b2 & 0xC0 != 0x80 { return None; }
        let cp = ((b0 as u32 & 0x0F) << 12) | ((b1 as u32 & 0x3F) << 6) | (b2 as u32 & 0x3F);
        if cp < 0x800 || (0xD800..=0xDFFF).contains(&cp) { return None; }
        return Some((cp, 3));
    }
    if b0 < 0xF5 {
        let b1 = *bytes.get(pos + 1)?;
        let b2 = *bytes.get(pos + 2)?;
        let b3 = *bytes.get(pos + 3)?;
        if b1 & 0xC0 != 0x80 || b2 & 0xC0 != 0x80 || b3 & 0xC0 != 0x80 { return None; }
        let cp = ((b0 as u32 & 0x07) << 18) | ((b1 as u32 & 0x3F) << 12)
               | ((b2 as u32 & 0x3F) << 6)  |  (b3 as u32 & 0x3F);
        if cp < 0x10000 || cp > 0x10FFFF { return None; }
        return Some((cp, 4));
    }
    None
}

// ─── Flag helpers ─────────────────────────────────────────────────────────────

#[inline(always)]
fn flag_is_set(flags: &[u64], flag_id: u16) -> bool {
    let w = (flag_id >> 6) as usize;
    w < flags.len() && (flags[w] >> (flag_id & 63)) & 1 != 0
}

// ─── Snapshot restore ─────────────────────────────────────────────────────────

#[inline(always)]
fn restore_snapshot(flags: &mut Vec<u64>, snaps: &mut Vec<SnapEntry>, mark: u32) {
    let mark = mark as usize;
    for entry in snaps[mark..].iter().rev() {
        if (entry.word as usize) < flags.len() { flags[entry.word as usize] = entry.val; }
    }
    snaps.truncate(mark);
}

// ─── Frame-stack helpers ──────────────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn fail_or_jump(
    on_fail:       u32,
    stack:         &mut Vec<Frame>,
    ip:            &mut u32,
    pos:           &mut Pos,
    flags:         &mut Vec<u64>,
    ctx_snapshots: &mut Vec<SnapEntry>,
    events:        &mut Vec<CaptureEvent>,
    tree_events:   &mut Vec<TreeEvent>,
    open_tokens:   &mut Vec<(SyntaxKind, bool, Pos)>,
    memo:          &mut Option<MemoTable>,
    error_ctx:     &mut ErrorContext,
) -> Result<(), ParseError> {
    if on_fail == u32::MAX {
        do_fail(stack, ip, pos, flags, ctx_snapshots,
                events, tree_events, open_tokens, memo, error_ctx)
    } else {
        *ip = on_fail;
        Ok(())
    }
}

#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn do_fail(
    stack:         &mut Vec<Frame>,
    ip:            &mut u32,
    pos:           &mut Pos,
    flags:         &mut Vec<u64>,
    ctx_snapshots: &mut Vec<SnapEntry>,
    events:        &mut Vec<CaptureEvent>,
    tree_events:   &mut Vec<TreeEvent>,
    open_tokens:   &mut Vec<(SyntaxKind, bool, Pos)>,
    memo:          &mut Option<MemoTable>,
    error_ctx:     &ErrorContext,
) -> Result<(), ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { alt, saved_pos, capture_mark, tree_mark, open_tokens_mark }) => {
                *ip  = alt;
                *pos = saved_pos;
                events.truncate(capture_mark        as usize);
                tree_events.truncate(tree_mark      as usize);
                open_tokens.truncate(open_tokens_mark as usize);
                return Ok(());
            }
            Some(Frame::ContextSave { snapshot_mark }) => {
                restore_snapshot(flags, ctx_snapshots, snapshot_mark);
            }
            Some(Frame::Return { .. }) => {}
            Some(Frame::MemoReturn { rule, start_pos, .. }) => {
                if let Some(m) = memo { m.insert_miss(rule, start_pos, error_ctx.furthest); }
            }
            None => {
                return Err(ParseError::NoMatch(error_ctx.to_diagnostic()));
            }
        }
    }
}

#[inline(always)]
fn pop_backtrack(stack: &mut Vec<Frame>) -> Result<(), ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { .. }) => return Ok(()),
            Some(_) => continue,
            None    => return Err(ParseError::BadGraph),
        }
    }
}

#[inline(always)]
fn pop_backtrack_pos(stack: &mut Vec<Frame>) -> Result<Pos, ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { saved_pos, .. }) => return Ok(saved_pos),
            Some(_) => continue,
            None    => return Err(ParseError::BadGraph),
        }
    }
}

/// Update ALL marks in the nearest `Backtrack` frame (used by `PartialCommit`).
#[inline(always)]
fn update_backtrack_marks(
    stack:            &mut Vec<Frame>,
    new_pos:          Pos,
    new_capture_mark: u32,
    new_tree_mark:    u32,
    new_open_mark:    u32,
) -> Result<(), ParseError> {
    for frame in stack.iter_mut().rev() {
        if let Frame::Backtrack { saved_pos, capture_mark, tree_mark, open_tokens_mark, .. } = frame {
            *saved_pos        = new_pos;
            *capture_mark     = new_capture_mark;
            *tree_mark        = new_tree_mark;
            *open_tokens_mark = new_open_mark;
            return Ok(());
        }
    }
    Err(ParseError::BadGraph)
}

#[inline(always)]
fn pop_context_save(stack: &mut Vec<Frame>) -> Result<u32, ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::ContextSave { snapshot_mark }) => return Ok(snapshot_mark),
            Some(_) => continue,
            None    => return Err(ParseError::BadGraph),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder::GrammarBuilder;
    use crate::error::Expected;

    fn minimal_graph() -> crate::builder::BuiltGraph {
        let mut g = GrammarBuilder::new();
        g.begin_rule("start");
        g.byte(b'a');
        g.end_of_input();
        g.accept();
        g.finish().expect("minimal graph valid")
    }

    #[test]
    fn parse_success() {
        let built = minimal_graph();
        let graph = built.as_graph();
        let mut engine = Engine::new();
        let out = engine.parse(&graph, b"a").expect("parse ok");
        assert_eq!(out.consumed, 1);
    }

    #[test]
    fn parse_fail_no_match_diagnostic_has_expected() {
        let built = minimal_graph();
        let graph = built.as_graph();
        let mut engine = Engine::new();
        let err = engine.parse(&graph, b"b").unwrap_err();
        let ParseError::NoMatch(diag) = err else { panic!("expected NoMatch"); };
        assert_eq!(diag.furthest, 0);
        assert!(!diag.expected.is_empty(), "diagnostic should list expected tokens");
        assert!(diag.expected.iter().any(|e| matches!(e, Expected::Byte(0x61))));
    }

    #[test]
    fn parse_fail_empty_input() {
        let built = minimal_graph();
        let graph = built.as_graph();
        let mut engine = Engine::new();
        let err = engine.parse(&graph, b"").unwrap_err();
        let ParseError::NoMatch(diag) = err else { panic!("expected NoMatch"); };
        assert_eq!(diag.furthest, 0);
        assert!(!diag.expected.is_empty());
    }
}
