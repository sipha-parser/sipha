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
//! ## `PartialCommit` (`zero_or_more`) mark update
//!
//! [`PartialCommit`] updates **all** backtrack marks — `saved_pos`,
//! `capture_mark`, `tree_mark`, and `open_tokens_mark` — so that a failed
//! subsequent iteration does not discard events from earlier completed
//! iterations.
//!
//! [`PartialCommit`]: crate::insn::Insn::PartialCommit

use crate::{
    context::{FlagMask, ParseContext},
    error::{ErrorContext, Expected},
    insn::{Insn, ParseGraph},
    memo::{MemoReplay, MemoTable},
    simd,
    types::{CaptureEvent, InsnId, Pos, RuleId, SyntaxKind, TreeEvent},
};

/// Close open nodes and insert an error node so the event list is well-nested.
pub(crate) fn insert_error_node_events(
    tree_events: &mut Vec<TreeEvent>,
    furthest: Pos,
    error_kind: SyntaxKind,
) {
    let mut depth = 0u32;
    for e in tree_events.iter() {
        match e {
            TreeEvent::NodeOpen { .. } => depth = depth.saturating_add(1),
            TreeEvent::NodeClose { .. } => depth = depth.saturating_sub(1),
            TreeEvent::Token { .. } => {}
        }
    }
    for _ in 0..depth {
        tree_events.push(TreeEvent::NodeClose { pos: furthest });
    }
    tree_events.push(TreeEvent::NodeOpen {
        kind: error_kind,
        field: None,
        pos: furthest,
    });
    tree_events.push(TreeEvent::NodeClose { pos: furthest });
}

// ─── Snapshot entry ───────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct SnapEntry {
    word: u32,
    val: u64,
}

// ─── Frame ────────────────────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug)]
enum Frame {
    Backtrack {
        alt: u32,
        saved_pos: Pos,
        /// Mark into `engine.events` (legacy captures).
        capture_mark: u32,
        /// Mark into `engine.tree_events` (green/red tree events).
        tree_mark: u32,
        /// Mark into `engine.open_tokens` (in-progress token spans).
        open_tokens_mark: u32,
    },
    Return {
        ret_ip: u32,
    },
    MemoReturn {
        ret_ip: u32,
        rule: RuleId,
        start_pos: Pos,
        events_mark: u32,
        tree_mark: u32,
    },
    ContextSave {
        snapshot_mark: u32,
    },
    /// Error recovery: on failure, skip until `sync_rule` matches then continue at resume.
    Recover {
        sync_rule: RuleId,
        resume: InsnId,
    },
    /// Marker when trying the sync rule during recovery; popped by `RecoveryResume`.
    RecoverSync {
        sync_rule: RuleId,
        resume: InsnId,
    },
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
            Self::NoMatch(d) => write!(f, "{d}"),
            Self::BadGraph => write!(f, "malformed parse graph"),
        }
    }
}
impl std::error::Error for ParseError {}

/// Result of parsing in multi-error recovery mode: partial output and all collected errors.
///
/// Returned by [`Engine::parse_recovering_multi`] when the grammar uses
/// [`recover_until`](crate::builder::GrammarBuilder::recover_until) and one or more
/// failures occur. `partial` contains tree events and consumed length up to the last
/// recovery point; `errors` holds each parse error in order.
#[derive(Debug)]
pub struct RecoverMultiResult {
    /// Partial parse output (events and consumed length) after collecting errors.
    pub partial: ParseOutput,
    /// All parse errors collected during recovery (in order of occurrence).
    pub errors: Vec<ParseError>,
}

#[cfg(feature = "miette")]
impl ParseError {
    /// If this is a parse failure ([`NoMatch`](Self::NoMatch)), convert it
    /// into a [`miette::Report`] with source and optional literal table for
    /// pretty-printed diagnostics. Returns `None` for [`BadGraph`](Self::BadGraph).
    ///
    /// Requires the `miette` feature. Example:
    ///
    /// ```ignore
    /// if let Err(e) = engine.parse(&graph, &source) {
    ///     if let Some(report) = e.to_miette_report(&source, "file.txt", Some(&graph.literals), Some(&graph.rule_names)) {
    ///         eprintln!("{:?}", report);
    ///     }
    /// }
    /// ```
    pub fn to_miette_report(
        &self,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&crate::insn::LiteralTable>,
        rule_names: Option<&[&'static str]>,
        expected_labels: Option<&[&'static str]>,
    ) -> Option<miette::Report> {
        match self {
            Self::NoMatch(diag) => {
                let m = diag.into_miette(source, name, literals, rule_names, expected_labels);
                Some(miette::Report::new(m))
            }
            Self::BadGraph => None,
        }
    }
}

// ─── Output ───────────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct ParseOutput {
    /// Number of input bytes consumed (always equals `input.len()` if the
    /// grammar ends with `end_of_input`).
    pub consumed: Pos,
    /// Legacy flat capture events.  Use `CaptureNode::build_forest` to get
    /// the capture tree.
    pub events: Vec<CaptureEvent>,
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
    #[must_use]
    pub fn build_green_tree(
        &self,
        input: &[u8],
    ) -> Option<std::sync::Arc<crate::green::GreenNode>> {
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
    #[must_use]
    pub fn syntax_root(&self, input: &[u8]) -> Option<crate::red::SyntaxNode> {
        self.build_green_tree(input)
            .map(crate::red::SyntaxNode::new_root)
    }
}

// ─── Engine ───────────────────────────────────────────────────────────────────

pub struct Engine {
    stack: Vec<Frame>,
    events: Vec<CaptureEvent>,
    tree_events: Vec<TreeEvent>,
    /// In-progress token spans: `(kind, is_trivia, start_pos)`.
    /// Pushed by `TokenBegin`, popped by `TokenEnd`.
    open_tokens: Vec<(SyntaxKind, bool, Pos)>,
    memo: Option<MemoTable>,
    error_ctx: ErrorContext,
    flags: Vec<u64>,
    ctx_snapshots: Vec<SnapEntry>,
}

impl Engine {
    #[must_use]
    pub fn new() -> Self {
        Self::with_capacity(1024, 256)
    }

    #[must_use]
    pub fn with_capacity(stack_cap: usize, capture_cap: usize) -> Self {
        Self {
            stack: Vec::with_capacity(stack_cap),
            events: Vec::with_capacity(capture_cap),
            tree_events: Vec::with_capacity(capture_cap),
            open_tokens: Vec::with_capacity(16),
            memo: None,
            error_ctx: ErrorContext::new(),
            flags: Vec::with_capacity(4),
            ctx_snapshots: Vec::with_capacity(64),
        }
    }

    #[must_use]
    pub fn with_memo(mut self) -> Self {
        self.memo = Some(MemoTable::new());
        self
    }

    /// Parse the input against the grammar.
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::NoMatch(diagnostic))` on parse failure, or
    /// `Err(ParseError::BadGraph)` if the graph is invalid.
    pub fn parse(&mut self, graph: &ParseGraph, input: &[u8]) -> Result<ParseOutput, ParseError> {
        self.parse_with_context(graph, input, &ParseContext::new())
    }

    /// Parse with the given context (e.g. for flags or error node kind).
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::NoMatch(diagnostic))` when the input does not
    /// match the grammar. Returns `Err(ParseError::BadGraph)` if the graph is invalid.
    pub fn parse_with_context(
        &mut self,
        graph: &ParseGraph,
        input: &[u8],
        context: &ParseContext,
    ) -> Result<ParseOutput, ParseError> {
        self.stack.clear();
        self.events.clear();
        self.tree_events.clear();
        self.open_tokens.clear();
        self.error_ctx.clear();
        self.ctx_snapshots.clear();
        if let Some(m) = &mut self.memo {
            m.clear();
        }

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
            &mut None,
            0,
        ) {
            Ok(consumed) => Ok(ParseOutput {
                consumed,
                events: std::mem::take(&mut self.events),
                tree_events: std::mem::take(&mut self.tree_events),
            }),
            Err(e) => Err(e),
        }
    }

    /// Parse in recovering mode: on failure, still return a partial [`ParseOutput`]
    /// built from the parser state at the failure point (e.g. for IDE or multi-error reporting).
    ///
    /// On [`Err`], the returned output may contain a partial tree and events; `consumed`
    /// is set to the error position ([`ErrorContext::furthest`]). Build a green tree from
    /// the partial `tree_events` with [`ParseOutput::build_green_tree`] (may return `None`
    /// if the partial events are not well-nested).
    ///
    /// # Errors
    ///
    /// Returns `Err((partial_output, parse_error))` when the parse fails.
    pub fn parse_recovering_with_context(
        &mut self,
        graph: &ParseGraph,
        input: &[u8],
        context: &ParseContext,
    ) -> Result<ParseOutput, (ParseOutput, ParseError)> {
        self.stack.clear();
        self.events.clear();
        self.tree_events.clear();
        self.open_tokens.clear();
        self.error_ctx.clear();
        self.ctx_snapshots.clear();
        if let Some(m) = &mut self.memo {
            m.clear();
        }
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
            &mut None,
            0,
        ) {
            Ok(consumed) => Ok(ParseOutput {
                consumed,
                events: std::mem::take(&mut self.events),
                tree_events: std::mem::take(&mut self.tree_events),
            }),
            Err(e) => {
                if let Some(kind) = context.error_node_kind() {
                    insert_error_node_events(&mut self.tree_events, self.error_ctx.furthest, kind);
                }
                Err((
                    ParseOutput {
                        consumed: self.error_ctx.furthest,
                        events: std::mem::take(&mut self.events),
                        tree_events: std::mem::take(&mut self.tree_events),
                    },
                    e,
                ))
            }
        }
    }

    /// Like [`parse_recovering_with_context`] with a default context.
    ///
    /// # Errors
    ///
    /// Returns `Err((partial_output, parse_error))` when the parse fails.
    pub fn parse_recovering(
        &mut self,
        graph: &ParseGraph,
        input: &[u8],
    ) -> Result<ParseOutput, (ParseOutput, ParseError)> {
        self.parse_recovering_with_context(graph, input, &ParseContext::new())
    }

    /// Parse in multi-error recovery mode: collect multiple errors until EOI or `max_errors`.
    ///
    /// Requires the grammar to use [`recover_until`](crate::builder::GrammarBuilder::recover_until)
    /// so that on each failure the VM can skip to a sync point and continue. Each failure
    /// is pushed to `errors`; parsing stops when no recovery frame is found, input ends,
    /// or `errors.len()` reaches `max_errors`.
    ///
    /// Returns `Ok(output)` on full success; `Err(RecoverMultiResult { partial, errors })`
    /// when at least one error was collected.
    ///
    /// # Errors
    ///
    /// Returns `Err(RecoverMultiResult { partial, errors })` when one or more
    /// parse errors were collected during recovery.
    pub fn parse_recovering_multi(
        &mut self,
        graph: &ParseGraph,
        input: &[u8],
        max_errors: usize,
    ) -> Result<ParseOutput, RecoverMultiResult> {
        self.parse_recovering_multi_with_context(graph, input, &ParseContext::new(), max_errors)
    }

    /// Like [`parse_recovering_multi`] with a custom context (e.g. for error node kind).
    ///
    /// # Errors
    ///
    /// Returns `Err(RecoverMultiResult { partial, errors })` when one or more
    /// parse errors were collected during recovery.
    pub fn parse_recovering_multi_with_context(
        &mut self,
        graph: &ParseGraph,
        input: &[u8],
        context: &ParseContext,
        max_errors: usize,
    ) -> Result<ParseOutput, RecoverMultiResult> {
        self.stack.clear();
        self.events.clear();
        self.tree_events.clear();
        self.open_tokens.clear();
        self.error_ctx.clear();
        self.ctx_snapshots.clear();
        if let Some(m) = &mut self.memo {
            m.clear();
        }
        self.flags.clear();
        self.flags.extend_from_slice(context.words());

        let mut errors = Vec::new();
        let cap = max_errors.max(1);
        let mut multi_errors = Some(&mut errors);

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
            &mut multi_errors,
            cap,
        ) {
            Ok(consumed) => Ok(ParseOutput {
                consumed,
                events: std::mem::take(&mut self.events),
                tree_events: std::mem::take(&mut self.tree_events),
            }),
            Err(e) => {
                errors.push(e);
                if let Some(kind) = context.error_node_kind() {
                    insert_error_node_events(&mut self.tree_events, self.error_ctx.furthest, kind);
                }
                Err(RecoverMultiResult {
                    partial: ParseOutput {
                        consumed: self.error_ctx.furthest,
                        events: std::mem::take(&mut self.events),
                        tree_events: std::mem::take(&mut self.tree_events),
                    },
                    errors,
                })
            }
        }
    }

    #[must_use]
    pub const fn error_context(&self) -> &ErrorContext {
        &self.error_ctx
    }
    #[must_use]
    pub fn memo_len(&self) -> Option<usize> {
        self.memo.as_ref().map(super::memo::MemoTable::len)
    }
    #[must_use]
    pub fn flag_words(&self) -> &[u64] {
        &self.flags
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

// ─── VM loop ─────────────────────────────────────────────────────────────────

#[inline(never)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::too_many_lines)] // VM dispatch loop; splitting would obscure control flow
#[allow(clippy::ptr_arg)] // need Vec for .push/.pop/.truncate
fn run(
    graph: &ParseGraph,
    input: &[u8],
    stack: &mut Vec<Frame>,
    events: &mut Vec<CaptureEvent>,
    tree_events: &mut Vec<TreeEvent>,
    open_tokens: &mut Vec<(SyntaxKind, bool, Pos)>,
    memo: &mut Option<MemoTable>,
    error_ctx: &mut ErrorContext,
    flags: &mut Vec<u64>,
    ctx_snapshots: &mut Vec<SnapEntry>,
    multi_errors: &mut Option<&mut Vec<ParseError>>,
    max_errors: usize,
) -> Result<Pos, ParseError> {
    let mut ip: u32 = graph.start();
    let mut pos: Pos = 0;

    loop {
        let insn = graph.insn(ip);

        macro_rules! fail_jump {
            ($on_fail:expr) => {
                fail_or_jump(
                    $on_fail,
                    graph,
                    input,
                    stack,
                    &mut ip,
                    &mut pos,
                    flags,
                    ctx_snapshots,
                    events,
                    tree_events,
                    open_tokens,
                    memo,
                    error_ctx,
                    multi_errors,
                    max_errors,
                )?
            };
        }

        match insn {
            // ── Byte terminals ────────────────────────────────────────────────
            Insn::Byte { byte, on_fail } => {
                if input.get(pos as usize).copied() == Some(byte) {
                    pos += 1;
                    ip += 1;
                } else {
                    error_ctx.record(pos, Expected::Byte(byte));
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
                error_ctx.record(pos, Expected::ByteRange(lo, hi));
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
                let label = graph
                    .class_labels
                    .get(label_id as usize)
                    .copied()
                    .unwrap_or("character class");
                error_ctx.record(pos, Expected::Class(label));
                fail_jump!(on_fail);
            }

            Insn::Literal { lit_id, on_fail } => {
                let lit = graph.literals.get(lit_id);
                if simd::literal_eq(input, pos, lit) {
                    pos += Pos::try_from(lit.len()).unwrap_or(0);
                    ip += 1;
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
                do_fail(
                    graph,
                    input,
                    stack,
                    &mut ip,
                    &mut pos,
                    flags,
                    ctx_snapshots,
                    events,
                    tree_events,
                    open_tokens,
                    memo,
                    error_ctx,
                    multi_errors,
                    max_errors,
                )?;
            }

            // ── Unicode codepoint terminals ───────────────────────────────────
            Insn::AnyChar { on_fail } => {
                if let Some((_cp, len)) = decode_utf8(input, pos as usize) {
                    pos += Pos::try_from(len).unwrap_or(0);
                    ip += 1;
                } else {
                    error_ctx.record(pos, Expected::AnyChar);
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
                error_ctx.record(pos, Expected::Char(codepoint));
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
                error_ctx.record(pos, Expected::CharRange(lo, hi));
                fail_jump!(on_fail);
            }

            // ── Control flow ──────────────────────────────────────────────────
            Insn::Jump { target } => {
                ip = target;
            }

            Insn::Call { rule } => {
                let entry = graph
                    .rule_entry
                    .get(rule as usize)
                    .copied()
                    .ok_or(ParseError::BadGraph)?;

                // Record that we're trying this rule so diagnostics can show "expected <rule>".
                error_ctx.record(pos, Expected::Rule(rule));

                if let Some(mt) = memo {
                    match mt.query_replay(rule, pos, events, tree_events) {
                        MemoReplay::Hit { end_pos } => {
                            pos = end_pos;
                            ip += 1;
                        }
                        MemoReplay::Miss { furthest } => {
                            error_ctx.record(furthest, Expected::Rule(rule));
                            do_fail(
                                graph,
                                input,
                                stack,
                                &mut ip,
                                &mut pos,
                                flags,
                                ctx_snapshots,
                                events,
                                tree_events,
                                open_tokens,
                                memo,
                                error_ctx,
                                multi_errors,
                                max_errors,
                            )?;
                        }
                        MemoReplay::Unknown => {
                            stack.push(Frame::MemoReturn {
                                ret_ip: ip + 1,
                                rule,
                                start_pos: pos,
                                events_mark: u32::try_from(events.len()).unwrap_or(0),
                                tree_mark: u32::try_from(tree_events.len()).unwrap_or(0),
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
                            if let Some(m) = memo {
                                let stored_events =
                                    events[events_mark as usize..].to_vec().into_boxed_slice();
                                let stored_tree = tree_events[tree_mark as usize..]
                                    .to_vec()
                                    .into_boxed_slice();
                                m.insert_hit(rule, start_pos, pos, stored_events, stored_tree);
                            }
                            ip = ret_ip;
                            break;
                        }
                        Some(Frame::ContextSave { snapshot_mark }) => {
                            restore_snapshot(flags, ctx_snapshots, snapshot_mark);
                        }
                        Some(Frame::Backtrack { .. }) => { /* defensive */ }
                        Some(f @ (Frame::Recover { .. } | Frame::RecoverSync { .. })) => {
                            stack.push(f);
                            return Err(ParseError::BadGraph);
                        }
                        None => return Err(ParseError::BadGraph),
                    }
                }
            }

            // ── Backtracking ──────────────────────────────────────────────────
            Insn::Choice { alt } => {
                stack.push(Frame::Backtrack {
                    alt,
                    saved_pos: pos,
                    capture_mark: u32::try_from(events.len()).unwrap_or(0),
                    tree_mark: u32::try_from(tree_events.len()).unwrap_or(0),
                    open_tokens_mark: u32::try_from(open_tokens.len()).unwrap_or(0),
                });
                ip += 1;
            }

            Insn::Commit { target } => {
                pop_backtrack(stack)?;
                ip = target;
            }

            Insn::BackCommit { target } => {
                pos = pop_backtrack_pos(stack)?;
                ip = target;
            }

            Insn::NegBackCommit { target } => {
                pos = pop_backtrack_pos(stack)?;
                ip = target;
                do_fail(
                    graph,
                    input,
                    stack,
                    &mut ip,
                    &mut pos,
                    flags,
                    ctx_snapshots,
                    events,
                    tree_events,
                    open_tokens,
                    memo,
                    error_ctx,
                    multi_errors,
                    max_errors,
                )?;
            }

            // Fix: PartialCommit must update ALL backtrack marks so that a
            // failed later iteration does not discard events from already-committed
            // earlier iterations.
            Insn::PartialCommit { target } => {
                update_backtrack_marks(
                    stack,
                    pos,
                    u32::try_from(events.len()).unwrap_or(0),
                    u32::try_from(tree_events.len()).unwrap_or(0),
                    u32::try_from(open_tokens.len()).unwrap_or(0),
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
                do_fail(
                    graph,
                    input,
                    stack,
                    &mut ip,
                    &mut pos,
                    flags,
                    ctx_snapshots,
                    events,
                    tree_events,
                    open_tokens,
                    memo,
                    error_ctx,
                    multi_errors,
                    max_errors,
                )?;
            }

            // ── Context flags ─────────────────────────────────────────────────
            Insn::IfFlag { flag_id, on_fail } => {
                if flag_is_set(flags, flag_id) {
                    ip += 1;
                } else {
                    error_ctx.record(
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
                if flag_is_set(flags, flag_id) {
                    error_ctx.record(
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
                let mark = u32::try_from(ctx_snapshots.len()).unwrap_or(0);
                for e in entries {
                    let w = e.word as usize;
                    let old = if w < flags.len() { flags[w] } else { 0 };
                    ctx_snapshots.push(SnapEntry {
                        word: e.word,
                        val: old,
                    });
                }
                FlagMask { entries }.apply(flags);
                stack.push(Frame::ContextSave {
                    snapshot_mark: mark,
                });
                ip += 1;
            }

            Insn::PopFlags => {
                let mark = pop_context_save(stack)?;
                restore_snapshot(flags, ctx_snapshots, mark);
                ip += 1;
            }

            // ── Legacy captures ───────────────────────────────────────────────
            Insn::CaptureBegin { tag } => {
                events.push(CaptureEvent::Open { tag, pos });
                ip += 1;
            }
            Insn::CaptureEnd { tag } => {
                events.push(CaptureEvent::Close { tag, pos });
                ip += 1;
            }

            // ── Green/Red tree ────────────────────────────────────────────────

            // Open a syntax node: push NodeOpen event immediately.
            // On backtracking, tree_events is truncated to the saved mark.
            Insn::NodeBegin { kind, field } => {
                tree_events.push(TreeEvent::NodeOpen { kind, field, pos });
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
                let (kind, is_trivia, start) = open_tokens.pop().ok_or(ParseError::BadGraph)?;
                tree_events.push(TreeEvent::Token {
                    kind,
                    start,
                    end: pos,
                    is_trivia,
                });
                ip += 1;
            }

            Insn::RecordExpectedLabel { label_id } => {
                error_ctx.record(pos, Expected::Label(label_id));
                ip += 1;
            }

            // ── Error recovery ─────────────────────────────────────────────────
            Insn::RecoverUntil { sync_rule, resume } => {
                stack.push(Frame::Recover { sync_rule, resume });
                ip += 1;
            }

            Insn::RecoveryResume => {
                // Body succeeded or sync rule matched; pop Recover or RecoverSync and continue.
                let mut put_back = Vec::new();
                let mut found = false;
                while let Some(f) = stack.pop() {
                    if matches!(f, Frame::RecoverSync { .. } | Frame::Recover { .. }) {
                        found = true;
                        break;
                    }
                    put_back.push(f);
                }
                for f in put_back.into_iter().rev() {
                    stack.push(f);
                }
                if !found {
                    return Err(ParseError::BadGraph);
                }
                ip += 1;
            }

            Insn::Accept => return Ok(pos),
        }
    }
}

// ─── UTF-8 decoder ───────────────────────────────────────────────────────────

#[inline]
fn decode_utf8(bytes: &[u8], pos: usize) -> Option<(u32, usize)> {
    let b0 = *bytes.get(pos)?;
    if b0 < 0x80 {
        return Some((u32::from(b0), 1));
    }
    if b0 < 0xC2 {
        return None;
    }
    if b0 < 0xE0 {
        let b1 = *bytes.get(pos + 1)?;
        if b1 & 0xC0 != 0x80 {
            return None;
        }
        return Some((((u32::from(b0) & 0x1F) << 6) | (u32::from(b1) & 0x3F), 2));
    }
    if b0 < 0xF0 {
        let b1 = *bytes.get(pos + 1)?;
        let b2 = *bytes.get(pos + 2)?;
        if b1 & 0xC0 != 0x80 || b2 & 0xC0 != 0x80 {
            return None;
        }
        let cp =
            ((u32::from(b0) & 0x0F) << 12) | ((u32::from(b1) & 0x3F) << 6) | (u32::from(b2) & 0x3F);
        if cp < 0x800 || (0xD800..=0xDFFF).contains(&cp) {
            return None;
        }
        return Some((cp, 3));
    }
    if b0 < 0xF5 {
        let b1 = *bytes.get(pos + 1)?;
        let b2 = *bytes.get(pos + 2)?;
        let b3 = *bytes.get(pos + 3)?;
        if b1 & 0xC0 != 0x80 || b2 & 0xC0 != 0x80 || b3 & 0xC0 != 0x80 {
            return None;
        }
        let cp = ((u32::from(b0) & 0x07) << 18)
            | ((u32::from(b1) & 0x3F) << 12)
            | ((u32::from(b2) & 0x3F) << 6)
            | (u32::from(b3) & 0x3F);
        if !(0x10000..=0x10_FFFF).contains(&cp) {
            return None;
        }
        return Some((cp, 4));
    }
    None
}

// ─── Flag helpers ─────────────────────────────────────────────────────────────

#[inline]
fn flag_is_set(flags: &[u64], flag_id: u16) -> bool {
    let w = (flag_id >> 6) as usize;
    w < flags.len() && (flags[w] >> (flag_id & 63)) & 1 != 0
}

// ─── Snapshot restore ─────────────────────────────────────────────────────────

#[inline]
fn restore_snapshot(flags: &mut [u64], snaps: &mut Vec<SnapEntry>, mark: u32) {
    let mark = mark as usize;
    for entry in snaps[mark..].iter().rev() {
        if (entry.word as usize) < flags.len() {
            flags[entry.word as usize] = entry.val;
        }
    }
    snaps.truncate(mark);
}

// ─── Frame-stack helpers ──────────────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
#[inline]
fn fail_or_jump(
    on_fail: u32,
    graph: &ParseGraph,
    input: &[u8],
    stack: &mut Vec<Frame>,
    ip: &mut u32,
    pos: &mut Pos,
    flags: &mut Vec<u64>,
    ctx_snapshots: &mut Vec<SnapEntry>,
    events: &mut Vec<CaptureEvent>,
    tree_events: &mut Vec<TreeEvent>,
    open_tokens: &mut Vec<(SyntaxKind, bool, Pos)>,
    memo: &mut Option<MemoTable>,
    error_ctx: &ErrorContext,
    multi_errors: &mut Option<&mut Vec<ParseError>>,
    max_errors: usize,
) -> Result<(), ParseError> {
    if on_fail == u32::MAX {
        do_fail(
            graph,
            input,
            stack,
            ip,
            pos,
            flags,
            ctx_snapshots,
            events,
            tree_events,
            open_tokens,
            memo,
            error_ctx,
            multi_errors,
            max_errors,
        )
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

#[allow(clippy::too_many_arguments)]
#[inline]
#[allow(clippy::ptr_arg)] // need Vec for .truncate
fn do_fail(
    graph: &ParseGraph,
    input: &[u8],
    stack: &mut Vec<Frame>,
    ip: &mut u32,
    pos: &mut Pos,
    flags: &mut Vec<u64>,
    ctx_snapshots: &mut Vec<SnapEntry>,
    events: &mut Vec<CaptureEvent>,
    tree_events: &mut Vec<TreeEvent>,
    open_tokens: &mut Vec<(SyntaxKind, bool, Pos)>,
    memo: &mut Option<MemoTable>,
    error_ctx: &ErrorContext,
    multi_errors: &mut Option<&mut Vec<ParseError>>,
    max_errors: usize,
) -> Result<(), ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack {
                alt,
                saved_pos,
                capture_mark,
                tree_mark,
                open_tokens_mark,
            }) => {
                *ip = alt;
                *pos = saved_pos;
                events.truncate(capture_mark as usize);
                tree_events.truncate(tree_mark as usize);
                open_tokens.truncate(open_tokens_mark as usize);
                return Ok(());
            }
            Some(Frame::ContextSave { snapshot_mark }) => {
                restore_snapshot(flags, ctx_snapshots, snapshot_mark);
            }
            Some(Frame::Return { .. }) => {}
            Some(Frame::MemoReturn {
                rule, start_pos, ..
            }) => {
                if let Some(m) = memo {
                    m.insert_miss(rule, start_pos, error_ctx.furthest);
                }
            }
            Some(
                Frame::Recover { sync_rule, resume } | Frame::RecoverSync { sync_rule, resume },
            ) => {
                if let Some(errs) = multi_errors {
                    errs.push(ParseError::NoMatch(error_ctx.to_diagnostic()));
                    if errs.len() >= max_errors {
                        return Err(ParseError::NoMatch(error_ctx.to_diagnostic()));
                    }
                }
                recovery_advance(pos, input);
                if (*pos as usize) >= input.len() {
                    // Synced to EOI; don't jump back into the recovery body — keep popping.
                } else {
                    let entry = *graph
                        .rule_entry
                        .get(sync_rule as usize)
                        .ok_or(ParseError::BadGraph)?;
                    stack.push(Frame::RecoverSync { sync_rule, resume });
                    stack.push(Frame::Return { ret_ip: resume });
                    *ip = entry;
                    return Ok(());
                }
            }
            None => {
                return Err(ParseError::NoMatch(error_ctx.to_diagnostic()));
            }
        }
    }
}

#[inline]
fn pop_backtrack(stack: &mut Vec<Frame>) -> Result<(), ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { .. }) => return Ok(()),
            Some(_) => {}
            None => return Err(ParseError::BadGraph),
        }
    }
}

#[inline]
fn pop_backtrack_pos(stack: &mut Vec<Frame>) -> Result<Pos, ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::Backtrack { saved_pos, .. }) => return Ok(saved_pos),
            Some(_) => {}
            None => return Err(ParseError::BadGraph),
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
    Err(ParseError::BadGraph)
}

#[inline]
fn pop_context_save(stack: &mut Vec<Frame>) -> Result<u32, ParseError> {
    loop {
        match stack.pop() {
            Some(Frame::ContextSave { snapshot_mark }) => return Ok(snapshot_mark),
            Some(_) => {}
            None => return Err(ParseError::BadGraph),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder::GrammarBuilder;
    use crate::error::Expected;
    use crate::types::classes;

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
        let ParseError::NoMatch(diag) = err else {
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
        let mut engine = Engine::new();
        let err = engine.parse(&graph, b"").unwrap_err();
        let ParseError::NoMatch(diag) = err else {
            panic!("expected NoMatch");
        };
        assert_eq!(diag.furthest, 0);
        assert!(!diag.expected.is_empty());
    }

    #[test]
    fn recover_until_skips_to_sync_then_continues() {
        let mut g = GrammarBuilder::new();
        g.begin_rule("start");
        g.zero_or_more(|g| {
            g.recover_until("semi", |g| {
                g.call("statement");
            });
        });
        g.end_of_input();
        g.accept();
        g.rule("semi", |g| {
            g.literal(b";");
        });
        g.rule("statement", |g| {
            g.class(classes::LOWER); // one letter
        });
        let built = g.finish().expect("grammar valid");
        let graph = built.as_graph();
        let mut engine = Engine::new();
        // "a;b;" parses: statement 'a', then next iteration statement 'b', then eoi.
        let out = engine.parse(&graph, b"a;b;").expect("parse ok");
        assert_eq!(out.consumed, 4);
        // "a x ; b ;" — at ' ' statement fails, skip until ';', then statement 'b', then ';', eoi (9 bytes).
        let out = engine.parse(&graph, b"a x ; b ;").expect("parse ok");
        assert_eq!(out.consumed, 9);
    }
}
