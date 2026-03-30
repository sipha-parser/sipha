use super::error::{ParseError, RecoverMultiResult};
use super::frames::{Frame, SnapEntry};
use super::output::ParseOutput;
use super::tree_events::insert_error_node_events;
use super::vm::{run, VmState};
use crate::diagnostics::error::ErrorContext;
use crate::parse::context::ParseContext;
use crate::parse::memo::MemoTable;
use crate::types::{CaptureEvent, Pos, SyntaxKind, TreeEvent};
use crate::{parse::insn::ParseGraph, parse::memo};

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
    /// `Err(ParseError::BadGraph(_))` if the graph is invalid.
    pub fn parse(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
    ) -> Result<ParseOutput, ParseError> {
        self.parse_with_context(graph, input, &ParseContext::new())
    }

    /// Parse with the given context (e.g. for flags or error node kind).
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::NoMatch(diagnostic))` when the input does not
    /// match the grammar. Returns `Err(ParseError::BadGraph(_))` if the graph is invalid.
    pub fn parse_with_context(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        context: &ParseContext,
    ) -> Result<ParseOutput, ParseError> {
        self.reset_for_parse(context);

        run(
            graph,
            input,
            &mut VmState {
                stack: &mut self.stack,
                events: &mut self.events,
                tree_events: &mut self.tree_events,
                open_tokens: &mut self.open_tokens,
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
            },
        )
        .map(|consumed| ParseOutput {
            consumed,
            events: std::mem::take(&mut self.events),
            tree_events: std::mem::take(&mut self.tree_events),
        })
    }

    /// Parse in recovering mode: on failure, still return a partial [`ParseOutput`].
    ///
    /// # Errors
    ///
    /// Returns `Err((partial_output, parse_error))` when the parse fails.
    pub fn parse_recovering_with_context(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        context: &ParseContext,
    ) -> Result<ParseOutput, (ParseOutput, ParseError)> {
        self.reset_for_parse(context);

        match run(
            graph,
            input,
            &mut VmState {
                stack: &mut self.stack,
                events: &mut self.events,
                tree_events: &mut self.tree_events,
                open_tokens: &mut self.open_tokens,
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
            },
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

    /// Like [`parse_recovering_with_context`](Engine::parse_recovering_with_context) with a default context.
    ///
    /// # Errors
    ///
    /// Returns `Err((partial_output, parse_error))` when the parse fails.
    pub fn parse_recovering(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
    ) -> Result<ParseOutput, (ParseOutput, ParseError)> {
        self.parse_recovering_with_context(graph, input, &ParseContext::new())
    }

    /// Parse in multi-error recovery mode: collect multiple errors until EOI or `max_errors`.
    ///
    /// # Errors
    ///
    /// Returns `Err(RecoverMultiResult { partial, errors })` when one or more
    /// parse errors were collected during recovery.
    pub fn parse_recovering_multi(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        max_errors: usize,
    ) -> Result<ParseOutput, RecoverMultiResult> {
        self.parse_recovering_multi_with_context(graph, input, &ParseContext::new(), max_errors)
    }

    /// Like [`parse_recovering_multi`](Engine::parse_recovering_multi) with a custom context (e.g. for error node kind).
    ///
    /// # Errors
    ///
    /// Returns `Err(RecoverMultiResult { partial, errors })` when one or more
    /// parse errors were collected during recovery.
    pub fn parse_recovering_multi_with_context(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        context: &ParseContext,
        max_errors: usize,
    ) -> Result<ParseOutput, RecoverMultiResult> {
        self.reset_for_parse(context);

        let mut errors = Vec::new();
        let cap = max_errors.max(1);
        let mut multi_errors = Some(&mut errors);

        match run(
            graph,
            input,
            &mut VmState {
                stack: &mut self.stack,
                events: &mut self.events,
                tree_events: &mut self.tree_events,
                open_tokens: &mut self.open_tokens,
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut multi_errors,
                max_errors: cap,
            },
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
        self.memo.as_ref().map(memo::MemoTable::len)
    }

    #[must_use]
    pub fn flag_words(&self) -> &[u64] {
        &self.flags
    }

    fn reset_for_parse(&mut self, context: &ParseContext) {
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
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}
