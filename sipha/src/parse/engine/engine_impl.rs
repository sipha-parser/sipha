#[cfg(feature = "trace")]
use super::ParseTracer;
use super::error::{ParseError, RecoverMultiResult};
use super::frames::{Frame, SnapEntry};
use super::output::ParseOutput;
use super::tree_events::insert_error_node_events;
use super::vm::run_from;
use super::vm::{VmState, run};
use crate::diagnostics::error::ErrorContext;
use crate::parse::context::ParseContext;
#[cfg(feature = "trace")]
use crate::parse::engine::VmObserver;
use crate::parse::insn::ParseGraph;
#[cfg(feature = "std")]
use crate::parse::memo;
#[cfg(feature = "std")]
use crate::parse::memo::MemoTable;
use crate::types::{CaptureEvent, Pos, RuleId, SyntaxKind, TreeEvent};
#[cfg(not(feature = "std"))]
use alloc::{string::ToString, vec::Vec};
#[cfg(feature = "std")]
use std::string::ToString;

pub struct Engine {
    stack: Vec<Frame>,
    events: Vec<CaptureEvent>,
    tree_events: Vec<TreeEvent>,
    /// In-progress token spans: `(kind, is_trivia, start_pos)`.
    /// Pushed by `TokenBegin`, popped by `TokenEnd`.
    open_tokens: Vec<(SyntaxKind, bool, Pos)>,
    #[cfg(feature = "std")]
    memo: Option<MemoTable>,
    error_ctx: ErrorContext,
    context_stack: Vec<u32>,
    flags: Vec<u64>,
    ctx_snapshots: Vec<SnapEntry>,
    #[cfg(feature = "trace")]
    rule_stack: Vec<crate::types::RuleId>,
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
            #[cfg(feature = "std")]
            memo: None,
            error_ctx: ErrorContext::new(),
            context_stack: Vec::with_capacity(16),
            flags: Vec::with_capacity(4),
            ctx_snapshots: Vec::with_capacity(64),
            #[cfg(feature = "trace")]
            rule_stack: Vec::with_capacity(64),
        }
    }

    #[must_use]
    #[cfg(feature = "std")]
    pub fn with_memo(self) -> Self {
        Self {
            memo: Some(MemoTable::new()),
            ..self
        }
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
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
                #[cfg(feature = "trace")]
                observer: None,
                #[cfg(feature = "trace")]
                tracer: None,
                #[cfg(feature = "trace")]
                rule_stack: &mut self.rule_stack,
            },
        )
        .map(|consumed| ParseOutput {
            consumed,
            events: core::mem::take(&mut self.events),
            tree_events: core::mem::take(&mut self.tree_events),
        })
    }

    /// Parse the input starting from a specific rule id (treating it as an entrypoint).
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::NoMatch(_))` on parse failure, or `Err(ParseError::BadGraph(_))`
    /// if `rule` is out of range or the graph is invalid.
    pub fn parse_rule(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        rule: RuleId,
    ) -> Result<ParseOutput, ParseError> {
        self.parse_rule_with_context(graph, input, rule, &ParseContext::new())
    }

    /// Like [`parse_rule`](Self::parse_rule) but resolves the rule by name.
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::UnknownRuleName(_))` if `rule_name` is not present in the graph.
    pub fn parse_rule_named(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        rule_name: &str,
    ) -> Result<ParseOutput, ParseError> {
        let rule = graph
            .rule_id(rule_name)
            .ok_or_else(|| ParseError::UnknownRuleName(rule_name.to_string()))?;
        self.parse_rule(graph, input, rule)
    }

    /// Parse a specific rule starting at byte position 0, with an explicit [`ParseContext`].
    ///
    /// The rule is treated as an entrypoint: when it reaches `Return`, the VM will treat
    /// returning to a sentinel address as success and return the current `pos` as `consumed`.
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::NoMatch(_))` on parse failure, or `Err(ParseError::BadGraph(_))`
    /// if `rule` is out of range or the graph is invalid.
    pub fn parse_rule_with_context(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        rule: RuleId,
        context: &ParseContext,
    ) -> Result<ParseOutput, ParseError> {
        self.reset_for_parse(context);

        let entry = *graph
            .rule_entry
            .get(rule as usize)
            .ok_or(ParseError::BadGraph(
                super::error::BadGraphKind::RuleEntryOutOfRange { rule },
            ))?;

        // Seed a sentinel return address so rules ending in `Return` can be used as entrypoints.
        self.stack.push(Frame::Return { ret_ip: u32::MAX });

        run_from(
            graph,
            input,
            &mut VmState {
                stack: &mut self.stack,
                events: &mut self.events,
                tree_events: &mut self.tree_events,
                open_tokens: &mut self.open_tokens,
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
                #[cfg(feature = "trace")]
                observer: None,
                #[cfg(feature = "trace")]
                tracer: None,
                #[cfg(feature = "trace")]
                rule_stack: &mut self.rule_stack,
            },
            entry,
            0,
        )
        .map(|consumed| ParseOutput {
            consumed,
            events: core::mem::take(&mut self.events),
            tree_events: core::mem::take(&mut self.tree_events),
        })
    }

    /// Parse a specific rule starting at `start_pos` within `input`.
    ///
    /// Feature-gated experimental API used for partial reparsing.
    ///
    /// The rule is treated as an entrypoint: when it reaches `Return`, the VM will treat
    /// returning to a sentinel address as success and return the current `pos` as `consumed`.
    ///
    /// # Errors
    ///
    /// Returns `Err(ParseError::NoMatch(_))` on parse failure, or `Err(ParseError::BadGraph(_))`
    /// if `rule` is out of range or the graph is invalid.
    #[cfg(feature = "partial-reparse")]
    pub fn parse_rule_at_with_context(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        rule: crate::types::RuleId,
        start_pos: Pos,
        context: &ParseContext,
    ) -> Result<ParseOutput, ParseError> {
        self.reset_for_parse(context);

        let entry = *graph
            .rule_entry
            .get(rule as usize)
            .ok_or(ParseError::BadGraph(
                super::error::BadGraphKind::RuleEntryOutOfRange { rule },
            ))?;

        // Seed a sentinel return address so rules ending in `Return` can be used as entrypoints.
        self.stack.push(Frame::Return { ret_ip: u32::MAX });

        run_from(
            graph,
            input,
            &mut VmState {
                stack: &mut self.stack,
                events: &mut self.events,
                tree_events: &mut self.tree_events,
                open_tokens: &mut self.open_tokens,
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
                #[cfg(feature = "trace")]
                observer: None,
                #[cfg(feature = "trace")]
                tracer: None,
                #[cfg(feature = "trace")]
                rule_stack: &mut self.rule_stack,
            },
            entry,
            start_pos,
        )
        .map(|consumed| ParseOutput {
            consumed,
            events: core::mem::take(&mut self.events),
            tree_events: core::mem::take(&mut self.tree_events),
        })
    }

    /// Like [`parse_with_context`](Self::parse_with_context) but with a VM observer hook.
    ///
    /// Requires the `trace` feature.
    #[cfg(feature = "trace")]
    pub fn parse_with_observer(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        context: &ParseContext,
        observer: &mut dyn VmObserver,
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
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
                observer: Some(observer),
                tracer: None,
                rule_stack: &mut self.rule_stack,
            },
        )
        .map(|consumed| ParseOutput {
            consumed,
            events: core::mem::take(&mut self.events),
            tree_events: core::mem::take(&mut self.tree_events),
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
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
                #[cfg(feature = "trace")]
                observer: None,
                #[cfg(feature = "trace")]
                tracer: None,
                #[cfg(feature = "trace")]
                rule_stack: &mut self.rule_stack,
            },
        ) {
            Ok(consumed) => Ok(ParseOutput {
                consumed,
                events: core::mem::take(&mut self.events),
                tree_events: core::mem::take(&mut self.tree_events),
            }),
            Err(e) => {
                if let Some(kind) = context.error_node_kind() {
                    insert_error_node_events(&mut self.tree_events, self.error_ctx.furthest, kind);
                }
                Err((
                    ParseOutput {
                        consumed: self.error_ctx.furthest,
                        events: core::mem::take(&mut self.events),
                        tree_events: core::mem::take(&mut self.tree_events),
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
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut multi_errors,
                max_errors: cap,
                #[cfg(feature = "trace")]
                observer: None,
                #[cfg(feature = "trace")]
                tracer: None,
                #[cfg(feature = "trace")]
                rule_stack: &mut self.rule_stack,
            },
        ) {
            Ok(consumed) => Ok(ParseOutput {
                consumed,
                events: core::mem::take(&mut self.events),
                tree_events: core::mem::take(&mut self.tree_events),
            }),
            Err(e) => {
                errors.push(e);
                if let Some(kind) = context.error_node_kind() {
                    insert_error_node_events(&mut self.tree_events, self.error_ctx.furthest, kind);
                }
                Err(RecoverMultiResult {
                    partial: ParseOutput {
                        consumed: self.error_ctx.furthest,
                        events: core::mem::take(&mut self.events),
                        tree_events: core::mem::take(&mut self.tree_events),
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
        #[cfg(feature = "std")]
        {
            self.memo.as_ref().map(memo::MemoTable::len)
        }
        #[cfg(not(feature = "std"))]
        {
            None
        }
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
        self.context_stack.clear();
        self.ctx_snapshots.clear();
        #[cfg(feature = "trace")]
        self.rule_stack.clear();
        #[cfg(feature = "std")]
        if let Some(m) = &mut self.memo {
            m.clear();
        }

        self.flags.clear();
        self.flags.extend_from_slice(context.words());
    }
}

#[cfg(feature = "trace")]
impl Engine {
    /// Like [`parse_with_context`](Self::parse_with_context) but with a runtime tracer.
    pub fn parse_with_tracer(
        &mut self,
        graph: &ParseGraph<'_>,
        input: &[u8],
        context: &ParseContext,
        tracer: &mut dyn ParseTracer,
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
                #[cfg(feature = "std")]
                memo: &mut self.memo,
                error_ctx: &mut self.error_ctx,
                context_stack: &mut self.context_stack,
                flags: &mut self.flags,
                ctx_snapshots: &mut self.ctx_snapshots,
                multi_errors: &mut None,
                max_errors: 0,
                observer: None,
                tracer: Some(tracer),
                rule_stack: &mut self.rule_stack,
            },
        )
        .map(|consumed| ParseOutput {
            consumed,
            events: core::mem::take(&mut self.events),
            tree_events: core::mem::take(&mut self.tree_events),
        })
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}
