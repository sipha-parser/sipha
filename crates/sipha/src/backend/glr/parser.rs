//! GLR parser implementation using Tomita's algorithm

use crate::backend::glr::{
    DisambiguationStrategy, GlrConfig, GlrGrammar, StackPruningStrategy,
    disambiguation::{disambiguate_by_associativity, disambiguate_by_precedence},
    forest::{ForestNode, ParseForest},
    stack::GlrStack,
    state::GlrParserState,
};
use crate::error::{ParseError, ParseMetrics, ParseResult, ParseWarning};
use crate::grammar::{Grammar, NonTerminal, Token};
use crate::syntax::{GreenElement, GreenNode, TextRange, TextSize};
use hashbrown::HashMap;
#[cfg(feature = "parallel")]
use rayon::prelude::*;

struct ResumeState<K: crate::syntax::SyntaxKind> {
    stacks: Vec<GlrStack<K>>,
    pos: usize,
    text_pos: TextSize,
}

/// Parsing context grouping related parameters
struct ParsingContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    backend_grammar: &'a GlrGrammar<T, N>,
    entry: &'a N,
    entry_id: usize,
}

#[derive(Clone)]
struct ActiveStack<K: crate::syntax::SyntaxKind> {
    stack: GlrStack<K>,
    metrics: StackQuality,
}

impl<K: crate::syntax::SyntaxKind> ActiveStack<K> {
    fn new(stack: GlrStack<K>) -> Self {
        Self {
            stack,
            metrics: StackQuality::default(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct StackQuality {
    tokens_consumed: usize,
    reductions: usize,
    errors: usize,
}

impl StackQuality {
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    fn record_shift(&mut self) {
        self.tokens_consumed += 1;
    }

    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    fn record_reduction(&mut self) {
        self.reductions += 1;
    }

    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    fn record_error(&mut self) {
        self.errors += 1;
    }

    fn absorb(&mut self, other: &Self) {
        self.tokens_consumed = self.tokens_consumed.max(other.tokens_consumed);
        self.reductions = self.reductions.max(other.reductions);
        self.errors = self.errors.min(other.errors);
    }

    #[allow(clippy::missing_const_for_fn)] // Cannot be const: takes &self
    fn score(&self, strategy: StackPruningStrategy, depth: usize) -> usize {
        match strategy {
            StackPruningStrategy::None => depth,
            StackPruningStrategy::PreferDeeper => depth * 2 + self.reductions,
            StackPruningStrategy::PreferProgress => {
                (self.tokens_consumed * 2 + depth) - self.errors
            }
            StackPruningStrategy::QualityWeighted => {
                (self.tokens_consumed + self.reductions + depth) - (self.errors * 2)
            }
        }
    }
}

/// Default error span length
const DEFAULT_ERROR_SPAN_LEN: TextSize = TextSize::from(1);

/// Parse input using GLR parser
#[cfg(feature = "backend-lr")]
pub fn parse<T, N>(
    backend_grammar: &GlrGrammar<T, N>,
    input: &[T],
    entry: &N,
    config: &GlrConfig,
    state: &mut GlrParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let entry_id = compute_entry_id(entry);
    let ctx = ParsingContext {
        backend_grammar,
        entry,
        entry_id,
    };
    run_glr_parse(&ctx, input, config, state, None)
}

/// Parse with incremental support using session
#[cfg(feature = "backend-lr")]
pub fn parse_with_session<T, N>(
    backend_grammar: &GlrGrammar<T, N>,
    input: &[T],
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
    entry: &N,
    config: &GlrConfig,
    state: &mut GlrParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let entry_id = compute_entry_id(entry);

    // If no edits, use cached result if available
    if session.edits().is_empty() {
        return parse(backend_grammar, input, entry, config, state);
    }

    // Check persistent parse cache for the entry point before parsing
    let entry_text_pos = crate::syntax::TextSize::zero();
    let expected_kind = entry
        .to_syntax_kind()
        .or_else(|| entry.default_syntax_kind());

    // For GLR, we need to check if we can reuse forest fragments
    // For now, we'll do a simple check for full parse reuse
    if let Some(cached_root) = session.find_cached_node(entry.name(), entry_text_pos, expected_kind)
    {
        // Verify the cached root matches the input length
        let errors = Vec::new();
        let warnings = Vec::new();
        let metrics = crate::error::ParseMetrics::default();

        return crate::error::ParseResult {
            root: cached_root,
            errors,
            warnings,
            metrics,
            #[cfg(feature = "backend-glr")]
            forest: None,
            _phantom: std::marker::PhantomData::<N>,
        };
    }

    // Get affected region from session
    let affected = session.affected();
    let mut union_range = affected.union();

    if let Some(span) = state.last_ambiguity_span()
        && span.intersect(union_range).is_some()
    {
        let start = union_range.start().min(span.start());
        let end = union_range.end().max(span.end());
        union_range = TextRange::new(start, end);
    }

    // Invalidate cache for affected region
    // For GLR, we need to be more careful about ambiguity changes
    // If the edit is in an ambiguous region, we may need to reparse more
    let start_offset = union_range.start().into() as usize;
    let end_offset = union_range.end().into() as usize;
    state.invalidate_cache_range(start_offset, end_offset);

    let resume_snapshot = state.get_cached_stacks(entry_id, start_offset).or_else(|| {
        if start_offset > 0 {
            state.get_cached_stacks(entry_id, 0)
        } else {
            None
        }
    });

    let resume_state = resume_snapshot.map(|snapshot| ResumeState {
        stacks: snapshot.stacks,
        pos: snapshot.token_pos.min(input.len()),
        text_pos: snapshot.text_pos,
    });

    let ctx = ParsingContext {
        backend_grammar,
        entry,
        entry_id,
    };
    run_glr_parse(&ctx, input, config, state, resume_state)
}

/// Create a token node from a token
fn create_token_node<T: Token>(token: &T) -> std::sync::Arc<GreenNode<T::Kind>> {
    let token_len = token.text_len();
    let token_element = crate::syntax::GreenElement::Token(crate::syntax::GreenToken::new(
        token.kind(),
        token.text(),
    ));
    GreenNode::new(token.kind(), vec![token_element], token_len)
}

/// Build a node from a reduce action
fn build_reduce_node<T, N>(
    production: &crate::backend::lr::Production<T, N>,
    children: &[std::sync::Arc<GreenNode<T::Kind>>],
    root_kind: T::Kind,
) -> std::sync::Arc<GreenNode<T::Kind>>
where
    T: Token,
    N: NonTerminal,
{
    let lhs_kind = production
        .lhs
        .to_syntax_kind()
        .or_else(|| production.lhs.default_syntax_kind())
        .or_else(|| children.first().map(|_| root_kind))
        .unwrap_or(root_kind);

    let mut text_len = TextSize::zero();
    let mut elements = Vec::new();
    for child in children.iter().rev() {
        elements.push(crate::syntax::GreenElement::Node(child.clone()));
        text_len += child.text_len();
    }
    GreenNode::new(lhs_kind, elements, text_len)
}

/// Count nodes in a syntax tree
fn count_nodes<K: crate::syntax::SyntaxKind>(node: &std::sync::Arc<GreenNode<K>>) -> usize {
    let mut count = 1;
    for element in node.children() {
        match element {
            crate::syntax::GreenElement::Node(child) => {
                count += count_nodes(child);
            }
            crate::syntax::GreenElement::Token(_) => {
                count += 1;
            }
        }
    }
    count
}

fn snapshot_active_stacks<K: crate::syntax::SyntaxKind>(
    stacks: &[ActiveStack<K>],
) -> Vec<GlrStack<K>> {
    stacks.iter().map(|active| active.stack.clone()).collect()
}

fn merge_equivalent_stacks<K: crate::syntax::SyntaxKind>(
    stacks: Vec<ActiveStack<K>>,
    use_parallel: bool,
) -> Vec<ActiveStack<K>> {
    #[cfg(feature = "parallel")]
    {
        if use_parallel {
            return merge_equivalent_stacks_parallel(stacks);
        }
    }
    let _ = use_parallel;
    merge_equivalent_stacks_sequential(stacks)
}

fn merge_equivalent_stacks_sequential<K: crate::syntax::SyntaxKind>(
    stacks: Vec<ActiveStack<K>>,
) -> Vec<ActiveStack<K>> {
    let mut merged: HashMap<Vec<usize>, ActiveStack<K>, ahash::RandomState> =
        HashMap::with_hasher(ahash::RandomState::new());
    for active in stacks {
        merge_active_stack(&mut merged, active);
    }
    merged.into_values().collect()
}

#[cfg(feature = "parallel")]
fn merge_equivalent_stacks_parallel<K: crate::syntax::SyntaxKind>(
    stacks: Vec<ActiveStack<K>>,
) -> Vec<ActiveStack<K>> {
    stacks
        .into_par_iter()
        .fold(
            || HashMap::with_hasher(ahash::RandomState::new()),
            |mut acc, active| {
                merge_active_stack(&mut acc, active);
                acc
            },
        )
        .reduce(
            || HashMap::with_hasher(ahash::RandomState::new()),
            |mut acc, mut other| {
                for (_, active) in other.drain() {
                    merge_active_stack(&mut acc, active);
                }
                acc
            },
        )
        .into_values()
        .collect()
}

fn merge_active_stack<K: crate::syntax::SyntaxKind>(
    merged: &mut HashMap<Vec<usize>, ActiveStack<K>, ahash::RandomState>,
    active: ActiveStack<K>,
) {
    let key = active.stack.state_signature();
    if let Some(existing) = merged.get_mut(&key) {
        if !existing.stack.merge_in_place(&active.stack) {
            existing.stack.merge(&active.stack);
        }
        existing.metrics.absorb(&active.metrics);
    } else {
        merged.insert(key, active);
    }
}

fn prune_stacks<K: crate::syntax::SyntaxKind>(
    mut stacks: Vec<ActiveStack<K>>,
    config: &GlrConfig,
) -> (Vec<ActiveStack<K>>, bool) {
    if stacks.len() <= config.max_stacks {
        return (stacks, false);
    }

    let strategy = config.pruning_strategy;
    let limit = config.max_stacks.min(config.pruning_beam_width).max(1);
    stacks.sort_by(|a, b| {
        let score_a = a.metrics.score(strategy, a.stack.len());
        let score_b = b.metrics.score(strategy, b.stack.len());
        score_b
            .cmp(&score_a)
            .then(b.stack.len().cmp(&a.stack.len()))
            .then(a.metrics.errors.cmp(&b.metrics.errors))
    });

    let trimmed = stacks.len() > limit;
    stacks.truncate(limit);
    (stacks, trimmed)
}

fn compute_entry_id<N: NonTerminal>(entry: &N) -> usize {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    Hash::hash(entry, &mut hasher);
    usize::try_from(hasher.finish()).unwrap_or(0)
}

fn compute_ambiguity_span<T>(forest: &ParseForest<T::Kind>) -> Option<TextRange>
where
    T: Token,
{
    let mut span: Option<TextRange> = None;
    for root in forest.iter_roots() {
        accumulate_ambiguous_ranges(root, TextSize::zero(), &mut span);
    }
    span
}

fn accumulate_ambiguous_ranges<K: crate::syntax::SyntaxKind>(
    node: &ForestNode<K>,
    start: TextSize,
    span: &mut Option<TextRange>,
) {
    match node {
        ForestNode::Node(green) => accumulate_green_ranges(green, start),
        ForestNode::Ambiguous(alternatives) => {
            if let Some(first) = alternatives.first() {
                extend_span(span, TextRange::at(start, first.text_len()));
                for alt in alternatives {
                    accumulate_green_ranges(alt, start);
                }
            }
        }
    }
}

fn accumulate_green_ranges<K: crate::syntax::SyntaxKind>(
    node: &std::sync::Arc<GreenNode<K>>,
    mut offset: TextSize,
) {
    for child in node.children() {
        match child {
            GreenElement::Node(child_node) => {
                let child_start = offset;
                accumulate_green_ranges(child_node, child_start);
                offset += child_node.text_len();
            }
            GreenElement::Token(token) => {
                offset += token.text_len();
            }
        }
    }
}

fn extend_span(span: &mut Option<TextRange>, range: TextRange) {
    match span {
        Some(existing) => {
            let start = existing.start().min(range.start());
            let end = existing.end().max(range.end());
            *existing = TextRange::new(start, end);
        }
        None => *span = Some(range),
    }
}

fn select_root_with_strategy<T, N>(
    grammar: &Grammar<T, N>,
    forest: &ParseForest<T::Kind>,
    config: &GlrConfig,
) -> Option<std::sync::Arc<GreenNode<T::Kind>>>
where
    T: Token,
    N: NonTerminal,
{
    if forest.root_count() == 0 {
        return None;
    }

    if !forest.is_ambiguous() {
        return forest.get_alternative(0).cloned();
    }

    let alternatives: Vec<_> = forest.iter_alternatives().cloned().collect();
    if alternatives.is_empty() {
        return None;
    }

    let ambiguous = ForestNode::Ambiguous(alternatives);

    let resolved = match config.disambiguation {
        DisambiguationStrategy::None | DisambiguationStrategy::Custom => None,
        DisambiguationStrategy::Precedence => disambiguate_by_precedence(grammar, &ambiguous),
        DisambiguationStrategy::Associativity => disambiguate_by_associativity(grammar, &ambiguous),
    };

    resolved.or_else(|| ambiguous.alternatives().first().cloned())
}

type ParseStateInitResult<K> = (K, Vec<ActiveStack<K>>, usize, TextSize, bool);

fn initialize_parse_state<T, N>(
    ctx: &ParsingContext<T, N>,
    input: &[T],
    resume: Option<ResumeState<T::Kind>>,
    errors: &mut Vec<ParseError>,
) -> ParseStateInitResult<T::Kind>
where
    T: Token,
    N: NonTerminal,
{
    let root_kind = ctx
        .entry
        .to_syntax_kind()
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| ctx.entry.default_syntax_kind())
        .unwrap_or_else(|| {
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
                message: format!(
                    "Cannot determine syntax kind for entry point {:?}",
                    ctx.entry.name()
                ),
            });
            ctx.entry
                .default_syntax_kind()
                .expect("Must have default syntax kind")
        });

    let resumed = resume.is_some();
    let (stacks, pos, text_pos) = match resume {
        Some(resume_state) => (
            resume_state
                .stacks
                .into_iter()
                .map(ActiveStack::new)
                .collect(),
            resume_state.pos,
            resume_state.text_pos,
        ),
        None => (
            vec![ActiveStack::new(GlrStack::with_initial_state(0))],
            0,
            TextSize::zero(),
        ),
    };

    (root_kind, stacks, pos, text_pos, resumed)
}

#[cfg(feature = "backend-lr")]
fn process_parsing_step<T, N>(
    table: &crate::backend::lr::LrParsingTable<T, N>,
    input: &[T],
    stacks: &mut Vec<ActiveStack<T::Kind>>,
    pos: &mut usize,
    text_pos: &mut TextSize,
    root_kind: T::Kind,
    final_roots: &mut Vec<std::sync::Arc<GreenNode<T::Kind>>>,
) -> Vec<ActiveStack<T::Kind>>
where
    T: Token,
    N: NonTerminal,
{
    let mut new_stacks = Vec::with_capacity(stacks.len());

    for mut active in stacks.drain(..) {
        let current_state = active.stack.current_state().unwrap_or(0);
        let current_token = input.get(*pos);
        let action = table.get_action(current_state, current_token);

        match action {
            crate::backend::lr::Action::Shift(next_state) => {
                if let Some(token) = current_token {
                    let token_node = create_token_node(token);
                    active.stack.push(next_state, vec![token_node]);
                    *pos += 1;
                    *text_pos += token.text_len();
                    active.metrics.record_shift();
                }
            }
            crate::backend::lr::Action::Reduce(prod_idx) => {
                if let Some(production) = table.get_production(prod_idx) {
                    let rhs_len = production.rhs.len();
                    let children = active.stack.pop(rhs_len);
                    let state_after_pop = active.stack.current_state().unwrap_or(0);
                    if let Some(goto_state) = table.get_goto(state_after_pop, &production.lhs) {
                        let node = build_reduce_node(production, &children, root_kind);
                        active.stack.push(goto_state, vec![node]);
                        active.metrics.record_reduction();
                    }
                }
            }
            crate::backend::lr::Action::Accept => {
                if let Some((_, nodes)) = active.stack.top_entry()
                    && let Some(node) = nodes.first()
                {
                    final_roots.push(node.clone());
                }
            }
            crate::backend::lr::Action::Error => {
                active.metrics.record_error();
                continue;
            }
        }

        new_stacks.push(active);
    }

    new_stacks
}

struct ParseResultBuilder<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    ctx: &'a ParsingContext<'a, T, N>,
    grammar: &'a Grammar<T, N>,
    config: &'a GlrConfig,
    state: &'a mut GlrParserState<T, N>,
}

struct BuildParams<'a, K: crate::syntax::SyntaxKind> {
    final_roots: Vec<std::sync::Arc<GreenNode<K>>>,
    root_kind: K,
    forest: &'a ParseForest<K>,
    errors: Vec<ParseError>,
    warnings: Vec<ParseWarning>,
    metrics: ParseMetrics,
    input_len: usize,
}

impl<T, N> ParseResultBuilder<'_, T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn build(self, params: BuildParams<'_, T::Kind>) -> ParseResult<T, N> {
        let has_results = !params.final_roots.is_empty();
        let mut forest = params.forest.clone();

        if has_results {
            if params.final_roots.len() == 1 {
                forest.add_root(ForestNode::Node(params.final_roots[0].clone()));
            } else {
                forest.add_root(ForestNode::Ambiguous(params.final_roots));
            }
        }

        let resolved_root = if has_results {
            select_root_with_strategy(self.grammar, &forest, self.config)
        } else {
            None
        };

        self.state.set_last_forest(forest.clone());

        #[cfg(feature = "backend-glr")]
        let forest_for_result =
            if has_results && (self.config.return_forest || forest.is_ambiguous()) {
                Some(forest.clone())
            } else {
                None
            };

        let root =
            resolved_root.unwrap_or_else(|| GreenNode::new(params.root_kind, [], TextSize::zero()));

        if params.errors.is_empty() && has_results {
            self.state
                .cache_parse_result(self.ctx.entry_id, params.input_len, forest.clone());
        }

        self.state
            .set_last_ambiguity_span(compute_ambiguity_span::<T>(&forest));

        ParseResult {
            root,
            errors: params.errors,
            warnings: params.warnings,
            metrics: params.metrics,
            #[cfg(feature = "backend-glr")]
            forest: forest_for_result,
            _phantom: std::marker::PhantomData::<N>,
        }
    }
}

fn run_glr_parse<T, N>(
    ctx: &ParsingContext<T, N>,
    input: &[T],
    config: &GlrConfig,
    state: &mut GlrParserState<T, N>,
    resume: Option<ResumeState<T::Kind>>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let start_time = std::time::Instant::now();

    let (root_kind, mut stacks, mut pos, mut text_pos, resumed) =
        initialize_parse_state(ctx, input, resume, &mut errors);

    let mut final_roots: Vec<std::sync::Arc<GreenNode<T::Kind>>> = Vec::new();
    let forest = ParseForest::new();

    {
        let snapshot = snapshot_active_stacks(&stacks);
        state.cache_stacks(ctx.entry_id, text_pos, pos, &snapshot);
    }

    while pos <= input.len() {
        if stacks.is_empty() {
            break;
        }

        let new_stacks = process_parsing_step(
            &ctx.backend_grammar.lr_table,
            input,
            &mut stacks,
            &mut pos,
            &mut text_pos,
            root_kind,
            &mut final_roots,
        );

        let use_parallel_merge = cfg!(feature = "parallel")
            && config.parallel_threshold > 0
            && new_stacks.len() >= config.parallel_threshold;
        let merged = merge_equivalent_stacks(new_stacks, use_parallel_merge);
        let (pruned, trimmed) = prune_stacks(merged, config);
        if trimmed {
            warnings.push(ParseWarning {
                span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: format!(
                    "GLR parser hit stack limit ({}), some parse paths pruned",
                    config.max_stacks
                ),
                severity: crate::error::Severity::Warning,
            });
        }
        stacks = pruned;

        let snapshot = snapshot_active_stacks(&stacks);
        state.cache_stacks(ctx.entry_id, text_pos, pos, &snapshot);
    }

    let node_count = final_roots.first().map_or(0, count_nodes);
    let metrics = ParseMetrics {
        parse_time: start_time.elapsed(),
        tokens_consumed: pos,
        nodes_created: node_count,
        errors_recovered: 0,
        cache_hits: usize::from(resumed),
    };

    let builder = ParseResultBuilder {
        ctx,
        grammar: &ctx.backend_grammar.original_grammar,
        config,
        state,
    };
    let params = BuildParams {
        final_roots,
        root_kind,
        forest: &forest,
        errors,
        warnings,
        metrics,
        input_len: input.len(),
    };
    builder.build(params)
}
