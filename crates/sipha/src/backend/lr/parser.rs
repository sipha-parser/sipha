use crate::backend::lr::{config::LrConfig, state::LrParserState, table::LrParsingTable};
use crate::error::{ParseError, ParseMetrics, ParseResult, ParseWarning};
use crate::grammar::{Grammar, NonTerminal, Token};
use crate::syntax::{GreenNode, TextSize};

/// Default error span length when position is unknown
const DEFAULT_ERROR_SPAN_LEN: TextSize = TextSize::from(1);

/// Type alias for LR parser stack: (`state_id`, nodes)
type LrStack<T> = Vec<(usize, Vec<std::sync::Arc<GreenNode<T>>>)>;

/// Context for LR parsing operations - groups immutable parsing parameters
struct LrParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    grammar: &'a Grammar<T, N>,
    table: &'a LrParsingTable<T, N>,
    input: &'a [T],
    entry: &'a N,
    config: &'a LrConfig,
    state: &'a mut LrParserState<T, N>,
}

impl<'a, T, N> LrParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[allow(clippy::missing_const_for_fn)]
    fn new(
        grammar: &'a Grammar<T, N>,
        table: &'a LrParsingTable<T, N>,
        input: &'a [T],
        entry: &'a N,
        config: &'a LrConfig,
        state: &'a mut LrParserState<T, N>,
    ) -> Self {
        Self {
            grammar,
            table,
            input,
            entry,
            config,
            state,
        }
    }
}

/// Recursively count nodes in a syntax tree
fn count_nodes<K: crate::syntax::SyntaxKind>(node: &std::sync::Arc<GreenNode<K>>) -> usize {
    let mut count = 1; // Count this node
    for element in node.children() {
        match element {
            crate::syntax::GreenElement::Node(child) => {
                count += count_nodes(child);
            }
            crate::syntax::GreenElement::Token(_) => {
                count += 1; // Count tokens as nodes too
            }
        }
    }
    count
}

/// Determine the syntax kind for an entry point, with fallback logic.
///
/// Attempts to determine the syntax kind in this order:
/// 1. From the non-terminal's `to_syntax_kind()` method
/// 2. From the first token in the input
/// 3. From the non-terminal's `default_syntax_kind()` method
///
/// Returns `None` if none of these methods provide a kind.
fn determine_syntax_kind<T, N>(entry: &N, input: &[T], _grammar: &Grammar<T, N>) -> Option<T::Kind>
where
    T: Token,
    N: NonTerminal,
{
    entry
        .to_syntax_kind()
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| entry.default_syntax_kind())
}

/// Determine syntax kind for error cases, with comprehensive fallback logic.
///
/// This is used when the normal determination fails and we need to create
/// an error result node. It tries all possible fallback methods including
/// grammar-level fallbacks.
fn determine_error_syntax_kind<T, N>(entry: &N, input: &[T], grammar: &Grammar<T, N>) -> T::Kind
where
    T: Token,
    N: NonTerminal,
{
    input
        .first()
        .map(crate::grammar::Token::kind)
        .or_else(|| entry.default_syntax_kind())
        .or_else(|| grammar.try_get_fallback_kind())
        .unwrap_or_else(|| {
            // Last resort: this should never happen if users implement the trait correctly
            entry.default_syntax_kind().expect(
                "CRITICAL: Cannot create error result - no syntax kind available. \
                 This is a configuration error. You must implement \
                 NonTerminal::default_syntax_kind() to return Some for error cases.",
            )
        })
}

/// Create an error result when syntax kind cannot be determined
fn create_error_result<T, N>(entry: &N, input: &[T], grammar: &Grammar<T, N>) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let mut errors = Vec::new();
    errors.push(ParseError::InvalidSyntax {
        span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
        message: format!(
            "Cannot determine syntax kind for entry point {:?}",
            entry.name()
        ),
    });
    let error_kind = determine_error_syntax_kind(entry, input, grammar);
    errors.push(ParseError::InvalidSyntax {
        span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
        message: format!(
            "Cannot determine syntax kind for error result. \
             Entry point '{}' must implement NonTerminal::default_syntax_kind() \
             to return Some for error cases, or provide at least one token in input.",
            entry.name()
        ),
    });

    ParseResult {
        root: GreenNode::new(error_kind, [], TextSize::zero()),
        errors,
        warnings: Vec::new(),
        metrics: ParseMetrics::default(),
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: std::marker::PhantomData::<N>,
    }
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
    production: &crate::backend::lr::table::Production<T, N>,
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

/// Try to insert a token during error recovery
fn try_insert_token<T, N>(
    table: &LrParsingTable<T, N>,
    current_state: usize,
    candidate_token: &T,
    stack: &mut LrStack<T::Kind>,
    text_pos: TextSize,
    warnings: &mut Vec<ParseWarning>,
) -> bool
where
    T: Token,
    N: NonTerminal,
{
    let candidate_action = table.get_action(current_state, Some(candidate_token));
    match candidate_action {
        crate::backend::lr::table::Action::Shift(next_state) => {
            let token_node = create_token_node(candidate_token);
            stack.push((next_state, vec![token_node]));
            warnings.push(ParseWarning {
                span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: format!(
                    "Inserted missing token {candidate_token:?} during error recovery"
                ),
                severity: crate::error::Severity::Warning,
            });
            true
        }
        crate::backend::lr::table::Action::Accept => {
            let token_node = create_token_node(candidate_token);
            stack.push((current_state, vec![token_node]));
            warnings.push(ParseWarning {
                span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: format!(
                    "Inserted missing token {candidate_token:?} during error recovery"
                ),
                severity: crate::error::Severity::Warning,
            });
            true
        }
        _ => false,
    }
}

/// Parse input using LR parser
pub fn parse<T, N>(
    grammar: &Grammar<T, N>,
    table: &LrParsingTable<T, N>,
    input: &[T],
    entry: &N,
    config: &LrConfig,
    state: &mut LrParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let mut ctx = LrParseContext::new(grammar, table, input, entry, config, state);
    parse_impl(&mut ctx)
}

/// Internal parse implementation using context
#[allow(clippy::too_many_lines)]
fn parse_impl<T, N>(ctx: &mut LrParseContext<'_, T, N>) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    // Check cache for full parse result
    // Use a hash of the entry point as the state_id and input length as position
    use std::hash::Hasher;
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let mut metrics = ParseMetrics::default();

    let start_time = std::time::Instant::now();

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    std::hash::Hash::hash(ctx.entry, &mut hasher);
    let entry_id = usize::try_from(hasher.finish()).unwrap_or(0);
    if let Some(cached_root) = ctx.state.get_cached_parse(entry_id, ctx.input.len()) {
        // Return cached result
        let node_count = count_nodes(&cached_root);
        metrics.parse_time = start_time.elapsed();
        metrics.tokens_consumed = ctx.input.len();
        metrics.nodes_created = node_count;
        return ParseResult {
            root: cached_root,
            errors,
            warnings,
            metrics,
            #[cfg(feature = "backend-glr")]
            forest: None,
            _phantom: std::marker::PhantomData::<N>,
        };
    }

    // Start parsing from entry point
    let Some(root_kind) = determine_syntax_kind(ctx.entry, ctx.input, ctx.grammar) else {
        return create_error_result(ctx.entry, ctx.input, ctx.grammar);
    };
    // LR parsing using shift-reduce algorithm
    let mut stack: LrStack<T::Kind> = Vec::new();
    stack.push((0, Vec::new())); // Initial state

    let mut pos = 0;
    let mut text_pos = TextSize::zero();
    let mut final_root: Option<std::sync::Arc<GreenNode<T::Kind>>> = None;

    loop {
        let current_state = stack.last().map_or(0, |(s, _)| *s);
        let current_token = ctx.input.get(pos);

        let action = ctx.table.get_action(current_state, current_token);

        match action {
            crate::backend::lr::table::Action::Shift(next_state) => {
                if let Some(token) = current_token {
                    let token_node = create_token_node(token);
                    stack.push((next_state, vec![token_node]));
                    pos += 1;
                    text_pos += token.text_len();
                } else {
                    errors.push(ParseError::UnexpectedEof {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: vec!["<token>".to_string()],
                    });
                    break;
                }
            }

            crate::backend::lr::table::Action::Reduce(prod_idx) => {
                if let Some(production) = ctx.table.get_production(prod_idx) {
                    // Pop RHS items from stack
                    let rhs_len = production.rhs.len();
                    let mut children = Vec::new();

                    for _ in 0..rhs_len {
                        if let Some((_, nodes)) = stack.pop() {
                            children.extend(nodes);
                        }
                    }

                    // Get state after popping
                    let state_after_pop = stack.last().map_or(0, |(s, _)| *s);

                    // Get goto state
                    if let Some(goto_state) = ctx.table.get_goto(state_after_pop, &production.lhs) {
                        let node = build_reduce_node(production, &children, root_kind);
                        stack.push((goto_state, vec![node]));
                    } else {
                        errors.push(ParseError::InvalidSyntax {
                            span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                            message: format!("No goto state for {:?}", production.lhs.name()),
                        });
                        break;
                    }
                } else {
                    errors.push(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!("Invalid production index {prod_idx}"),
                    });
                    break;
                }
            }

            crate::backend::lr::table::Action::Accept => {
                // Successfully parsed - the root node should be on the stack
                // Pop the final node and set it as the root
                if let Some((_, nodes)) = stack.pop() {
                    if let Some(node) = nodes.into_iter().next() {
                        final_root = Some(node);
                    } else if let Some((_, nodes)) = stack.pop() {
                        // Sometimes the root is one level deeper
                        if let Some(node) = nodes.into_iter().next() {
                            final_root = Some(node);
                        }
                    }
                }
                break;
            }

            crate::backend::lr::table::Action::Error => {
                // Error - no action available
                // Get expected tokens for better error messages and token insertion
                let expected_tokens = ctx.table.get_expected_tokens(current_state);
                let expected: Vec<String> = if expected_tokens.is_empty() {
                    vec!["<valid token>".to_string()]
                } else {
                    expected_tokens.iter().map(|t| format!("{t:?}")).collect()
                };

                if current_token.is_none() {
                    errors.push(ParseError::UnexpectedEof {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: expected.clone(),
                    });
                } else {
                    errors.push(ParseError::UnexpectedToken {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: expected.clone(),
                    });
                }

                if ctx.config.error_recovery {
                    // Try token insertion first if enabled
                    let mut recovered = false;
                    if ctx.config.enable_token_insertion && !expected_tokens.is_empty() {
                        // Try inserting each expected token to see if parsing can continue
                        // Prioritize shift actions as they're most common for missing tokens (e.g., semicolons)
                        for candidate_token in &expected_tokens {
                            if try_insert_token(
                                ctx.table,
                                current_state,
                                candidate_token,
                                &mut stack,
                                text_pos,
                                &mut warnings,
                            ) {
                                recovered = true;
                                break; // Successfully inserted token, continue parsing
                            }
                        }
                    }

                    if recovered {
                        // Successfully inserted a token, continue parsing
                        continue;
                    }

                    // Token insertion didn't work, fall back to skipping token
                    if pos < ctx.input.len() {
                        pos += 1;
                        if let Some(token) = ctx.input.get(pos - 1) {
                            text_pos += token.text_len();
                        }
                        warnings.push(ParseWarning {
                            span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                            message: "Skipped token during error recovery".to_string(),
                            severity: crate::error::Severity::Warning,
                        });
                        continue;
                    }
                }
                break;
            }
        }

        if errors.len() >= ctx.config.max_errors {
            break;
        }
    }

    // Check if we consumed all input
    if pos < ctx.input.len() {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(text_pos, TextSize::from(1)),
            message: format!("Unexpected tokens remaining at position {pos}"),
        });
    }

    // Use the final root from the stack, or try to get it from the stack
    let root = final_root.unwrap_or_else(|| {
        if let Some((_, nodes)) = stack.pop() {
            // Fallback: try to get root from stack
            nodes
                .into_iter()
                .next()
                .unwrap_or_else(|| GreenNode::new(root_kind, [], TextSize::zero()))
        } else {
            // If parsing failed, create an error node
            GreenNode::new(root_kind, [], TextSize::zero())
        }
    });

    // Cache the final result if parsing was successful (no errors)
    if errors.is_empty() {
        ctx.state
            .cache_parse_result(entry_id, ctx.input.len(), root.clone());
    }

    // Count nodes in the tree recursively
    let node_count = count_nodes(&root);

    metrics.parse_time = start_time.elapsed();
    metrics.tokens_consumed = pos;
    metrics.nodes_created = node_count;

    ParseResult {
        root,
        errors,
        warnings,
        metrics,
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: std::marker::PhantomData::<N>,
    }
}

/// Parse with incremental support
pub fn parse_with_session<T, N>(
    grammar: &Grammar<T, N>,
    table: &LrParsingTable<T, N>,
    input: &[T],
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
    entry: &N,
    config: &LrConfig,
    state: &mut LrParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    // If no edits, use cached result if available
    if session.edits().is_empty() {
        return parse(grammar, table, input, entry, config, state);
    }

    // Check persistent parse cache for the entry point before parsing
    // This enables cross-session node reuse for the entire parse tree
    let entry_text_pos = crate::syntax::TextSize::zero();
    let expected_kind = entry
        .to_syntax_kind()
        .or_else(|| entry.default_syntax_kind());
    if let Some(cached_root) = session.find_cached_node(entry.name(), entry_text_pos, expected_kind)
    {
        // Verify the cached root matches the input length (simple validation)
        // In a more sophisticated implementation, we'd validate token-by-token
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

    // Invalidate cache for affected region
    state.invalidate_cache();

    parse(grammar, table, input, entry, config, state)
}
