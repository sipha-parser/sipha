use crate::backend::lr::{config::LrConfig, state::LrParserState, table::LrParsingTable};
use crate::error::{ParseError, ParseMetrics, ParseResult, ParseWarning};
use crate::grammar::{Grammar, NonTerminal, Token};
use crate::syntax::{GreenNode, TextSize};

/// Default error span length when position is unknown
const DEFAULT_ERROR_SPAN_LEN: TextSize = TextSize::from(1);

/// Type alias for LR parser stack: (`state_id`, nodes)
type LrStack<T> = Vec<(usize, Vec<std::sync::Arc<GreenNode<T>>>)>;

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

/// Parse input using LR parser
#[allow(clippy::too_many_lines)]
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
    // Check cache for full parse result
    // Use a hash of the entry point as the state_id and input length as position
    use std::hash::Hasher;
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let mut metrics = ParseMetrics::default();

    let start_time = std::time::Instant::now();

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    std::hash::Hash::hash(&entry, &mut hasher);
    let entry_id = usize::try_from(hasher.finish()).unwrap_or(0);
    if let Some(cached_root) = state.get_cached_parse(entry_id, input.len()) {
        // Return cached result
        let node_count = count_nodes(&cached_root);
        metrics.parse_time = start_time.elapsed();
        metrics.tokens_consumed = input.len();
        metrics.nodes_created = node_count;
        return ParseResult {
            root: cached_root,
            errors,
            warnings,
            metrics,
            _phantom: std::marker::PhantomData::<N>,
        };
    }

    // Start parsing from entry point
    let Some(root_kind) = determine_syntax_kind(entry, input, grammar) else {
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

        return ParseResult {
            root: GreenNode::new(error_kind, [], TextSize::zero()),
            errors,
            warnings,
            metrics,
            _phantom: std::marker::PhantomData::<N>,
        };
    };
    // LR parsing using shift-reduce algorithm
    let mut stack: LrStack<T::Kind> = Vec::new();
    stack.push((0, Vec::new())); // Initial state

    let mut pos = 0;
    let mut text_pos = TextSize::zero();
    let mut final_root: Option<std::sync::Arc<GreenNode<T::Kind>>> = None;

    loop {
        let current_state = stack.last().map_or(0, |(s, _)| *s);
        let current_token = input.get(pos);

        let action = table.get_action(current_state, current_token);

        match action {
            crate::backend::lr::table::Action::Shift(next_state) => {
                if let Some(token) = current_token {
                    let token_text = token.text();
                    let token_len = token.text_len();
                    // Create a token node (leaf node containing just the token)
                    let token_element = crate::syntax::GreenElement::Token(
                        crate::syntax::GreenToken::new(token.kind(), token_text.clone()),
                    );
                    let token_node = GreenNode::new(token.kind(), vec![token_element], token_len);

                    stack.push((next_state, vec![token_node]));
                    pos += 1;
                    text_pos += token_len;
                } else {
                    errors.push(ParseError::UnexpectedEof {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: vec!["<token>".to_string()],
                    });
                    break;
                }
            }

            crate::backend::lr::table::Action::Reduce(prod_idx) => {
                if let Some(production) = table.get_production(prod_idx) {
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
                    if let Some(goto_state) = table.get_goto(state_after_pop, &production.lhs) {
                        // Create node for LHS
                        let lhs_kind = production
                            .lhs
                            .to_syntax_kind()
                            .or_else(|| production.lhs.default_syntax_kind())
                            .or_else(|| children.first().map(|_| root_kind))
                            .unwrap_or(root_kind);

                        // Build node from children
                        let mut text_len = TextSize::zero();
                        let mut elements = Vec::new();
                        for child in children.iter().rev() {
                            // Convert Arc<GreenNode> to GreenElement
                            elements.push(crate::syntax::GreenElement::Node(child.clone()));
                            text_len += child.text_len();
                        }
                        let node = GreenNode::new(lhs_kind, elements, text_len);

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
                let expected = vec!["<valid token>".to_string()];
                if current_token.is_none() {
                    errors.push(ParseError::UnexpectedEof {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected,
                    });
                } else {
                    errors.push(ParseError::UnexpectedToken {
                        span: crate::syntax::TextRange::at(text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected,
                    });
                }

                if config.error_recovery {
                    // Try error recovery: skip token
                    if pos < input.len() {
                        pos += 1;
                        if let Some(token) = input.get(pos - 1) {
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

        if errors.len() >= config.max_errors {
            break;
        }
    }

    // Check if we consumed all input
    if pos < input.len() {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(text_pos, TextSize::from(1)),
            message: format!("Unexpected tokens remaining at position {pos}"),
        });
    }

    // Use the final root from the stack, or try to get it from the stack
    let root = final_root.map_or_else(
        || {
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
        },
        |root_node| root_node,
    );

    // Cache the final result if parsing was successful (no errors)
    if errors.is_empty() {
        state.cache_parse_result(entry_id, input.len(), root.clone());
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
        _phantom: std::marker::PhantomData::<N>,
    }
}

/// Parse with incremental support
#[allow(clippy::too_many_arguments)]
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

    // Invalidate cache for affected region
    state.invalidate_cache();

    parse(grammar, table, input, entry, config, state)
}
