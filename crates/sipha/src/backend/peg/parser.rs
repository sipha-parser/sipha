use crate::backend::peg::{config::PegConfig, grammar::PegGrammar, state::ParseMemo};
use crate::error::{ParseError, ParseMetrics, ParseResult, ParseWarning};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};
use crate::syntax::{GreenNode, GreenNodeBuilder, TextSize};

/// Default error span length when position is unknown
const DEFAULT_ERROR_SPAN_LEN: TextSize = TextSize::from(1);

/// Helper to determine root kind and handle error case
fn determine_root_kind_or_error<T, N>(
    entry: &N,
    input: &[T],
    grammar: &Grammar<T, N>,
    errors: &mut Vec<ParseError>,
) -> Option<T::Kind>
where
    T: Token,
    N: NonTerminal,
{
    entry
        .to_syntax_kind()
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| entry.default_syntax_kind())
        .or_else(|| {
            // Can't determine kind - add error
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
                message: format!(
                    "Cannot determine syntax kind for entry point {:?}. \
                     Implement NonTerminal::to_syntax_kind() or NonTerminal::default_syntax_kind()",
                    entry.name()
                ),
            });
            // Try to get a fallback kind for the error tree
            input
                .first()
                .map(crate::grammar::Token::kind)
                .or_else(|| entry.default_syntax_kind())
                .or_else(|| grammar.try_get_fallback_kind())
        })
}

/// Helper to create error result when root kind cannot be determined
fn create_error_result<T, N>(
    entry: &N,
    input: &[T],
    grammar: &Grammar<T, N>,
    errors: Vec<ParseError>,
    warnings: Vec<ParseWarning>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let error_kind = input
        .first()
        .map(crate::grammar::Token::kind)
        .or_else(|| entry.default_syntax_kind())
        .or_else(|| grammar.try_get_fallback_kind())
        .unwrap_or_else(|| {
            entry.default_syntax_kind().expect(
                "CRITICAL: Cannot create error result - no syntax kind available. \
                 This is a configuration error. You must implement \
                 NonTerminal::default_syntax_kind() to return Some for error cases.",
            )
        });

    ParseResult {
        root: GreenNode::new(error_kind, [], TextSize::zero()),
        errors,
        warnings,
        metrics: ParseMetrics::default(),
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: std::marker::PhantomData,
    }
}

/// Build final parse result from builder and metrics
fn build_parse_result<K: crate::syntax::SyntaxKind>(
    builder: GreenNodeBuilder<K>,
    root_kind: K,
    pos: usize,
    start_time: std::time::Instant,
    errors: &mut Vec<ParseError>,
) -> (std::sync::Arc<GreenNode<K>>, ParseMetrics) {
    let node_count = builder.node_count();
    let root = match builder.finish() {
        Ok(root) => root,
        Err(e) => {
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
                message: format!("Builder error: {e}"),
            });
            GreenNode::new(root_kind, [], TextSize::zero())
        }
    };

    let metrics = ParseMetrics {
        parse_time: start_time.elapsed(),
        tokens_consumed: pos,
        nodes_created: node_count,
        ..Default::default()
    };

    (root, metrics)
}

/// Context for PEG parsing operations
struct ParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    backend_grammar: &'a PegGrammar<T, N>,
    input: &'a [T],
    config: &'a PegConfig,
    state: &'a mut crate::backend::peg::state::PegParserState<T, N>,
    /// Track backtracking depth to prevent infinite loops
    backtrack_depth: usize,
    /// Optional incremental session for node reuse
    session: Option<&'a crate::incremental::IncrementalSession<'a, T::Kind>>,
    /// Track visited rules in current parse path to detect left recursion
    visited_rules: Vec<N>,
}

impl<'a, T, N> ParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new parse context
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: takes references
    fn new(
        backend_grammar: &'a PegGrammar<T, N>,
        input: &'a [T],
        config: &'a PegConfig,
        state: &'a mut crate::backend::peg::state::PegParserState<T, N>,
        session: Option<&'a crate::incremental::IncrementalSession<'a, T::Kind>>,
    ) -> Self {
        Self {
            backend_grammar,
            input,
            config,
            state,
            backtrack_depth: 0,
            session,
            visited_rules: Vec::new(),
        }
    }
}

/// Result of parsing an expression
#[derive(Debug)]
enum ParseExprResult {
    /// Success: (`end_position`, `text_length`)
    #[allow(dead_code)] // end_pos is used in memoized node reuse
    Success { end_pos: usize, text_len: TextSize },
    /// Failure: backtrack
    Failure,
}

/// Parse a grammar expression using PEG semantics
///
/// PEG parsers use ordered choice (first match wins) and backtracking.
/// This function returns the end position if successful, or None if it fails.
#[allow(clippy::too_many_lines)]
fn parse_expr<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    expr: &Expr<T, N>,
    pos: &mut usize,
    text_pos: &mut TextSize,
    builder: &mut GreenNodeBuilder<T::Kind>,
    errors: &mut Vec<ParseError>,
    warnings: &mut Vec<ParseWarning>,
) -> Result<ParseExprResult, ParseError>
where
    T: Token,
    N: NonTerminal,
{
    // Check backtrack depth limit
    if ctx.backtrack_depth > ctx.config.max_backtrack_depth {
        return Err(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
            message: format!(
                "Maximum backtrack depth ({}) exceeded",
                ctx.config.max_backtrack_depth
            ),
        });
    }

    match expr {
        ExtendedExpr::Core(CoreExpr::Token(expected)) => {
            if *pos < ctx.input.len() {
                let token = &ctx.input[*pos];
                if token == expected {
                    let text = token.text();
                    let text_len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
                    builder
                        .token(token.kind(), text)
                        .map_err(|e| ParseError::InvalidSyntax {
                            span: crate::syntax::TextRange::at(*text_pos, text_len),
                            message: format!("Builder error: {e}"),
                        })?;
                    *pos += 1;
                    *text_pos += text_len;
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len,
                    })
                } else {
                    Ok(ParseExprResult::Failure)
                }
            } else {
                Ok(ParseExprResult::Failure)
            }
        }

        ExtendedExpr::Core(CoreExpr::Rule(nt)) => {
            let start_pos = *pos;
            let start_text_pos = *text_pos;

            // Detect left recursion: if this rule is already in the visited path
            if ctx.visited_rules.contains(nt) {
                // Left recursion detected - add warning and fail to avoid infinite loop
                warnings.push(ParseWarning::warning(
                    crate::syntax::TextRange::at(start_text_pos, DEFAULT_ERROR_SPAN_LEN),
                    format!(
                        "Left recursion detected for rule '{}' at position {}. \
                         PEG parsers require special handling for left-recursive rules.",
                        nt.name(),
                        start_pos
                    ),
                ));
                return Ok(ParseExprResult::Failure);
            }

            // Add this rule to visited path (will be removed when done)
            ctx.visited_rules.push(nt.clone());

            // First, check incremental session for reusable nodes
            if let Some(session) = ctx.session {
                let expected_kind = nt.to_syntax_kind().or_else(|| nt.default_syntax_kind());
                if let Some(cached_node) =
                    session.find_cached_node(nt.name(), start_text_pos, expected_kind)
                {
                    // Reuse the cached node
                    builder.reuse_node(cached_node.clone()).map_err(|e| {
                        ParseError::InvalidSyntax {
                            span: crate::syntax::TextRange::at(
                                start_text_pos,
                                DEFAULT_ERROR_SPAN_LEN,
                            ),
                            message: format!("Builder error: {e}"),
                        }
                    })?;

                    // Advance position by the node's text length
                    let node_text_len = cached_node.text_len();
                    let target_text_pos = start_text_pos + node_text_len;
                    let mut current_text_pos = start_text_pos;

                    // Count tokens until we've consumed the node's text length
                    while *pos < ctx.input.len() && current_text_pos < target_text_pos {
                        let token = &ctx.input[*pos];
                        let token_len = token.text_len();
                        if current_text_pos + token_len > target_text_pos {
                            break;
                        }
                        *pos += 1;
                        current_text_pos += token_len;
                    }

                    *text_pos = current_text_pos;
                    ctx.visited_rules.pop(); // Remove from visited path
                    return Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: node_text_len,
                    });
                }

                // Try to find a reusable node from the session
                if let Some(candidate) = session.find_reusable_at(start_text_pos, expected_kind)
                    && builder.reuse_node(candidate.node.clone()).is_ok()
                {
                    let node_text_len = candidate.node.text_len();
                    let target_text_pos = start_text_pos + node_text_len;
                    let mut current_text_pos = start_text_pos;

                    while *pos < ctx.input.len() && current_text_pos < target_text_pos {
                        let token = &ctx.input[*pos];
                        let token_len = token.text_len();
                        if current_text_pos + token_len > target_text_pos {
                            break;
                        }
                        *pos += 1;
                        current_text_pos += token_len;
                    }

                    *text_pos = current_text_pos;
                    ctx.visited_rules.pop(); // Remove from visited path
                    return Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: node_text_len,
                    });
                }
            }

            // Check memoization cache if enabled
            if ctx.config.enable_memoization
                && let Some(memo) = ctx.state.get_memo(nt, start_pos)
            {
                match memo {
                    ParseMemo::Success { node, end_pos } => {
                        // Reuse cached node
                        builder.reuse_node(node.clone()).map_err(|e| {
                            ParseError::InvalidSyntax {
                                span: crate::syntax::TextRange::at(
                                    start_text_pos,
                                    DEFAULT_ERROR_SPAN_LEN,
                                ),
                                message: format!("Builder error: {e}"),
                            }
                        })?;

                        // Advance position by the node's text length
                        let node_text_len = node.text_len();
                        let target_text_pos = start_text_pos + node_text_len;
                        let mut current_text_pos = start_text_pos;

                        // Count tokens until we've consumed the node's text length
                        while *pos < ctx.input.len() && current_text_pos < target_text_pos {
                            let token = &ctx.input[*pos];
                            let token_len = token.text_len();
                            if current_text_pos + token_len > target_text_pos {
                                break;
                            }
                            *pos += 1;
                            current_text_pos += token_len;
                        }

                        // Ensure we're at least at end_pos (token position from memo)
                        if *pos < end_pos {
                            *pos = end_pos;
                            // Recalculate text position from tokens
                            current_text_pos = start_text_pos;
                            for i in start_pos..end_pos.min(ctx.input.len()) {
                                current_text_pos += ctx.input[i].text_len();
                            }
                        }

                        *text_pos = current_text_pos;
                        ctx.visited_rules.pop(); // Remove from visited path
                        return Ok(ParseExprResult::Success {
                            end_pos: *pos,
                            text_len: node_text_len,
                        });
                    }
                    ParseMemo::Failure => {
                        ctx.visited_rules.pop(); // Remove from visited path
                        return Ok(ParseExprResult::Failure);
                    }
                }
            }

            // Get rule from grammar
            let Some(rule) = ctx.backend_grammar.original_grammar.get_rule(nt) else {
                errors.push(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!("Undefined rule: {}", nt.name()),
                });
                ctx.visited_rules.pop(); // Remove from visited path
                return Ok(ParseExprResult::Failure);
            };

            // For memoization, we need to extract the node we build
            // Use a temporary builder to capture the node
            let mut temp_builder = GreenNodeBuilder::new();
            let node_kind = nt
                .to_syntax_kind()
                .or_else(|| ctx.input.get(*pos).map(crate::grammar::Token::kind))
                .or_else(|| nt.default_syntax_kind());

            if let Some(kind) = node_kind {
                temp_builder.start_node(kind);
            }

            // Parse the rule's expression
            let mut temp_pos = *pos;
            let mut temp_text_pos = *text_pos;
            let result = parse_expr(
                ctx,
                &rule.rhs,
                &mut temp_pos,
                &mut temp_text_pos,
                &mut temp_builder,
                errors,
                warnings,
            );

            match &result {
                Ok(ParseExprResult::Success { .. }) => {
                    // Finish the node in temp builder
                    if node_kind.is_some() {
                        let _ = temp_builder.finish_node();
                    }

                    // Extract the node we built
                    if let Ok(node) = temp_builder.finish() {
                        // Add the node to the main builder
                        builder.reuse_node(node.clone()).map_err(|e| {
                            ParseError::InvalidSyntax {
                                span: crate::syntax::TextRange::at(
                                    *text_pos,
                                    DEFAULT_ERROR_SPAN_LEN,
                                ),
                                message: format!("Builder error: {e}"),
                            }
                        })?;

                        // Update positions
                        *pos = temp_pos;
                        *text_pos = temp_text_pos;

                        // Cache the result if memoization is enabled
                        if ctx.config.enable_memoization {
                            ctx.state.set_memo(
                                nt,
                                start_pos,
                                ParseMemo::Success {
                                    node,
                                    end_pos: temp_pos,
                                },
                                ctx.config.max_memo_size,
                            );
                        }

                        ctx.visited_rules.pop(); // Remove from visited path
                        Ok(ParseExprResult::Success {
                            end_pos: *pos,
                            text_len: *text_pos - start_text_pos,
                        })
                    } else {
                        // Builder error - fall back to parsing directly in main builder
                        *pos = start_pos;
                        *text_pos = start_text_pos;
                        let result =
                            parse_expr(ctx, &rule.rhs, pos, text_pos, builder, errors, warnings);
                        ctx.visited_rules.pop(); // Remove from visited path
                        result
                    }
                }
                Ok(ParseExprResult::Failure) => {
                    // Cache failure if memoization is enabled
                    if ctx.config.enable_memoization {
                        ctx.state.set_memo(
                            nt,
                            start_pos,
                            ParseMemo::Failure,
                            ctx.config.max_memo_size,
                        );
                    }
                    ctx.visited_rules.pop(); // Remove from visited path
                    Ok(ParseExprResult::Failure)
                }
                Err(e) => {
                    ctx.visited_rules.pop(); // Remove from visited path
                    // Convert error to owned by creating a new one with the same message
                    Err(ParseError::InvalidSyntax {
                        span: e.span(),
                        message: e.to_string(),
                    })
                }
            }
        }

        ExtendedExpr::Core(CoreExpr::Any) => {
            if *pos < ctx.input.len() {
                let token = &ctx.input[*pos];
                let text = token.text();
                let text_len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
                builder
                    .token(token.kind(), text)
                    .map_err(|e| ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, text_len),
                        message: format!("Builder error: {e}"),
                    })?;
                *pos += 1;
                *text_pos += text_len;
                Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len,
                })
            } else {
                Ok(ParseExprResult::Failure)
            }
        }

        ExtendedExpr::Core(CoreExpr::Eof) => {
            if *pos >= ctx.input.len() {
                Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len: TextSize::zero(),
                })
            } else {
                Ok(ParseExprResult::Failure)
            }
        }

        ExtendedExpr::Core(CoreExpr::Empty) => Ok(ParseExprResult::Success {
            end_pos: *pos,
            text_len: TextSize::zero(),
        }),

        ExtendedExpr::Core(CoreExpr::Seq(exprs)) => {
            let start_pos = *pos;
            let start_text_pos = *text_pos;
            let mut total_text_len = TextSize::zero();

            for expr in exprs {
                match parse_expr(
                    ctx,
                    &ExtendedExpr::Core(expr.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                ) {
                    Ok(ParseExprResult::Success { text_len, .. }) => {
                        total_text_len += text_len;
                    }
                    Ok(ParseExprResult::Failure) => {
                        // Backtrack: restore position
                        *pos = start_pos;
                        *text_pos = start_text_pos;
                        return Ok(ParseExprResult::Failure);
                    }
                    Err(e) => return Err(e),
                }
            }

            Ok(ParseExprResult::Success {
                end_pos: *pos,
                text_len: total_text_len,
            })
        }

        ExtendedExpr::Core(CoreExpr::Choice(alternatives)) => {
            // PEG: ordered choice - try alternatives in order, first match wins
            let start_pos = *pos;
            let start_text_pos = *text_pos;

            for (idx, alt) in alternatives.iter().enumerate() {
                let alt_expr = ExtendedExpr::Core(alt.clone());
                // Check backtrack depth before trying alternative
                if ctx.backtrack_depth >= ctx.config.max_backtrack_depth {
                    return Err(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!(
                            "Maximum backtrack depth ({}) exceeded in choice",
                            ctx.config.max_backtrack_depth
                        ),
                    });
                }

                // Reset position for each alternative
                *pos = start_pos;
                *text_pos = start_text_pos;

                ctx.backtrack_depth += 1;
                let result = parse_expr(ctx, &alt_expr, pos, text_pos, builder, errors, warnings);
                ctx.backtrack_depth -= 1;

                match result {
                    Ok(ParseExprResult::Success { .. }) => {
                        // First successful match - return it (PEG semantics)
                        return result;
                    }
                    Ok(ParseExprResult::Failure) => {
                        // Try next alternative - continue loop
                    }
                    Err(e) => {
                        // If this is the last alternative, return the error
                        if idx == alternatives.len() - 1 {
                            return Err(e);
                        }
                        // Otherwise, try next alternative - continue loop
                    }
                }
            }

            // All alternatives failed
            Ok(ParseExprResult::Failure)
        }

        ExtendedExpr::Core(CoreExpr::Opt(expr)) => {
            // Optional: try to parse, but don't fail if it doesn't match
            let start_text_pos = *text_pos;
            match parse_expr(
                ctx,
                &ExtendedExpr::Core((**expr).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            ) {
                Ok(ParseExprResult::Success { text_len, .. }) => {
                    // Matched - return the text length
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len,
                    })
                }
                Ok(ParseExprResult::Failure) => {
                    // Didn't match, but that's OK for optional - return zero length
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: TextSize::zero(),
                    })
                }
                Err(e) => {
                    // Error during parsing - still treat as optional failure
                    warnings.push(ParseWarning::warning(
                        crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        format!("Error in optional expression: {e}"),
                    ));
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: *text_pos - start_text_pos,
                    })
                }
            }
        }

        ExtendedExpr::Core(CoreExpr::Repeat { expr, min, max }) => {
            let mut count = 0;
            let mut total_text_len = TextSize::zero();

            loop {
                // Check backtrack depth before each iteration
                if ctx.backtrack_depth >= ctx.config.max_backtrack_depth {
                    return Err(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!(
                            "Maximum backtrack depth ({}) exceeded in repeat",
                            ctx.config.max_backtrack_depth
                        ),
                    });
                }

                let saved_pos = *pos;
                let saved_text_pos = *text_pos;

                ctx.backtrack_depth += 1;
                let result = parse_expr(
                    ctx,
                    &ExtendedExpr::Core((**expr).clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                );
                ctx.backtrack_depth -= 1;

                match result {
                    Ok(ParseExprResult::Success { text_len, .. }) => {
                        count += 1;
                        total_text_len += text_len;

                        // Check max limit
                        if let Some(max_count) = max
                            && count >= *max_count
                        {
                            break;
                        }
                    }
                    Ok(ParseExprResult::Failure) => {
                        // Restore position and break
                        *pos = saved_pos;
                        *text_pos = saved_text_pos;
                        break;
                    }
                    Err(e) => {
                        // Error - restore and return
                        *pos = saved_pos;
                        *text_pos = saved_text_pos;
                        return Err(e);
                    }
                }
            }

            // Check min requirement
            if count < *min {
                Ok(ParseExprResult::Failure)
            } else {
                Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len: total_text_len,
                })
            }
        }

        ExtendedExpr::Core(CoreExpr::Separated {
            item,
            separator,
            min,
            trailing,
        }) => {
            let start_pos = *pos;
            let start_text_pos = *text_pos;
            let mut count = 0;
            let mut total_text_len = TextSize::zero();

            // Parse first item
            match parse_expr(
                ctx,
                &ExtendedExpr::Core((**item).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            ) {
                Ok(ParseExprResult::Success { text_len, .. }) => {
                    count += 1;
                    total_text_len += text_len;
                }
                Ok(ParseExprResult::Failure) => {
                    *pos = start_pos;
                    *text_pos = start_text_pos;
                    if count < *min {
                        return Ok(ParseExprResult::Failure);
                    }
                    return Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: total_text_len,
                    });
                }
                Err(e) => return Err(e),
            }

            // Parse separator-item pairs
            loop {
                // Check backtrack depth before each iteration
                if ctx.backtrack_depth >= ctx.config.max_backtrack_depth {
                    return Err(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!(
                            "Maximum backtrack depth ({}) exceeded in separated list",
                            ctx.config.max_backtrack_depth
                        ),
                    });
                }

                let saved_pos = *pos;
                let saved_text_pos = *text_pos;

                ctx.backtrack_depth += 1;
                let sep_result = parse_expr(
                    ctx,
                    &ExtendedExpr::Core((**separator).clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                );
                ctx.backtrack_depth -= 1;

                // Try separator
                match sep_result {
                    Ok(ParseExprResult::Success { .. }) => {
                        // Separator matched, now try item
                        ctx.backtrack_depth += 1;
                        let item_result = parse_expr(
                            ctx,
                            &ExtendedExpr::Core((**item).clone()),
                            pos,
                            text_pos,
                            builder,
                            errors,
                            warnings,
                        );
                        ctx.backtrack_depth -= 1;

                        match item_result {
                            Ok(ParseExprResult::Success { text_len, .. }) => {
                                count += 1;
                                total_text_len += text_len;
                            }
                            Ok(ParseExprResult::Failure) => {
                                // Item didn't match - check trailing separator
                                match trailing {
                                    crate::grammar::expr::TrailingSeparator::Allow
                                    | crate::grammar::expr::TrailingSeparator::Require => {
                                        // Trailing separator is OK
                                        break;
                                    }
                                    crate::grammar::expr::TrailingSeparator::Forbid => {
                                        // Restore to before separator
                                        *pos = saved_pos;
                                        *text_pos = saved_text_pos;
                                        break;
                                    }
                                }
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    Ok(ParseExprResult::Failure) => {
                        // No separator - done
                        break;
                    }
                    Err(e) => return Err(e),
                }
            }

            if count < *min {
                *pos = start_pos;
                *text_pos = start_text_pos;
                Ok(ParseExprResult::Failure)
            } else {
                Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len: total_text_len,
                })
            }
        }

        ExtendedExpr::Core(CoreExpr::Delimited {
            open,
            content,
            close,
        }) => {
            let start_text_pos = *text_pos;

            // Parse opening delimiter
            match parse_expr(
                ctx,
                &ExtendedExpr::Core((**open).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            ) {
                Ok(ParseExprResult::Success { .. }) => {}
                Ok(ParseExprResult::Failure) => return Ok(ParseExprResult::Failure),
                Err(e) => return Err(e),
            }

            // Parse content
            let _ = parse_expr(
                ctx,
                &ExtendedExpr::Core((**content).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            );

            // Parse closing delimiter
            match parse_expr(
                ctx,
                &ExtendedExpr::Core((**close).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            ) {
                Ok(ParseExprResult::Success { text_len: _, .. }) => Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len: (*text_pos) - start_text_pos,
                }),
                Ok(ParseExprResult::Failure) => {
                    // Missing closing delimiter
                    if ctx.config.error_recovery {
                        // Try to recover by skipping tokens until we find the closing delimiter
                        let recovery_start_text_pos = *text_pos;
                        let mut recovered = false;
                        let mut skipped_tokens = 0;
                        let mut skipped_text_len = TextSize::zero();

                        // Try to find the closing delimiter by parsing it at each position
                        while *pos < ctx.input.len() {
                            let saved_pos = *pos;
                            let saved_text_pos = *text_pos;

                            // Try to parse the closing delimiter
                            match parse_expr(
                                ctx,
                                &ExtendedExpr::Core((**close).clone()),
                                pos,
                                text_pos,
                                builder,
                                errors,
                                warnings,
                            ) {
                                Ok(ParseExprResult::Success { .. }) => {
                                    // Found closing delimiter - recovery successful
                                    recovered = true;
                                    break;
                                }
                                Ok(ParseExprResult::Failure) => {
                                    // Not the closing delimiter - skip this token
                                    *pos = saved_pos + 1;
                                    *text_pos = saved_text_pos + ctx.input[saved_pos].text_len();
                                    skipped_tokens += 1;
                                    skipped_text_len += ctx.input[saved_pos].text_len();
                                }
                                Err(_) => {
                                    // Error during recovery - skip token and continue
                                    *pos = saved_pos + 1;
                                    *text_pos = saved_text_pos + ctx.input[saved_pos].text_len();
                                    skipped_tokens += 1;
                                    skipped_text_len += ctx.input[saved_pos].text_len();
                                }
                            }
                        }

                        if recovered {
                            // Add warning about recovery
                            warnings.push(ParseWarning::warning(
                                crate::syntax::TextRange::at(recovery_start_text_pos, skipped_text_len),
                                format!(
                                    "Error recovery: skipped {skipped_tokens} token(s) to find closing delimiter"
                                ),
                            ));
                        } else {
                            // Recovery failed - add error but continue
                            errors.push(ParseError::InvalidSyntax {
                                span: crate::syntax::TextRange::at(
                                    recovery_start_text_pos,
                                    DEFAULT_ERROR_SPAN_LEN,
                                ),
                                message: "Missing closing delimiter (recovery failed)".to_string(),
                            });
                        }
                    } else {
                        // Recovery disabled - error but continue
                        errors.push(ParseError::InvalidSyntax {
                            span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                            message: "Missing closing delimiter".to_string(),
                        });
                    }
                    // Both branches continue parsing - return success
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: (*text_pos) - start_text_pos,
                    })
                }
                Err(e) => {
                    // Convert error to owned by creating a new one with the same message
                    Err(ParseError::InvalidSyntax {
                        span: e.span(),
                        message: e.to_string(),
                    })
                }
            }
        }

        ExtendedExpr::Lookahead(expr) => {
            // Positive lookahead: check if expression matches without consuming
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            let result = parse_expr(ctx, expr, pos, text_pos, builder, errors, warnings);
            // Restore position (lookahead doesn't consume)
            *pos = saved_pos;
            *text_pos = saved_text_pos;
            match result {
                Ok(ParseExprResult::Success { .. }) => Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len: TextSize::zero(),
                }),
                Ok(ParseExprResult::Failure) => Ok(ParseExprResult::Failure),
                Err(_e) => {
                    // For lookahead, we don't propagate errors - just treat as failure
                    Ok(ParseExprResult::Failure)
                }
            }
        }

        ExtendedExpr::NotLookahead(expr) => {
            // Negative lookahead: check if expression doesn't match
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            let result = parse_expr(ctx, expr, pos, text_pos, builder, errors, warnings);
            // Restore position
            *pos = saved_pos;
            *text_pos = saved_text_pos;
            match result {
                Ok(ParseExprResult::Success { .. }) | Err(_) => Ok(ParseExprResult::Failure),
                Ok(ParseExprResult::Failure) => Ok(ParseExprResult::Success {
                    end_pos: *pos,
                    text_len: TextSize::zero(),
                }),
            }
        }

        ExtendedExpr::Core(CoreExpr::Label { name: _, expr }) => {
            // Labels are for tree construction - just parse the expression
            parse_expr(
                ctx,
                &ExtendedExpr::Core((**expr).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            )
        }

        ExtendedExpr::Core(CoreExpr::Node { kind, expr }) => {
            // Convert NonTerminal to SyntaxKind using the trait method
            let node_kind = kind
                .to_syntax_kind()
                .or_else(|| ctx.input.get(*pos).map(crate::grammar::Token::kind))
                .or_else(|| ctx.input.first().map(crate::grammar::Token::kind))
                .or_else(|| kind.default_syntax_kind());

            if let Some(k) = node_kind {
                builder.start_node(k);
                let result = parse_expr(
                    ctx,
                    &ExtendedExpr::Core((**expr).clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                );
                builder
                    .finish_node()
                    .map_err(|e| ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!("Builder error: {e}"),
                    })?;
                result
            } else {
                // Can't determine kind - add warning and continue parsing without the node wrapper
                warnings.push(ParseWarning::warning(
                    crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    format!(
                        "Cannot determine syntax kind for node {:?}. \
                         Implement NonTerminal::to_syntax_kind() or NonTerminal::default_syntax_kind()",
                        kind.name()
                    ),
                ));
                // Continue parsing without the node wrapper
                parse_expr(
                    ctx,
                    &ExtendedExpr::Core((**expr).clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                )
            }
        }

        ExtendedExpr::Core(CoreExpr::Flatten(expr)) => {
            // Flatten removes one level of nesting
            // For now, just parse the expression (flattening is handled in tree construction)
            parse_expr(
                ctx,
                &ExtendedExpr::Core((**expr).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            )
        }

        ExtendedExpr::Core(CoreExpr::Prune(expr)) => {
            // Prune removes nodes with only one child
            // For now, just parse the expression
            parse_expr(
                ctx,
                &ExtendedExpr::Core((**expr).clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
            )
        }

        #[cfg(feature = "backend-peg")]
        ExtendedExpr::Cut(expr) => {
            // Cut operator: prevent backtracking past this point
            // Once this succeeds, we can't backtrack to try other alternatives
            let result = parse_expr(ctx, expr, pos, text_pos, builder, errors, warnings);
            // Cut succeeded - mark that we've committed to this path
            // In PEG, this means we can't backtrack past this point in the current choice
            // For now, we'll track this in the context (could add a flag to prevent backtracking)
            if let Ok(ParseExprResult::Success { .. }) = &result {
                // Cut succeeded - committed to this path
            }
            result
        }

        ExtendedExpr::TokenClass { class } => {
            // Match any token that belongs to the specified class
            if *pos < ctx.input.len() {
                let token = &ctx.input[*pos];
                // Use the helper function from token_class module
                if crate::grammar::token_class::token_matches_class(token, class) {
                    let text = token.text();
                    let text_len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
                    builder
                        .token(token.kind(), text)
                        .map_err(|e| ParseError::InvalidSyntax {
                            span: crate::syntax::TextRange::at(*text_pos, text_len),
                            message: format!("Builder error: {e}"),
                        })?;
                    *pos += 1;
                    *text_pos += text_len;
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len,
                    })
                } else {
                    Ok(ParseExprResult::Failure)
                }
            } else {
                Ok(ParseExprResult::Failure)
            }
        }

        ExtendedExpr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            // Parse condition first
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            match parse_expr(ctx, condition, pos, text_pos, builder, errors, warnings) {
                Ok(ParseExprResult::Success { .. }) => {
                    // Condition succeeded - parse then_expr
                    parse_expr(ctx, then_expr, pos, text_pos, builder, errors, warnings)
                }
                Ok(ParseExprResult::Failure) | Err(_) => {
                    // Condition failed - restore position and try else_expr if provided
                    *pos = saved_pos;
                    *text_pos = saved_text_pos;
                    else_expr
                        .as_ref()
                        .map_or(Ok(ParseExprResult::Failure), |else_expr| {
                            parse_expr(ctx, else_expr, pos, text_pos, builder, errors, warnings)
                        })
                }
            }
        }

        ExtendedExpr::SemanticPredicate { expr, predicate } => {
            // Parse the expression first
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            match parse_expr(ctx, expr, pos, text_pos, builder, errors, warnings) {
                Ok(ParseExprResult::Success { .. }) => {
                    // Expression matched - check semantic predicate
                    if predicate.check(&ctx.backend_grammar.original_grammar, ctx.input, saved_pos)
                    {
                        // Predicate passed - success
                        Ok(ParseExprResult::Success {
                            end_pos: *pos,
                            text_len: *text_pos - saved_text_pos,
                        })
                    } else {
                        // Predicate failed - restore and fail
                        *pos = saved_pos;
                        *text_pos = saved_text_pos;
                        Ok(ParseExprResult::Failure)
                    }
                }
                Ok(ParseExprResult::Failure) | Err(_) => {
                    // Expression didn't match - restore and fail
                    *pos = saved_pos;
                    *text_pos = saved_text_pos;
                    Ok(ParseExprResult::Failure)
                }
            }
        }

        ExtendedExpr::Backreference { capture_id } => {
            // Match previously captured content
            // For now, this is a stub - full implementation would require
            // tracking captures in the parse context
            warnings.push(ParseWarning::warning(
                crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                format!("Backreference {capture_id:?} not yet fully implemented in PEG backend"),
            ));
            Ok(ParseExprResult::Failure)
        }

        ExtendedExpr::RecoveryPoint { expr, sync_tokens } => {
            // Recovery point for error recovery
            let start_text_pos = *text_pos;

            // Try to parse the expression normally
            match parse_expr(ctx, expr, pos, text_pos, builder, errors, warnings) {
                Ok(ParseExprResult::Success { .. }) => {
                    // Success - return normally
                    Ok(ParseExprResult::Success {
                        end_pos: *pos,
                        text_len: (*text_pos) - start_text_pos,
                    })
                }
                Ok(ParseExprResult::Failure) | Err(_) => {
                    // Parse failed - try error recovery
                    if ctx.config.error_recovery && !sync_tokens.is_empty() {
                        // Skip tokens until we find a sync token
                        let mut recovered = false;
                        let mut skipped_tokens = 0;
                        let mut skipped_text_len = TextSize::zero();

                        while *pos < ctx.input.len() {
                            let token = &ctx.input[*pos];

                            // Check if this token matches any sync token
                            if sync_tokens.iter().any(|sync| token == sync) {
                                // Found sync token - recovery successful
                                recovered = true;
                                break;
                            }

                            // Skip this token
                            skipped_tokens += 1;
                            skipped_text_len += token.text_len();
                            *pos += 1;
                            *text_pos += token.text_len();
                        }

                        if recovered {
                            // Add warning about recovery
                            warnings.push(ParseWarning::warning(
                                crate::syntax::TextRange::at(start_text_pos, skipped_text_len),
                                format!(
                                    "Error recovery: skipped {skipped_tokens} token(s) to find sync token"
                                ),
                            ));

                            // Try parsing again from the sync token position
                            match parse_expr(ctx, expr, pos, text_pos, builder, errors, warnings) {
                                Ok(ParseExprResult::Success { .. }) => {
                                    Ok(ParseExprResult::Success {
                                        end_pos: *pos,
                                        text_len: (*text_pos) - start_text_pos,
                                    })
                                }
                                Ok(ParseExprResult::Failure) => {
                                    // Still failed after recovery
                                    Ok(ParseExprResult::Failure)
                                }
                                Err(e) => Err(e),
                            }
                        } else {
                            // No sync token found - recovery failed
                            Ok(ParseExprResult::Failure)
                        }
                    } else {
                        // Error recovery disabled or no sync tokens - return failure
                        Ok(ParseExprResult::Failure)
                    }
                }
            }
        }
        #[cfg(feature = "backend-pratt")]
        ExtendedExpr::PrattOperator { .. } => {
            Err(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: "PrattOperator expressions are not supported in PEG parser backend. Use the Pratt parser backend instead.".to_string(),
            })
        }
    }
}

/// Parse input using PEG parser
pub fn parse<T, N>(
    backend_grammar: &PegGrammar<T, N>,
    input: &[T],
    entry: &N,
    config: &PegConfig,
    state: &mut crate::backend::peg::state::PegParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    parse_with_session(backend_grammar, input, entry, config, state, None)
}

/// Parse with incremental session support
pub fn parse_with_session<T, N>(
    backend_grammar: &PegGrammar<T, N>,
    input: &[T],
    entry: &N,
    config: &PegConfig,
    state: &mut crate::backend::peg::state::PegParserState<T, N>,
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let mut builder = GreenNodeBuilder::new();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    let start_time = std::time::Instant::now();

    // Start parsing from entry point
    let Some(root_kind) =
        determine_root_kind_or_error(entry, input, &backend_grammar.original_grammar, &mut errors)
    else {
        return create_error_result(
            entry,
            input,
            &backend_grammar.original_grammar,
            errors,
            warnings,
        );
    };
    builder.start_node(root_kind);

    let mut pos = 0;
    let mut text_pos = TextSize::zero();

    let mut ctx = ParseContext::new(backend_grammar, input, config, state, session);

    // Get rule for entry point
    if let Some(rule) = backend_grammar.original_grammar.get_rule(entry) {
        match parse_expr(
            &mut ctx,
            &rule.rhs,
            &mut pos,
            &mut text_pos,
            &mut builder,
            &mut errors,
            &mut warnings,
        ) {
            Ok(ParseExprResult::Success { .. }) => {
                // Check if we consumed all input
                if pos < input.len() {
                    errors.push(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(text_pos, TextSize::from(1)),
                        message: format!("Unexpected tokens remaining at position {pos}"),
                    });
                }
            }
            Ok(ParseExprResult::Failure) => {
                errors.push(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(text_pos, TextSize::from(1)),
                    message: format!("Parse failed at position {pos}"),
                });
            }
            Err(e) => {
                errors.push(e);
            }
        }
    } else {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
            message: format!("Entry point rule not found: {}", entry.name()),
        });
    }

    // Don't call finish_node() on root - finish() expects it on the stack
    let (root, metrics) = build_parse_result(builder, root_kind, pos, start_time, &mut errors);

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
