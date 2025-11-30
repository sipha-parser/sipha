use crate::backend::pratt::{config::PrattConfig, grammar::PrattGrammar, state::PrattParserState};
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
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
                message: format!(
                    "Cannot determine syntax kind for entry point {:?}",
                    entry.name()
                ),
            });
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
            entry
                .default_syntax_kind()
                .expect("CRITICAL: Cannot create error result - no syntax kind available")
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

/// Context for Pratt parsing operations
struct ParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    backend_grammar: &'a PrattGrammar<T, N>,
    input: &'a [T],
    config: &'a PrattConfig,
    state: &'a mut PrattParserState<T, N>,
    /// Optional incremental session for node reuse
    session: Option<&'a crate::incremental::IncrementalSession<'a, T::Kind>>,
}

impl<'a, T, N> ParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new parse context
    #[allow(clippy::missing_const_for_fn)]
    fn new(
        backend_grammar: &'a PrattGrammar<T, N>,
        input: &'a [T],
        config: &'a PrattConfig,
        state: &'a mut PrattParserState<T, N>,
        session: Option<&'a crate::incremental::IncrementalSession<'a, T::Kind>>,
    ) -> Self {
        Self {
            backend_grammar,
            input,
            config,
            state,
            session,
        }
    }
}

/// Parse input tokens using Pratt parsing
pub fn parse<T, N>(
    backend_grammar: &PrattGrammar<T, N>,
    input: &[T],
    entry: &N,
    config: &PrattConfig,
    state: &mut PrattParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    parse_with_session(backend_grammar, input, entry, config, state, None)
}

/// Parse with incremental session support
pub fn parse_with_session<T, N>(
    backend_grammar: &PrattGrammar<T, N>,
    input: &[T],
    entry: &N,
    config: &PrattConfig,
    state: &mut PrattParserState<T, N>,
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let start_time = std::time::Instant::now();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Determine root kind
    let root_kind = match determine_root_kind_or_error(
        entry,
        input,
        &backend_grammar.original_grammar,
        &mut errors,
    ) {
        Some(kind) => kind,
        None => {
            return create_error_result(
                entry,
                input,
                &backend_grammar.original_grammar,
                errors,
                warnings,
            );
        }
    };

    // Create builder
    let mut builder = GreenNodeBuilder::new();
    let mut pos = 0;
    let mut text_pos = TextSize::zero();

    // Get the rule for the entry point
    let rule = match backend_grammar.original_grammar.get_rule(entry) {
        Some(r) => r,
        None => {
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
                message: format!("Entry point '{}' not found in grammar", entry.name()),
            });
            return create_error_result(
                entry,
                input,
                &backend_grammar.original_grammar,
                errors,
                warnings,
            );
        }
    };

    let mut ctx = ParseContext::new(backend_grammar, input, config, state, session);

    // Parse the expression with minimum precedence 0
    match parse_expr(
        &mut ctx,
        &rule.rhs,
        &mut pos,
        &mut text_pos,
        &mut builder,
        &mut errors,
        &mut warnings,
        0,
    ) {
        Ok(_) => {
            // Success
        }
        Err(e) => {
            errors.push(e);
        }
    }

    // Build final result
    let (root, metrics) = build_parse_result(builder, root_kind, pos, start_time, &mut errors);

    ParseResult {
        root,
        errors,
        warnings,
        metrics,
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

/// Parse an expression with a minimum precedence level
///
/// Pratt parsing uses precedence to determine operator binding.
/// This function parses expressions, stopping when it encounters
/// an operator with precedence less than `min_precedence`.
#[allow(clippy::too_many_lines)]
fn parse_expr<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    expr: &Expr<T, N>,
    pos: &mut usize,
    text_pos: &mut TextSize,
    builder: &mut GreenNodeBuilder<T::Kind>,
    errors: &mut Vec<ParseError>,
    warnings: &mut Vec<ParseWarning>,
    min_precedence: u32,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        ExtendedExpr::Core(core_expr) => parse_core_expr(
            ctx,
            core_expr,
            pos,
            text_pos,
            builder,
            errors,
            warnings,
            min_precedence,
        ),
        ExtendedExpr::PrattOperator {
            expr: inner_expr,
            precedence,
            associativity,
        } => {
            // Parse the inner expression first
            parse_expr(
                ctx,
                inner_expr,
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                *precedence,
            )?;

            // Then parse operators with precedence >= min_precedence
            while *pos < ctx.input.len() {
                let token = &ctx.input[*pos];
                if let Some(op_info) = ctx.backend_grammar.get_operator(token) {
                    // Check if this operator should be parsed
                    let should_parse = match associativity {
                        crate::grammar::hint::Associativity::Left => {
                            op_info.precedence >= min_precedence
                        }
                        crate::grammar::hint::Associativity::Right => {
                            op_info.precedence > min_precedence
                        }
                        crate::grammar::hint::Associativity::None => {
                            op_info.precedence > min_precedence
                        }
                    };

                    if should_parse {
                        // Parse the operator
                        let text = token.text();
                        let text_len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
                        builder.token(token.kind(), text).map_err(|e| {
                            ParseError::InvalidSyntax {
                                span: crate::syntax::TextRange::at(*text_pos, text_len),
                                message: format!("Builder error: {e}"),
                            }
                        })?;
                        *pos += 1;
                        *text_pos += text_len;

                        // Parse the right-hand side with appropriate precedence
                        let next_precedence = match associativity {
                            crate::grammar::hint::Associativity::Left => op_info.precedence + 1,
                            crate::grammar::hint::Associativity::Right => op_info.precedence,
                            crate::grammar::hint::Associativity::None => op_info.precedence + 1,
                        };
                        parse_expr(
                            ctx,
                            inner_expr,
                            pos,
                            text_pos,
                            builder,
                            errors,
                            warnings,
                            next_precedence,
                        )?;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            Ok(())
        }
        ExtendedExpr::RecoveryPoint { expr, .. } => {
            // Parse the inner expression, ignoring recovery point for now
            parse_expr(
                ctx,
                expr,
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            )
        }
        ExtendedExpr::Lookahead(_)
        | ExtendedExpr::NotLookahead(_)
        | ExtendedExpr::TokenClass { .. }
        | ExtendedExpr::Conditional { .. }
        | ExtendedExpr::SemanticPredicate { .. }
        | ExtendedExpr::Backreference { .. } => {
            // These extended expressions are not supported by Pratt parser
            Err(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: format!("Unsupported expression type in Pratt parser: {:?}", expr),
            })
        }
        #[cfg(feature = "backend-peg")]
        ExtendedExpr::Cut(_) => {
            // Cut operator is not supported by Pratt parser
            Err(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: "Cut operator is not supported by Pratt parser".to_string(),
            })
        }
    }
}

/// Parse a core expression
fn parse_core_expr<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    expr: &CoreExpr<T, N>,
    pos: &mut usize,
    text_pos: &mut TextSize,
    builder: &mut GreenNodeBuilder<T::Kind>,
    errors: &mut Vec<ParseError>,
    warnings: &mut Vec<ParseWarning>,
    min_precedence: u32,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        CoreExpr::Token(expected) => {
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
                    Ok(())
                } else {
                    Err(ParseError::UnexpectedToken {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: vec![format!("{:?}", expected)],
                    })
                }
            } else {
                Err(ParseError::UnexpectedEof {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    expected: vec![format!("{:?}", expected)],
                })
            }
        }
        CoreExpr::Rule(nt) => {
            let rule = match ctx.backend_grammar.original_grammar.get_rule(nt) {
                Some(r) => r,
                None => {
                    return Err(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!("Rule '{}' not found", nt.name()),
                    });
                }
            };
            parse_expr(
                ctx,
                &rule.rhs,
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            )
        }
        CoreExpr::Seq(exprs) => {
            for e in exprs {
                parse_expr(
                    ctx,
                    &ExtendedExpr::Core(e.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                    min_precedence,
                )?;
            }
            Ok(())
        }
        CoreExpr::Choice(exprs) => {
            // Try each alternative in order (first match wins)
            let start_pos = *pos;
            let start_text_pos = *text_pos;
            let mut last_error = None;

            for e in exprs {
                *pos = start_pos;
                *text_pos = start_text_pos;
                match parse_expr(
                    ctx,
                    &ExtendedExpr::Core(e.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                    min_precedence,
                ) {
                    Ok(()) => return Ok(()),
                    Err(e) => last_error = Some(e),
                }
            }

            Err(last_error.unwrap_or_else(|| ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                message: "All alternatives failed in choice expression".to_string(),
            }))
        }
        CoreExpr::Opt(e) => {
            let start_pos = *pos;
            let start_text_pos = *text_pos;
            match parse_expr(
                ctx,
                &ExtendedExpr::Core(*e.clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            ) {
                Ok(()) => Ok(()),
                Err(_) => {
                    // Optional expression failed, reset position and continue
                    *pos = start_pos;
                    *text_pos = start_text_pos;
                    Ok(())
                }
            }
        }
        CoreExpr::Repeat {
            expr: e, min, max, ..
        } => {
            let mut count = 0;
            loop {
                let start_pos = *pos;
                let start_text_pos = *text_pos;
                match parse_expr(
                    ctx,
                    &ExtendedExpr::Core(*e.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                    min_precedence,
                ) {
                    Ok(()) => {
                        count += 1;
                        if *pos == start_pos {
                            // No progress, break to avoid infinite loop
                            break;
                        }
                    }
                    Err(_) => {
                        // Reset position and break
                        *pos = start_pos;
                        *text_pos = start_text_pos;
                        break;
                    }
                }
                if let Some(max_count) = max {
                    if count >= *max_count {
                        break;
                    }
                }
            }
            if count < *min {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!(
                        "Repeat expression requires at least {} matches, got {}",
                        min, count
                    ),
                })
            } else {
                Ok(())
            }
        }
        CoreExpr::Empty => Ok(()),
        CoreExpr::Any => {
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
                Ok(())
            } else {
                Err(ParseError::UnexpectedEof {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    expected: vec!["any token".to_string()],
                })
            }
        }
        CoreExpr::Eof => {
            if *pos >= ctx.input.len() {
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    expected: vec!["EOF".to_string()],
                })
            }
        }
        CoreExpr::Separated {
            item,
            separator,
            min,
            ..
        } => {
            // Parse first item
            parse_expr(
                ctx,
                &ExtendedExpr::Core(*item.clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            )?;
            let mut count = 1;

            // Parse separator-item pairs
            while *pos < ctx.input.len() {
                let start_pos = *pos;
                let start_text_pos = *text_pos;
                match parse_expr(
                    ctx,
                    &ExtendedExpr::Core(*separator.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                    min_precedence,
                ) {
                    Ok(()) => {
                        match parse_expr(
                            ctx,
                            &ExtendedExpr::Core(*item.clone()),
                            pos,
                            text_pos,
                            builder,
                            errors,
                            warnings,
                            min_precedence,
                        ) {
                            Ok(()) => count += 1,
                            Err(_) => {
                                *pos = start_pos;
                                *text_pos = start_text_pos;
                                break;
                            }
                        }
                    }
                    Err(_) => {
                        *pos = start_pos;
                        *text_pos = start_text_pos;
                        break;
                    }
                }
            }

            if count < *min {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!(
                        "Separated list requires at least {} items, got {}",
                        min, count
                    ),
                })
            } else {
                Ok(())
            }
        }
        CoreExpr::Delimited {
            open,
            content,
            close,
        } => {
            parse_expr(
                ctx,
                &ExtendedExpr::Core(*open.clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            )?;
            parse_expr(
                ctx,
                &ExtendedExpr::Core(*content.clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            )?;
            parse_expr(
                ctx,
                &ExtendedExpr::Core(*close.clone()),
                pos,
                text_pos,
                builder,
                errors,
                warnings,
                min_precedence,
            )?;
            Ok(())
        }
        CoreExpr::Label { name: _, expr: e } => parse_expr(
            ctx,
            &ExtendedExpr::Core(*e.clone()),
            pos,
            text_pos,
            builder,
            errors,
            warnings,
            min_precedence,
        ),
        CoreExpr::Node { kind, expr: e } => {
            // Convert NonTerminal to SyntaxKind
            let node_kind = kind
                .to_syntax_kind()
                .or_else(|| ctx.input.get(*pos).map(crate::grammar::Token::kind))
                .or_else(|| ctx.input.first().map(crate::grammar::Token::kind))
                .or_else(|| kind.default_syntax_kind());

            if let Some(k) = node_kind {
                builder.start_node(k);
                parse_expr(
                    ctx,
                    &ExtendedExpr::Core(*e.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                    min_precedence,
                )?;
                builder
                    .finish_node()
                    .map_err(|e| ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        message: format!("Builder error: {e}"),
                    })?;
                Ok(())
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
                    &ExtendedExpr::Core(*e.clone()),
                    pos,
                    text_pos,
                    builder,
                    errors,
                    warnings,
                    min_precedence,
                )
            }
        }
        CoreExpr::Flatten(e) => parse_expr(
            ctx,
            &ExtendedExpr::Core(*e.clone()),
            pos,
            text_pos,
            builder,
            errors,
            warnings,
            min_precedence,
        ),
        CoreExpr::Prune(e) => parse_expr(
            ctx,
            &ExtendedExpr::Core(*e.clone()),
            pos,
            text_pos,
            builder,
            errors,
            warnings,
            min_precedence,
        ),
    }
}
