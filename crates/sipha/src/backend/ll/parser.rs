use crate::grammar::{Grammar, Token, NonTerminal, Expr};
use crate::backend::ll::{table::ParsingTable, config::LlConfig};
use crate::error::{ParseResult, ParseError, ParseMetrics, ParseWarning};
use crate::syntax::{GreenNodeBuilder, GreenNode, TextSize};
use crate::incremental::TextEdit;
use hashbrown::HashMap;

/// Default error span length when position is unknown
const DEFAULT_ERROR_SPAN_LEN: TextSize = TextSize::from(1);

/// Parser state for LL parser
#[derive(Debug)]
pub struct LlParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Cache for incremental parsing: (`rule`, `start_pos`, `version`) -> `node`
    parse_cache: HashMap<CacheKey<N>, std::sync::Arc<GreenNode<T::Kind>>, ahash::RandomState>,
    /// Current cache version (incremented on each edit)
    cache_version: usize,
    _phantom: std::marker::PhantomData<(T, N)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey<N: NonTerminal> {
    rule: N,
    start_pos: usize,
    version: usize,
}

impl<T, N> LlParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            parse_cache: HashMap::with_hasher(ahash::RandomState::new()),
            cache_version: 0,
            _phantom: std::marker::PhantomData,
        }
    }
    
    fn cache_key(&self, rule: &N, start_pos: usize) -> CacheKey<N> {
        CacheKey {
            rule: rule.clone(),
            start_pos,
            version: self.cache_version,
        }
    }
    
    fn get_cached(&self, rule: &N, start_pos: usize) -> Option<std::sync::Arc<GreenNode<T::Kind>>> {
        let key = self.cache_key(rule, start_pos);
        self.parse_cache.get(&key).cloned()
    }
    
    /// Cache a parsed node for incremental parsing performance.
    /// Reserved for future incremental parsing optimization. Currently not called
    /// (cache lookup is disabled) but infrastructure is in place.
    #[allow(dead_code)] // Reserved for future use
    fn cache_result(&mut self, rule: &N, start_pos: usize, node: std::sync::Arc<GreenNode<T::Kind>>) {
        let key = self.cache_key(rule, start_pos);
        self.parse_cache.insert(key, node);
    }
    
    pub(crate) fn invalidate_cache(&mut self) {
        self.cache_version += 1;
        // Clear old cache entries (keep recent ones for potential reuse)
        if self.parse_cache.len() > 1000 {
            self.parse_cache.clear();
        }
    }
}

impl<T, N> Default for LlParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}

/// Parse input using LL parser
#[allow(clippy::too_many_lines)]
pub fn parse<T, N>(
    grammar: &Grammar<T, N>,
    table: &ParsingTable<T, N>,
    input: &[T],
    entry: &N,
    config: &LlConfig,
    state: &mut LlParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let mut builder = GreenNodeBuilder::new();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let mut metrics = ParseMetrics::default();
    
    let start_time = std::time::Instant::now();
    
    // Start parsing from entry point
    // Try to get syntax kind from non-terminal, fallback to first token, then default
    let Some(root_kind) = entry.to_syntax_kind()
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| entry.default_syntax_kind()) else {
        // Can't determine kind - return error result
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
            message: format!(
                "Cannot determine syntax kind for entry point {:?}. \
                 Implement NonTerminal::to_syntax_kind() or NonTerminal::default_syntax_kind()",
                entry.name()
            ),
        });
        // Return early with error - we can't build a tree without a root kind
        // Try to get a fallback kind for the error tree
        let error_kind = input.first()
            .map(crate::grammar::Token::kind)
            .or_else(|| entry.default_syntax_kind())
            .or_else(|| grammar.try_get_fallback_kind())
            .unwrap_or_else(|| {
                // Last resort: if we still can't get a kind, we need to provide
                // a very clear error message. However, we still need a kind to create
                // the error result node. This is a configuration error that users must fix.
                // We'll use the entry point's name in the error message.
                errors.push(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
                    message: format!(
                        "Cannot determine syntax kind for error result. \
                         Entry point '{}' must implement NonTerminal::default_syntax_kind() \
                         to return Some for error cases, or provide at least one token in input.",
                        entry.name()
                    ),
                });
                // We still need a kind - try one more time with a clearer message
                // This should never happen if users implement the trait correctly
                entry.default_syntax_kind()
                    .expect(
                        "CRITICAL: Cannot create error result - no syntax kind available. \
                         This is a configuration error. You must implement \
                         NonTerminal::default_syntax_kind() to return Some for error cases."
                    )
            });
        
        return ParseResult {
            root: GreenNode::new(
                error_kind,
                [],
                TextSize::zero(),
            ),
            errors,
            warnings,
            metrics,
            _phantom: std::marker::PhantomData,
        };
    };
    builder.start_node(root_kind);
    
    let mut pos = 0;
    let mut text_pos = TextSize::zero();
    
    match parse_expr(
        grammar,
        table,
        input,
        &mut pos,
        &mut text_pos,
        entry,
        &mut builder,
        &mut errors,
        &mut warnings,
        config,
        state,
    ) {
        Ok(()) => {
            // Check if we consumed all input
            if pos < input.len() {
                errors.push(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(text_pos, TextSize::from(1)),
                    message: format!("Unexpected tokens remaining at position {pos}"),
                });
            }
        }
        Err(e) => {
            errors.push(e);
        }
    }
    
    if let Err(e) = builder.finish_node() {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
            message: format!("Builder error: {e}"),
        });
    }
    let node_count = builder.node_count();
    let root = match builder.finish() {
        Ok(root) => root,
        Err(e) => {
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
                message: format!("Builder error: {e}"),
            });
            // Return a minimal error tree
            GreenNode::new(
                root_kind,
                [],
                TextSize::zero(),
            )
        }
    };
    
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
pub fn parse_incremental<T, N>(
    grammar: &Grammar<T, N>,
    table: &ParsingTable<T, N>,
    input: &[T],
    old_tree: Option<&GreenNode<T::Kind>>,
    edits: &[TextEdit],
    entry: &N,
    config: &LlConfig,
    state: &mut LlParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    // If no edits, use cached result if available
    if edits.is_empty() {
        return parse(grammar, table, input, entry, config, state);
    }
    
    // Compute affected range from edits for future optimization
    let affected_start = edits.iter()
        .map(|e| e.range.start())
        .min()
        .unwrap_or(crate::syntax::TextSize::zero());
    
    // For now, use full reparse with cache invalidation
    // Future optimization: only reparse affected regions and reuse nodes from old_tree
    let _ = (old_tree, affected_start);
    
    // Invalidate cache for affected region
    state.invalidate_cache();
    
    parse(grammar, table, input, entry, config, state)
}

#[allow(clippy::too_many_arguments)]
fn parse_expr<T, N>(
    grammar: &Grammar<T, N>,
    table: &ParsingTable<T, N>,
    input: &[T],
    pos: &mut usize,
    text_pos: &mut TextSize,
    nt: &N,
    builder: &mut GreenNodeBuilder<T::Kind>,
    errors: &mut Vec<ParseError>,
    warnings: &mut Vec<ParseWarning>,
    config: &LlConfig,
    state: &mut LlParserState<T, N>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    // Check cache first (for incremental parsing and performance)
    let start_pos = *pos;
    // Note: Cache lookup is disabled for now as it requires more sophisticated
    // integration with the builder. This is a placeholder for future optimization.
    let _cached = state.get_cached(nt, start_pos);
    
    // Get the rule for this non-terminal
    let rule = grammar.get_rule(nt)
        .ok_or_else(|| ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
            message: format!("Undefined rule: {nt:?}"),
        })?;
    
    // Determine which alternative to use based on lookahead
    // Get up to k tokens of lookahead (optimized for common case of k=1)
    let k = config.lookahead.min(table.k());
    let lookahead = if k == 1 {
        // Fast path: single token lookahead
        &input[*pos..(*pos + 1).min(input.len())]
    } else {
        &input[*pos..(*pos + k).min(input.len())]
    };
    let Some(alt_idx) = table.get(nt, lookahead) else {
        // Error: no production for this lookahead
        let expected: Vec<String> = table.first_set(nt)
            .map_or_else(|| vec!["<unknown>".to_string()], |first| first.iter().map(|t| format!("{t:?}")).collect());
        
        return Err(ParseError::UnexpectedToken {
            span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
            expected,
        });
    };
    
    // Parse the selected alternative
    match &rule.rhs {
        Expr::Choice(alternatives) => {
            if alt_idx < alternatives.len() {
                parse_expr_inner(
                    grammar,
                    table,
                    input,
                    pos,
                    text_pos,
                    &alternatives[alt_idx],
                    builder,
                    errors,
                    warnings,
                    config,
                    state,
                )
            } else {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!("Invalid alternative index {alt_idx}"),
                })
            }
        }
        _ => {
            parse_expr_inner(
                grammar,
                table,
                input,
                pos,
                text_pos,
                &rule.rhs,
                builder,
                errors,
                warnings,
                config,
                state,
            )
        }
    }
}

#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
fn parse_expr_inner<T, N>(
    grammar: &Grammar<T, N>,
    table: &ParsingTable<T, N>,
    input: &[T],
    pos: &mut usize,
    text_pos: &mut TextSize,
    expr: &Expr<T, N>,
    builder: &mut GreenNodeBuilder<T::Kind>,
    errors: &mut Vec<ParseError>,
    warnings: &mut Vec<ParseWarning>,
    config: &LlConfig,
    state: &mut LlParserState<T, N>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(expected) => {
            if let Some(token) = input.get(*pos) {
                // Compare tokens directly - they implement PartialEq by kind
                if token == expected {
                    let token_text = token.text();
                    let token_len = token.text_len();
                    builder.token(token.kind(), token_text)
                        .expect("Internal error: token() called without parent node");
                    *pos += 1;
                    *text_pos += token_len;
                    Ok(())
                } else {
                    Err(ParseError::UnexpectedToken {
                        span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: vec![format!("{expected:?}")],
                    })
                }
            } else {
                Err(ParseError::UnexpectedEof {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    expected: vec![format!("{:?}", expected)],
                })
            }
        }
        
        Expr::Rule(nt) => {
            parse_expr(grammar, table, input, pos, text_pos, nt, builder, errors, warnings, config, state)
        }
        
        Expr::Seq(exprs) => {
            for expr in exprs {
                parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state)?;
            }
            Ok(())
        }
        
        Expr::Choice(alternatives) => {
            // Should have been resolved by table lookup, but handle fallback
            for alt in alternatives {
                let saved_pos = *pos;
                let saved_text_pos = *text_pos;
                if parse_expr_inner(grammar, table, input, pos, text_pos, alt, builder, errors, warnings, config, state).is_ok() {
                    return Ok(());
                }
                *pos = saved_pos;
                *text_pos = saved_text_pos;
            }
            Err(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                message: "No alternative matched".to_string(),
            })
        }
        
        Expr::Opt(expr) => {
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            if parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state).is_err() {
                *pos = saved_pos;
                *text_pos = saved_text_pos;
            }
            Ok(())
        }
        
        Expr::Repeat { expr, min, max } => {
            let mut count = 0;
            loop {
                let saved_pos = *pos;
                let saved_text_pos = *text_pos;
                if parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state).is_err() {
                    *pos = saved_pos;
                    *text_pos = saved_text_pos;
                    break;
                }
                count += 1;
                if let Some(max_count) = max
                    && count >= *max_count {
                    break;
                }
            }
            if count < *min {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!("Expected at least {min} repetitions, got {count}"),
                })
            } else {
                Ok(())
            }
        }
        
        Expr::Empty => Ok(()),
        
        Expr::Any => {
            if let Some(token) = input.get(*pos) {
                let token_text = token.text();
                let token_len = token.text_len();
                builder.token(token.kind(), token_text)
                    .expect("Internal error: token() called without parent node");
                *pos += 1;
                *text_pos += token_len;
                Ok(())
            } else {
                Err(ParseError::UnexpectedEof {
                    span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                    expected: vec!["<any token>".to_string()],
                })
            }
        }
        
        Expr::Eof => {
            if *pos >= input.len() {
                Ok(())
            } else {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: "Expected end of file".to_string(),
                })
            }
        }
        
        Expr::Node { kind, expr } => {
            // Convert NonTerminal to SyntaxKind using the trait method
            let node_kind = kind.to_syntax_kind()
                .or_else(|| input.get(*pos).map(crate::grammar::Token::kind))
                .or_else(|| input.first().map(crate::grammar::Token::kind))
                .or_else(|| kind.default_syntax_kind());
            
            if let Some(k) = node_kind {
                builder.start_node(k);
                let result = parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state);
                if builder.finish_node().is_err() {
                    errors.push(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                        message: "Builder error: unmatched finish_node".to_string(),
                    });
                }
                result
            } else {
                // Can't determine kind - this is an error but we'll continue parsing
                warnings.push(ParseWarning {
                    span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                    message: format!(
                        "Cannot determine syntax kind for node {:?}. \
                         Implement NonTerminal::to_syntax_kind() or NonTerminal::default_syntax_kind()",
                        kind.name()
                    ),
                    severity: crate::error::Severity::Warning,
                });
                // Continue parsing without the node wrapper
                parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state)
            }
        }
        
        Expr::Label { name: _name, expr } => {
            // Labels are metadata, don't affect parsing
            parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state)
        }
        
        Expr::Flatten(expr) => {
            // Flatten doesn't affect parsing, just tree structure
            parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state)
        }
        
        Expr::Prune(expr) => {
            // Prune doesn't affect parsing
            parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state)
        }
        
        Expr::Lookahead(expr) => {
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            let result = parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state);
            *pos = saved_pos;
            *text_pos = saved_text_pos;
            result
        }
        
        Expr::NotLookahead(expr) => {
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            let result = parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state);
            *pos = saved_pos;
            *text_pos = saved_text_pos;
            match result {
                Ok(()) => Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                    message: "Negative lookahead matched".to_string(),
                }),
                Err(_) => Ok(()),
            }
        }
        
        Expr::Separated { item, separator, min, trailing } => {
            let mut count = 0;
            let mut first = true;
            
            loop {
                if !first {
                    let saved_pos = *pos;
                    let saved_text_pos = *text_pos;
                    if parse_expr_inner(grammar, table, input, pos, text_pos, separator, builder, errors, warnings, config, state).is_err() {
                        *pos = saved_pos;
                        *text_pos = saved_text_pos;
                        break;
                    }
                }
                first = false;
                
                let saved_pos = *pos;
                let saved_text_pos = *text_pos;
                if parse_expr_inner(grammar, table, input, pos, text_pos, item, builder, errors, warnings, config, state).is_err() {
                    *pos = saved_pos;
                    *text_pos = saved_text_pos;
                    break;
                }
                count += 1;
            }
            
            // Check trailing separator
            match trailing {
                crate::grammar::TrailingSeparator::Forbid | crate::grammar::TrailingSeparator::Allow => {
                    // Already handled - separator parse failed or optional
                }
                crate::grammar::TrailingSeparator::Require => {
                    let saved_pos = *pos;
                    let saved_text_pos = *text_pos;
                    if parse_expr_inner(grammar, table, input, pos, text_pos, separator, builder, errors, warnings, config, state).is_ok() {
                        // Good
                    } else {
                        *pos = saved_pos;
                        *text_pos = saved_text_pos;
                    }
                }
            }
            
            if count < *min {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(*text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!("Expected at least {min} items, got {count}"),
                })
            } else {
                Ok(())
            }
        }
        
        Expr::Delimited { open, content, close, recover } => {
            parse_expr_inner(grammar, table, input, pos, text_pos, open, builder, errors, warnings, config, state)?;
            let saved_pos = *pos;
            let saved_text_pos = *text_pos;
            let result = parse_expr_inner(grammar, table, input, pos, text_pos, content, builder, errors, warnings, config, state);
            
            if result.is_err() && *recover && config.error_recovery {
                // Error recovery: try to find and consume the close token
                // Skip tokens until we find the close token or EOF
                let close_first = close.first_set(grammar);
                let mut recovered = false;
                
                while *pos < input.len() {
                    if let Some(token) = input.get(*pos) {
                        if close_first.contains(token) {
                            // Found close token, consume it
                            recovered = true;
                            break;
                        }
                        // Skip this token
                        *pos += 1;
                        *text_pos += token.text_len();
                    } else {
                        break;
                    }
                }
                
                if recovered {
                    // Parse the close token
                    parse_expr_inner(grammar, table, input, pos, text_pos, close, builder, errors, warnings, config, state)?;
                    // Report the recovery
                    let span_len = TextSize::from((*text_pos).into().saturating_sub(saved_text_pos.into()));
                    warnings.push(ParseWarning {
                        span: crate::syntax::TextRange::at(saved_text_pos, span_len),
                        message: "Recovered from error in delimited expression".to_string(),
                        severity: crate::error::Severity::Warning,
                    });
                    Ok(())
                } else {
                    // Couldn't recover, restore position and return error
                    *pos = saved_pos;
                    *text_pos = saved_text_pos;
                    parse_expr_inner(grammar, table, input, pos, text_pos, close, builder, errors, warnings, config, state)?;
                    result
                }
            } else {
                parse_expr_inner(grammar, table, input, pos, text_pos, close, builder, errors, warnings, config, state)?;
                result
            }
        }
        
        Expr::RecoveryPoint { expr, sync_tokens } => {
            let result = parse_expr_inner(grammar, table, input, pos, text_pos, expr, builder, errors, warnings, config, state);
            if result.is_err() && config.error_recovery {
                // Error recovery: skip until sync token
                while *pos < input.len() {
                    let token = &input[*pos];
                    if sync_tokens.contains(token) {
                        break;
                    }
                    *pos += 1;
                    *text_pos += TextSize::from(1);
                }
            }
            result
        }
    }
}

