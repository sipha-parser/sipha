use crate::backend::ll::{config::LlConfig, table::ParsingTable};
use crate::error::{ParseError, ParseMetrics, ParseResult, ParseWarning};
use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use crate::syntax::{GreenNode, GreenNodeBuilder, TextSize};
use hashbrown::HashMap;

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
                .or_else(|| {
                    // Last resort error
                    errors.push(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
                        message: format!(
                            "Cannot determine syntax kind for error result. \
                             Entry point '{}' must implement NonTerminal::default_syntax_kind() \
                             to return Some for error cases, or provide at least one token in input.",
                            entry.name()
                        ),
                    });
                    None
                })
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

/// Create error result when syntax kind cannot be determined (for incremental parsing)
fn create_incremental_error_result<T, N>(
    entry: &N,
    input: &[T],
    grammar: &Grammar<T, N>,
    mut errors: Vec<ParseError>,
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
            errors.push(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(TextSize::zero(), DEFAULT_ERROR_SPAN_LEN),
                message: format!(
                    "Cannot determine syntax kind for error result. \
                     Entry point '{}' must implement NonTerminal::default_syntax_kind() \
                     to return Some for error cases, or provide at least one token in input.",
                    entry.name()
                ),
            });
            entry.default_syntax_kind().expect(
                "CRITICAL: Cannot create error result - no syntax kind available. \
                 This is a configuration error. You must implement \
                 NonTerminal::default_syntax_kind() to return Some for error cases.",
            )
        });

    ParseResult {
        root: GreenNode::new(error_kind, [], TextSize::zero()),
        errors,
        warnings: Vec::new(),
        metrics: ParseMetrics::default(),
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: std::marker::PhantomData,
    }
}

/// Parser state for LL parser
#[derive(Debug)]
pub struct LlParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Cache for incremental parsing: (`rule`, `start_pos`, `version`, `text_start`) -> `node`
    /// Cache keys include text position for range-based invalidation
    parse_cache: HashMap<CacheKey<N>, std::sync::Arc<GreenNode<T::Kind>>, ahash::RandomState>,
    /// Current cache version (incremented on each edit)
    cache_version: usize,
    /// Maximum cache size before eviction
    max_cache_size: usize,
    /// Number of cache versions to keep in history
    cache_history: usize,
    _phantom: std::marker::PhantomData<(T, N)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey<N: NonTerminal> {
    rule: N,
    start_pos: usize,
    version: usize,
    /// Text range for this cached node (start position in text)
    /// This is used for range-based cache invalidation
    text_start: crate::syntax::TextSize,
}

impl<N: NonTerminal> CacheKey<N> {
    /// Create a new cache key
    #[must_use]
    const fn new(
        rule: N,
        start_pos: usize,
        version: usize,
        text_start: crate::syntax::TextSize,
    ) -> Self {
        Self {
            rule,
            start_pos,
            version,
            text_start,
        }
    }

    /// Check if this cache entry intersects with the given text range
    #[must_use]
    fn intersects(
        &self,
        range: crate::syntax::TextRange,
        node_text_len: crate::syntax::TextSize,
    ) -> bool {
        let node_range = crate::syntax::TextRange::at(self.text_start, node_text_len);
        node_range.intersect(range).is_some()
    }
}

/// Context for parsing operations - groups immutable parsing parameters
struct ParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    grammar: &'a Grammar<T, N>,
    table: &'a ParsingTable<T, N>,
    input: &'a [T],
    config: &'a LlConfig,
    state: &'a mut LlParserState<T, N>,
}

impl<'a, T, N> ParseContext<'a, T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new parse context
    #[must_use]
    const fn new(
        grammar: &'a Grammar<T, N>,
        table: &'a ParsingTable<T, N>,
        input: &'a [T],
        config: &'a LlConfig,
        state: &'a mut LlParserState<T, N>,
    ) -> Self {
        Self {
            grammar,
            table,
            input,
            config,
            state,
        }
    }
}

/// Position tracking during parsing
struct ParsePosition {
    pos: usize,
    text_pos: TextSize,
}

impl ParsePosition {
    /// Create a new parse position
    #[must_use]
    const fn new(pos: usize, text_pos: TextSize) -> Self {
        Self { pos, text_pos }
    }

    /// Create a parse position at the start of input
    ///
    /// Reserved for future use in parser state management and debugging.
    #[must_use]
    #[allow(dead_code)] // Reserved for future parser state APIs
    const fn start() -> Self {
        Self::new(0, TextSize::zero())
    }
}

/// Output structures for parsing - groups mutable output parameters
struct ParseOutput<'a, K: crate::syntax::SyntaxKind> {
    builder: &'a mut GreenNodeBuilder<K>,
    errors: &'a mut Vec<ParseError>,
    warnings: &'a mut Vec<ParseWarning>,
}

impl<'a, K: crate::syntax::SyntaxKind> ParseOutput<'a, K> {
    /// Create a new parse output
    #[must_use]
    const fn new(
        builder: &'a mut GreenNodeBuilder<K>,
        errors: &'a mut Vec<ParseError>,
        warnings: &'a mut Vec<ParseWarning>,
    ) -> Self {
        Self {
            builder,
            errors,
            warnings,
        }
    }
}

impl<T, N> LlParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn new() -> Self {
        // Use default cache settings (matching LlConfig defaults)
        Self::with_cache_settings(2000, 2)
    }

    /// Create parser state with custom cache settings
    #[must_use]
    pub fn with_cache_settings(max_cache_size: usize, cache_history: usize) -> Self {
        Self {
            parse_cache: HashMap::with_hasher(ahash::RandomState::new()),
            cache_version: 0,
            max_cache_size,
            cache_history,
            _phantom: std::marker::PhantomData,
        }
    }

    fn cache_key(
        &self,
        rule: &N,
        start_pos: usize,
        text_start: crate::syntax::TextSize,
    ) -> CacheKey<N> {
        CacheKey::new(rule.clone(), start_pos, self.cache_version, text_start)
    }

    fn get_cached(
        &self,
        rule: &N,
        start_pos: usize,
        text_start: crate::syntax::TextSize,
    ) -> Option<std::sync::Arc<GreenNode<T::Kind>>> {
        let key = self.cache_key(rule, start_pos, text_start);
        self.parse_cache.get(&key).cloned()
    }

    /// Cache a parse result for future reuse
    ///
    /// Reserved for future use when per-rule caching is implemented.
    /// Currently, caching is handled at the [`IncrementalParser`] level.
    #[allow(dead_code)] // Reserved for future per-rule caching
    fn cache_result(
        &mut self,
        rule: &N,
        start_pos: usize,
        text_start: crate::syntax::TextSize,
        node: std::sync::Arc<GreenNode<T::Kind>>,
    ) {
        let key = self.cache_key(rule, start_pos, text_start);
        self.parse_cache.insert(key, node);
    }

    /// Invalidate the entire cache (fallback when range-based invalidation isn't available)
    #[allow(dead_code)] // Used as fallback in some code paths
    pub(crate) fn invalidate_cache(&mut self) {
        // Clear old cache entries (keep recent ones for potential reuse)
        // Use a more sophisticated eviction strategy: keep cache size reasonable
        self.cache_version += 1;

        if self.parse_cache.len() > self.max_cache_size {
            // Remove oldest entries (those with version < current - cache_history)
            // This allows some history while preventing unbounded growth
            let min_version = self.cache_version.saturating_sub(self.cache_history);
            self.parse_cache.retain(|key, _| key.version >= min_version);

            // If still too large, clear half of remaining entries
            if self.parse_cache.len() > self.max_cache_size {
                let target_size = self.max_cache_size / 2;
                let mut entries: Vec<_> = self.parse_cache.drain().collect();
                // Keep entries with highest versions
                entries.sort_by_key(|(k, _)| std::cmp::Reverse(k.version));
                entries.truncate(target_size);
                for (key, value) in entries {
                    self.parse_cache.insert(key, value);
                }
            }
        }
    }

    /// Invalidate cache entries that intersect with the given text range
    ///
    /// This provides selective cache invalidation, only removing entries that
    /// overlap with the affected region. This is more efficient than invalidating
    /// the entire cache for small edits.
    pub(crate) fn invalidate_cache_range(&mut self, range: crate::syntax::TextRange) {
        // Increment version for new entries
        self.cache_version += 1;

        // Remove entries that intersect with the affected range
        self.parse_cache
            .retain(|key, node| !key.intersects(range, node.text_len()));

        // Also evict old entries if cache is too large
        if self.parse_cache.len() > self.max_cache_size {
            let min_version = self.cache_version.saturating_sub(self.cache_history);
            self.parse_cache.retain(|key, _| key.version >= min_version);
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

/// Parse a delimited expression with error recovery
#[allow(clippy::too_many_arguments)]
fn parse_delimited<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    pos: &mut ParsePosition,
    open: &Expr<T, N>,
    content: &Expr<T, N>,
    close: &Expr<T, N>,
    recover: bool,
    output: &mut ParseOutput<'_, T::Kind>,
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    parse_expr_inner(ctx, pos, open, output, session)?;
    let saved_pos = pos.pos;
    let saved_text_pos = pos.text_pos;
    let result = parse_expr_inner(ctx, pos, content, output, session);

    if result.is_err() && recover && ctx.config.error_recovery {
        // Error recovery: try to find and consume the close token
        let close_first = close.first_set(ctx.grammar);
        let mut recovered = false;

        while pos.pos < ctx.input.len() {
            if let Some(token) = ctx.input.get(pos.pos) {
                if close_first.contains(token) {
                    recovered = true;
                    break;
                }
                pos.pos += 1;
                pos.text_pos += token.text_len();
            } else {
                break;
            }
        }

        if recovered {
            parse_expr_inner(ctx, pos, close, output, session)?;
            let span_len =
                TextSize::from((pos.text_pos).into().saturating_sub(saved_text_pos.into()));
            output.warnings.push(ParseWarning {
                span: crate::syntax::TextRange::at(saved_text_pos, span_len),
                message: "Recovered from error in delimited expression".to_string(),
                severity: crate::error::Severity::Warning,
            });
            Ok(())
        } else {
            pos.pos = saved_pos;
            pos.text_pos = saved_text_pos;
            parse_expr_inner(ctx, pos, close, output, session)?;
            result
        }
    } else {
        parse_expr_inner(ctx, pos, close, output, session)?;
        result
    }
}

/// Parse a separated list expression
#[allow(clippy::too_many_arguments)]
fn parse_separated<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    pos: &mut ParsePosition,
    item: &Expr<T, N>,
    separator: &Expr<T, N>,
    min: usize,
    trailing: crate::grammar::TrailingSeparator,
    output: &mut ParseOutput<'_, T::Kind>,
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    let mut count = 0;
    let mut first = true;

    loop {
        if !first {
            let saved_pos = pos.pos;
            let saved_text_pos = pos.text_pos;
            if parse_expr_inner(ctx, pos, separator, output, session).is_err() {
                pos.pos = saved_pos;
                pos.text_pos = saved_text_pos;
                break;
            }
        }
        first = false;

        let saved_pos = pos.pos;
        let saved_text_pos = pos.text_pos;
        if parse_expr_inner(ctx, pos, item, output, session).is_err() {
            pos.pos = saved_pos;
            pos.text_pos = saved_text_pos;
            break;
        }
        count += 1;
    }

    // Check trailing separator
    match trailing {
        crate::grammar::TrailingSeparator::Forbid | crate::grammar::TrailingSeparator::Allow => {
            // Already handled
        }
        crate::grammar::TrailingSeparator::Require => {
            let saved_pos = pos.pos;
            let saved_text_pos = pos.text_pos;
            if parse_expr_inner(ctx, pos, separator, output, session).is_err() {
                pos.pos = saved_pos;
                pos.text_pos = saved_text_pos;
            }
        }
    }

    if count < min {
        Err(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(pos.text_pos, DEFAULT_ERROR_SPAN_LEN),
            message: format!("Expected at least {min} items, got {count}"),
        })
    } else {
        Ok(())
    }
}

/// Parse input using LL parser
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

    let start_time = std::time::Instant::now();

    // Start parsing from entry point
    // Try to get syntax kind from non-terminal, fallback to first token, then default
    let Some(root_kind) = determine_root_kind_or_error(entry, input, grammar, &mut errors) else {
        return create_error_result(entry, input, grammar, errors, warnings);
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

/// Parse with incremental support
#[allow(clippy::too_many_arguments)]
pub fn parse_with_session<T, N>(
    grammar: &Grammar<T, N>,
    table: &ParsingTable<T, N>,
    input: &[T],
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
    entry: &N,
    config: &LlConfig,
    state: &mut LlParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    // If no edits, use cached result if available
    if session.edits().is_empty() {
        return parse(grammar, table, input, entry, config, state);
    }

    // Use incremental parsing with node reuse
    parse_incremental_with_session(grammar, table, input, session, entry, config, state)
}

/// Parse with incremental session support, attempting node reuse
#[allow(clippy::too_many_arguments)]
fn parse_incremental_with_session<T, N>(
    grammar: &Grammar<T, N>,
    table: &ParsingTable<T, N>,
    input: &[T],
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
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

    let start_time = std::time::Instant::now();

    // Start parsing from entry point
    let Some(root_kind) = entry
        .to_syntax_kind()
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| entry.default_syntax_kind())
    else {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
            message: format!(
                "Cannot determine syntax kind for entry point {:?}. \
                 Implement NonTerminal::to_syntax_kind() or NonTerminal::default_syntax_kind()",
                entry.name()
            ),
        });
        return create_incremental_error_result(entry, input, grammar, errors);
    };
    builder.start_node(root_kind);

    let mut pos = 0;
    let mut text_pos = TextSize::zero();

    match parse_expr_with_session(
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
        session,
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

#[allow(clippy::too_many_arguments)] // Required for parser API
fn parse_expr_with_session<T, N>(
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
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    // Try to get expected kind from non-terminal for better matching
    let expected_kind = nt
        .to_syntax_kind()
        .or_else(|| input.get(*pos).map(crate::grammar::Token::kind))
        .or_else(|| nt.default_syntax_kind());

    // First, check persistent parse cache for cross-session node reuse
    if let Some(cached_node) = session.find_cached_node(nt.name(), *text_pos, expected_kind) {
        // Reuse the cached node
        builder
            .reuse_node(cached_node.clone())
            .map_err(|_| ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                message: "Failed to reuse cached node in builder".to_string(),
            })?;

        // Advance token position by counting tokens until we've consumed the node's text length
        let node_text_len = cached_node.text_len();
        let target_text_pos = *text_pos + node_text_len;
        let mut current_text = *text_pos;
        while *pos < input.len() && current_text < target_text_pos {
            let token = &input[*pos];
            let token_len = token.text_len();
            if current_text + token_len > target_text_pos {
                break;
            }
            *pos += 1;
            current_text += token_len;
        }

        // Update text position to match what we consumed
        *text_pos = current_text;

        return Ok(());
    }

    // Fall back to session-local reusable nodes
    if let Some(candidate) = session.find_reusable_at(*text_pos, expected_kind) {
        // Reuse the node
        builder
            .reuse_node(candidate.node.clone())
            .map_err(|_| ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*text_pos, TextSize::from(1)),
                message: "Failed to reuse node in builder".to_string(),
            })?;

        // Advance token position by counting tokens until we've consumed the node's text length
        let target_text_pos = candidate.range.end();
        let mut current_text = *text_pos;
        while *pos < input.len() && current_text < target_text_pos {
            let token = &input[*pos];
            let token_len = token.text_len();
            if current_text + token_len > target_text_pos {
                // This token would exceed the node's range, stop here
                break;
            }
            *pos += 1;
            current_text += token_len;
        }

        // Update text position to match what we consumed
        *text_pos = current_text;

        return Ok(());
    }

    // No reusable node found, fall back to normal parsing with session support
    let mut ctx = ParseContext::new(grammar, table, input, config, state);
    let mut pos_struct = ParsePosition::new(*pos, *text_pos);
    let mut output = ParseOutput::new(builder, errors, warnings);
    let result = parse_expr_impl(&mut ctx, &mut pos_struct, nt, &mut output, Some(session));
    *pos = pos_struct.pos;
    *text_pos = pos_struct.text_pos;
    result
}

/// Parameters for node reuse attempt
struct ReuseParams<'a, T: Token> {
    text_pos: &'a mut TextSize,
    pos: &'a mut usize,
    input: &'a [T],
    expected_kind: Option<T::Kind>,
}

/// Helper to try reusing a node at the current position
/// Checks persistent cache first, then session-local reusable nodes
fn try_reuse_node<T, N>(
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
    params: &mut ReuseParams<'_, T>,
    builder: &mut GreenNodeBuilder<T::Kind>,
    nt: &N,
) -> Result<bool, ParseError>
where
    T: Token,
    N: NonTerminal,
{
    let Some(session) = session else {
        return Ok(false);
    };

    // First, check persistent parse cache for cross-session node reuse
    if let Some(cached_node) =
        session.find_cached_node(nt.name(), *params.text_pos, params.expected_kind)
    {
        // Reuse the cached node
        builder
            .reuse_node(cached_node.clone())
            .map_err(|_| ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*params.text_pos, TextSize::from(1)),
                message: "Failed to reuse cached node in builder".to_string(),
            })?;

        // Advance token position by counting tokens until we've consumed the node's text length
        let node_text_len = cached_node.text_len();
        let target_text_pos = *params.text_pos + node_text_len;
        let mut current_text = *params.text_pos;
        while *params.pos < params.input.len() && current_text < target_text_pos {
            let token = &params.input[*params.pos];
            let token_len = token.text_len();
            if current_text + token_len > target_text_pos {
                break;
            }
            *params.pos += 1;
            current_text += token_len;
        }

        *params.text_pos = current_text;
        return Ok(true);
    }

    // Fall back to session-local reusable nodes
    if let Some(candidate) = session.find_reusable_at(*params.text_pos, params.expected_kind) {
        // Reuse the node
        builder
            .reuse_node(candidate.node.clone())
            .map_err(|_| ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(*params.text_pos, TextSize::from(1)),
                message: "Failed to reuse node in builder".to_string(),
            })?;

        // Advance token position by counting tokens until we've consumed the node's text length
        let target_text_pos = candidate.range.end();
        let mut current_text = *params.text_pos;
        while *params.pos < params.input.len() && current_text < target_text_pos {
            let token = &params.input[*params.pos];
            let token_len = token.text_len();
            if current_text + token_len > target_text_pos {
                break;
            }
            *params.pos += 1;
            current_text += token_len;
        }

        *params.text_pos = current_text;
        Ok(true)
    } else {
        Ok(false)
    }
}

#[allow(clippy::too_many_arguments)] // Required for parser API
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
    let mut ctx = ParseContext::new(grammar, table, input, config, state);
    let mut pos_struct = ParsePosition::new(*pos, *text_pos);
    let mut output = ParseOutput::new(builder, errors, warnings);
    let result = parse_expr_impl(&mut ctx, &mut pos_struct, nt, &mut output, None);
    *pos = pos_struct.pos;
    *text_pos = pos_struct.text_pos;
    result
}

#[allow(clippy::too_many_lines)]
fn parse_expr_impl<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    pos: &mut ParsePosition,
    nt: &N,
    output: &mut ParseOutput<'_, T::Kind>,
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    // Check cache first (for incremental parsing and performance)
    let start_pos = pos.pos;
    let start_text_pos = pos.text_pos;

    // First, check persistent parse cache for cross-session node reuse
    if let Some(session) = session {
        let expected_kind = nt.to_syntax_kind().or_else(|| nt.default_syntax_kind());
        if let Some(cached_node) =
            session.find_cached_node(nt.name(), start_text_pos, expected_kind)
        {
            // Reuse the cached node
            output
                .builder
                .reuse_node(cached_node.clone())
                .map_err(|_| ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(start_text_pos, TextSize::from(1)),
                    message: "Failed to reuse persistent cached node in builder".to_string(),
                })?;

            // Advance position by the node's text length
            let node_text_len = cached_node.text_len();
            let target_text_pos = start_text_pos + node_text_len;
            let mut current_text_pos = start_text_pos;

            // Count tokens until we've consumed the node's text length
            while pos.pos < ctx.input.len() && current_text_pos < target_text_pos {
                let token = &ctx.input[pos.pos];
                let token_len = token.text_len();
                if current_text_pos + token_len > target_text_pos {
                    break;
                }
                pos.pos += 1;
                current_text_pos += token_len;
            }

            pos.text_pos = current_text_pos;
            return Ok(());
        }
    }

    // Fall back to backend-local cache
    if let Some(cached_node) = ctx.state.get_cached(nt, start_pos, start_text_pos) {
        // Reuse the cached node
        output
            .builder
            .reuse_node(cached_node.clone())
            .map_err(|_| ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(start_text_pos, TextSize::from(1)),
                message: "Failed to reuse cached node in builder".to_string(),
            })?;

        // Advance position by the node's text length
        let node_text_len = cached_node.text_len();
        let target_text_pos = start_text_pos + node_text_len;
        let mut current_text_pos = start_text_pos;

        // Count tokens until we've consumed the node's text length
        while pos.pos < ctx.input.len() && current_text_pos < target_text_pos {
            let token = &ctx.input[pos.pos];
            let token_len = token.text_len();
            if current_text_pos + token_len > target_text_pos {
                // This token would exceed the node's range, stop here
                break;
            }
            pos.pos += 1;
            current_text_pos += token_len;
        }

        pos.text_pos = current_text_pos;
        return Ok(());
    }

    // Try to reuse a node from the incremental session
    if let Some(session) = session {
        // Get the expected syntax kind for this non-terminal
        let expected_kind = nt.to_syntax_kind().or_else(|| nt.default_syntax_kind());

        // Try to find a reusable node at the current position
        if let Some(candidate) = session.find_reusable_at(start_text_pos, expected_kind) {
            // Validate that the candidate node matches what we expect
            // Check that it doesn't intersect with affected regions (already checked in find_reusable_at)
            // Check that the kind matches if we have an expected kind
            if expected_kind.is_none() || candidate.node.kind() == expected_kind.unwrap() {
                // Reuse the candidate node
                if output.builder.reuse_node(candidate.node.clone()).is_ok() {
                    // Advance position by the node's text length
                    let node_text_len = candidate.node.text_len();
                    let target_text_pos = start_text_pos + node_text_len;
                    let mut current_text_pos = start_text_pos;

                    // Count tokens until we've consumed the node's text length
                    while pos.pos < ctx.input.len() && current_text_pos < target_text_pos {
                        let token = &ctx.input[pos.pos];
                        let token_len = token.text_len();
                        if current_text_pos + token_len > target_text_pos {
                            // This token would exceed the node's range, stop here
                            break;
                        }
                        pos.pos += 1;
                        current_text_pos += token_len;
                    }

                    pos.text_pos = current_text_pos;

                    // Update metrics to track reuse
                    // Note: metrics are not directly accessible here, but we could add them to ParseOutput

                    return Ok(());
                }
            }
        }
    }

    // Get the rule for this non-terminal
    let rule = ctx
        .grammar
        .get_rule(nt)
        .ok_or_else(|| ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
            message: format!("Undefined rule: {nt:?}"),
        })?;

    // Determine which alternative to use based on lookahead
    // Get up to k tokens of lookahead (optimized for common case of k=1)
    let k = ctx.config.lookahead.min(ctx.table.k());
    let lookahead = if k == 1 {
        // Fast path: single token lookahead
        &ctx.input[pos.pos..(pos.pos + 1).min(ctx.input.len())]
    } else {
        &ctx.input[pos.pos..(pos.pos + k).min(ctx.input.len())]
    };
    let Some(alt_idx) = ctx.table.get(nt, lookahead) else {
        // Error: no production for this lookahead
        let expected: Vec<String> = ctx.table.first_set(nt).map_or_else(
            || vec!["<unknown>".to_string()],
            |first| first.iter().map(|t| format!("{t:?}")).collect(),
        );

        return Err(ParseError::UnexpectedToken {
            span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
            expected,
        });
    };

    // Parse the selected alternative
    // Cache the result if parsing was successful
    // Note: We can't easily extract the node we just built from the builder,
    // so we'll cache it on the next parse when we can capture it.
    // For now, we'll rely on the incremental session for node reuse.
    match &rule.rhs {
        Expr::Choice(alternatives) => {
            if alt_idx < alternatives.len() {
                parse_expr_inner(ctx, pos, &alternatives[alt_idx], output, session)
            } else {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(pos.text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!("Invalid alternative index {alt_idx}"),
                })
            }
        }
        _ => parse_expr_inner(ctx, pos, &rule.rhs, output, session),
    }
}

#[allow(clippy::too_many_lines)]
fn parse_expr_inner<T, N>(
    ctx: &mut ParseContext<'_, T, N>,
    pos: &mut ParsePosition,
    expr: &Expr<T, N>,
    output: &mut ParseOutput<'_, T::Kind>,
    session: Option<&crate::incremental::IncrementalSession<'_, T::Kind>>,
) -> Result<(), ParseError>
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(expected) => {
            if let Some(token) = ctx.input.get(pos.pos) {
                // Compare tokens directly - they implement PartialEq by kind
                if token == expected {
                    let token_text = token.text();
                    let token_len = token.text_len();
                    output
                        .builder
                        .token(token.kind(), token_text)
                        .expect("Internal error: token() called without parent node");
                    pos.pos += 1;
                    pos.text_pos += token_len;
                    Ok(())
                } else {
                    Err(ParseError::UnexpectedToken {
                        span: crate::syntax::TextRange::at(pos.text_pos, DEFAULT_ERROR_SPAN_LEN),
                        expected: vec![format!("{expected:?}")],
                    })
                }
            } else {
                Err(ParseError::UnexpectedEof {
                    span: crate::syntax::TextRange::at(pos.text_pos, DEFAULT_ERROR_SPAN_LEN),
                    expected: vec![format!("{:?}", expected)],
                })
            }
        }

        Expr::Rule(nt) => {
            // Try to reuse a node at this position before parsing
            let expected_kind = nt
                .to_syntax_kind()
                .or_else(|| ctx.input.get(pos.pos).map(crate::grammar::Token::kind))
                .or_else(|| nt.default_syntax_kind());

            let mut reuse_params = ReuseParams {
                text_pos: &mut pos.text_pos,
                pos: &mut pos.pos,
                input: ctx.input,
                expected_kind,
            };
            if try_reuse_node(session, &mut reuse_params, output.builder, nt)? {
                return Ok(());
            }

            parse_expr_impl(ctx, pos, nt, output, session)
        }

        Expr::Seq(exprs) => {
            for expr in exprs {
                parse_expr_inner(ctx, pos, expr, output, session)?;
            }
            Ok(())
        }

        Expr::Choice(alternatives) => {
            // Should have been resolved by table lookup, but handle fallback
            for alt in alternatives {
                let saved_pos = pos.pos;
                let saved_text_pos = pos.text_pos;
                if parse_expr_inner(ctx, pos, alt, output, session).is_ok() {
                    return Ok(());
                }
                pos.pos = saved_pos;
                pos.text_pos = saved_text_pos;
            }
            Err(ParseError::InvalidSyntax {
                span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
                message: "No alternative matched".to_string(),
            })
        }

        Expr::Opt(expr) => {
            let saved_pos = pos.pos;
            let saved_text_pos = pos.text_pos;
            if parse_expr_inner(ctx, pos, expr, output, session).is_err() {
                pos.pos = saved_pos;
                pos.text_pos = saved_text_pos;
            }
            Ok(())
        }

        Expr::Repeat { expr, min, max } => {
            let mut count = 0;
            loop {
                let saved_pos = pos.pos;
                let saved_text_pos = pos.text_pos;
                if parse_expr_inner(ctx, pos, expr, output, session).is_err() {
                    pos.pos = saved_pos;
                    pos.text_pos = saved_text_pos;
                    break;
                }
                count += 1;
                if let Some(max_count) = max
                    && count >= *max_count
                {
                    break;
                }
            }
            if count < *min {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(pos.text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: format!("Expected at least {min} repetitions, got {count}"),
                })
            } else {
                Ok(())
            }
        }

        Expr::Empty => Ok(()),

        Expr::Any => {
            if let Some(token) = ctx.input.get(pos.pos) {
                let token_text = token.text();
                let token_len = token.text_len();
                output
                    .builder
                    .token(token.kind(), token_text)
                    .expect("Internal error: token() called without parent node");
                pos.pos += 1;
                pos.text_pos += token_len;
                Ok(())
            } else {
                Err(ParseError::UnexpectedEof {
                    span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
                    expected: vec!["<any token>".to_string()],
                })
            }
        }

        Expr::Eof => {
            if pos.pos >= ctx.input.len() {
                Ok(())
            } else {
                Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(pos.text_pos, DEFAULT_ERROR_SPAN_LEN),
                    message: "Expected end of file".to_string(),
                })
            }
        }

        Expr::Node { kind, expr } => {
            // Convert NonTerminal to SyntaxKind using the trait method
            let node_kind = kind
                .to_syntax_kind()
                .or_else(|| ctx.input.get(pos.pos).map(crate::grammar::Token::kind))
                .or_else(|| ctx.input.first().map(crate::grammar::Token::kind))
                .or_else(|| kind.default_syntax_kind());

            if let Some(k) = node_kind {
                output.builder.start_node(k);
                let result = parse_expr_inner(ctx, pos, expr, output, session);
                if output.builder.finish_node().is_err() {
                    output.errors.push(ParseError::InvalidSyntax {
                        span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
                        message: "Builder error: unmatched finish_node".to_string(),
                    });
                }
                result
            } else {
                // Can't determine kind - this is an error but we'll continue parsing
                output.warnings.push(ParseWarning {
                    span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
                    message: format!(
                        "Cannot determine syntax kind for node {:?}. \
                         Implement NonTerminal::to_syntax_kind() or NonTerminal::default_syntax_kind()",
                        kind.name()
                    ),
                    severity: crate::error::Severity::Warning,
                });
                // Continue parsing without the node wrapper
                parse_expr_inner(ctx, pos, expr, output, session)
            }
        }

        Expr::Label { name: _name, expr } => {
            // Labels are metadata, don't affect parsing
            parse_expr_inner(ctx, pos, expr, output, session)
        }

        Expr::Flatten(expr) => {
            // Flatten doesn't affect parsing, just tree structure
            parse_expr_inner(ctx, pos, expr, output, session)
        }

        Expr::Prune(expr) => {
            // Prune doesn't affect parsing
            parse_expr_inner(ctx, pos, expr, output, session)
        }

        Expr::Lookahead(expr) => {
            let saved_pos = pos.pos;
            let saved_text_pos = pos.text_pos;
            let result = parse_expr_inner(ctx, pos, expr, output, session);
            pos.pos = saved_pos;
            pos.text_pos = saved_text_pos;
            result
        }

        Expr::NotLookahead(expr) => {
            let saved_pos = pos.pos;
            let saved_text_pos = pos.text_pos;
            let result = parse_expr_inner(ctx, pos, expr, output, session);
            pos.pos = saved_pos;
            pos.text_pos = saved_text_pos;
            match result {
                Ok(()) => Err(ParseError::InvalidSyntax {
                    span: crate::syntax::TextRange::at(pos.text_pos, TextSize::from(1)),
                    message: "Negative lookahead matched".to_string(),
                }),
                Err(_) => Ok(()),
            }
        }

        Expr::Separated {
            item,
            separator,
            min,
            trailing,
        } => parse_separated(ctx, pos, item, separator, *min, *trailing, output, session),

        Expr::Delimited {
            open,
            content,
            close,
            recover,
        } => parse_delimited(ctx, pos, open, content, close, *recover, output, session),

        Expr::RecoveryPoint { expr, sync_tokens } => {
            let result = parse_expr_inner(ctx, pos, expr, output, session);
            if result.is_err() && ctx.config.error_recovery {
                // Error recovery: skip until sync token
                while pos.pos < ctx.input.len() {
                    let token = &ctx.input[pos.pos];
                    if sync_tokens.contains(token) {
                        break;
                    }
                    pos.pos += 1;
                    pos.text_pos += TextSize::from(1);
                }
            }
            result
        }
    }
}
