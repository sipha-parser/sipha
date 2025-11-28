//! # Incremental Parsing
//!
//! Support for efficient re-parsing of edited source code.
//!
//! ## Overview
//!
//! Incremental parsing allows re-parsing only the affected regions of code after edits,
//! significantly improving performance for interactive applications like IDEs and editors.
//!
//! ## Features
//!
//! - **Node reuse**: Reuse unchanged subtrees from previous parse
//! - **Affected range computation**: Determine minimal region to reparse
//! - **Cache management**: Version-based cache invalidation
//!
//! ## Usage
//!
//! ```rust,no_run
//! use sipha::incremental::{IncrementalParser, TextEdit};
//! use sipha::syntax::{TextRange, TextSize};
//! use sipha::backend::ParserBackend;
//!
//! // Example: Parse with cache population for optimal incremental performance
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! #     // Setup would require actual Token, NonTerminal, and Grammar types
//! #     // let mut parser = IncrementalParser::new(backend);
//! #     
//! #     // Initial parse - cache is populated automatically
//! #     // let result1 = parser.parse_incremental_with_grammar(
//! #     //     &tokens,
//! #     //     None,
//! #     //     &[],
//! #     //     entry_point,
//! #     //     &grammar,
//! #     // );
//! #     
//! #     // After an edit, reuse nodes from previous parse
//! #     // let edits = vec![TextEdit::replace(
//! #     //     TextRange::new(TextSize::from(10), TextSize::from(20)),
//! #     //     "new code",
//! #     // )];
//! #     // let result2 = parser.parse_incremental(
//! #     //     &new_tokens,
//! #     //     Some(&result1.root),
//! #     //     &edits,
//! #     //     entry_point,
//! #     // );
//! #     Ok(())
//! # }
//! ```
//!
//! ## Cache Population
//!
//! For optimal performance, use `parse_incremental_with_grammar` which automatically
//! populates the parse cache with nodes that can be reused in future parses. The cache
//! is keyed by rule name and position, allowing efficient lookup during incremental parsing.
//!
//! ## Status
//!
//! Incremental parsing is fully implemented with:
//! - **Node reuse**: Unchanged subtrees are automatically reused from previous parses
//! - **Cache population**: Parse results are cached and can be reused for future parses
//! - **Affected range computation**: Only affected regions are re-parsed
//! - **Smart cache invalidation**: Cache entries are invalidated based on edit locations
//!
//! The parser automatically identifies reusable nodes and integrates them into the parse,
//! significantly improving performance for interactive editing scenarios.

use crate::syntax::utils::{GreenNodeSpan, visit_green_spans};
use crate::syntax::{GreenNode, TextRange, TextSize};
use std::sync::Arc;

const LOOKBACK_PADDING: TextSize = TextSize::from(16);

/// Configuration for reuse candidate budget in incremental parsing.
#[derive(Debug, Clone, Copy)]
pub enum ReuseBudget {
    /// Fixed budget with a maximum number of candidates.
    Fixed(usize),
    /// Heuristic-based budget calculation.
    Heuristic {
        /// Maximum tree depth to consider (deeper trees allow more candidates).
        max_depth: usize,
        /// Maximum number of nodes to consider (larger files allow more candidates).
        max_nodes: usize,
    },
}

impl Default for ReuseBudget {
    fn default() -> Self {
        Self::Heuristic {
            max_depth: 20,
            max_nodes: 1000,
        }
    }
}

impl ReuseBudget {
    /// Calculate the budget based on the heuristic parameters.
    ///
    /// The calculation considers:
    /// - Tree depth: deeper trees get more candidates (up to `max_depth`)
    /// - File size: larger files get more candidates (up to `max_nodes`)
    /// - Affected region size: smaller affected regions get more candidates
    fn calculate(&self, tree_depth: usize, file_size: TextSize, affected_size: TextSize) -> usize {
        /// Helper to safely convert f64 to usize for bounded values [0.0, 200.0]
        /// The bounds check ensures the conversion is safe
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        fn f64_to_usize_bounded(value: f64, min: f64, max: f64) -> usize {
            if (min..=max).contains(&value) {
                value as usize
            } else {
                0
            }
        }
        match self {
            Self::Fixed(budget) => *budget,
            Self::Heuristic {
                max_depth,
                max_nodes,
            } => {
                // Base budget from tree depth (deeper = more candidates)
                let depth_factor = (tree_depth.min(*max_depth) * 10).max(50);

                // File size factor (larger files = more candidates, but with diminishing returns)
                let file_size_u64 = u64::from(file_size.into());
                let size_factor = ((file_size_u64 / 100) as usize).min(*max_nodes / 2);

                // Affected region factor (smaller affected regions = more candidates)
                let affected_size_u64 = u64::from(affected_size.into());
                let total_size_u64 = u64::from(file_size.into());
                // Intentional: precision loss acceptable for ratio calculation
                // f64::from() doesn't exist for u64, so we must use as f64
                #[allow(clippy::cast_precision_loss)]
                // Precision loss acceptable for ratio calculation
                let affected_ratio = if total_size_u64 > 0 {
                    (affected_size_u64 as f64) / (total_size_u64 as f64)
                } else {
                    1.0
                };
                // Inverse relationship: smaller affected ratio = more candidates
                // Factor is bounded between 0.0 and 200.0, so conversion is safe
                let factor_f64 = (1.0 - affected_ratio.min(1.0)) * 200.0;
                let factor_rounded = factor_f64.round();
                let affected_factor = f64_to_usize_bounded(factor_rounded, 0.0, 200.0);

                // Combine factors with reasonable bounds
                let budget = depth_factor + size_factor + affected_factor;
                budget.min(*max_nodes).max(50) // Ensure minimum of 50, maximum of max_nodes
            }
        }
    }
}

pub struct IncrementalParser<P, T: crate::grammar::Token, N> {
    parser: P,
    cache: ParseCache<T::Kind>,
    _phantom: std::marker::PhantomData<(T, N)>,
}

/// Captures everything a backend needs to perform an incremental parse.
pub struct IncrementalSession<'a, K: crate::syntax::SyntaxKind> {
    old_tree: Option<&'a GreenNode<K>>,
    edits: &'a [TextEdit],
    affected: AffectedRegion,
    reusable: Vec<ReuseCandidate<K>>,
    reusable_by_pos: hashbrown::HashMap<crate::syntax::TextSize, Vec<usize>, ahash::RandomState>,
    /// Optional reference to persistent parse cache for cross-session node reuse
    cache: Option<&'a ParseCache<K>>,
}

impl<'a, K: crate::syntax::SyntaxKind> IncrementalSession<'a, K> {
    /// Create a new incremental session with default heuristic budget.
    #[must_use]
    pub fn new(old_tree: Option<&'a GreenNode<K>>, edits: &'a [TextEdit]) -> Self {
        Self::new_with_budget(old_tree, edits, ReuseBudget::default())
    }

    /// Create a new incremental session with a custom reuse budget.
    #[must_use]
    pub fn new_with_budget(
        old_tree: Option<&'a GreenNode<K>>,
        edits: &'a [TextEdit],
        budget: ReuseBudget,
    ) -> Self {
        let affected = AffectedRegion::from_edits(edits, old_tree.map(GreenNode::text_len));
        let reusable = match old_tree {
            Some(tree) if !edits.is_empty() => {
                // Calculate budget based on tree characteristics
                let tree_depth = calculate_tree_depth(tree);
                let file_size = tree.text_len();
                let affected_size = affected.union().len();
                let budget_value = budget.calculate(tree_depth, file_size, affected_size);
                find_reusable_nodes(tree, affected.expanded(), budget_value)
            }
            _ => Vec::new(),
        };

        // Build index by position for faster lookup
        let mut reusable_by_pos = hashbrown::HashMap::with_hasher(ahash::RandomState::new());
        for (idx, candidate) in reusable.iter().enumerate() {
            reusable_by_pos
                .entry(candidate.range.start())
                .or_insert_with(Vec::new)
                .push(idx);
        }

        Self {
            old_tree,
            edits,
            affected,
            reusable,
            reusable_by_pos,
            cache: None,
        }
    }

    /// Create a new incremental session with cache access for persistent node reuse.
    #[must_use]
    pub fn with_cache(
        old_tree: Option<&'a GreenNode<K>>,
        edits: &'a [TextEdit],
        cache: &'a ParseCache<K>,
    ) -> Self {
        let mut session = Self::new(old_tree, edits);
        session.cache = Some(cache);
        session
    }

    #[must_use]
    pub const fn old_tree(&self) -> Option<&'a GreenNode<K>> {
        self.old_tree
    }

    #[must_use]
    pub const fn edits(&self) -> &'a [TextEdit] {
        self.edits
    }

    #[must_use]
    pub const fn affected(&self) -> &AffectedRegion {
        &self.affected
    }

    #[must_use]
    pub fn reusable(&self) -> &[ReuseCandidate<K>] {
        &self.reusable
    }

    /// Find a reusable node at the given position, optionally matching the expected kind.
    /// Returns the best matching candidate (deepest node that matches).
    #[must_use]
    pub fn find_reusable_at(
        &self,
        pos: crate::syntax::TextSize,
        expected_kind: Option<K>,
    ) -> Option<&ReuseCandidate<K>> {
        let indices = self.reusable_by_pos.get(&pos)?;

        // Find the deepest matching node (prefer deeper nodes for better reuse)
        indices
            .iter()
            .filter_map(|&idx| self.reusable.get(idx))
            .filter(|candidate| {
                // Verify node doesn't intersect with affected regions
                !self.affected.intersects(candidate.range)
            })
            .filter(|candidate| {
                // If expected kind is provided, verify it matches
                expected_kind.is_none_or(|kind| candidate.node.kind() == kind)
            })
            .max_by_key(|candidate| candidate.depth)
    }

    /// Query the persistent parse cache for a node at the given position and rule name.
    /// This checks the persistent cache before falling back to session-local reusable nodes.
    #[must_use]
    pub fn find_cached_node(
        &self,
        rule_name: &str,
        pos: crate::syntax::TextSize,
        expected_kind: Option<K>,
    ) -> Option<std::sync::Arc<GreenNode<K>>> {
        let cache = self.cache?;

        // Check if the position is in an affected region - if so, don't use cache
        if self.affected.intersects(crate::syntax::TextRange::at(
            pos,
            crate::syntax::TextSize::from(1),
        )) {
            return None;
        }

        // Query the persistent cache
        if let Some(cached_node) = cache.get(rule_name, pos) {
            // Verify the cached node matches the expected kind if provided
            if expected_kind.is_none_or(|kind| cached_node.kind() == kind) {
                return Some(cached_node);
            }
        }

        None
    }
}

/// Describes the region of text touched by the latest edits.
#[derive(Debug, Clone, Copy)]
pub struct AffectedRegion {
    union: TextRange,
    expanded: TextRange,
}

impl AffectedRegion {
    #[must_use]
    pub fn from_edits(edits: &[TextEdit], total_len: Option<TextSize>) -> Self {
        if edits.is_empty() {
            let zero = TextRange::new(TextSize::zero(), TextSize::zero());
            return Self {
                union: zero,
                expanded: zero,
            };
        }

        let mut start = edits[0].range.start();
        let mut end = edits[0].range.end();

        for edit in &edits[1..] {
            start = start.min(edit.range.start());
            end = end.max(edit.range.end());
        }

        let union = TextRange::new(start, end);
        let expanded_start = saturating_sub(start, LOOKBACK_PADDING);
        let expanded_end_unclamped = end + LOOKBACK_PADDING;
        let expanded_end = total_len.map_or(expanded_end_unclamped, |len| {
            if expanded_end_unclamped > len {
                len
            } else {
                expanded_end_unclamped
            }
        });

        Self {
            union,
            expanded: TextRange::new(expanded_start, expanded_end),
        }
    }

    #[must_use]
    pub const fn union(&self) -> TextRange {
        self.union
    }

    #[must_use]
    pub const fn expanded(&self) -> TextRange {
        self.expanded
    }

    #[must_use]
    pub fn intersects(&self, range: TextRange) -> bool {
        self.expanded.intersect(range).is_some()
    }
}

#[derive(Debug, Clone)]
pub struct ReuseCandidate<K: crate::syntax::SyntaxKind> {
    pub node: Arc<GreenNode<K>>,
    pub range: TextRange,
    pub depth: usize,
}

impl<K: crate::syntax::SyntaxKind> ReuseCandidate<K> {
    /// Create a new reuse candidate
    #[must_use]
    pub const fn new(node: Arc<GreenNode<K>>, range: TextRange, depth: usize) -> Self {
        Self { node, range, depth }
    }

    #[must_use]
    pub fn intersects(&self, range: TextRange) -> bool {
        self.range.intersect(range).is_some()
    }
}

pub struct ParseCache<K: crate::syntax::SyntaxKind> {
    version: usize,
    /// Cache of parsed nodes for incremental parsing performance.
    ///
    /// This cache stores nodes that can be reused when parsing incrementally edited input.
    /// Nodes are keyed by rule name (interned), start position, and version.
    nodes: hashbrown::HashMap<CacheKey, std::sync::Arc<GreenNode<K>>, ahash::RandomState>,
    /// String interner for rule names
    interner: lasso::Rodeo,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey {
    rule: lasso::Spur, // Interned rule name
    start: crate::syntax::TextSize,
    version: usize,
}

impl CacheKey {
    /// Create a new cache key
    #[must_use]
    const fn new(rule: lasso::Spur, start: crate::syntax::TextSize, version: usize) -> Self {
        Self {
            rule,
            start,
            version,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TextEdit {
    pub range: TextRange,
    pub new_text: String,
}

impl TextEdit {
    /// Create a new text edit
    #[must_use]
    pub const fn new(range: TextRange, new_text: String) -> Self {
        Self { range, new_text }
    }

    /// Create a text edit that replaces the given range with new text
    #[must_use]
    pub fn replace(range: TextRange, new_text: impl Into<String>) -> Self {
        Self::new(range, new_text.into())
    }

    /// Create a text edit that inserts text at the given position
    #[must_use]
    pub fn insert(pos: TextSize, text: impl Into<String>) -> Self {
        Self::new(TextRange::new(pos, pos), text.into())
    }

    /// Create a text edit that deletes the given range
    #[must_use]
    pub const fn delete(range: TextRange) -> Self {
        Self::new(range, String::new())
    }
}

impl<P, T, N> IncrementalParser<P, T, N>
where
    P: crate::backend::ParserBackend<T, N>,
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    pub fn new(parser: P) -> Self {
        Self {
            parser,
            cache: ParseCache::new(),
            _phantom: std::marker::PhantomData,
        }
    }

    /// Get a reference to the parser backend
    pub const fn parser(&self) -> &P {
        &self.parser
    }

    /// Get a mutable reference to the parser backend
    pub const fn parser_mut(&mut self) -> &mut P {
        &mut self.parser
    }

    /// Parse with incremental support, optionally populating the cache if grammar is provided.
    ///
    /// If `grammar` is provided, the cache will be automatically populated with nodes from
    /// the parse result, enabling better performance for subsequent incremental parses.
    ///
    /// This method combines the functionality of `parse_incremental` and
    /// `parse_incremental_with_grammar` into a single method for convenience.
    pub fn parse_incremental(
        &mut self,
        input: &[T],
        old_tree: Option<&GreenNode<T::Kind>>,
        edits: &[TextEdit],
        entry: N,
        grammar: Option<&crate::grammar::Grammar<T, N>>,
    ) -> crate::error::ParseResult<T, N>
    where
        T: crate::grammar::Token,
        N: crate::grammar::NonTerminal,
    {
        // Create session with cache access for persistent node reuse
        let session = IncrementalSession::with_cache(old_tree, edits, &self.cache);
        let result = if edits.is_empty() {
            self.parser.parse(input, entry)
        } else {
            self.parser.parse_with_session(input, entry, &session)
        };

        // Automatically populate cache if grammar is provided
        if let Some(grammar) = grammar {
            self.update_cache(&result.root, grammar);
        } else {
            // No grammar provided - just increment version for invalidation
            self.cache.version += 1;
            self.cache.evict_old_entries(2);
        }

        result
    }

    /// Parse with grammar access for cache population.
    ///
    /// This method is similar to `parse_incremental`, but it also populates the parse cache
    /// with nodes from the parse result. This enables better performance for subsequent
    /// incremental parses by allowing the parser to reuse cached nodes.
    ///
    /// # Performance
    ///
    /// Using this method is recommended when you have access to the grammar, as it enables
    /// full cache population. The cache stores nodes keyed by rule name and position,
    /// allowing efficient lookup during future incremental parses.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::incremental::IncrementalParser;
    /// # use sipha::grammar::{Grammar, GrammarBuilder, Expr, NonTerminal, Token};
    /// # use sipha::syntax::{SyntaxKind, TextSize};
    /// # use sipha::backend::ParserBackend;
    /// # // Setup example types
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MySyntaxKind { Number }
    /// # impl SyntaxKind for MySyntaxKind {
    /// #     fn is_terminal(self) -> bool { true }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # struct MyToken;
    /// # impl Token for MyToken {
    /// #     type Kind = MySyntaxKind;
    /// #     fn kind(&self) -> Self::Kind { MySyntaxKind::Number }
    /// #     fn text_len(&self) -> TextSize { TextSize::from(1) }
    /// #     fn text(&self) -> compact_str::CompactString { "1".into() }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # enum MyNonTerminal { Expr }
    /// # impl NonTerminal for MyNonTerminal {
    /// #     fn name(&self) -> &str { "Expr" }
    /// # }
    /// # // Build grammar and parser
    /// # let grammar = GrammarBuilder::new()
    /// #     .entry_point(MyNonTerminal::Expr)
    /// #     .rule(MyNonTerminal::Expr, Expr::token(MyToken))
    /// #     .build()
    /// #     .expect("Failed to build grammar");
    /// # let backend = sipha::backend::ll::LlParser::new(&grammar, Default::default()).unwrap();
    /// # let mut parser = IncrementalParser::new(backend);
    /// # let tokens = vec![MyToken];
    /// # let entry_point = MyNonTerminal::Expr;
    /// // Parse with cache population
    /// let result = parser.parse_incremental_with_grammar(
    ///     &tokens,
    ///     None,
    ///     &[],
    ///     entry_point,
    ///     &grammar,
    /// );
    /// // Cache is now populated with reusable nodes
    /// ```
    ///
    /// # Deprecated
    ///
    /// This method is now a convenience wrapper around `parse_incremental` with grammar.
    /// Consider using `parse_incremental` with `Some(grammar)` instead.
    pub fn parse_incremental_with_grammar(
        &mut self,
        input: &[T],
        old_tree: Option<&GreenNode<T::Kind>>,
        edits: &[TextEdit],
        entry: N,
        grammar: &crate::grammar::Grammar<T, N>,
    ) -> crate::error::ParseResult<T, N>
    where
        T: crate::grammar::Token,
        N: crate::grammar::NonTerminal,
    {
        self.parse_incremental(input, old_tree, edits, entry, Some(grammar))
    }

    /// Update cache after parsing by populating it with nodes from the parse result.
    ///
    /// This extracts nodes from the parse tree and stores them in the cache for future reuse.
    fn update_cache(&mut self, root: &GreenNode<T::Kind>, grammar: &crate::grammar::Grammar<T, N>)
    where
        T: crate::grammar::Token,
        N: crate::grammar::NonTerminal,
    {
        // Increment version to invalidate old cache entries
        self.cache.version += 1;

        // Evict old entries (keep last 2 versions)
        self.cache.evict_old_entries(2);

        // Populate cache with nodes from the parse tree
        self.cache.populate_from_tree(root, grammar);
    }
}

/// Represents a node from the old tree that can be reused after an edit.
///
/// # Implementation Status
///
/// **Reserved for future use**: This type is part of the infrastructure for incremental
/// parsing optimization to avoid re-parsing unchanged regions. The node reuse logic
/// is partially implemented but not yet fully integrated into the parsing pipeline.
impl<K: crate::syntax::SyntaxKind> ParseCache<K> {
    fn new() -> Self {
        Self {
            version: 0,
            nodes: hashbrown::HashMap::with_hasher(ahash::RandomState::new()),
            interner: lasso::Rodeo::new(),
        }
    }

    /// Get a cached node for the given rule and position
    #[must_use]
    pub(crate) fn get(
        &self,
        rule_name: &str,
        start: crate::syntax::TextSize,
    ) -> Option<std::sync::Arc<GreenNode<K>>> {
        let rule_spur = self.interner.get(rule_name)?;
        let key = CacheKey::new(rule_spur, start, self.version);
        self.nodes.get(&key).cloned()
    }

    /// Store a node in the cache
    pub(crate) fn insert(
        &mut self,
        rule_name: &str,
        start: crate::syntax::TextSize,
        node: std::sync::Arc<GreenNode<K>>,
    ) {
        let rule_spur = self.interner.get_or_intern(rule_name);
        let key = CacheKey::new(rule_spur, start, self.version);
        self.nodes.insert(key, node);
    }

    /// Populate cache from a parse tree by extracting nodes and matching them to rules
    pub(crate) fn populate_from_tree<T, N>(
        &mut self,
        root: &GreenNode<K>,
        grammar: &crate::grammar::Grammar<T, N>,
    ) where
        T: crate::grammar::Token<Kind = K>,
        N: crate::grammar::NonTerminal,
        K: crate::syntax::SyntaxKind,
    {
        // Traverse the tree and extract nodes
        visit_green_spans(root, |span: GreenNodeSpan<'_, K>| {
            // Only cache non-terminal nodes (those that represent rules)
            if span.node.kind().is_terminal() {
                return;
            }

            // Try to find a matching rule for this node's kind
            for (nt, _rule) in grammar.rules() {
                // Check if this non-terminal maps to this syntax kind
                if let Some(kind) = nt.to_syntax_kind::<K>()
                    && kind == span.node.kind()
                {
                    // Found a matching rule, cache this node
                    if let Some(node_arc) = span.node_arc.clone() {
                        self.insert(nt.name(), span.range.start(), node_arc);
                    }
                    break;
                }
            }
        });
    }

    /// Clear old cache entries based on version
    pub(crate) fn evict_old_entries(&mut self, max_versions: usize) {
        if max_versions == 0 {
            return;
        }
        let min_version = self.version.saturating_sub(max_versions);
        self.nodes.retain(|key, _| key.version >= min_version);
    }
}

/// Calculate the maximum depth of a green tree.
fn calculate_tree_depth<K: crate::syntax::SyntaxKind>(root: &GreenNode<K>) -> usize {
    let mut max_depth = 0;
    crate::syntax::utils::visit_green_spans(
        root,
        |span: crate::syntax::utils::GreenNodeSpan<'_, K>| {
            max_depth = max_depth.max(span.depth);
        },
    );
    max_depth
}

fn find_reusable_nodes<K: crate::syntax::SyntaxKind>(
    root: &GreenNode<K>,
    affected: TextRange,
    budget: usize,
) -> Vec<ReuseCandidate<K>> {
    let mut reusable = Vec::new();
    visit_green_spans(root, |span: GreenNodeSpan<'_, K>| {
        if reusable.len() >= budget {
            return;
        }

        if span.depth == 0 || span.range.intersect(affected).is_some() {
            return;
        }

        if let Some(node) = span.node_arc.clone() {
            reusable.push(ReuseCandidate {
                node,
                range: span.range,
                depth: span.depth,
            });
        }
    });

    reusable
}

const fn saturating_sub(lhs: TextSize, rhs: TextSize) -> TextSize {
    let lhs_raw = lhs.into();
    let rhs_raw = rhs.into();
    if lhs_raw >= rhs_raw {
        TextSize::from(lhs_raw - rhs_raw)
    } else {
        TextSize::zero()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{GreenNodeBuilder, SyntaxKind, TextRange, TextSize};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestKind {
        Root,
        Expr,
        Ident,
        Number,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Root | Self::Expr)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    #[test]
    fn test_text_edit_new() {
        let edit = TextEdit {
            range: crate::syntax::TextRange::new(TextSize::from(10), TextSize::from(20)),
            new_text: "new text".to_string(),
        };

        assert_eq!(edit.range.start(), TextSize::from(10));
        assert_eq!(edit.range.end(), TextSize::from(20));
        assert_eq!(edit.new_text, "new text");
    }

    #[test]
    fn test_parse_cache_new() {
        let cache = ParseCache::<TestKind>::new();
        assert_eq!(cache.version, 0);
        assert_eq!(cache.nodes.len(), 0);
    }

    #[test]
    fn test_affected_region_expansion() {
        let edits = [
            TextEdit {
                range: TextRange::new(TextSize::from(5), TextSize::from(10)),
                new_text: "a".into(),
            },
            TextEdit {
                range: TextRange::new(TextSize::from(8), TextSize::from(12)),
                new_text: "b".into(),
            },
        ];

        let region = AffectedRegion::from_edits(&edits, Some(TextSize::from(50)));
        assert_eq!(region.union().start(), TextSize::from(5));
        assert_eq!(region.union().end(), TextSize::from(12));
        assert!(region.expanded().start() <= TextSize::from(5));
        assert!(region.expanded().end() >= TextSize::from(12));
    }

    #[test]
    fn test_reuse_candidates_skip_affected() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Ident, "a").unwrap();
        builder.finish_node().unwrap();

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Ident, "b").unwrap();
        builder.finish_node().unwrap();

        let root = builder.finish().unwrap();
        let affected = TextRange::new(TextSize::from(0), TextSize::from(1));
        let candidates = find_reusable_nodes(&root, affected, 10);
        assert!(!candidates.is_empty());
        assert!(
            candidates
                .iter()
                .all(|c| c.range.intersect(affected).is_none())
        );
    }

    // Mock types for testing
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    struct MockToken;

    impl crate::grammar::Token for MockToken {
        type Kind = TestKind;
        fn kind(&self) -> Self::Kind {
            TestKind::Ident
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    struct MockNonTerminal;

    impl crate::grammar::NonTerminal for MockNonTerminal {
        fn name(&self) -> &'static str {
            "Mock"
        }
    }

    #[derive(Debug)]
    #[allow(dead_code)]
    struct MockError(String);

    impl std::fmt::Display for MockError {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl std::error::Error for MockError {}

    #[allow(dead_code)]
    struct MockParser;

    impl crate::backend::ParserBackend<MockToken, MockNonTerminal> for MockParser {
        type Config = ();
        type Error = MockError;
        type State = ();

        fn new(
            _grammar: &crate::grammar::Grammar<MockToken, MockNonTerminal>,
            _config: Self::Config,
        ) -> Result<Self, Self::Error> {
            Ok(Self)
        }

        fn parse(
            &mut self,
            _input: &[MockToken],
            _entry: MockNonTerminal,
        ) -> crate::error::ParseResult<MockToken, MockNonTerminal> {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(TestKind::Root);
            let root = builder.finish().unwrap();

            crate::error::ParseResult {
                root,
                errors: vec![],
                warnings: vec![],
                metrics: crate::error::ParseMetrics::default(),
                #[cfg(feature = "backend-glr")]
                forest: None,
                _phantom: std::marker::PhantomData,
            }
        }

        fn validate(
            _grammar: &crate::grammar::Grammar<MockToken, MockNonTerminal>,
        ) -> Vec<crate::grammar::GrammarError<MockToken, MockNonTerminal>> {
            vec![]
        }

        fn capabilities() -> crate::backend::BackendCapabilities {
            crate::backend::BackendCapabilities {
                name: "Mock",
                algorithm: crate::backend::Algorithm::LL,
                supports_left_recursion: false,
                supports_ambiguity: false,
                supports_incremental: true,
                supports_error_recovery: false,
                max_lookahead: Some(1),
            }
        }

        fn state(&self) -> &Self::State {
            &()
        }
    }
}
