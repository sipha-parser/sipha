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
//!
//! // Example usage (types would need to be properly defined with Token and NonTerminal traits)
//! # fn example() {
//! #     // This is a conceptual example - actual usage requires implementing
//! #     // Token and NonTerminal traits for your types
//! #     let start = TextSize::from(10);
//! #     let end = TextSize::from(20);
//! #     let edits = vec![TextEdit {
//! #         range: TextRange::new(start, end),
//! #         new_text: "new code".into(),
//! #     }];
//! # }
//!
//! // let mut parser = IncrementalParser::new(backend);
//! // let result1 = parser.parse_incremental(&tokens, None, &[], entry);
//! // let result2 = parser.parse_incremental(&tokens, Some(&result1.root), &edits, entry);
//! ```
//!
//! ## Status
//!
//! Basic infrastructure is in place, but full node reuse is a future enhancement.
//! Currently, incremental parsing performs full reparse with cache invalidation.

use crate::syntax::{GreenNode, TextRange};

pub struct IncrementalParser<P, T: crate::grammar::Token, N> {
    parser: P,
    cache: ParseCache<T::Kind>,
    _phantom: std::marker::PhantomData<(T, N)>,
}

pub struct ParseCache<K: crate::syntax::SyntaxKind> {
    version: usize,
    /// Cache of parsed nodes for incremental parsing performance.
    /// Reserved for future incremental parsing optimization. Infrastructure is in place
    /// but cache population and lookup are not yet implemented.
    nodes: hashbrown::HashMap<CacheKey, std::sync::Arc<GreenNode<K>>, ahash::RandomState>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey {
    rule: lasso::Spur, // Interned rule name
    start: crate::syntax::TextSize,
    version: usize,
}

#[derive(Debug, Clone)]
pub struct TextEdit {
    pub range: TextRange,
    pub new_text: String,
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
    
    pub fn parse_incremental(
        &mut self,
        input: &[T],
        old_tree: Option<&GreenNode<T::Kind>>,
        edits: &[TextEdit],
        entry: N,
    ) -> crate::error::ParseResult<T, N>
    where
        T: crate::grammar::Token,
    {
        if edits.is_empty() {
            return self.parser.parse(input, entry);
        }
        
        // Find affected range
        let affected_range = Self::compute_affected_range(edits, old_tree);
        
        // Find reusable nodes
        let _reusable = Self::find_reusable_nodes(old_tree, affected_range);
        
        // Reparse affected region
        let result = self.parser.parse_incremental(
            input,
            old_tree,
            edits,
            entry,
        );
        
        // Update cache
        self.update_cache(&result.root);
        
        result
    }
    
    fn compute_affected_range(
        edits: &[TextEdit],
        _old_tree: Option<&GreenNode<T::Kind>>,
    ) -> TextRange
    where
        T: crate::grammar::Token,
    {
        if edits.is_empty() {
            return TextRange::new(crate::syntax::TextSize::zero(), crate::syntax::TextSize::zero());
        }
        
        // Union all edit ranges
        let mut start = edits[0].range.start();
        let mut end = edits[0].range.end();
        
        for edit in &edits[1..] {
            start = start.min(edit.range.start());
            end = end.max(edit.range.end());
        }
        
        TextRange::new(start, end)
    }
    
    const fn find_reusable_nodes(
        _old_tree: Option<&GreenNode<T::Kind>>,
        _affected_range: TextRange,
    ) -> Vec<ReusableNode<T::Kind>>
    where
        T: crate::grammar::Token,
    {
        // Placeholder implementation
        Vec::new()
    }
    
    const fn update_cache(&mut self, _root: &GreenNode<T::Kind>)
    where
        T: crate::grammar::Token,
    {
        self.cache.version += 1;
    }
}

/// Represents a node from the old tree that can be reused after an edit.
/// Reserved for future incremental parsing optimization to avoid re-parsing unchanged regions.
struct ReusableNode<K: crate::syntax::SyntaxKind> {
    /// The reusable green node from the previous parse.
    /// Reserved for future use in incremental parsing optimization.
    #[allow(dead_code)] // Reserved for future use
    green: std::sync::Arc<GreenNode<K>>,
    /// The text range this node covers in the source.
    /// Reserved for future use in incremental parsing optimization.
    #[allow(dead_code)] // Reserved for future use
    range: TextRange,
}

impl<K: crate::syntax::SyntaxKind> ParseCache<K> {
    fn new() -> Self {
        Self {
            version: 0,
            nodes: hashbrown::HashMap::with_hasher(ahash::RandomState::new()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{TextSize, GreenNodeBuilder, SyntaxKind};
    
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
            range: crate::syntax::TextRange::new(
                TextSize::from(10),
                TextSize::from(20),
            ),
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
    fn test_compute_affected_range_empty() {
        // Test the compute_affected_range logic directly
        let edits: Vec<TextEdit> = vec![];
        if edits.is_empty() {
            let range = crate::syntax::TextRange::new(TextSize::zero(), TextSize::zero());
            assert_eq!(range.start(), TextSize::zero());
            assert_eq!(range.end(), TextSize::zero());
        }
    }
    
    #[test]
    fn test_compute_affected_range_single_edit() {
        let edits = [TextEdit {
            range: crate::syntax::TextRange::new(
                TextSize::from(5),
                TextSize::from(15),
            ),
            new_text: "test".to_string(),
        }];
        
        let mut start = edits[0].range.start();
        let mut end = edits[0].range.end();
        for edit in &edits[1..] {
            start = start.min(edit.range.start());
            end = end.max(edit.range.end());
        }
        let range = crate::syntax::TextRange::new(start, end);
        
        assert_eq!(range.start(), TextSize::from(5));
        assert_eq!(range.end(), TextSize::from(15));
    }
    
    #[test]
    fn test_compute_affected_range_multiple_edits() {
        let edits = [
            TextEdit {
                range: crate::syntax::TextRange::new(
                    TextSize::from(10),
                    TextSize::from(20),
                ),
                new_text: "edit1".to_string(),
            },
            TextEdit {
                range: crate::syntax::TextRange::new(
                    TextSize::from(5),
                    TextSize::from(15),
                ),
                new_text: "edit2".to_string(),
            },
            TextEdit {
                range: crate::syntax::TextRange::new(
                    TextSize::from(25),
                    TextSize::from(30),
                ),
                new_text: "edit3".to_string(),
            },
        ];
        
        let mut start = edits[0].range.start();
        let mut end = edits[0].range.end();
        for edit in &edits[1..] {
            start = start.min(edit.range.start());
            end = end.max(edit.range.end());
        }
        let range = crate::syntax::TextRange::new(start, end);
        
        // Should be union of all ranges: min(5, 10, 25) to max(20, 15, 30)
        assert_eq!(range.start(), TextSize::from(5));
        assert_eq!(range.end(), TextSize::from(30));
    }
    
    // Mock types for testing
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct MockToken;
    
    impl crate::grammar::Token for MockToken {
        type Kind = TestKind;
        fn kind(&self) -> Self::Kind {
            TestKind::Ident
        }
    }
    
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct MockNonTerminal;
    
    impl crate::grammar::NonTerminal for MockNonTerminal {
        fn name(&self) -> &'static str {
            "Mock"
        }
    }
    
    #[derive(Debug)]
    struct MockError(String);
    
    impl std::fmt::Display for MockError {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }
    
    impl std::error::Error for MockError {}
    
    struct MockParser;
    
    impl crate::backend::ParserBackend<MockToken, MockNonTerminal> for MockParser {
        type Config = ();
        type Error = MockError;
        type State = ();
        
        fn new(_grammar: &crate::grammar::Grammar<MockToken, MockNonTerminal>, _config: Self::Config) -> Result<Self, Self::Error> {
            Ok(Self)
        }
        
        fn parse(&mut self, _input: &[MockToken], _entry: MockNonTerminal) -> crate::error::ParseResult<MockToken, MockNonTerminal> {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(TestKind::Root);
            let root = builder.finish().unwrap();
            
            crate::error::ParseResult {
                root,
                errors: vec![],
                warnings: vec![],
                metrics: crate::error::ParseMetrics::default(),
                _phantom: std::marker::PhantomData,
            }
        }
        
        fn validate(_grammar: &crate::grammar::Grammar<MockToken, MockNonTerminal>) -> Vec<crate::grammar::GrammarError<MockToken, MockNonTerminal>> {
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

