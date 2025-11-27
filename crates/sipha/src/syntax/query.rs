#[cfg(feature = "query")]
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode};

/// Type alias for custom predicate function
#[cfg(feature = "query")]
#[allow(clippy::type_complexity)]
type NodePredicateFn<K> = Box<dyn Fn(&SyntaxNode<K>) -> bool>;

/// Predicate for matching nodes - avoids boxing for common cases
#[cfg(feature = "query")]
enum NodePredicate<K: SyntaxKind> {
    /// Match by exact kind (most common case, no allocation)
    Kind(K),
    /// Custom predicate function (requires boxing)
    Custom(NodePredicateFn<K>),
}

/// Query builder for finding nodes in syntax trees
#[cfg(feature = "query")]
pub struct QueryBuilder<K: SyntaxKind> {
    predicates: Vec<NodePredicate<K>>,
    max_results: Option<usize>,
}

#[cfg(feature = "query")]
impl<K: SyntaxKind> QueryBuilder<K> {
    /// Create a new query builder
    #[must_use]
    pub const fn new() -> Self {
        Self {
            predicates: Vec::new(),
            max_results: None,
        }
    }

    /// Filter by syntax kind.
    ///
    /// This is optimized to avoid boxing - the most common use case.
    #[must_use]
    pub fn with_kind(mut self, kind: K) -> Self {
        self.predicates.push(NodePredicate::Kind(kind));
        self
    }

    /// Filter by predicate function.
    ///
    /// Note: This requires boxing the function. For better performance,
    /// use `with_kind()` when possible, or consider using `find_matching()`
    /// directly on `SyntaxNode` which may avoid intermediate allocations.
    #[must_use]
    pub fn with_predicate<F>(mut self, predicate: F) -> Self
    where
        F: Fn(&SyntaxNode<K>) -> bool + 'static,
    {
        self.predicates
            .push(NodePredicate::Custom(Box::new(predicate)));
        self
    }

    /// Limit the number of results
    #[must_use]
    pub const fn limit(mut self, max: usize) -> Self {
        self.max_results = Some(max);
        self
    }

    /// Execute the query starting from the given node
    #[must_use]
    pub fn find(&self, root: &SyntaxNode<K>) -> Vec<SyntaxNode<K>> {
        let mut results = Vec::new();
        let max = self.max_results.unwrap_or(usize::MAX);

        for node in root.descendants() {
            if results.len() >= max {
                break;
            }

            if self.predicates.iter().all(|p| match p {
                NodePredicate::Kind(k) => node.kind() == *k,
                NodePredicate::Custom(f) => f(&node),
            }) {
                results.push(node);
            }
        }

        results
    }

    /// Find the first matching node
    #[must_use]
    pub fn find_first(&self, root: &SyntaxNode<K>) -> Option<SyntaxNode<K>> {
        root.descendants().find(|node| {
            self.predicates.iter().all(|p| match p {
                NodePredicate::Kind(k) => node.kind() == *k,
                NodePredicate::Custom(f) => f(node),
            })
        })
    }
}

#[cfg(feature = "query")]
impl<K: SyntaxKind> Default for QueryBuilder<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "query")]
impl<K: SyntaxKind> SyntaxNode<K> {
    /// Find all nodes matching the given kind
    #[must_use]
    pub fn find_by_kind(&self, kind: K) -> Vec<Self> {
        QueryBuilder::new().with_kind(kind).find(self)
    }

    /// Find the first node matching the given kind
    #[must_use]
    pub fn find_first_by_kind(&self, kind: K) -> Option<Self> {
        QueryBuilder::new().with_kind(kind).find_first(self)
    }

    /// Find all nodes matching the predicate
    #[must_use]
    pub fn find_matching<F>(&self, predicate: F) -> Vec<Self>
    where
        F: Fn(&Self) -> bool + 'static,
    {
        QueryBuilder::new().with_predicate(predicate).find(self)
    }

    /// Find the first node matching the predicate
    #[must_use]
    pub fn find_first_matching<F>(&self, predicate: F) -> Option<Self>
    where
        F: Fn(&Self) -> bool + 'static,
    {
        QueryBuilder::new()
            .with_predicate(predicate)
            .find_first(self)
    }

    /// Find nodes by path (sequence of kinds)
    pub fn find_by_path(&self, path: &[K]) -> Vec<Self>
    where
        K: Clone,
    {
        if path.is_empty() {
            return vec![self.clone()];
        }

        let mut results = Vec::new();
        let first_kind = &path[0];
        let rest = &path[1..];

        for node in self.descendants() {
            if node.kind() == *first_kind {
                if rest.is_empty() {
                    results.push(node);
                } else {
                    // Recursively search in children
                    let child_results = node.find_by_path(rest);
                    results.extend(child_results);
                }
            }
        }

        results
    }

    /// Create a query builder for complex queries
    #[must_use]
    pub const fn query(&self) -> QueryBuilder<K> {
        QueryBuilder::new()
    }
}

/// Simple XPath-style query parser (basic implementation)
#[cfg(feature = "query")]
pub struct XPathQuery<K: SyntaxKind> {
    segments: Vec<QuerySegment<K>>,
}

#[cfg(feature = "query")]
enum QuerySegment<K: SyntaxKind> {
    /// Direct child: /Kind
    Child(K),
    /// Descendant: //Kind
    Descendant(K),
    /// Any child: /*
    AnyChild,
    /// Any descendant: //*
    AnyDescendant,
}

#[cfg(feature = "query")]
impl<K: SyntaxKind> XPathQuery<K> {
    /// Parse a simple XPath-style query
    /// Supports: /Kind, //Kind, /*, //*
    ///
    /// # Panics
    ///
    /// May panic if the query string contains invalid UTF-8 sequences.
    pub fn parse(query: &str, kind_parser: impl Fn(&str) -> Option<K>) -> Option<Self> {
        let mut segments = Vec::new();
        let mut chars = query.chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                '/' => {
                    if chars.peek() == Some(&'/') {
                        chars.next(); // consume second '/'
                        if matches!(chars.peek(), Some('*')) {
                            chars.next();
                            segments.push(QuerySegment::AnyDescendant);
                        } else {
                            // Parse kind name
                            let mut name = String::new();
                            while let Some(&c) = chars.peek() {
                                if c == '/' || c.is_whitespace() {
                                    break;
                                }
                                name.push(chars.next().unwrap());
                            }
                            if let Some(kind) = kind_parser(&name) {
                                segments.push(QuerySegment::Descendant(kind));
                            } else {
                                return None;
                            }
                        }
                    } else if matches!(chars.peek(), Some('*')) {
                        chars.next();
                        segments.push(QuerySegment::AnyChild);
                    } else {
                        // Parse kind name
                        let mut name = String::new();
                        while let Some(&c) = chars.peek() {
                            if c == '/' || c.is_whitespace() {
                                break;
                            }
                            name.push(chars.next().unwrap());
                        }
                        if let Some(kind) = kind_parser(&name) {
                            segments.push(QuerySegment::Child(kind));
                        } else {
                            return None;
                        }
                    }
                }
                _ if ch.is_whitespace() => {}
                _ => return None,
            }
        }

        Some(Self { segments })
    }

    /// Execute the query
    #[must_use]
    pub fn execute(&self, root: &SyntaxNode<K>) -> Vec<SyntaxNode<K>> {
        let mut current = vec![root.clone()];

        for segment in &self.segments {
            let mut next = Vec::new();

            match segment {
                QuerySegment::Child(kind) => {
                    for node in &current {
                        for child in node.children() {
                            if let SyntaxElement::Node(n) = child
                                && n.kind() == *kind
                            {
                                next.push(n);
                            }
                        }
                    }
                }
                QuerySegment::Descendant(kind) => {
                    for node in &current {
                        next.extend(node.find_by_kind(*kind));
                    }
                }
                QuerySegment::AnyChild => {
                    for node in &current {
                        for child in node.children() {
                            if let SyntaxElement::Node(n) = child {
                                next.push(n);
                            }
                        }
                    }
                }
                QuerySegment::AnyDescendant => {
                    for node in &current {
                        next.extend(node.descendants());
                    }
                }
            }

            current = next;
        }

        current
    }
}

#[cfg(all(test, feature = "query"))]
mod tests {
    use super::*;
    use crate::syntax::{GreenNodeBuilder, SyntaxKind};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
        Expr,
        Term,
        Ident,
        Number,
        Plus,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Root | Self::Expr | Self::Term)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    fn build_test_tree() -> SyntaxNode<TestKind> {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Ident, "x").unwrap();
        builder.token(TestKind::Plus, "+").unwrap();
        builder.token(TestKind::Number, "42").unwrap();
        builder.finish_node().unwrap();

        builder.start_node(TestKind::Term);
        builder.token(TestKind::Number, "100").unwrap();
        builder.finish_node().unwrap();

        let root = builder.finish().unwrap();
        SyntaxNode::new_root(root)
    }

    #[test]
    fn test_query_builder_new() {
        let builder = QueryBuilder::<TestKind>::new();
        assert_eq!(builder.predicates.len(), 0);
        assert_eq!(builder.max_results, None);
    }

    #[test]
    fn test_query_builder_with_kind() {
        let tree = build_test_tree();
        let results = QueryBuilder::new().with_kind(TestKind::Expr).find(&tree);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind(), TestKind::Expr);
    }

    #[test]
    fn test_query_builder_with_predicate() {
        let tree = build_test_tree();
        let results = QueryBuilder::new()
            .with_predicate(|node| matches!(node.kind(), TestKind::Expr | TestKind::Term))
            .find(&tree);

        // Should find both Expr and Term nodes (descendants of Root)
        assert_eq!(results.len(), 2);
        let kinds: Vec<_> = results.iter().map(SyntaxNode::kind).collect();
        assert!(kinds.contains(&TestKind::Expr));
        assert!(kinds.contains(&TestKind::Term));
    }

    #[test]
    fn test_query_builder_limit() {
        let tree = build_test_tree();
        let results = QueryBuilder::new()
            .with_kind(TestKind::Expr)
            .limit(1)
            .find(&tree);

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_query_builder_find_first() {
        let tree = build_test_tree();
        let result = QueryBuilder::new()
            .with_kind(TestKind::Expr)
            .find_first(&tree);

        assert!(result.is_some());
        assert_eq!(result.unwrap().kind(), TestKind::Expr);
    }

    #[test]
    fn test_query_builder_find_first_none() {
        let tree = build_test_tree();
        let result = QueryBuilder::new()
            .with_kind(TestKind::Root) // Root is not in descendants
            .find_first(&tree);

        assert!(result.is_none());
    }

    #[test]
    fn test_find_by_kind() {
        let tree = build_test_tree();
        // find_by_kind only finds nodes, not tokens
        let results = tree.find_by_kind(TestKind::Expr);

        assert_eq!(results.len(), 1); // One Expr node
        assert_eq!(results[0].kind(), TestKind::Expr);
    }

    #[test]
    fn test_find_first_by_kind() {
        let tree = build_test_tree();
        let result = tree.find_first_by_kind(TestKind::Expr);

        assert!(result.is_some());
        assert_eq!(result.unwrap().kind(), TestKind::Expr);
    }

    #[test]
    fn test_find_matching() {
        let tree = build_test_tree();
        let results =
            tree.find_matching(|node| matches!(node.kind(), TestKind::Expr | TestKind::Term));

        // Should find both Expr and Term nodes (descendants of Root)
        assert_eq!(results.len(), 2);
        let kinds: Vec<_> = results.iter().map(SyntaxNode::kind).collect();
        assert!(kinds.contains(&TestKind::Expr));
        assert!(kinds.contains(&TestKind::Term));
    }

    #[test]
    fn test_find_by_path() {
        let tree = build_test_tree();
        // find_by_path works with node kinds
        let results = tree.find_by_path(&[TestKind::Expr]);

        // Should find Expr node
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind(), TestKind::Expr);
    }

    #[test]
    fn test_find_by_path_empty() {
        let tree = build_test_tree();
        let results = tree.find_by_path(&[]);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind(), TestKind::Root);
    }

    #[test]
    fn test_xpath_query_parse_simple() {
        let query = XPathQuery::<TestKind>::parse("/Expr", |s| match s {
            "Expr" => Some(TestKind::Expr),
            "Term" => Some(TestKind::Term),
            "Root" => Some(TestKind::Root),
            _ => None,
        });

        assert!(query.is_some());
    }

    #[test]
    fn test_xpath_query_parse_descendant() {
        let query = XPathQuery::<TestKind>::parse("//Number", |s| match s {
            "Number" => Some(TestKind::Number),
            _ => None,
        });

        assert!(query.is_some());
    }

    #[test]
    fn test_xpath_query_parse_any_child() {
        let query = XPathQuery::<TestKind>::parse("/*", |_| None);
        assert!(query.is_some());
    }

    #[test]
    fn test_xpath_query_parse_any_descendant() {
        let query = XPathQuery::<TestKind>::parse("//*", |_| None);
        assert!(query.is_some());
    }

    #[test]
    fn test_xpath_query_parse_invalid() {
        let query = XPathQuery::<TestKind>::parse("invalid", |_| None);
        assert!(query.is_none());
    }

    #[test]
    fn test_xpath_query_execute() {
        let tree = build_test_tree();
        let query = XPathQuery::<TestKind>::parse("/Expr", |s| match s {
            "Expr" => Some(TestKind::Expr),
            _ => None,
        })
        .unwrap();

        let results = query.execute(&tree);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind(), TestKind::Expr);
    }

    #[test]
    fn test_xpath_query_execute_descendant() {
        let tree = build_test_tree();
        // XPath queries work with node kinds
        let query = XPathQuery::<TestKind>::parse("//Term", |s| match s {
            "Term" => Some(TestKind::Term),
            "Expr" => Some(TestKind::Expr),
            _ => None,
        })
        .unwrap();

        let results = query.execute(&tree);
        assert_eq!(results.len(), 1); // One Term node
        assert_eq!(results[0].kind(), TestKind::Term);
    }
}
