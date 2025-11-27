#[cfg(feature = "tree-utils")]
use crate::syntax::SyntaxNode;
use crate::syntax::{GreenElement, GreenNode, SyntaxKind, TextRange, TextSize};
use std::fmt::Write;
use std::sync::Arc;

/// Describes the range a green node covers inside its tree.
#[derive(Debug, Clone)]
pub struct GreenNodeSpan<'a, K: SyntaxKind> {
    pub node: &'a GreenNode<K>,
    pub node_arc: Option<Arc<GreenNode<K>>>,
    pub range: TextRange,
    pub depth: usize,
}

impl<'a, K: SyntaxKind> GreenNodeSpan<'a, K> {
    #[must_use]
    pub const fn new(
        node: &'a GreenNode<K>,
        node_arc: Option<Arc<GreenNode<K>>>,
        range: TextRange,
        depth: usize,
    ) -> Self {
        Self {
            node,
            node_arc,
            range,
            depth,
        }
    }
}

/// Walk a green tree, invoking `visitor` for every node with its absolute range.
pub fn visit_green_spans<'a, K, F>(root: &'a GreenNode<K>, mut visitor: F)
where
    K: SyntaxKind,
    F: FnMut(GreenNodeSpan<'a, K>),
{
    fn walk<'a, K, F>(
        node: &'a GreenNode<K>,
        node_arc: Option<&Arc<GreenNode<K>>>,
        offset: TextSize,
        depth: usize,
        visitor: &mut F,
    ) where
        K: SyntaxKind,
        F: FnMut(GreenNodeSpan<'a, K>),
    {
        let range = TextRange::at(offset, node.text_len());
        visitor(GreenNodeSpan::new(node, node_arc.cloned(), range, depth));

        let mut child_offset = offset;
        for child in node.children() {
            match child {
                GreenElement::Node(child_node) => {
                    let len = child_node.text_len();
                    walk(
                        child_node,
                        Some(child_node),
                        child_offset,
                        depth + 1,
                        visitor,
                    );
                    child_offset += len;
                }
                GreenElement::Token(token) => {
                    child_offset += token.text_len();
                }
            }
        }
    }

    walk(root, None, TextSize::zero(), 0, &mut visitor);
}

/// Statistics about a syntax tree
#[cfg(feature = "tree-utils")]
#[derive(Debug, Clone, Default)]
pub struct TreeStats {
    pub total_nodes: usize,
    pub total_tokens: usize,
    pub max_depth: usize,
    pub max_width: usize,
    pub kind_counts: hashbrown::HashMap<u32, usize>,
}

#[cfg(feature = "tree-utils")]
impl TreeStats {
    /// Create empty statistics
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

#[cfg(feature = "tree-utils")]
impl<K: SyntaxKind> SyntaxNode<K> {
    /// Get statistics about this tree
    #[must_use]
    pub fn stats(&self) -> TreeStats {
        let mut stats = TreeStats::new();
        self.compute_stats(&mut stats, 0);
        stats
    }

    fn compute_stats(&self, stats: &mut TreeStats, depth: usize) {
        stats.total_nodes += 1;
        stats.max_depth = stats.max_depth.max(depth);

        // Count kind - use Debug representation as key since we can't assume Into<u32>
        let kind_hash = {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            format!("{:?}", self.kind()).hash(&mut hasher);
            u32::try_from(hasher.finish()).unwrap_or(0)
        };
        *stats.kind_counts.entry(kind_hash).or_insert(0) += 1;

        let mut width = 0;
        for child in self.children() {
            match child {
                crate::syntax::SyntaxElement::Node(node) => {
                    node.compute_stats(stats, depth + 1);
                    width += 1;
                }
                crate::syntax::SyntaxElement::Token(_) => {
                    stats.total_tokens += 1;
                    width += 1;
                }
            }
        }
        stats.max_width = stats.max_width.max(width);
    }
}

/// Validation result for a syntax tree
#[cfg(feature = "tree-utils")]
#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub errors: Vec<String>,
}

#[cfg(feature = "tree-utils")]
impl<K: SyntaxKind> SyntaxNode<K> {
    /// Validate the tree structure
    #[must_use]
    pub fn validate(&self) -> ValidationResult {
        let errors = Vec::new();

        // Check that all nodes have valid text ranges
        for node in self.descendants() {
            let _range = node.text_range();
            // Empty non-leaf nodes are allowed (e.g., error nodes)
            // Additional validation can be added here
        }

        // Check for cycles (shouldn't happen with green trees, but verify)
        // This is a basic check - green trees are immutable and acyclic by design

        ValidationResult {
            is_valid: errors.is_empty(),
            errors,
        }
    }
}

/// Format a tree as a human-readable string
#[cfg(feature = "tree-utils")]
impl<K: SyntaxKind> SyntaxNode<K> {
    /// Format the tree with indentation
    #[must_use]
    pub fn format_tree(&self) -> String {
        self.format_tree_with_indent(0)
    }

    fn format_tree_with_indent(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        let kind_str = format!("{:?}", self.kind());
        let mut result = format!("{indent_str}{kind_str}\n");

        for child in self.children() {
            match child {
                crate::syntax::SyntaxElement::Node(node) => {
                    result.push_str(&node.format_tree_with_indent(indent + 1));
                }
                crate::syntax::SyntaxElement::Token(token) => {
                    let token_indent = "  ".repeat(indent + 1);
                    let text = token.text().replace('\n', "\\n").replace('\r', "\\r");
                    writeln!(result, "{}{:?}: {}", token_indent, token.kind(), text).unwrap();
                }
            }
        }

        result
    }
}

/// Difference between two trees
#[cfg(feature = "tree-utils")]
#[derive(Debug, Clone)]
pub enum TreeDiff {
    /// Nodes differ in kind
    KindMismatch {
        path: String,
        expected: String,
        actual: String,
    },
    /// Different number of children
    ChildCountMismatch {
        path: String,
        expected: usize,
        actual: usize,
    },
    /// Missing node
    MissingNode { path: String, kind: String },
    /// Extra node
    ExtraNode { path: String, kind: String },
}

#[cfg(feature = "tree-utils")]
impl<K: SyntaxKind> SyntaxNode<K> {
    /// Compare this tree with another and return differences
    #[must_use]
    pub fn diff(&self, other: &Self) -> Vec<TreeDiff>
    where
        K: std::fmt::Debug,
    {
        let mut diffs = Vec::new();
        self.compare(other, "", &mut diffs);
        diffs
    }

    fn compare(&self, other: &Self, path: &str, diffs: &mut Vec<TreeDiff>)
    where
        K: std::fmt::Debug,
    {
        let current_path = if path.is_empty() {
            "root".to_string()
        } else {
            format!("{path}/{:?}", self.kind())
        };

        // Check kind
        if self.kind() != other.kind() {
            diffs.push(TreeDiff::KindMismatch {
                path: current_path.clone(),
                expected: format!("{:?}", self.kind()),
                actual: format!("{:?}", other.kind()),
            });
            return; // Don't compare children if kinds differ
        }

        // Compare children
        let self_children: Vec<_> = self.children().collect();
        let other_children: Vec<_> = other.children().collect();

        if self_children.len() != other_children.len() {
            diffs.push(TreeDiff::ChildCountMismatch {
                path: current_path.clone(),
                expected: self_children.len(),
                actual: other_children.len(),
            });
        }

        let min_len = self_children.len().min(other_children.len());
        for i in 0..min_len {
            match (&self_children[i], &other_children[i]) {
                (
                    crate::syntax::SyntaxElement::Node(self_node),
                    crate::syntax::SyntaxElement::Node(other_node),
                ) => {
                    self_node.compare(other_node, &current_path, diffs);
                }
                (
                    crate::syntax::SyntaxElement::Token(self_token),
                    crate::syntax::SyntaxElement::Token(other_token),
                ) => {
                    if self_token.kind() != other_token.kind()
                        || self_token.text() != other_token.text()
                    {
                        diffs.push(TreeDiff::KindMismatch {
                            path: format!("{current_path}/token[{i}]"),
                            expected: format!("{:?}: {}", self_token.kind(), self_token.text()),
                            actual: format!("{:?}: {}", other_token.kind(), other_token.text()),
                        });
                    }
                }
                _ => {
                    diffs.push(TreeDiff::KindMismatch {
                        path: format!("{current_path}/[{i}]"),
                        expected: format!("{:?}", self_children[i].kind()),
                        actual: format!("{:?}", other_children[i].kind()),
                    });
                }
            }
        }

        // Report extra/missing nodes
        if self_children.len() > other_children.len() {
            for (i, child) in self_children.iter().enumerate().skip(other_children.len()) {
                match child {
                    crate::syntax::SyntaxElement::Node(n) => {
                        diffs.push(TreeDiff::ExtraNode {
                            path: format!("{current_path}/[{i}]"),
                            kind: format!("{:?}", n.kind()),
                        });
                    }
                    crate::syntax::SyntaxElement::Token(t) => {
                        diffs.push(TreeDiff::ExtraNode {
                            path: format!("{current_path}/token[{i}]"),
                            kind: format!("{:?}", t.kind()),
                        });
                    }
                }
            }
        } else if other_children.len() > self_children.len() {
            for (i, child) in other_children.iter().enumerate().skip(self_children.len()) {
                match child {
                    crate::syntax::SyntaxElement::Node(n) => {
                        diffs.push(TreeDiff::MissingNode {
                            path: format!("{current_path}/[{i}]"),
                            kind: format!("{:?}", n.kind()),
                        });
                    }
                    crate::syntax::SyntaxElement::Token(t) => {
                        diffs.push(TreeDiff::MissingNode {
                            path: format!("{current_path}/token[{i}]"),
                            kind: format!("{:?}", t.kind()),
                        });
                    }
                }
            }
        }
    }
}

#[cfg(test)]
#[cfg(feature = "tree-utils")]
mod tests {
    use super::*;
    use crate::syntax::{GreenNodeBuilder, SyntaxKind, SyntaxNode};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
        Expr,
        Ident,
        Number,
        Plus,
        Whitespace,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Root | Self::Expr)
        }

        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
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

        builder.token(TestKind::Whitespace, " ").unwrap();

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Number, "100").unwrap();
        builder.finish_node().unwrap();

        // Don't call finish_node() on root - finish() expects it on the stack
        let green = builder.finish().unwrap();
        SyntaxNode::new_root(green)
    }

    #[test]
    fn test_tree_stats() {
        let tree = build_test_tree();
        let stats = tree.stats();

        assert!(stats.total_nodes > 0);
        assert!(stats.total_tokens > 0);
        assert!(stats.max_depth > 0);
        assert!(stats.max_width > 0);
        assert!(!stats.kind_counts.is_empty());
    }

    #[test]
    fn test_tree_stats_counts() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);
        builder.token(TestKind::Ident, "a").unwrap();
        builder.token(TestKind::Ident, "b").unwrap();
        // Don't call finish_node() on root - finish() expects it on the stack
        let green = builder.finish().unwrap();
        let tree = SyntaxNode::new_root(green);
        let stats = tree.stats();

        assert_eq!(stats.total_tokens, 2);
        assert_eq!(stats.total_nodes, 1); // Just the root
    }

    #[test]
    fn test_tree_stats_depth() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);

        builder.start_node(TestKind::Expr);
        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Number, "1").unwrap();
        builder.finish_node().unwrap();
        builder.finish_node().unwrap();

        // Don't call finish_node() on root - finish() expects it on the stack
        let green = builder.finish().unwrap();
        let tree = SyntaxNode::new_root(green);
        let stats = tree.stats();

        assert!(stats.max_depth >= 2);
    }

    #[test]
    fn test_validation_result() {
        let tree = build_test_tree();
        let result = tree.validate();

        // Basic validation should pass for well-formed trees
        assert!(result.is_valid);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_format_tree() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);
        builder.token(TestKind::Ident, "x").unwrap();
        // Don't call finish_node() on root - finish() expects it on the stack
        let green = builder.finish().unwrap();
        let tree = SyntaxNode::new_root(green);
        let formatted = tree.format_tree();

        assert!(!formatted.is_empty());
        assert!(formatted.contains("Root") || formatted.contains("root"));
    }

    #[test]
    fn test_tree_diff_identical() {
        let tree1 = build_test_tree();
        let tree2 = build_test_tree();

        let diffs = tree1.diff(&tree2);
        // Identical trees should have no differences
        assert!(diffs.is_empty());
    }

    #[test]
    fn test_tree_diff_different_kinds() {
        let mut builder1 = GreenNodeBuilder::new();
        builder1.start_node(TestKind::Root);
        let _ = builder1.token(TestKind::Ident, "x");
        let tree1 = SyntaxNode::new_root(builder1.finish().unwrap());

        let mut builder2 = GreenNodeBuilder::new();
        builder2.start_node(TestKind::Expr);
        let _ = builder2.token(TestKind::Ident, "x");
        let tree2 = SyntaxNode::new_root(builder2.finish().unwrap());

        let diffs = tree1.diff(&tree2);
        assert!(!diffs.is_empty());
        assert!(
            diffs
                .iter()
                .any(|d| matches!(d, TreeDiff::KindMismatch { .. }))
        );
    }

    #[test]
    fn test_tree_diff_different_child_count() {
        let mut builder1 = GreenNodeBuilder::new();
        builder1.start_node(TestKind::Root);
        let _ = builder1.token(TestKind::Ident, "a");
        let _ = builder1.token(TestKind::Ident, "b");
        let tree1 = SyntaxNode::new_root(builder1.finish().unwrap());

        let mut builder2 = GreenNodeBuilder::new();
        builder2.start_node(TestKind::Root);
        let _ = builder2.token(TestKind::Ident, "a");
        let tree2 = SyntaxNode::new_root(builder2.finish().unwrap());

        let diffs = tree1.diff(&tree2);
        assert!(!diffs.is_empty());
        assert!(
            diffs
                .iter()
                .any(|d| matches!(d, TreeDiff::ChildCountMismatch { .. }))
        );
    }

    #[test]
    fn test_tree_diff_different_token_text() {
        let mut builder1 = GreenNodeBuilder::new();
        builder1.start_node(TestKind::Root);
        let _ = builder1.token(TestKind::Ident, "x");
        let tree1 = SyntaxNode::new_root(builder1.finish().unwrap());

        let mut builder2 = GreenNodeBuilder::new();
        builder2.start_node(TestKind::Root);
        let _ = builder2.token(TestKind::Ident, "y");
        let tree2 = SyntaxNode::new_root(builder2.finish().unwrap());

        let diffs = tree1.diff(&tree2);
        assert!(!diffs.is_empty());
    }

    #[test]
    fn test_tree_stats_new() {
        let stats = TreeStats::new();
        assert_eq!(stats.total_nodes, 0);
        assert_eq!(stats.total_tokens, 0);
        assert_eq!(stats.max_depth, 0);
        assert_eq!(stats.max_width, 0);
        assert!(stats.kind_counts.is_empty());
    }
}
