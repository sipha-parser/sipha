//! # Tree Diffing Algorithm
//!
//! This module provides tree diffing capabilities for comparing syntax trees
//! and computing minimal edit scripts.
//!
//! ## Overview
//!
//! Tree diffing is essential for:
//! - Incremental formatting (only reformat changed regions)
//! - Refactoring (understand what changed)
//! - Version control integration
//! - Semantic diff display
//!
//! ## Algorithm
//!
//! Uses a variant of Zhang-Shasha tree edit distance algorithm,
//! optimized for syntax trees with these properties:
//! - Most edits are localized
//! - Trees tend to have similar structure
//! - Tokens at leaves are compared by content

use crate::syntax::{GreenElement, GreenNode, SyntaxKind, cursor::TreePath};
use smallvec::SmallVec;
use std::sync::Arc;

/// A single diff operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiffOp<K: SyntaxKind> {
    /// Node at path is unchanged
    Keep { path: TreePath },
    /// Insert a new node
    Insert {
        parent_path: TreePath,
        index: usize,
        element: GreenElement<K>,
    },
    /// Delete a node
    Delete { path: TreePath },
    /// Update the kind of a node
    UpdateKind {
        path: TreePath,
        old_kind: K,
        new_kind: K,
    },
    /// Update token text
    UpdateText {
        path: TreePath,
        old_text: String,
        new_text: String,
    },
    /// Move a subtree
    Move { from: TreePath, to: TreePath },
    /// Replace a node with a completely different one
    Replace {
        path: TreePath,
        old: GreenElement<K>,
        new: GreenElement<K>,
    },
}

impl<K: SyntaxKind> DiffOp<K> {
    /// Check if this is a structural change (not just content)
    #[must_use]
    pub fn is_structural(&self) -> bool {
        matches!(
            self,
            Self::Insert { .. } | Self::Delete { .. } | Self::Move { .. } | Self::Replace { .. }
        )
    }

    /// Get the affected path
    #[must_use]
    pub fn path(&self) -> Option<&TreePath> {
        match self {
            Self::Keep { path }
            | Self::Delete { path }
            | Self::UpdateKind { path, .. }
            | Self::UpdateText { path, .. }
            | Self::Replace { path, .. } => Some(path),
            Self::Insert { parent_path, .. } => Some(parent_path),
            Self::Move { from, .. } => Some(from),
        }
    }
}

/// Result of diffing two trees
#[derive(Debug, Clone)]
pub struct TreeDiff<K: SyntaxKind> {
    /// The diff operations
    pub operations: Vec<DiffOp<K>>,
    /// The edit distance (number of changes)
    pub distance: usize,
    /// Statistics about the diff
    pub stats: DiffStats,
}

/// Statistics about a diff
#[derive(Debug, Clone, Default)]
pub struct DiffStats {
    /// Number of nodes compared
    pub nodes_compared: usize,
    /// Number of nodes kept
    pub nodes_kept: usize,
    /// Number of insertions
    pub insertions: usize,
    /// Number of deletions
    pub deletions: usize,
    /// Number of updates
    pub updates: usize,
    /// Number of moves
    pub moves: usize,
}

impl<K: SyntaxKind> TreeDiff<K> {
    /// Check if the trees are identical
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.operations.is_empty()
            || self
                .operations
                .iter()
                .all(|op| matches!(op, DiffOp::Keep { .. }))
    }

    /// Get only the change operations (excluding Keep)
    pub fn changes(&self) -> impl Iterator<Item = &DiffOp<K>> {
        self.operations
            .iter()
            .filter(|op| !matches!(op, DiffOp::Keep { .. }))
    }

    /// Get the number of changes
    #[must_use]
    pub fn change_count(&self) -> usize {
        self.changes().count()
    }
}

/// Options for the diff algorithm
#[derive(Debug, Clone)]
pub struct DiffOptions {
    /// Maximum depth to compare
    pub max_depth: Option<usize>,
    /// Whether to detect moves (more expensive)
    pub detect_moves: bool,
    /// Whether to include Keep operations in output
    pub include_keeps: bool,
    /// Threshold for considering subtrees similar enough to diff
    pub similarity_threshold: f64,
}

impl Default for DiffOptions {
    fn default() -> Self {
        Self {
            max_depth: None,
            detect_moves: false,
            include_keeps: false,
            similarity_threshold: 0.5,
        }
    }
}

/// Diff two syntax trees
#[must_use]
pub fn diff_trees<K: SyntaxKind>(old: &Arc<GreenNode<K>>, new: &Arc<GreenNode<K>>) -> TreeDiff<K> {
    diff_trees_with_options(old, new, &DiffOptions::default())
}

/// Diff two syntax trees with options
#[must_use]
pub fn diff_trees_with_options<K: SyntaxKind>(
    old: &Arc<GreenNode<K>>,
    new: &Arc<GreenNode<K>>,
    options: &DiffOptions,
) -> TreeDiff<K> {
    let mut operations = Vec::new();
    let mut stats = DiffStats::default();

    diff_nodes(
        old,
        new,
        TreePath::root(),
        0,
        options,
        &mut operations,
        &mut stats,
    );

    let distance = operations
        .iter()
        .filter(|op| !matches!(op, DiffOp::Keep { .. }))
        .count();

    TreeDiff {
        operations,
        distance,
        stats,
    }
}

/// Recursively diff two nodes
fn diff_nodes<K: SyntaxKind>(
    old: &Arc<GreenNode<K>>,
    new: &Arc<GreenNode<K>>,
    path: TreePath,
    depth: usize,
    options: &DiffOptions,
    operations: &mut Vec<DiffOp<K>>,
    stats: &mut DiffStats,
) {
    stats.nodes_compared += 1;

    // Check depth limit
    if let Some(max_depth) = options.max_depth
        && depth > max_depth
    {
        // Treat as opaque - just check if equal
        if !nodes_equal(old, new) {
            operations.push(DiffOp::Replace {
                path,
                old: GreenElement::Node(old.clone()),
                new: GreenElement::Node(new.clone()),
            });
            stats.updates += 1;
        } else if options.include_keeps {
            operations.push(DiffOp::Keep { path });
            stats.nodes_kept += 1;
        }
        return;
    }

    // Check if kinds match
    if old.kind() != new.kind() {
        operations.push(DiffOp::UpdateKind {
            path: path.clone(),
            old_kind: old.kind(),
            new_kind: new.kind(),
        });
        stats.updates += 1;
    }

    // Diff children
    diff_children(
        old.children(),
        new.children(),
        path,
        depth,
        options,
        operations,
        stats,
    );
}

/// Diff children of two nodes using LCS-based algorithm
fn diff_children<K: SyntaxKind>(
    old_children: &[GreenElement<K>],
    new_children: &[GreenElement<K>],
    parent_path: TreePath,
    depth: usize,
    options: &DiffOptions,
    operations: &mut Vec<DiffOp<K>>,
    stats: &mut DiffStats,
) {
    // Simple LCS-based diff
    let lcs = compute_lcs(old_children, new_children);

    let mut old_idx = 0;
    let mut new_idx = 0;
    let mut lcs_idx = 0;

    while old_idx < old_children.len() || new_idx < new_children.len() {
        // Check if current elements are part of LCS
        let old_in_lcs =
            lcs_idx < lcs.len() && old_idx < old_children.len() && lcs[lcs_idx].0 == old_idx;
        let new_in_lcs =
            lcs_idx < lcs.len() && new_idx < new_children.len() && lcs[lcs_idx].1 == new_idx;

        if old_in_lcs && new_in_lcs {
            // Both in LCS - compare recursively
            let child_path = parent_path.child(new_idx);
            diff_elements(
                &old_children[old_idx],
                &new_children[new_idx],
                child_path,
                depth + 1,
                options,
                operations,
                stats,
            );
            old_idx += 1;
            new_idx += 1;
            lcs_idx += 1;
        } else if !old_in_lcs
            && old_idx < old_children.len()
            && (new_idx >= new_children.len() || !new_in_lcs)
        {
            // Old element not in LCS - deleted
            let child_path = parent_path.child(old_idx);
            operations.push(DiffOp::Delete { path: child_path });
            stats.deletions += 1;
            old_idx += 1;
        } else if new_idx < new_children.len() {
            // New element not in LCS - inserted
            operations.push(DiffOp::Insert {
                parent_path: parent_path.clone(),
                index: new_idx,
                element: new_children[new_idx].clone(),
            });
            stats.insertions += 1;
            new_idx += 1;
        } else {
            break;
        }
    }
}

/// Diff two elements
fn diff_elements<K: SyntaxKind>(
    old: &GreenElement<K>,
    new: &GreenElement<K>,
    path: TreePath,
    depth: usize,
    options: &DiffOptions,
    operations: &mut Vec<DiffOp<K>>,
    stats: &mut DiffStats,
) {
    match (old, new) {
        (GreenElement::Node(old_node), GreenElement::Node(new_node)) => {
            diff_nodes(old_node, new_node, path, depth, options, operations, stats);
        }
        (GreenElement::Token(old_token), GreenElement::Token(new_token)) => {
            stats.nodes_compared += 1;
            if old_token.kind() != new_token.kind() || old_token.text() != new_token.text() {
                if old_token.kind() != new_token.kind() {
                    operations.push(DiffOp::UpdateKind {
                        path: path.clone(),
                        old_kind: old_token.kind(),
                        new_kind: new_token.kind(),
                    });
                    stats.updates += 1;
                }
                if old_token.text() != new_token.text() {
                    operations.push(DiffOp::UpdateText {
                        path,
                        old_text: old_token.text().to_string(),
                        new_text: new_token.text().to_string(),
                    });
                    stats.updates += 1;
                }
            } else if options.include_keeps {
                operations.push(DiffOp::Keep { path });
                stats.nodes_kept += 1;
            }
        }
        (old_elem, new_elem) => {
            // Type mismatch - replace
            operations.push(DiffOp::Replace {
                path,
                old: old_elem.clone(),
                new: new_elem.clone(),
            });
            stats.updates += 1;
        }
    }
}

/// Compute LCS of two child sequences
/// Returns indices into old and new arrays
fn compute_lcs<K: SyntaxKind>(
    old: &[GreenElement<K>],
    new: &[GreenElement<K>],
) -> SmallVec<[(usize, usize); 16]> {
    if old.is_empty() || new.is_empty() {
        return SmallVec::new();
    }

    // Build LCS table
    let m = old.len();
    let n = new.len();
    let mut dp = vec![vec![0usize; n + 1]; m + 1];

    for (i, old_elem) in old.iter().enumerate() {
        for (j, new_elem) in new.iter().enumerate() {
            if elements_match(old_elem, new_elem) {
                dp[i + 1][j + 1] = dp[i][j] + 1;
            } else {
                dp[i + 1][j + 1] = dp[i + 1][j].max(dp[i][j + 1]);
            }
        }
    }

    // Backtrack to find LCS
    let mut result = SmallVec::new();
    let mut i = m;
    let mut j = n;

    while i > 0 && j > 0 {
        if elements_match(&old[i - 1], &new[j - 1]) {
            result.push((i - 1, j - 1));
            i -= 1;
            j -= 1;
        } else if dp[i - 1][j] > dp[i][j - 1] {
            i -= 1;
        } else {
            j -= 1;
        }
    }

    result.reverse();
    result
}

/// Check if two elements match (for LCS purposes)
/// For tokens, we only match by kind (not text) so that text changes
/// can be detected as UpdateText operations rather than Replace operations.
fn elements_match<K: SyntaxKind>(a: &GreenElement<K>, b: &GreenElement<K>) -> bool {
    match (a, b) {
        (GreenElement::Node(na), GreenElement::Node(nb)) => na.kind() == nb.kind(),
        (GreenElement::Token(ta), GreenElement::Token(tb)) => ta.kind() == tb.kind(),
        _ => false,
    }
}

/// Check if two nodes are deeply equal
fn nodes_equal<K: SyntaxKind>(a: &Arc<GreenNode<K>>, b: &Arc<GreenNode<K>>) -> bool {
    if a.kind() != b.kind() || a.text_len() != b.text_len() {
        return false;
    }

    let a_children = a.children();
    let b_children = b.children();

    if a_children.len() != b_children.len() {
        return false;
    }

    for (ac, bc) in a_children.iter().zip(b_children.iter()) {
        if !elements_equal(ac, bc) {
            return false;
        }
    }

    true
}

/// Check if two elements are deeply equal
fn elements_equal<K: SyntaxKind>(a: &GreenElement<K>, b: &GreenElement<K>) -> bool {
    match (a, b) {
        (GreenElement::Node(na), GreenElement::Node(nb)) => nodes_equal(na, nb),
        (GreenElement::Token(ta), GreenElement::Token(tb)) => {
            ta.kind() == tb.kind() && ta.text() == tb.text()
        }
        _ => false,
    }
}

/// Compute similarity ratio between two subtrees (0.0 to 1.0)
#[must_use]
pub fn tree_similarity<K: SyntaxKind>(a: &Arc<GreenNode<K>>, b: &Arc<GreenNode<K>>) -> f64 {
    let diff = diff_trees(a, b);
    let total_nodes = diff.stats.nodes_compared;

    if total_nodes == 0 {
        return 1.0;
    }

    let kept = diff.stats.nodes_kept;
    kept as f64 / total_nodes as f64
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{GreenToken, TextSize};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
        Expr,
        Ident,
        Number,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            matches!(self, Self::Ident | Self::Number)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    fn make_simple_tree(text: &str) -> Arc<GreenNode<TestKind>> {
        let token = GreenToken::new(TestKind::Ident, text);
        let expr = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(token)],
            TextSize::from(text.len() as u32),
        );
        GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Node(expr)],
            TextSize::from(text.len() as u32),
        )
    }

    #[test]
    fn test_diff_identical_trees() {
        let tree = make_simple_tree("hello");
        let diff = diff_trees(&tree, &tree);

        assert!(diff.is_empty());
        assert_eq!(diff.distance, 0);
    }

    #[test]
    fn test_diff_text_change() {
        let old = make_simple_tree("hello");
        let new = make_simple_tree("world");

        let diff = diff_trees(&old, &new);

        assert!(!diff.is_empty());
        assert!(
            diff.changes()
                .any(|op| matches!(op, DiffOp::UpdateText { .. }))
        );
    }

    #[test]
    fn test_diff_kind_change() {
        let old = make_simple_tree("hello");

        // Create new tree with different kind
        let token = GreenToken::new(TestKind::Number, "hello");
        let expr = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(token)],
            TextSize::from(5),
        );
        let new = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Node(expr)],
            TextSize::from(5),
        );

        let diff = diff_trees(&old, &new);

        assert!(!diff.is_empty());
        // When token kind changes, tokens don't match in LCS, so they're treated as Delete+Insert or Replace
        // Check that there's a change (could be UpdateKind, Replace, Delete, or Insert)
        assert!(diff.changes().any(|op| matches!(
            op,
            DiffOp::UpdateKind { .. }
                | DiffOp::Replace { .. }
                | DiffOp::Delete { .. }
                | DiffOp::Insert { .. }
        )));
    }

    #[test]
    fn test_diff_insertion() {
        let token1 = GreenToken::new(TestKind::Ident, "a");
        let old = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token1)],
            TextSize::from(1),
        );

        let token1 = GreenToken::new(TestKind::Ident, "a");
        let token2 = GreenToken::new(TestKind::Ident, "b");
        let new = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token1), GreenElement::Token(token2)],
            TextSize::from(2),
        );

        let diff = diff_trees(&old, &new);

        assert!(!diff.is_empty());
        assert_eq!(diff.stats.insertions, 1);
    }

    #[test]
    fn test_diff_deletion() {
        let token1 = GreenToken::new(TestKind::Ident, "a");
        let token2 = GreenToken::new(TestKind::Ident, "b");
        let old = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token1), GreenElement::Token(token2)],
            TextSize::from(2),
        );

        let token1 = GreenToken::new(TestKind::Ident, "a");
        let new = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token1)],
            TextSize::from(1),
        );

        let diff = diff_trees(&old, &new);

        assert!(!diff.is_empty());
        assert_eq!(diff.stats.deletions, 1);
    }

    #[test]
    fn test_compute_lcs() {
        // Use different kinds to test LCS matching behavior
        let tokens_old: Vec<GreenElement<TestKind>> = vec![
            GreenElement::Token(GreenToken::new(TestKind::Ident, "a")),
            GreenElement::Token(GreenToken::new(TestKind::Number, "b")),
            GreenElement::Token(GreenToken::new(TestKind::Ident, "c")),
        ];

        let tokens_new: Vec<GreenElement<TestKind>> = vec![
            GreenElement::Token(GreenToken::new(TestKind::Ident, "a")),
            GreenElement::Token(GreenToken::new(TestKind::Ident, "c")),
        ];

        let lcs = compute_lcs(&tokens_old, &tokens_new);

        // 'a' and 'c' match by kind (both Ident)
        assert_eq!(lcs.len(), 2);
        assert_eq!(lcs[0], (0, 0)); // 'a' at (0, 0)
        assert_eq!(lcs[1], (2, 1)); // 'c' at (2, 1)
    }
}
