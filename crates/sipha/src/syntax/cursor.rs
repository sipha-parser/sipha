//! # Cursor API for Tree Navigation
//!
//! This module provides a cursor-based navigation API for syntax trees,
//! enabling efficient traversal without allocations.
//!
//! ## Overview
//!
//! The cursor API provides a stateful way to navigate syntax trees.
//! Unlike iterator-based traversal which creates new objects, cursors
//! mutate their internal state, making them allocation-free after creation.
//!
//! Key benefits:
//! - Zero allocation traversal
//! - O(1) parent/sibling/child navigation
//! - Efficient for depth-first and breadth-first traversals
//! - Compatible with typed AST access
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::syntax::cursor::TreeCursor;
//!
//! let cursor = TreeCursor::new(&root_node);
//!
//! // Navigate to first child
//! if cursor.goto_first_child() {
//!     println!("Child kind: {:?}", cursor.kind());
//!     
//!     // Navigate to siblings
//!     while cursor.goto_next_sibling() {
//!         println!("Sibling kind: {:?}", cursor.kind());
//!     }
//!     
//!     // Go back up
//!     cursor.goto_parent();
//! }
//! ```

use crate::syntax::{GreenElement, GreenNode, SyntaxKind, TextRange, TextSize};
use smallvec::SmallVec;
use std::sync::Arc;

/// A frame in the cursor's navigation stack
#[derive(Debug, Clone)]
struct CursorFrame<K: SyntaxKind> {
    /// The node at this level
    node: Arc<GreenNode<K>>,
    /// Current child index
    child_index: usize,
    /// Offset of this node from the root
    offset: TextSize,
}

/// A cursor for navigating a syntax tree
///
/// The cursor maintains a stack of frames representing the path
/// from the root to the current position.
#[derive(Debug, Clone)]
pub struct TreeCursor<K: SyntaxKind> {
    /// Navigation stack (root at index 0)
    stack: SmallVec<[CursorFrame<K>; 16]>,
}

impl<K: SyntaxKind> TreeCursor<K> {
    /// Create a new cursor at the root of a tree
    #[must_use]
    pub fn new(root: Arc<GreenNode<K>>) -> Self {
        Self {
            stack: smallvec::smallvec![CursorFrame {
                node: root,
                child_index: 0,
                offset: TextSize::zero(),
            }],
        }
    }

    /// Get the current node
    #[must_use]
    pub fn node(&self) -> &Arc<GreenNode<K>> {
        &self.current_frame().node
    }

    /// Get the kind of the current node
    #[must_use]
    pub fn kind(&self) -> K {
        self.current_frame().node.kind()
    }

    /// Get the text range of the current node
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        let frame = self.current_frame();
        TextRange::at(frame.offset, frame.node.text_len())
    }

    /// Get the start offset of the current node
    #[must_use]
    pub fn start_offset(&self) -> TextSize {
        self.current_frame().offset
    }

    /// Get the depth in the tree (0 = root)
    #[must_use]
    pub fn depth(&self) -> usize {
        self.stack.len() - 1
    }

    /// Check if we're at the root
    #[must_use]
    pub fn is_at_root(&self) -> bool {
        self.stack.len() == 1
    }

    /// Navigate to the first child
    ///
    /// Returns `true` if successful, `false` if there are no children.
    pub fn goto_first_child(&mut self) -> bool {
        let current = self.current_frame();
        let children = current.node.children();

        if children.is_empty() {
            return false;
        }

        // Find first node child (skip tokens if needed)
        for (i, child) in children.iter().enumerate() {
            if let GreenElement::Node(node) = child {
                let child_offset = self.compute_child_offset(i);
                self.stack.push(CursorFrame {
                    node: node.clone(),
                    child_index: i,
                    offset: child_offset,
                });
                return true;
            }
        }

        false
    }

    /// Navigate to the first child, including tokens
    ///
    /// Returns `true` if there are any children (node or token).
    /// Note: Tokens can only be accessed via `current_element()`.
    pub fn goto_first_child_including_tokens(&mut self) -> bool {
        let current = self.current_frame();
        let children = current.node.children();

        if children.is_empty() {
            return false;
        }

        if let GreenElement::Node(node) = &children[0] {
            let child_offset = self.compute_child_offset(0);
            self.stack.push(CursorFrame {
                node: node.clone(),
                child_index: 0,
                offset: child_offset,
            });
        }

        true
    }

    /// Navigate to the next sibling
    ///
    /// Returns `true` if successful, `false` if there are no more siblings.
    pub fn goto_next_sibling(&mut self) -> bool {
        if self.stack.len() < 2 {
            return false;
        }

        let current_index = self.current_frame().child_index;
        let parent = &self.stack[self.stack.len() - 2];
        let siblings = parent.node.children();

        // Find next node sibling
        for (i, sibling) in siblings.iter().enumerate().skip(current_index + 1) {
            if let GreenElement::Node(node) = sibling {
                let child_offset = self.compute_child_offset_from_parent(parent, i);
                self.stack.last_mut().unwrap().node = node.clone();
                self.stack.last_mut().unwrap().child_index = i;
                self.stack.last_mut().unwrap().offset = child_offset;
                return true;
            }
        }

        false
    }

    /// Navigate to the previous sibling
    ///
    /// Returns `true` if successful, `false` if there are no previous siblings.
    pub fn goto_prev_sibling(&mut self) -> bool {
        if self.stack.len() < 2 {
            return false;
        }

        let current_index = self.current_frame().child_index;
        if current_index == 0 {
            return false;
        }

        let parent = &self.stack[self.stack.len() - 2];
        let siblings = parent.node.children();

        // Find previous node sibling
        for i in (0..current_index).rev() {
            if let GreenElement::Node(node) = &siblings[i] {
                let child_offset = self.compute_child_offset_from_parent(parent, i);
                self.stack.last_mut().unwrap().node = node.clone();
                self.stack.last_mut().unwrap().child_index = i;
                self.stack.last_mut().unwrap().offset = child_offset;
                return true;
            }
        }

        false
    }

    /// Navigate to the parent
    ///
    /// Returns `true` if successful, `false` if already at root.
    pub fn goto_parent(&mut self) -> bool {
        if self.stack.len() > 1 {
            self.stack.pop();
            true
        } else {
            false
        }
    }

    /// Navigate to a specific child by index
    ///
    /// Returns `true` if successful, `false` if index out of bounds or not a node.
    pub fn goto_child(&mut self, index: usize) -> bool {
        let current = self.current_frame();
        let children = current.node.children();

        if index >= children.len() {
            return false;
        }

        if let GreenElement::Node(node) = &children[index] {
            let child_offset = self.compute_child_offset(index);
            self.stack.push(CursorFrame {
                node: node.clone(),
                child_index: index,
                offset: child_offset,
            });
            true
        } else {
            false
        }
    }

    /// Navigate to the first child containing a byte offset
    ///
    /// Returns `true` if a child containing the offset was found.
    pub fn goto_first_child_for_byte(&mut self, byte: u32) -> bool {
        let current = self.current_frame();
        let children = current.node.children();
        let base_offset = u32::from(current.offset);

        let mut child_start = base_offset;

        for (i, child) in children.iter().enumerate() {
            let child_len = u32::from(child.text_len());
            let child_end = child_start + child_len;

            if byte >= child_start
                && byte < child_end
                && let GreenElement::Node(node) = child
            {
                self.stack.push(CursorFrame {
                    node: node.clone(),
                    child_index: i,
                    offset: TextSize::from(child_start),
                });
                return true;
            }

            child_start = child_end;
        }

        false
    }

    /// Navigate to the last child
    ///
    /// Returns `true` if successful, `false` if there are no children.
    pub fn goto_last_child(&mut self) -> bool {
        let current = self.current_frame();
        let children = current.node.children();

        // Find last node child
        for i in (0..children.len()).rev() {
            if let GreenElement::Node(node) = &children[i] {
                let child_offset = self.compute_child_offset(i);
                self.stack.push(CursorFrame {
                    node: node.clone(),
                    child_index: i,
                    offset: child_offset,
                });
                return true;
            }
        }

        false
    }

    /// Reset to root
    pub fn reset(&mut self) {
        if self.stack.len() > 1 {
            let root = self.stack.remove(0);
            self.stack.clear();
            self.stack.push(root);
        }
    }

    /// Get the current element (node or token)
    ///
    /// Returns `None` if we're looking at a token (use parent to get context).
    #[must_use]
    pub fn current_element(&self) -> Option<&GreenElement<K>> {
        if self.stack.len() < 2 {
            return None;
        }

        let current_index = self.current_frame().child_index;
        let parent = &self.stack[self.stack.len() - 2];
        parent.node.children().get(current_index)
    }

    /// Iterate over all children of the current node
    pub fn children(&self) -> impl Iterator<Item = (usize, &GreenElement<K>)> {
        self.current_frame().node.children().iter().enumerate()
    }

    /// Find a child node with a specific kind
    #[must_use]
    pub fn find_child(&self, kind: K) -> Option<usize> {
        self.current_frame()
            .node
            .children()
            .iter()
            .position(|c| c.kind() == kind)
    }

    /// Get the child count of the current node
    #[must_use]
    pub fn child_count(&self) -> usize {
        self.current_frame().node.child_count()
    }

    /// Check if the current node is a leaf
    #[must_use]
    pub fn is_leaf(&self) -> bool {
        self.current_frame().node.is_leaf()
    }

    /// Get a path from root to current position
    #[must_use]
    pub fn path(&self) -> TreePath {
        TreePath {
            indices: self.stack.iter().skip(1).map(|f| f.child_index).collect(),
        }
    }

    /// Navigate to a path
    ///
    /// Returns `true` if the entire path was navigated successfully.
    pub fn goto_path(&mut self, path: &TreePath) -> bool {
        self.reset();

        for &index in &path.indices {
            if !self.goto_child(index) {
                return false;
            }
        }

        true
    }

    // Helper methods

    fn current_frame(&self) -> &CursorFrame<K> {
        self.stack
            .last()
            .expect("cursor stack should never be empty")
    }

    fn compute_child_offset(&self, child_index: usize) -> TextSize {
        let frame = self.current_frame();
        self.compute_child_offset_from_parent(frame, child_index)
    }

    #[allow(clippy::unused_self)]
    fn compute_child_offset_from_parent(
        &self,
        parent: &CursorFrame<K>,
        child_index: usize,
    ) -> TextSize {
        let mut offset = parent.offset;

        for child in parent.node.children().iter().take(child_index) {
            offset = TextSize::from(u32::from(offset) + u32::from(child.text_len()));
        }

        offset
    }
}

/// A path from root to a specific node in the tree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TreePath {
    /// Child indices at each level
    pub indices: SmallVec<[usize; 16]>,
}

impl TreePath {
    /// Create an empty path (pointing to root)
    #[must_use]
    pub fn root() -> Self {
        Self {
            indices: SmallVec::new(),
        }
    }

    /// Create a path from indices
    #[must_use]
    pub fn from_indices(indices: impl IntoIterator<Item = usize>) -> Self {
        Self {
            indices: indices.into_iter().collect(),
        }
    }

    /// Get the depth of this path
    #[must_use]
    pub fn depth(&self) -> usize {
        self.indices.len()
    }

    /// Check if this is the root path
    #[must_use]
    pub fn is_root(&self) -> bool {
        self.indices.is_empty()
    }

    /// Get the parent path
    #[must_use]
    pub fn parent(&self) -> Option<Self> {
        if self.indices.is_empty() {
            None
        } else {
            let mut indices = self.indices.clone();
            indices.pop();
            Some(Self { indices })
        }
    }

    /// Create a child path
    #[must_use]
    pub fn child(&self, index: usize) -> Self {
        let mut indices = self.indices.clone();
        indices.push(index);
        Self { indices }
    }

    /// Check if this path is an ancestor of another
    #[must_use]
    pub fn is_ancestor_of(&self, other: &Self) -> bool {
        if self.indices.len() >= other.indices.len() {
            return false;
        }

        self.indices
            .iter()
            .zip(other.indices.iter())
            .all(|(a, b)| a == b)
    }

    /// Check if this path is a descendant of another
    #[must_use]
    pub fn is_descendant_of(&self, other: &Self) -> bool {
        other.is_ancestor_of(self)
    }
}

impl Default for TreePath {
    fn default() -> Self {
        Self::root()
    }
}

/// Preorder iterator using a cursor
pub struct PreorderCursor<K: SyntaxKind> {
    cursor: TreeCursor<K>,
    done: bool,
}

impl<K: SyntaxKind> PreorderCursor<K> {
    /// Create a new preorder iterator
    #[must_use]
    pub fn new(root: Arc<GreenNode<K>>) -> Self {
        Self {
            cursor: TreeCursor::new(root),
            done: false,
        }
    }
}

impl<K: SyntaxKind> Iterator for PreorderCursor<K> {
    type Item = (Arc<GreenNode<K>>, TextRange);

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let result = (self.cursor.node().clone(), self.cursor.text_range());

        // Try to go to first child
        if self.cursor.goto_first_child() {
            return Some(result);
        }

        // Try to go to next sibling
        if self.cursor.goto_next_sibling() {
            return Some(result);
        }

        // Go up and try siblings
        while self.cursor.goto_parent() {
            if self.cursor.goto_next_sibling() {
                return Some(result);
            }
        }

        self.done = true;
        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{GreenElement, GreenToken};

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

    fn make_tree() -> Arc<GreenNode<TestKind>> {
        // Root
        //   ├── Expr
        //   │     └── Ident "x"
        //   └── Expr
        //         └── Number "42"

        let ident = GreenToken::new(TestKind::Ident, "x");
        let expr1 = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(ident)],
            TextSize::from(1),
        );

        let number = GreenToken::new(TestKind::Number, "42");
        let expr2 = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(number)],
            TextSize::from(2),
        );

        GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Node(expr1), GreenElement::Node(expr2)],
            TextSize::from(3),
        )
    }

    #[test]
    fn test_cursor_new() {
        let tree = make_tree();
        let cursor = TreeCursor::new(tree);

        assert_eq!(cursor.kind(), TestKind::Root);
        assert!(cursor.is_at_root());
        assert_eq!(cursor.depth(), 0);
    }

    #[test]
    fn test_cursor_navigation() {
        let tree = make_tree();
        let mut cursor = TreeCursor::new(tree);

        // Go to first child (Expr)
        assert!(cursor.goto_first_child());
        assert_eq!(cursor.kind(), TestKind::Expr);
        assert_eq!(cursor.depth(), 1);

        // Go to next sibling (second Expr)
        assert!(cursor.goto_next_sibling());
        assert_eq!(cursor.kind(), TestKind::Expr);

        // No more siblings
        assert!(!cursor.goto_next_sibling());

        // Go back to parent
        assert!(cursor.goto_parent());
        assert_eq!(cursor.kind(), TestKind::Root);
        assert!(cursor.is_at_root());
    }

    #[test]
    fn test_cursor_path() {
        let tree = make_tree();
        let mut cursor = TreeCursor::new(tree.clone());

        cursor.goto_first_child();
        let path1 = cursor.path();
        assert_eq!(path1.depth(), 1);

        cursor.goto_next_sibling();
        let path2 = cursor.path();
        assert_eq!(path2.depth(), 1);

        // Navigate using path
        cursor.reset();
        assert!(cursor.goto_path(&path1));
        assert_eq!(cursor.kind(), TestKind::Expr);
    }

    #[test]
    fn test_tree_path_relationships() {
        let root = TreePath::root();
        let child = root.child(0);
        let grandchild = child.child(1);

        assert!(root.is_ancestor_of(&child));
        assert!(root.is_ancestor_of(&grandchild));
        assert!(child.is_ancestor_of(&grandchild));

        assert!(child.is_descendant_of(&root));
        assert!(grandchild.is_descendant_of(&root));
        assert!(grandchild.is_descendant_of(&child));

        assert!(!child.is_ancestor_of(&root));
        assert!(!root.is_descendant_of(&child));
    }

    #[test]
    fn test_preorder_iteration() {
        let tree = make_tree();
        let iter = PreorderCursor::new(tree);

        let nodes: Vec<_> = iter.collect();
        assert_eq!(nodes.len(), 3); // Root, Expr, Expr

        assert_eq!(nodes[0].0.kind(), TestKind::Root);
        assert_eq!(nodes[1].0.kind(), TestKind::Expr);
        assert_eq!(nodes[2].0.kind(), TestKind::Expr);
    }
}
