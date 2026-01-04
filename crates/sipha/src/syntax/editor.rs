//! # Syntax Editor for Tree Mutations
//!
//! This module provides a mutable editing layer for syntax trees,
//! enabling refactoring operations while maintaining immutability.
//!
//! ## Overview
//!
//! Since green trees are immutable, we need a way to express mutations
//! that can be applied to produce a new tree. The `SyntaxEditor` collects
//! edits and applies them all at once, producing a new tree.
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::syntax::editor::{SyntaxEditor, TreeEdit};
//!
//! let mut editor = SyntaxEditor::new(root);
//!
//! // Queue edits
//! editor.replace(path, new_node);
//! editor.delete(another_path);
//! editor.insert_before(path, node);
//!
//! // Apply all edits to get new tree
//! let new_root = editor.apply();
//! ```

use crate::syntax::{GreenElement, GreenNode, GreenToken, SyntaxKind, TextSize, cursor::TreePath};
use std::sync::Arc;

/// An edit operation on a syntax tree
#[derive(Debug, Clone)]
pub enum TreeEdit<K: SyntaxKind> {
    /// Replace a node at the given path
    Replace {
        path: TreePath,
        node: Arc<GreenNode<K>>,
    },
    /// Replace with a token
    ReplaceWithToken {
        path: TreePath,
        token: GreenToken<K>,
    },
    /// Insert a node before the node at the given path
    InsertBefore {
        path: TreePath,
        element: GreenElement<K>,
    },
    /// Insert a node after the node at the given path
    InsertAfter {
        path: TreePath,
        element: GreenElement<K>,
    },
    /// Delete the node at the given path
    Delete { path: TreePath },
    /// Wrap the node at the given path in a new parent
    Wrap { path: TreePath, kind: K },
    /// Unwrap: replace a node with its children
    Unwrap { path: TreePath },
    /// Change the kind of a node
    ChangeKind { path: TreePath, new_kind: K },
}

/// A syntax tree editor for collecting and applying mutations
#[derive(Debug)]
pub struct SyntaxEditor<K: SyntaxKind> {
    root: Arc<GreenNode<K>>,
    edits: Vec<TreeEdit<K>>,
}

impl<K: SyntaxKind> SyntaxEditor<K> {
    /// Create a new syntax editor for the given root node
    #[must_use]
    pub fn new(root: Arc<GreenNode<K>>) -> Self {
        Self {
            root,
            edits: Vec::new(),
        }
    }

    /// Get the root node
    #[must_use]
    pub fn root(&self) -> &Arc<GreenNode<K>> {
        &self.root
    }

    /// Get pending edits
    #[must_use]
    pub fn edits(&self) -> &[TreeEdit<K>] {
        &self.edits
    }

    /// Check if there are pending edits
    #[must_use]
    pub fn has_edits(&self) -> bool {
        !self.edits.is_empty()
    }

    /// Replace a node at the given path
    pub fn replace(&mut self, path: TreePath, node: Arc<GreenNode<K>>) {
        self.edits.push(TreeEdit::Replace { path, node });
    }

    /// Replace with a token
    pub fn replace_with_token(&mut self, path: TreePath, token: GreenToken<K>) {
        self.edits.push(TreeEdit::ReplaceWithToken { path, token });
    }

    /// Insert an element before the node at the given path
    pub fn insert_before(&mut self, path: TreePath, element: GreenElement<K>) {
        self.edits.push(TreeEdit::InsertBefore { path, element });
    }

    /// Insert an element after the node at the given path
    pub fn insert_after(&mut self, path: TreePath, element: GreenElement<K>) {
        self.edits.push(TreeEdit::InsertAfter { path, element });
    }

    /// Delete the node at the given path
    pub fn delete(&mut self, path: TreePath) {
        self.edits.push(TreeEdit::Delete { path });
    }

    /// Wrap the node at the given path in a new parent of the given kind
    pub fn wrap(&mut self, path: TreePath, kind: K) {
        self.edits.push(TreeEdit::Wrap { path, kind });
    }

    /// Unwrap: replace a node with its children
    pub fn unwrap(&mut self, path: TreePath) {
        self.edits.push(TreeEdit::Unwrap { path });
    }

    /// Change the kind of a node
    pub fn change_kind(&mut self, path: TreePath, new_kind: K) {
        self.edits.push(TreeEdit::ChangeKind { path, new_kind });
    }

    /// Clear all pending edits
    pub fn clear(&mut self) {
        self.edits.clear();
    }

    /// Apply all pending edits and return the new root
    ///
    /// Edits are applied in order. Later edits may reference paths
    /// that were created or modified by earlier edits.
    #[must_use]
    pub fn apply(self) -> Arc<GreenNode<K>> {
        let mut root = self.root;

        // Sort edits by path depth (deepest first) to avoid invalidating paths
        let mut edits = self.edits;
        edits.sort_by(|a, b| {
            let depth_a = edit_path(a).map_or(0, super::cursor::TreePath::depth);
            let depth_b = edit_path(b).map_or(0, super::cursor::TreePath::depth);
            depth_b.cmp(&depth_a) // Deepest first
        });

        for edit in edits {
            root = apply_single_edit(root, edit);
        }

        root
    }
}

/// Get the path from an edit
fn edit_path<K: SyntaxKind>(edit: &TreeEdit<K>) -> Option<&TreePath> {
    match edit {
        TreeEdit::Replace { path, .. }
        | TreeEdit::ReplaceWithToken { path, .. }
        | TreeEdit::InsertBefore { path, .. }
        | TreeEdit::InsertAfter { path, .. }
        | TreeEdit::Delete { path }
        | TreeEdit::Wrap { path, .. }
        | TreeEdit::Unwrap { path }
        | TreeEdit::ChangeKind { path, .. } => Some(path),
    }
}

/// Apply a single edit to the tree
fn apply_single_edit<K: SyntaxKind>(
    root: Arc<GreenNode<K>>,
    edit: TreeEdit<K>,
) -> Arc<GreenNode<K>> {
    match edit {
        TreeEdit::Replace { path, node } => replace_at_path(&root, &path, GreenElement::Node(node)),
        TreeEdit::ReplaceWithToken { path, token } => {
            replace_at_path(&root, &path, GreenElement::Token(token))
        }
        TreeEdit::InsertBefore { path, element } => insert_at_path(&root, &path, element, false),
        TreeEdit::InsertAfter { path, element } => insert_at_path(&root, &path, element, true),
        TreeEdit::Delete { path } => delete_at_path(&root, &path),
        TreeEdit::Wrap { path, kind } => wrap_at_path(&root, &path, kind),
        TreeEdit::Unwrap { path } => unwrap_at_path(&root, &path),
        TreeEdit::ChangeKind { path, new_kind } => change_kind_at_path(&root, &path, new_kind),
    }
}

/// Replace element at a path
fn replace_at_path<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    path: &TreePath,
    replacement: GreenElement<K>,
) -> Arc<GreenNode<K>> {
    if path.is_root() {
        // Cannot replace root with an element, only with a node
        match replacement {
            GreenElement::Node(n) => return n,
            GreenElement::Token(_) => return node.clone(),
        }
    }

    let indices = &path.indices;
    replace_recursive(node, indices, 0, replacement)
}

fn replace_recursive<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    indices: &[usize],
    depth: usize,
    replacement: GreenElement<K>,
) -> Arc<GreenNode<K>> {
    if depth >= indices.len() {
        // This shouldn't happen if path is valid
        return node.clone();
    }

    let target_index = indices[depth];
    let children: Vec<_> = node.children().to_vec();

    if target_index >= children.len() {
        return node.clone();
    }

    let new_children: Vec<GreenElement<K>> = if depth == indices.len() - 1 {
        // This is the target level
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    replacement.clone()
                } else {
                    child
                }
            })
            .collect()
    } else {
        // Need to go deeper
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(child_node) = child {
                        GreenElement::Node(replace_recursive(
                            &child_node,
                            indices,
                            depth + 1,
                            replacement.clone(),
                        ))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    };

    let text_len = compute_text_len(&new_children);
    GreenNode::new(node.kind(), new_children, text_len)
}

/// Insert element at a path
fn insert_at_path<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    path: &TreePath,
    element: GreenElement<K>,
    after: bool,
) -> Arc<GreenNode<K>> {
    if path.is_root() {
        // Cannot insert before/after root
        return node.clone();
    }

    let indices = &path.indices;
    insert_recursive(node, indices, 0, element, after)
}

fn insert_recursive<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    indices: &[usize],
    depth: usize,
    element: GreenElement<K>,
    after: bool,
) -> Arc<GreenNode<K>> {
    if depth >= indices.len() {
        return node.clone();
    }

    let target_index = indices[depth];
    let children: Vec<_> = node.children().to_vec();

    if target_index >= children.len() {
        return node.clone();
    }

    let new_children: Vec<GreenElement<K>> = if depth == indices.len() - 1 {
        // This is the target level
        let mut result = Vec::with_capacity(children.len() + 1);
        for (i, child) in children.into_iter().enumerate() {
            if i == target_index && !after {
                result.push(element.clone());
            }
            result.push(child);
            if i == target_index && after {
                result.push(element.clone());
            }
        }
        result
    } else {
        // Need to go deeper
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(child_node) = child {
                        GreenElement::Node(insert_recursive(
                            &child_node,
                            indices,
                            depth + 1,
                            element.clone(),
                            after,
                        ))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    };

    let text_len = compute_text_len(&new_children);
    GreenNode::new(node.kind(), new_children, text_len)
}

/// Delete element at a path
fn delete_at_path<K: SyntaxKind>(node: &Arc<GreenNode<K>>, path: &TreePath) -> Arc<GreenNode<K>> {
    if path.is_root() {
        // Cannot delete root
        return node.clone();
    }

    let indices = &path.indices;
    delete_recursive(node, indices, 0)
}

fn delete_recursive<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    indices: &[usize],
    depth: usize,
) -> Arc<GreenNode<K>> {
    if depth >= indices.len() {
        return node.clone();
    }

    let target_index = indices[depth];
    let children: Vec<_> = node.children().to_vec();

    if target_index >= children.len() {
        return node.clone();
    }

    let new_children: Vec<GreenElement<K>> = if depth == indices.len() - 1 {
        // This is the target level - remove the element
        children
            .into_iter()
            .enumerate()
            .filter(|(i, _)| *i != target_index)
            .map(|(_, child)| child)
            .collect()
    } else {
        // Need to go deeper
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(child_node) = child {
                        GreenElement::Node(delete_recursive(&child_node, indices, depth + 1))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    };

    let text_len = compute_text_len(&new_children);
    GreenNode::new(node.kind(), new_children, text_len)
}

/// Wrap element at a path in a new node
fn wrap_at_path<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    path: &TreePath,
    kind: K,
) -> Arc<GreenNode<K>> {
    if path.is_root() {
        // Wrap root
        let root_as_element = GreenElement::Node(node.clone());
        let text_len = node.text_len();
        return GreenNode::new(kind, vec![root_as_element], text_len);
    }

    let indices = &path.indices;
    wrap_recursive(node, indices, 0, kind)
}

fn wrap_recursive<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    indices: &[usize],
    depth: usize,
    kind: K,
) -> Arc<GreenNode<K>> {
    if depth >= indices.len() {
        return node.clone();
    }

    let target_index = indices[depth];
    let children: Vec<_> = node.children().to_vec();

    if target_index >= children.len() {
        return node.clone();
    }

    let new_children: Vec<GreenElement<K>> = if depth == indices.len() - 1 {
        // This is the target level - wrap the element
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    let text_len = child.text_len();
                    GreenElement::Node(GreenNode::new(kind, vec![child], text_len))
                } else {
                    child
                }
            })
            .collect()
    } else {
        // Need to go deeper
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(child_node) = child {
                        GreenElement::Node(wrap_recursive(&child_node, indices, depth + 1, kind))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    };

    let text_len = compute_text_len(&new_children);
    GreenNode::new(node.kind(), new_children, text_len)
}

/// Unwrap element at a path
fn unwrap_at_path<K: SyntaxKind>(node: &Arc<GreenNode<K>>, path: &TreePath) -> Arc<GreenNode<K>> {
    if path.is_root() {
        // Unwrap root - return first child if it's a node
        if let Some(GreenElement::Node(child)) = node.children().first() {
            return child.clone();
        }
        return node.clone();
    }

    let indices = &path.indices;
    unwrap_recursive(node, indices, 0)
}

fn unwrap_recursive<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    indices: &[usize],
    depth: usize,
) -> Arc<GreenNode<K>> {
    if depth >= indices.len() {
        return node.clone();
    }

    let target_index = indices[depth];
    let children: Vec<_> = node.children().to_vec();

    if target_index >= children.len() {
        return node.clone();
    }

    let new_children: Vec<GreenElement<K>> = if depth == indices.len() - 1 {
        // This is the target level - unwrap the element
        let mut result = Vec::new();
        for (i, child) in children.into_iter().enumerate() {
            if i == target_index {
                if let GreenElement::Node(inner) = child {
                    result.extend(inner.children().iter().cloned());
                } else {
                    result.push(child);
                }
            } else {
                result.push(child);
            }
        }
        result
    } else {
        // Need to go deeper
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(child_node) = child {
                        GreenElement::Node(unwrap_recursive(&child_node, indices, depth + 1))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    };

    let text_len = compute_text_len(&new_children);
    GreenNode::new(node.kind(), new_children, text_len)
}

/// Change kind of node at a path
fn change_kind_at_path<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    path: &TreePath,
    new_kind: K,
) -> Arc<GreenNode<K>> {
    if path.is_root() {
        // Change root kind
        return GreenNode::new(new_kind, node.children().to_vec(), node.text_len());
    }

    let indices = &path.indices;
    change_kind_recursive(node, indices, 0, new_kind)
}

fn change_kind_recursive<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    indices: &[usize],
    depth: usize,
    new_kind: K,
) -> Arc<GreenNode<K>> {
    if depth >= indices.len() {
        return node.clone();
    }

    let target_index = indices[depth];
    let children: Vec<_> = node.children().to_vec();

    if target_index >= children.len() {
        return node.clone();
    }

    let new_children: Vec<GreenElement<K>> = if depth == indices.len() - 1 {
        // This is the target level - change the kind
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(inner) = child {
                        GreenElement::Node(GreenNode::new(
                            new_kind,
                            inner.children().to_vec(),
                            inner.text_len(),
                        ))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    } else {
        // Need to go deeper
        children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
                if i == target_index {
                    if let GreenElement::Node(child_node) = child {
                        GreenElement::Node(change_kind_recursive(
                            &child_node,
                            indices,
                            depth + 1,
                            new_kind,
                        ))
                    } else {
                        child
                    }
                } else {
                    child
                }
            })
            .collect()
    };

    let text_len = compute_text_len(&new_children);
    GreenNode::new(node.kind(), new_children, text_len)
}

/// Compute total text length of children
fn compute_text_len<K: SyntaxKind>(children: &[GreenElement<K>]) -> TextSize {
    children.iter().fold(TextSize::zero(), |acc, child| {
        TextSize::from(u32::from(acc) + u32::from(child.text_len()))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
        Expr,
        Wrapper,
        Ident,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            matches!(self, Self::Ident)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    fn make_tree() -> Arc<GreenNode<TestKind>> {
        let ident = GreenToken::new(TestKind::Ident, "x");
        let expr = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(ident)],
            TextSize::from(1),
        );

        GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Node(expr)],
            TextSize::from(1),
        )
    }

    #[test]
    fn test_editor_replace() {
        let tree = make_tree();
        let mut editor = SyntaxEditor::new(tree);

        let new_ident = GreenToken::new(TestKind::Ident, "y");
        let new_expr = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(new_ident)],
            TextSize::from(1),
        );

        let path = TreePath::from_indices([0]);
        editor.replace(path, new_expr);

        let result = editor.apply();
        assert_eq!(result.kind(), TestKind::Root);
    }

    #[test]
    fn test_editor_delete() {
        let tree = make_tree();
        let mut editor = SyntaxEditor::new(tree);

        let path = TreePath::from_indices([0]);
        editor.delete(path);

        let result = editor.apply();
        assert_eq!(result.child_count(), 0);
    }

    #[test]
    fn test_editor_wrap() {
        let tree = make_tree();
        let mut editor = SyntaxEditor::new(tree);

        let path = TreePath::from_indices([0]);
        editor.wrap(path, TestKind::Wrapper);

        let result = editor.apply();
        assert_eq!(result.child_count(), 1);

        if let GreenElement::Node(wrapper) = &result.children()[0] {
            assert_eq!(wrapper.kind(), TestKind::Wrapper);
            assert_eq!(wrapper.child_count(), 1);
        } else {
            panic!("Expected node");
        }
    }

    #[test]
    fn test_editor_insert_before() {
        let tree = make_tree();
        let mut editor = SyntaxEditor::new(tree);

        let new_token = GreenToken::new(TestKind::Ident, "y");
        let new_expr = GreenNode::new(
            TestKind::Expr,
            vec![GreenElement::Token(new_token)],
            TextSize::from(1),
        );

        let path = TreePath::from_indices([0]);
        editor.insert_before(path, GreenElement::Node(new_expr));

        let result = editor.apply();
        assert_eq!(result.child_count(), 2);
    }
}
