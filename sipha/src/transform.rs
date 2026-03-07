//! Tree transformation: replace subtrees by building new green nodes.
//!
//! In addition to the [walk](crate::walk) (read-only traversal), you can **transform** the tree:
//! at each node, a [`Transformer`] may return a replacement [`GreenNode`], which is then used
//! instead of the original subtree. The rest of the tree is rebuilt so that the root is a new
//! [`SyntaxNode`].
//!
//! Use this to normalize forms (e.g. rewrite `a += 10` into `a = a + 10`) or to apply
//! language-specific rewrites.

use std::sync::Arc;

use crate::green::{GreenElement, GreenNode};
use crate::red::SyntaxNode;

/// Result of transforming a single node: either keep the node (and recurse into children)
/// or replace the entire subtree with the given green node.
///
/// Return `None` to keep the node and apply transformations to its children.
/// Return `Some(arc)` to replace this node and all its descendants with the new tree.
pub type TransformResult = Option<Arc<GreenNode>>;

/// Trait for transforming the syntax tree.
///
/// Implement [`transform_node`](Transformer::transform_node). For each node, return:
/// - **`None`**: keep the node and recurse into its children (children may still be transformed).
/// - **`Some(arc)`**: replace this node and its entire subtree with the given green node; no
///   recursion into the original children.
pub trait Transformer {
    /// Transform a single node. Return `None` to keep, `Some(green)` to replace.
    fn transform_node(&mut self, node: &SyntaxNode) -> TransformResult;
}

/// Runs the transformer over the tree and returns a new root with all replacements applied.
///
/// The tree is traversed top-down. For each node, [`Transformer::transform_node`] is called.
/// If it returns `Some(replacement)`, that green node is used and the original subtree is not
/// visited. If it returns `None`, the node is kept and the function recurses into its children,
/// building a new green node from the (possibly transformed) children.
///
/// The returned root is a new [`SyntaxNode`] with offset 0; positions in the new tree reflect
/// the new text.
pub fn transform(root: &SyntaxNode, transformer: &mut impl Transformer) -> SyntaxNode {
    let new_green = rebuild(root, transformer);
    SyntaxNode::new_root(new_green)
}

fn rebuild(node: &SyntaxNode, transformer: &mut impl Transformer) -> Arc<GreenNode> {
    if let Some(replacement) = transformer.transform_node(node) {
        return replacement;
    }
    let green = node.green();
    let mut new_children = Vec::with_capacity(green.children.len());
    let mut off = node.offset();
    for child in &green.children {
        match child {
            GreenElement::Node(child_green) => {
                let child_red = SyntaxNode::new(Arc::clone(child_green), off);
                new_children.push(GreenElement::Node(rebuild(&child_red, transformer)));
                off += child_green.text_len;
            }
            GreenElement::Token(t) => {
                new_children.push(GreenElement::Token(Arc::clone(t)));
                off += t.text_len;
            }
        }
    }
    GreenNode::new(node.kind(), new_children)
}
