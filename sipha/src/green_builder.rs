//! # Green tree builder
//!
//! Fluent API for constructing [`GreenNode`] and [`GreenToken`] trees without
//! manually building `Vec<GreenElement>`. Use for codegen, transforms, or
//! building expected trees in tests.
//!
//! ```ignore
//! let node = GreenBuilder::node(KIND_EXPR, |b| {
//!     b.token(TOK_IDENT, "x", false);
//!     b.token(TOK_PLUS, "+", false);
//!     b.token(TOK_NUM, "1", false);
//! });
//! ```

use std::sync::Arc;

use crate::green::{GreenElement, GreenNode, GreenToken};
use crate::types::SyntaxKind;

/// Fluent builder for a single green node. Add children with [`token`](Self::token)
/// and [`node`](Self::node), then call [`finish`](Self::finish) or drop to get the node.
pub struct GreenBuilder {
    kind: SyntaxKind,
    children: Vec<GreenElement>,
}

impl GreenBuilder {
    /// Start building a node with the given kind.
    #[inline]
    #[must_use] 
    pub const fn new(kind: SyntaxKind) -> Self {
        Self {
            kind,
            children: Vec::new(),
        }
    }

    /// Add a token child.
    #[inline]
    pub fn token(&mut self, kind: SyntaxKind, text: &str, is_trivia: bool) -> &mut Self {
        self.children
            .push(GreenElement::Token(GreenToken::new(kind, text, is_trivia)));
        self
    }

    /// Add a child node (from an existing green node).
    #[inline]
    pub fn child_node(&mut self, node: Arc<GreenNode>) -> &mut Self {
        self.children.push(GreenElement::Node(node));
        self
    }

    /// Build a child node with a closure and add it.
    #[inline]
    pub fn node(&mut self, kind: SyntaxKind, f: impl FnOnce(&mut Self)) -> &mut Self {
        let child = Self::node_standalone(kind, f);
        self.children.push(GreenElement::Node(child));
        self
    }

    /// Finish and return the built node.
    #[inline]
    #[must_use] 
    pub fn finish(mut self) -> Arc<GreenNode> {
        GreenNode::new(self.kind, std::mem::take(&mut self.children))
    }

    /// Build a node with the given kind and children (standalone, not as a child).
    pub fn node_standalone(kind: SyntaxKind, f: impl FnOnce(&mut Self)) -> Arc<GreenNode> {
        let mut b = Self::new(kind);
        f(&mut b);
        b.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_simple_node() {
        let root = GreenBuilder::node_standalone(1, |b| {
            b.token(2, "x", false).token(3, "+", false).token(4, "1", false);
        });
        assert_eq!(root.kind, 1);
        assert_eq!(root.children.len(), 3);
        assert_eq!(root.collect_text(), "x+1");
    }

    #[test]
    fn build_nested() {
        let root = GreenBuilder::node_standalone(10, |b| {
            b.node(11, |b| {
                b.token(2, "a", false);
            })
            .token(3, " ", true)
            .node(11, |b| {
                b.token(2, "b", false);
            });
        });
        assert_eq!(root.kind, 10);
        assert_eq!(root.children.len(), 3);
        assert_eq!(root.collect_text(), "a b");
    }
}
