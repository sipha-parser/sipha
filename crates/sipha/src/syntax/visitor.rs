use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
#[cfg(feature = "visitor")]
use std::ops::ControlFlow;

/// Trait for visiting syntax trees
#[cfg(feature = "visitor")]
pub trait SyntaxVisitor<K: SyntaxKind> {
    /// Called when entering a node (before visiting children)
    fn enter_node(&mut self, node: &SyntaxNode<K>) -> ControlFlow<()> {
        let _ = node;
        ControlFlow::Continue(())
    }

    /// Called when exiting a node (after visiting children)
    fn exit_node(&mut self, node: &SyntaxNode<K>) -> ControlFlow<()> {
        let _ = node;
        ControlFlow::Continue(())
    }

    /// Called when visiting a token
    fn visit_token(&mut self, token: &SyntaxToken<K>) -> ControlFlow<()> {
        let _ = token;
        ControlFlow::Continue(())
    }
}

/// Walker that drives tree traversal and calls visitor methods
#[cfg(feature = "visitor")]
pub struct SyntaxWalker<'v, V, K: SyntaxKind> {
    visitor: &'v mut V,
    _phantom: std::marker::PhantomData<K>,
}

#[cfg(feature = "visitor")]
impl<'v, V, K: SyntaxKind> SyntaxWalker<'v, V, K>
where
    V: SyntaxVisitor<K>,
{
    /// Create a new walker with the given visitor
    pub const fn new(visitor: &'v mut V) -> Self {
        Self {
            visitor,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Walk the tree in pre-order (node before children)
    pub fn walk_preorder(&mut self, root: &SyntaxNode<K>) -> ControlFlow<()> {
        // Visit node
        self.visitor.enter_node(root)?;

        // Visit children
        for child in root.children() {
            match child {
                SyntaxElement::Node(node) => {
                    self.walk_preorder(&node)?;
                }
                SyntaxElement::Token(token) => {
                    self.visitor.visit_token(&token)?;
                }
            }
        }

        // Exit node
        self.visitor.exit_node(root)?;

        ControlFlow::Continue(())
    }

    /// Walk the tree in post-order (node after children)
    pub fn walk_postorder(&mut self, root: &SyntaxNode<K>) -> ControlFlow<()> {
        // Visit children first
        for child in root.children() {
            match child {
                SyntaxElement::Node(node) => {
                    self.walk_postorder(&node)?;
                }
                SyntaxElement::Token(token) => {
                    self.visitor.visit_token(&token)?;
                }
            }
        }

        self.visitor.exit_node(root)?;

        ControlFlow::Continue(())
    }

    /// Walk the tree and call visitor methods
    /// Uses pre-order traversal by default
    pub fn walk(&mut self, root: &SyntaxNode<K>) -> ControlFlow<()> {
        self.walk_preorder(root)
    }
}

#[cfg(feature = "visitor")]
impl<K: SyntaxKind> SyntaxNode<K> {
    /// Walk this node with a visitor
    pub fn walk_with<V: SyntaxVisitor<K>>(&self, visitor: &mut V) -> ControlFlow<()> {
        let mut walker = SyntaxWalker::new(visitor);
        walker.walk(self)
    }
}
