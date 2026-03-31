//! Typed CST (rowan-like) wrappers over [`SyntaxNode`](crate::tree::red::SyntaxNode).
//!
//! This module is intentionally lightweight: it provides a small trait for typed
//! node wrappers and a set of convenience accessors for casting and traversing.
//!
//! Users typically define newtype wrappers:
//!
//! ```ignore
//! #[derive(Clone, Debug)]
//! pub struct Expr(sipha::tree::red::SyntaxNode);
//! impl sipha::tree::ast::AstNode for Expr {
//!     fn can_cast(kind: sipha::types::SyntaxKind) -> bool { kind == Kind::Expr.into_syntax_kind() }
//!     fn cast(node: sipha::tree::red::SyntaxNode) -> Option<Self> {
//!         Self::can_cast(node.kind()).then(|| Self(node))
//!     }
//!     fn syntax(&self) -> &sipha::tree::red::SyntaxNode { &self.0 }
//! }
//! ```

use crate::tree::red::{SyntaxNode, SyntaxToken};
use crate::types::{FieldId, SyntaxKind};

/// A typed wrapper around a [`SyntaxNode`].
///
/// This is a CST (concrete syntax tree) abstraction: wrappers are backed by the
/// original syntax tree and preserve trivia and exact structure.
pub trait AstNode: Sized {
    /// `true` if nodes of `kind` can be represented by this wrapper.
    fn can_cast(kind: SyntaxKind) -> bool;

    /// Convert `node` into `Self` if its kind matches.
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Borrow the underlying syntax node.
    fn syntax(&self) -> &SyntaxNode;
}

/// Convenience helpers for working with typed CST wrappers.
pub trait AstNodeExt {
    /// Cast this node into a typed wrapper.
    fn ast<N: AstNode>(&self) -> Option<N>;

    /// First direct child that can be cast to `N`.
    fn child<N: AstNode>(&self) -> Option<N>;

    /// Iterate direct children that can be cast to `N`.
    fn children<'a, N: AstNode + 'a>(&'a self) -> impl Iterator<Item = N> + 'a;

    /// First direct child node labeled with `field_id` that can be cast to `N`.
    fn field_child<N: AstNode>(&self, field_id: FieldId) -> Option<N>;

    /// First direct child token with the given `kind`.
    fn token(&self, kind: SyntaxKind) -> Option<SyntaxToken>;

    /// Iterate direct child tokens with the given `kind`.
    fn tokens(&self, kind: SyntaxKind) -> impl Iterator<Item = SyntaxToken> + '_;
}

impl AstNodeExt for SyntaxNode {
    #[inline]
    fn ast<N: AstNode>(&self) -> Option<N> {
        N::cast(self.clone())
    }

    #[inline]
    fn child<N: AstNode>(&self) -> Option<N> {
        self.child_nodes().find_map(N::cast)
    }

    #[inline]
    fn children<'a, N: AstNode + 'a>(&'a self) -> impl Iterator<Item = N> + 'a {
        self.child_nodes().filter_map(N::cast)
    }

    #[inline]
    fn field_child<N: AstNode>(&self, field_id: FieldId) -> Option<N> {
        self.field_by_id(field_id).and_then(N::cast)
    }

    #[inline]
    fn token(&self, kind: SyntaxKind) -> Option<SyntaxToken> {
        self.child_tokens().find(|t| t.kind() == kind)
    }

    #[inline]
    fn tokens(&self, kind: SyntaxKind) -> impl Iterator<Item = SyntaxToken> + '_ {
        self.child_tokens().filter(move |t| t.kind() == kind)
    }
}

