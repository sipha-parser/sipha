//! # Tree walk (visitor)
//!
//! Generic pre- and post-order traversal over a sipha red tree. Use this for
//! formatting, scope analysis, type checking, linting, and other passes that
//! need to visit every node and optionally every token.
//!
//! Implement [`Visitor`] and call [`SyntaxNode::walk`] (or the free function
//! [`walk`]) with your visitor and [`WalkOptions`]. Dispatch on node kind via
//! `node.kind()` or `node.kind_as::<YourKind>()` (see [`crate::types::FromSyntaxKind`]).

use std::ops::ControlFlow;

use crate::red::{SyntaxElement, SyntaxNode, SyntaxToken};

/// Options that control how the tree is traversed.
#[derive(Clone, Debug)]
pub struct WalkOptions {
    /// Visit leaf tokens (identifiers, literals, operators, etc.).
    /// If false, only syntax nodes are visited.
    pub visit_tokens: bool,
    /// When visiting tokens, include trivia (whitespace, comments).
    /// Ignored if `visit_tokens` is false.
    pub visit_trivia: bool,
}

impl Default for WalkOptions {
    fn default() -> Self {
        Self {
            visit_tokens: true,
            visit_trivia: false,
        }
    }
}

impl WalkOptions {
    /// Only visit syntax nodes (no tokens). Useful for structure-only analysis.
    #[must_use]
    pub const fn nodes_only() -> Self {
        Self {
            visit_tokens: false,
            visit_trivia: false,
        }
    }

    /// Visit nodes and semantic tokens only (no trivia).
    #[must_use]
    pub fn semantic_only() -> Self {
        Self::default()
    }

    /// Visit nodes and all tokens including trivia. Useful for formatting.
    #[must_use]
    pub const fn full() -> Self {
        Self {
            visit_tokens: true,
            visit_trivia: true,
        }
    }
}

/// Result of a visitor callback: continue walking or stop.
pub type WalkResult = ControlFlow<(), ()>;

/// Trait for visiting a sipha syntax tree.
///
/// Override only the methods you need; default implementations do nothing and
/// return `WalkResult::Continue(())`. Use `node.kind()` or
/// `node.kind_as::<YourKind>()` to dispatch on node type.
///
/// # Order of calls
///
/// For each node, `enter_node` is called first (pre-order), then children are
/// visited, then `leave_node` (post-order). This supports scoping: push scope
/// in `enter_node` for block/function nodes, pop in `leave_node`.
///
/// # Early termination
///
/// Return `WalkResult::Break(())` from any callback to stop the walk; the
/// break propagates to the caller of [`walk`].
///
/// # Collecting results
///
/// Store any result in your visitor struct and read it after `walk` returns.
pub trait Visitor {
    /// Called before visiting this node's children (pre-order).
    fn enter_node(&mut self, _node: &SyntaxNode) -> WalkResult {
        WalkResult::Continue(())
    }

    /// Called after visiting this node's children (post-order).
    fn leave_node(&mut self, _node: &SyntaxNode) -> WalkResult {
        WalkResult::Continue(())
    }

    /// Called for each token when `WalkOptions::visit_tokens` is true.
    /// Trivia is included only when `WalkOptions::visit_trivia` is true.
    fn visit_token(&mut self, _token: &SyntaxToken) -> WalkResult {
        WalkResult::Continue(())
    }
}

fn walk_node(node: &SyntaxNode, visitor: &mut impl Visitor, options: &WalkOptions) -> WalkResult {
    if visitor.enter_node(node) == WalkResult::Break(()) {
        return WalkResult::Break(());
    }

    if options.visit_tokens {
        for elem in node.children() {
            match elem {
                SyntaxElement::Node(child) => {
                    if walk_node(&child, visitor, options) == WalkResult::Break(()) {
                        let _ = visitor.leave_node(node);
                        return WalkResult::Break(());
                    }
                }
                SyntaxElement::Token(token) => {
                    if token.is_trivia() && !options.visit_trivia {
                        continue;
                    }
                    if visitor.visit_token(&token) == WalkResult::Break(()) {
                        let _ = visitor.leave_node(node);
                        return WalkResult::Break(());
                    }
                }
            }
        }
    } else {
        for child in node.child_nodes() {
            if walk_node(&child, visitor, options) == WalkResult::Break(()) {
                let _ = visitor.leave_node(node);
                return WalkResult::Break(());
            }
        }
    }

    if visitor.leave_node(node) == WalkResult::Break(()) {
        return WalkResult::Break(());
    }
    WalkResult::Continue(())
}

impl SyntaxNode {
    /// Walks the subtree rooted at this node with the given visitor and options.
    ///
    /// Returns `ControlFlow::Break(())` if the visitor requested early termination.
    pub fn walk(&self, visitor: &mut impl Visitor, options: &WalkOptions) -> WalkResult {
        walk_node(self, visitor, options)
    }
}

/// Walks the tree starting at `root` with the given visitor and options.
///
/// Equivalent to `root.walk(visitor, options)`. Returns `ControlFlow::Break(())`
/// if the visitor requested early termination.
#[inline]
pub fn walk(root: &SyntaxNode, visitor: &mut impl Visitor, options: &WalkOptions) -> WalkResult {
    root.walk(visitor, options)
}
