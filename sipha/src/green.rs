//! # Green Tree
//!
//! An immutable, position-independent tree of syntax structure.
//!
//! Every node caches its total `text_len` so that positions can be recomputed
//! in O(children) during red-tree traversal.  Every leaf [`GreenToken`] stores
//! its source text as an `Arc<str>` so the tree is **self-contained** —
//! consumers never need to keep the raw input buffer alive.
//!
//! ## Building
//!
//! Call [`build_green_tree`] with the raw `input` bytes and the
//! [`crate::types::TreeEvent`] slice from
//! [`crate::engine::ParseOutput::tree_events`].  Text is sliced out of
//! `input` once and interned into each token's `Arc<str>`.
//!
//! ## Sharing / incremental reparse
//!
//! All nodes are `Arc`-wrapped.  Identical subtrees can be aliased — the
//! foundation for zero-copy incremental reparsing.
//!
//! ## Trivia
//!
//! Trivia tokens (whitespace, comments) live as ordinary [`GreenElement::Token`]
//! children with `is_trivia = true`.  The red layer provides filtered views
//! such as [`crate::red::SyntaxNode::non_trivia_tokens`] and
//! [`crate::red::SyntaxNode::token_groups`].

use std::sync::Arc;
use crate::types::{FromSyntaxKind, SyntaxKind, TreeEvent};

// ─── GreenToken ───────────────────────────────────────────────────────────────

/// A leaf in the green tree.
///
/// Stores its source text as an `Arc<str>` so the tree is self-contained;
/// `text_len` is cached separately so layout-dependent offset computation
/// never re-allocates.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GreenToken {
    /// Grammar-defined kind.
    pub kind:      SyntaxKind,
    /// Byte length of [`text`].  Equal to `text.len() as u32`.
    pub text_len:  u32,
    /// `true` for whitespace, comments, and other trivia.
    pub is_trivia: bool,
    /// Raw source text of this token (UTF-8).
    pub text:      Arc<str>,
}

impl GreenToken {
    /// Construct from already-sliced text.
    pub fn new(kind: SyntaxKind, text: &str, is_trivia: bool) -> Arc<Self> {
        Arc::new(Self {
            kind,
            text_len: text.len() as u32,
            is_trivia,
            text: text.into(),
        })
    }

    /// The token's source text, without needing the original input buffer.
    #[inline]
    pub fn text(&self) -> &str { &self.text }

    /// Build a trivia token containing a single space. Use for normalizing whitespace.
    #[inline]
    pub fn space(kind: SyntaxKind) -> Arc<Self> {
        Self::new(kind, " ", true)
    }

    /// Build a trivia token containing a single newline.
    #[inline]
    pub fn newline(kind: SyntaxKind) -> Arc<Self> {
        Self::new(kind, "\n", true)
    }

    /// Interpret the stored kind as a custom enum that implements [`FromSyntaxKind`].
    #[inline]
    pub fn kind_as<K: FromSyntaxKind>(&self) -> Option<K> {
        K::from_syntax_kind(self.kind)
    }
}

// ─── GreenNode ────────────────────────────────────────────────────────────────

/// An inner node in the green tree.
#[derive(Clone, Debug)]
pub struct GreenNode {
    /// Grammar-defined kind.
    pub kind:     SyntaxKind,
    /// Total byte length of all descendants (including trivia).
    pub text_len: u32,
    /// Ordered children.
    pub children: Box<[GreenElement]>,
}

impl GreenNode {
    pub fn new(kind: SyntaxKind, children: Vec<GreenElement>) -> Arc<Self> {
        let text_len = children.iter().map(GreenElement::text_len).sum();
        Arc::new(Self { kind, text_len, children: children.into() })
    }

    /// Interpret the stored kind as a custom enum that implements [`FromSyntaxKind`].
    #[inline]
    pub fn kind_as<K: FromSyntaxKind>(&self) -> Option<K> {
        K::from_syntax_kind(self.kind)
    }

    /// Reconstruct the node's full text by concatenating all token texts.
    ///
    /// Returns a freshly-allocated `String`; for a borrowed slice over the
    /// original source use [`crate::red::SyntaxNode::text`].
    pub fn collect_text(&self) -> String {
        let mut out = String::with_capacity(self.text_len as usize);
        collect_text_into(self, &mut out);
        out
    }
}

fn collect_text_into(node: &GreenNode, out: &mut String) {
    for child in node.children.iter() {
        match child {
            GreenElement::Node(n)  => collect_text_into(n, out),
            GreenElement::Token(t) => out.push_str(&t.text),
        }
    }
}

// ─── GreenElement ─────────────────────────────────────────────────────────────

/// A child of a [`GreenNode`] — either an inner node or a leaf token.
#[derive(Clone, Debug)]
pub enum GreenElement {
    Node(Arc<GreenNode>),
    Token(Arc<GreenToken>),
}

impl GreenElement {
    /// Total byte length of this element (including all descendants and trivia).
    #[inline]
    pub fn text_len(&self) -> u32 {
        match self {
            GreenElement::Node(n)  => n.text_len,
            GreenElement::Token(t) => t.text_len,
        }
    }

    /// `true` if this element is a trivia token.
    #[inline]
    pub fn is_trivia(&self) -> bool {
        match self {
            GreenElement::Token(t) => t.is_trivia,
            GreenElement::Node(_)  => false,
        }
    }

    /// `SyntaxKind` of the element.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        match self {
            GreenElement::Node(n)  => n.kind,
            GreenElement::Token(t) => t.kind,
        }
    }

    /// Interpret the stored kind as a custom enum that implements [`FromSyntaxKind`].
    #[inline]
    pub fn kind_as<K: FromSyntaxKind>(&self) -> Option<K> {
        K::from_syntax_kind(self.kind())
    }
}

// ─── build_green_tree ─────────────────────────────────────────────────────────

/// Build a green tree from the [`TreeEvent`] stream produced by the VM.
///
/// `input` is the raw source bytes — used to slice token and trivia text into
/// each [`GreenToken`]'s `Arc<str>`.  Pass an empty slice if you don't need
/// text stored in the tree (then `GreenToken::text` will be `""`).
///
/// Events must form a well-nested `NodeOpen`/`NodeClose` sequence with `Token`
/// leaves interspersed.  Returns the root [`GreenNode`], or `None` in these
/// cases:
///
/// - **Empty stream** — `events` is empty.
/// - **Unclosed node** — a `NodeOpen` has no matching `NodeClose` (stack
///   non-empty at end of stream).
/// - **Close without open** — a `NodeClose` occurs when the node stack is
///   empty (e.g. mismatched or reordered events).
///
/// If the grammar emits multiple top-level nodes (unusual), they are wrapped
/// in a synthetic root with `kind = u16::MAX`.
pub fn build_green_tree(input: &[u8], events: &[TreeEvent]) -> Option<Arc<GreenNode>> {
    // Stack of open nodes: (kind, children_so_far)
    let mut stack: Vec<(SyntaxKind, Vec<GreenElement>)> = Vec::new();
    let mut roots: Vec<GreenElement> = Vec::new();

    for &ev in events {
        match ev {
            TreeEvent::NodeOpen { kind, .. } => {
                stack.push((kind, Vec::new()));
            }

            TreeEvent::NodeClose { .. } => {
                let (kind, children) = stack.pop()?;
                let node = GreenNode::new(kind, children);
                push_element(&mut stack, &mut roots, GreenElement::Node(node));
            }

            TreeEvent::Token { kind, start, end, is_trivia } => {
                // Skip zero-length tokens: they contribute nothing to the tree
                // and arise when optional trivia (e.g. `zero_or_more` inside
                // `g.trivia()`) matches the empty string.
                if start == end { continue; }
                let text = input.get(start as usize..end as usize)
                    .and_then(|b| std::str::from_utf8(b).ok())
                    .unwrap_or("");
                let tok = GreenToken::new(kind, text, is_trivia);
                push_element(&mut stack, &mut roots, GreenElement::Token(tok));
            }
        }
    }

    if !stack.is_empty() { return None; } // unclosed NodeOpen

    match roots.len() {
        0 => None,
        1 => match roots.remove(0) {
            GreenElement::Node(n) => Some(n),
            GreenElement::Token(t) => {
                // Lone leaf — wrap in a synthetic root.
                Some(GreenNode::new(t.kind, vec![GreenElement::Token(t)]))
            }
        },
        _ => Some(GreenNode::new(u16::MAX, roots)),
    }
}

#[inline]
fn push_element(
    stack: &mut Vec<(SyntaxKind, Vec<GreenElement>)>,
    roots: &mut Vec<GreenElement>,
    elem:  GreenElement,
) {
    match stack.last_mut() {
        Some((_, children)) => children.push(elem),
        None                => roots.push(elem),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TreeEvent;

    #[test]
    fn build_green_tree_single_token() {
        let input = b"abc";
        let events = [TreeEvent::Token {
            kind: 1,
            start: 0,
            end: 3,
            is_trivia: false,
        }];
        let root = build_green_tree(input, &events).expect("valid events");
        assert_eq!(root.kind, 1); // lone token wrapped in synthetic node
        assert_eq!(root.text_len, 3);
        assert_eq!(root.children.len(), 1);
        let token = match &root.children[0] {
            GreenElement::Token(t) => t.as_ref(),
            _ => panic!("expected token"),
        };
        assert_eq!(token.text(), "abc");
        assert!(!token.is_trivia);
    }

    #[test]
    fn build_green_tree_node_with_token() {
        let input = b"x";
        let events = [
            TreeEvent::NodeOpen { kind: 10, pos: 0 },
            TreeEvent::Token {
                kind: 2,
                start: 0,
                end: 1,
                is_trivia: false,
            },
            TreeEvent::NodeClose { pos: 1 },
        ];
        let root = build_green_tree(input, &events).expect("valid events");
        assert_eq!(root.kind, 10);
        assert_eq!(root.text_len, 1);
        assert_eq!(root.children.len(), 1);
        if let GreenElement::Token(t) = &root.children[0] {
            assert_eq!(t.text(), "x");
        } else {
            panic!("expected token");
        }
    }

    #[test]
    fn build_green_tree_empty_returns_none() {
        let root = build_green_tree(b"", &[]);
        assert!(root.is_none());
    }
}
