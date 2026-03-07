//! # Trivia helpers
//!
//! Utilities for building or replacing trivia (whitespace, comments) in green
//! trees. Use for formatters that normalize spacing or inject newlines.

use std::sync::Arc;

use crate::green::{GreenElement, GreenNode, GreenToken};
use crate::types::SyntaxKind;

/// Build a trivia token containing a single space.
#[inline]
#[must_use] 
pub fn space(kind: SyntaxKind) -> Arc<GreenToken> {
    GreenToken::space(kind)
}

/// Build a trivia token containing a single newline.
#[inline]
#[must_use] 
pub fn newline(kind: SyntaxKind) -> Arc<GreenToken> {
    GreenToken::newline(kind)
}

/// Replace the leading trivia of a green node's direct children with the given tokens.
///
/// Leading trivia is the initial run of trivia tokens before the first semantic token.
/// The rest of the node's children are unchanged. Returns a new node.
pub fn replace_leading_trivia(
    node: &GreenNode,
    trivia_kind: SyntaxKind,
    new_leading: &[&str],
) -> Arc<GreenNode> {
    let mut children: Vec<GreenElement> = node.children.to_vec();
    let mut i = 0;
    while i < children.len() {
        match &children[i] {
            GreenElement::Token(t) if t.is_trivia => {
                i += 1;
            }
            _ => break,
        }
    }
    let new_trivia: Vec<GreenElement> = new_leading
        .iter()
        .map(|s| GreenElement::Token(GreenToken::new(trivia_kind, s, true)))
        .collect();
    let text_len_new: u32 = new_trivia.iter().map(GreenElement::text_len).sum::<u32>()
        + children[i..].iter().map(GreenElement::text_len).sum::<u32>();
    let mut out = new_trivia;
    out.extend(children.drain(i..));
    Arc::new(GreenNode {
        kind: node.kind,
        text_len: text_len_new,
        children: out.into(),
        child_fields: node.child_fields.clone(),
    })
}

/// Replace the trailing trivia of a green node's direct children with the given tokens.
///
/// Trailing trivia is the final run of trivia tokens after the last semantic token.
pub fn replace_trailing_trivia(
    node: &GreenNode,
    trivia_kind: SyntaxKind,
    new_trailing: &[&str],
) -> Arc<GreenNode> {
    let mut children: Vec<GreenElement> = node.children.to_vec();
    let mut trail_start = children.len();
    while trail_start > 0 {
        match &children[trail_start - 1] {
            GreenElement::Token(t) if t.is_trivia => {
                trail_start -= 1;
            }
            _ => break,
        }
    }
    let new_trivia: Vec<GreenElement> = new_trailing
        .iter()
        .map(|s| GreenElement::Token(GreenToken::new(trivia_kind, s, true)))
        .collect();
    let text_len_new: u32 = children[..trail_start].iter().map(GreenElement::text_len).sum::<u32>()
        + new_trivia.iter().map(GreenElement::text_len).sum::<u32>();
    let mut out: Vec<GreenElement> = children.drain(..trail_start).collect();
    out.extend(new_trivia);
    Arc::new(GreenNode {
        kind: node.kind,
        text_len: text_len_new,
        children: out.into(),
        child_fields: node.child_fields.clone(),
    })
}
