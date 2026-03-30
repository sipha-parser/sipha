//! Formatter options and helpers for sipha syntax trees.
//!
//! Re-exports sipha's emit API and adds presets for common formatting choices
//! (e.g. semantic-only output, or full round-trip with trivia).

use crate::tree::red::SyntaxNode;

pub use crate::tree::emit::{syntax_root_to_string, EmitOptions};

/// Format the tree to string, including all tokens and trivia (lossless round-trip).
#[inline]
#[must_use]
pub fn format_full(root: &SyntaxNode) -> String {
    crate::tree::emit::syntax_root_to_string(root, &EmitOptions::full())
}

/// Format the tree to string, semantic tokens only (no trivia).
#[inline]
#[must_use]
pub fn format_semantic_only(root: &SyntaxNode) -> String {
    crate::tree::emit::syntax_root_to_string(root, &EmitOptions::semantic_only())
}

/// Format the tree, optionally skipping a sentinel token kind (e.g. EOF).
#[inline]
#[must_use]
pub fn format_with_skip(root: &SyntaxNode, include_trivia: bool, skip_kind: u16) -> String {
    crate::tree::emit::syntax_root_to_string(
        root,
        &EmitOptions {
            include_trivia,
            skip_kind: Some(skip_kind),
        },
    )
}
