//! # Round-trip emit (tree to string)
//!
//! Emit the syntax tree back to a string by walking tokens in order. Use for
//! formatters (identity pass), tests (compare trees by printed form), or
//! codegen. Optionally include or strip trivia.
//!
//! Requires the `walk` feature.

use super::red::{SyntaxNode, SyntaxToken};
use super::walk::{Visitor, WalkOptions, WalkResult, walk};

/// Options for emitting a syntax tree to string.
#[derive(Clone, Debug)]
pub struct EmitOptions {
    /// If true, include trivia (whitespace, comments) in the output.
    /// If false, only semantic tokens are emitted.
    pub include_trivia: bool,
    /// If set, tokens with this kind are skipped (e.g. EOF sentinel).
    /// Comparison is by raw `SyntaxKind` (e.g. from your grammar).
    pub skip_kind: Option<u16>,
}

impl Default for EmitOptions {
    fn default() -> Self {
        Self {
            include_trivia: true,
            skip_kind: None,
        }
    }
}

impl EmitOptions {
    /// Emit only semantic tokens (no trivia).
    #[must_use]
    pub const fn semantic_only() -> Self {
        Self {
            include_trivia: false,
            skip_kind: None,
        }
    }

    /// Emit all tokens including trivia.
    #[must_use]
    pub fn full() -> Self {
        Self::default()
    }
}

/// Visitor that collects token text into a string.
struct EmitVisitor {
    out: String,
    options: EmitOptions,
}

impl Visitor for EmitVisitor {
    fn visit_token(&mut self, token: &SyntaxToken) -> WalkResult {
        if let Some(skip) = self.options.skip_kind {
            if token.kind() == skip {
                return WalkResult::Continue(());
            }
        }
        if token.is_trivia() && !self.options.include_trivia {
            return WalkResult::Continue(());
        }
        self.out.push_str(token.text());
        WalkResult::Continue(())
    }
}

/// Emit the syntax tree to a string by concatenating tokens in document order.
///
/// Uses the same traversal as the walk API; options control whether trivia
/// is included and whether a sentinel token kind (e.g. EOF) is skipped.
#[must_use]
pub fn syntax_root_to_string(root: &SyntaxNode, options: &EmitOptions) -> String {
    let mut visitor = EmitVisitor {
        out: String::new(),
        options: options.clone(),
    };
    let walk_opts = if options.include_trivia {
        WalkOptions::full()
    } else {
        WalkOptions::semantic_only()
    };
    let _ = walk(root, &mut visitor, &walk_opts);
    visitor.out
}
