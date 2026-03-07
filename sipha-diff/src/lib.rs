//! Tree diff and comparison for sipha syntax trees.
//!
//! Compare two syntax trees by their emitted text (round-trip). Useful for
//! tests (assert formatted output) or refactors (compare before/after).

use sipha::emit::{syntax_root_to_string, EmitOptions};
use sipha::red::SyntaxNode;

/// Returns true if both trees emit the same string (same tokens and trivia).
#[inline]
pub fn trees_equal(a: &SyntaxNode, b: &SyntaxNode) -> bool {
    syntax_root_to_string(a, &EmitOptions::full()) == syntax_root_to_string(b, &EmitOptions::full())
}

/// Returns true if both trees have the same semantic content (ignoring trivia).
#[inline]
pub fn trees_equal_semantic(a: &SyntaxNode, b: &SyntaxNode) -> bool {
    syntax_root_to_string(a, &EmitOptions::semantic_only())
        == syntax_root_to_string(b, &EmitOptions::semantic_only())
}

/// Format a short diff message: "expected" vs "got" (full round-trip).
pub fn format_diff(expected: &SyntaxNode, got: &SyntaxNode) -> String {
    let expected_str = syntax_root_to_string(expected, &EmitOptions::full());
    let got_str = syntax_root_to_string(got, &EmitOptions::full());
    if expected_str == got_str {
        return "trees are equal".to_string();
    }
    format!(
        "expected ({} bytes):\n  {:?}\ngot ({} bytes):\n  {:?}",
        expected_str.len(),
        truncate_for_display(&expected_str, 200),
        got_str.len(),
        truncate_for_display(&got_str, 200)
    )
}

fn truncate_for_display(s: &str, max: usize) -> String {
    let s = s.replace('\n', "\\n");
    if s.len() <= max {
        s
    } else {
        format!("{}...", &s[..max])
    }
}
