//! Tree diff and comparison for sipha syntax trees.
//!
//! Compare two syntax trees by their emitted text (round-trip). Useful for
//! tests (assert formatted output) or refactors (compare before/after).
//!
//! ## Grammar tests with S-expressions
//!
//! Use [`assert_parse_eq`] or the [`assert_parse!`] macro to compare a parsed
//! tree against an expected S-expression string (e.g. `"(ROOT (EXPR (NUM \"1\")))"`).

use sipha::emit::{syntax_root_to_string, EmitOptions};
use sipha::red::SyntaxNode;
pub use sipha::sexp::{syntax_node_to_sexp, SexpOptions};

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

// ─── Grammar test helpers (S-expression) ────────────────────────────────────

/// Assert that parsing `input` yields a tree whose S-expression equals `expected_sexp`.
///
/// `parse_result` is typically `parse(input)` or `parse_expression(input)` returning
/// `Result<Option<SyntaxNode>, E>`. On failure prints expected vs got S-expression.
pub fn assert_parse_eq<E: std::fmt::Debug>(
    parse_result: Result<Option<SyntaxNode>, E>,
    _input: &str,
    expected_sexp: &str,
    options: &SexpOptions,
) {
    let root = parse_result
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e))
        .expect("parse returned None (no root)");
    let got = syntax_node_to_sexp(&root, options);
    assert_eq!(
        got,
        expected_sexp,
        "S-expression mismatch:\nexpected:\n  {}\ngot:\n  {}",
        expected_sexp,
        got
    );
}

/// Assert that parsing `input` with `parse_fn` produces a tree matching `expected_sexp`.
///
/// Example:
/// ```ignore
/// assert_parse!(|s| leekscript::parse(s), "1 + 2", "(ROOT (EXPR ...))");
/// ```
#[macro_export]
macro_rules! assert_parse {
    ($parse_fn:expr, $input:expr, $expected_sexp:expr) => {
        $crate::assert_parse_eq(
            $parse_fn($input),
            $input,
            $expected_sexp,
            &$crate::SexpOptions::semantic_only(),
        )
    };
    ($parse_fn:expr, $input:expr, $expected_sexp:expr, $options:expr) => {
        $crate::assert_parse_eq($parse_fn($input), $input, $expected_sexp, $options)
    };
}
