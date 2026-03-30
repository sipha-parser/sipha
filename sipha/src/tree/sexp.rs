//! S-expression serialization of syntax trees for grammar tests.
//!
//! Use [`syntax_node_to_sexp`] to produce a string like `(ROOT (EXPR (NUM "1")))`
//! and compare with expected strings in tests.

use super::red::{SyntaxElement, SyntaxNode, SyntaxToken};
use crate::types::SyntaxKind;

/// Options for S-expression output.
#[derive(Clone, Debug, Default)]
pub struct SexpOptions {
    /// Include trivia tokens in the output.
    pub include_trivia: bool,
    /// If set, use this to map `SyntaxKind` to a readable name (e.g. "ROOT", "EXPR").
    /// Otherwise numeric kind is used.
    pub kind_to_name: Option<fn(SyntaxKind) -> Option<&'static str>>,
    /// Truncate token text to this length (with "..."); `None` = no truncation.
    pub max_token_len: Option<usize>,
}

impl SexpOptions {
    /// Structure and semantic tokens only; no trivia.
    #[must_use]
    pub fn semantic_only() -> Self {
        Self {
            include_trivia: false,
            ..Self::default()
        }
    }

    /// Full tree including trivia.
    #[must_use]
    pub fn full() -> Self {
        Self {
            include_trivia: true,
            ..Self::default()
        }
    }
}

fn escape_str(s: &str, max_len: Option<usize>) -> String {
    let s = max_len.map_or_else(
        || s.to_string(),
        |max| {
            if s.len() > max {
                format!("{}...", &s[..max])
            } else {
                s.to_string()
            }
        },
    );
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out.push('"');
    out
}

fn kind_str(kind: SyntaxKind, opts: &SexpOptions) -> String {
    if let Some(f) = opts.kind_to_name {
        if let Some(name) = f(kind) {
            return name.to_string();
        }
    }
    kind.to_string()
}

/// Serialize a syntax tree to an S-expression string.
///
/// Format: `(KIND child1 child2 ...)` for nodes; tokens as `(KIND "text")`.
/// Trivia is included only when [`SexpOptions::include_trivia`] is true.
#[must_use]
pub fn syntax_node_to_sexp(node: &SyntaxNode, opts: &SexpOptions) -> String {
    let mut out = String::new();
    append_node_to_sexp(node, opts, &mut out);
    out
}

fn append_node_to_sexp(node: &SyntaxNode, opts: &SexpOptions, out: &mut String) {
    let k = kind_str(node.kind(), opts);
    out.push('(');
    out.push_str(&k);
    for child in node.children() {
        match child {
            SyntaxElement::Node(n) => {
                out.push(' ');
                append_node_to_sexp(&n, opts, out);
            }
            SyntaxElement::Token(t) => {
                if t.is_trivia() && !opts.include_trivia {
                    continue;
                }
                out.push(' ');
                append_token_to_sexp(&t, opts, out);
            }
        }
    }
    out.push(')');
}

fn append_token_to_sexp(tok: &SyntaxToken, opts: &SexpOptions, out: &mut String) {
    let k = kind_str(tok.kind(), opts);
    let text = escape_str(tok.text(), opts.max_token_len);
    out.push('(');
    out.push_str(&k);
    out.push(' ');
    out.push_str(&text);
    out.push(')');
}
