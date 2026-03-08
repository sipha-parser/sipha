//! Format a sipha red syntax tree as a readable ASCII/Unicode tree.
//!
//! Grammar-agnostic: node and token labels come from a `kind_name` callback
//! so any grammar can display its syntax kinds.

use std::collections::HashSet;
use std::sync::Arc;

use crate::red::{SyntaxElement, SyntaxNode, SyntaxToken};
use crate::types::SyntaxKind;

/// Options for formatting the syntax tree.
#[derive(Clone, Debug)]
pub struct TreeDisplayOptions {
    /// Include leaf tokens in the tree (otherwise only nodes are shown).
    pub show_tokens: bool,
    /// When showing tokens, include trivia (whitespace, comments).
    pub show_trivia: bool,
    /// Maximum length of token text to display; longer text is truncated with "...".
    pub max_token_text_len: usize,
    /// Indent string for each level (e.g. "  " or "│ ").
    pub indent: String,
}

impl Default for TreeDisplayOptions {
    fn default() -> Self {
        Self {
            show_tokens: true,
            show_trivia: false,
            max_token_text_len: 40,
            indent: "  ".to_string(),
        }
    }
}

impl TreeDisplayOptions {
    /// Structure-only: only syntax nodes, no tokens.
    #[must_use]
    pub fn structure_only() -> Self {
        Self {
            show_tokens: false,
            ..Self::default()
        }
    }

    /// Full tree: nodes and all tokens including trivia.
    #[must_use]
    pub fn full() -> Self {
        Self {
            show_trivia: true,
            ..Self::default()
        }
    }
}

/// Identity for cycle detection: (green node pointer, offset).
fn node_id(node: &SyntaxNode) -> (usize, u32) {
    (Arc::as_ptr(node.green()) as usize, node.offset())
}

/// Elements to show for a node (filtered by `show_trivia`), with `is_last` for tree connectors.
fn visible_elements(node: &SyntaxNode, options: &TreeDisplayOptions) -> Vec<(SyntaxElement, bool)> {
    let elements: Vec<SyntaxElement> = node.children().collect();
    let visible: Vec<_> = elements
        .into_iter()
        .filter(|e| !matches!(e, SyntaxElement::Token(t) if t.is_trivia() && !options.show_trivia))
        .collect();
    let n = visible.len();
    visible
        .into_iter()
        .enumerate()
        .map(|(i, e)| (e, i == n.saturating_sub(1)))
        .collect()
}

/// Format a syntax tree starting at `root` as a multi-line string.
///
/// `kind_name` maps a syntax kind (u16) to a display string for nodes and tokens.
/// Detects cycles and prints `[cycle]` to avoid infinite recursion.
#[must_use]
pub fn format_syntax_tree(
    root: &SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: impl Fn(SyntaxKind) -> String,
) -> String {
    let mut out = String::new();
    let mut visited = HashSet::new();
    format_node(
        root.clone(),
        options,
        &kind_name,
        "",
        true,
        &mut out,
        &mut visited,
    );
    out
}

fn format_node_header(prefix: &str, is_last: bool, kind_str: &str, out: &mut String, cycle: bool) {
    let connector = if is_last { "└── " } else { "├── " };
    out.push_str(prefix);
    out.push_str(connector);
    out.push_str(kind_str);
    if cycle {
        out.push_str(" [cycle]\n");
    } else {
        out.push('\n');
    }
}

fn format_node(
    node: SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    prefix: &str,
    is_last: bool,
    out: &mut String,
    visited: &mut HashSet<(usize, u32)>,
) {
    let id = node_id(&node);
    let kind_str = kind_name(node.kind());
    if !visited.insert(id) {
        format_node_header(prefix, is_last, &kind_str, out, true);
        return;
    }

    format_node_header(prefix, is_last, &kind_str, out, false);

    let new_prefix = if is_last {
        format!("{prefix}    ")
    } else {
        format!("{prefix}│   ")
    };

    format_node_children(node, options, kind_name, &new_prefix, out, visited);
    visited.remove(&id);
}

fn format_node_children(
    node: SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    new_prefix: &str,
    out: &mut String,
    visited: &mut HashSet<(usize, u32)>,
) {
    if options.show_tokens {
        for (elem, is_last) in visible_elements(&node, options) {
            match elem {
                SyntaxElement::Node(n) => {
                    format_node(n, options, kind_name, new_prefix, is_last, out, visited)
                }
                SyntaxElement::Token(t) => {
                    format_token(&t, options, kind_name, new_prefix, is_last, out)
                }
            }
        }
    } else {
        let child_nodes: Vec<SyntaxNode> = node.child_nodes().collect();
        let last_idx = child_nodes.len().saturating_sub(1);
        for (i, child) in child_nodes.into_iter().enumerate() {
            format_node(
                child,
                options,
                kind_name,
                new_prefix,
                i == last_idx,
                out,
                visited,
            );
        }
    }
}

fn format_token(
    token: &SyntaxToken,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    prefix: &str,
    is_last: bool,
    out: &mut String,
) {
    let kind_str = kind_name(token.kind());
    let connector = if is_last { "└── " } else { "├── " };
    out.push_str(prefix);
    out.push_str(connector);
    out.push_str(&kind_str);
    let text = token.text();
    if !text.is_empty() {
        let display = if text.len() > options.max_token_text_len {
            format!("{:?}...", &text[..options.max_token_text_len])
        } else {
            format!("{text:?}")
        };
        out.push(' ');
        out.push_str(&display);
    }
    if token.is_trivia() {
        out.push_str(" (trivia)");
    }
    out.push('\n');
}
