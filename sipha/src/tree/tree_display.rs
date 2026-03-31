//! Format a sipha red syntax tree as a readable ASCII/Unicode tree.
//!
//! Grammar-agnostic: node and token labels come from a `kind_name` callback
//! so any grammar can display its syntax kinds.

use std::collections::HashSet;
use std::sync::Arc;

use super::green::GreenElement;
use super::red::{SyntaxElement, SyntaxNode, SyntaxToken};
use crate::types::{FieldId, Span, SyntaxKind};

/// How to draw the tree connectors.
#[derive(Clone, Copy, Debug, Default)]
pub enum TreeDisplayStyle {
    /// Unicode box-drawing characters (default).
    #[default]
    Unicode,
    /// ASCII fallbacks for environments that don’t render Unicode well.
    Ascii,
}

/// Options for formatting the syntax tree.
#[derive(Clone, Debug)]
pub struct TreeDisplayOptions {
    /// Include leaf tokens in the tree (otherwise only nodes are shown).
    pub show_tokens: bool,
    /// When showing tokens, include trivia (whitespace, comments).
    pub show_trivia: bool,
    /// Maximum length of token text to display; longer text is truncated with "...".
    pub max_token_text_len: usize,
    /// Include `[start..end)` byte ranges for nodes/tokens.
    pub show_ranges: bool,
    /// Include `@start` byte offsets for nodes/tokens.
    pub show_offsets: bool,
    /// Annotate children with field labels when available.
    pub show_field_labels: bool,
    /// Align columns for sibling items at the same depth.
    ///
    /// When enabled, the formatter does a quick prepass to compute per-depth column widths so
    /// the location (`@…` / `[…]`) and token preview align vertically.
    pub align_columns: bool,
    /// Connector style.
    pub style: TreeDisplayStyle,
}

impl Default for TreeDisplayOptions {
    fn default() -> Self {
        Self {
            show_tokens: true,
            show_trivia: false,
            max_token_text_len: 40,
            show_ranges: false,
            show_offsets: false,
            show_field_labels: false,
            align_columns: false,
            style: TreeDisplayStyle::Unicode,
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

#[derive(Clone)]
struct VisibleChild {
    elem: SyntaxElement,
    field: Option<FieldId>,
    is_last: bool,
}

/// Elements to show for a node (filtered by trivia), with optional child field ids.
fn visible_children(node: &SyntaxNode, options: &TreeDisplayOptions) -> Vec<VisibleChild> {
    // We walk the green children directly so we can preserve child index → field id.
    let green = node.green();
    let mut off = node.offset();
    let mut tmp: Vec<(SyntaxElement, Option<FieldId>)> = Vec::with_capacity(green.children.len());

    for (i, child) in green.children.iter().enumerate() {
        let start = off;
        off = off.saturating_add(child.text_len());

        if child.is_trivia() && !options.show_trivia {
            continue;
        }

        let field = green.child_field(i);
        let elem = match child {
            GreenElement::Node(n) => SyntaxElement::Node(SyntaxNode::new(Arc::clone(n), start)),
            GreenElement::Token(t) => SyntaxElement::Token(SyntaxToken::new(Arc::clone(t), start)),
        };
        tmp.push((elem, field));
    }

    let n = tmp.len();
    tmp.into_iter()
        .enumerate()
        .map(|(i, (elem, field))| VisibleChild {
            elem,
            field,
            is_last: i == n.saturating_sub(1),
        })
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
    format_syntax_tree_with(root, options, kind_name, |_| None)
}

/// Like [`format_syntax_tree`], but can also resolve child field ids to display names.
#[must_use]
pub fn format_syntax_tree_with(
    root: &SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: impl Fn(SyntaxKind) -> String,
    field_name: impl Fn(FieldId) -> Option<String>,
) -> String {
    let mut out = String::new();
    let mut visited = HashSet::new();
    let align = if options.align_columns {
        Some(measure_alignment(root, options, &kind_name, &field_name))
    } else {
        None
    };
    format_node(
        root,
        options,
        &kind_name,
        &field_name,
        align.as_ref(),
        "",
        true,
        None,
        0,
        &mut out,
        &mut visited,
    );
    out
}

#[derive(Clone, Debug, Default)]
struct Alignment {
    label_max_by_depth: Vec<usize>,
    loc_max_by_depth: Vec<usize>,
}

fn ensure_len<T: Default>(v: &mut Vec<T>, depth: usize) {
    if v.len() <= depth {
        v.resize_with(depth + 1, Default::default);
    }
}

fn measure_alignment(
    root: &SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    field_name: &impl Fn(FieldId) -> Option<String>,
) -> Alignment {
    let mut a = Alignment::default();
    let mut visited = HashSet::new();
    measure_node(
        root,
        options,
        kind_name,
        field_name,
        0,
        None,
        &mut a,
        &mut visited,
    );
    a
}

fn measure_node(
    node: &SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    field_name: &impl Fn(FieldId) -> Option<String>,
    depth: usize,
    field: Option<FieldId>,
    a: &mut Alignment,
    visited: &mut HashSet<(usize, u32)>,
) {
    let id = node_id(node);
    if !visited.insert(id) {
        // Still count the header line; don’t recurse.
        let label_len =
            kind_name(node.kind()).len() + field_label_str(field, options, field_name).len();
        let loc_len = loc_str(node.text_range(), options).len();
        ensure_len(&mut a.label_max_by_depth, depth);
        ensure_len(&mut a.loc_max_by_depth, depth);
        a.label_max_by_depth[depth] = a.label_max_by_depth[depth].max(label_len);
        a.loc_max_by_depth[depth] = a.loc_max_by_depth[depth].max(loc_len);
        return;
    }

    let label_len =
        kind_name(node.kind()).len() + field_label_str(field, options, field_name).len();
    let loc_len = loc_str(node.text_range(), options).len();
    ensure_len(&mut a.label_max_by_depth, depth);
    ensure_len(&mut a.loc_max_by_depth, depth);
    a.label_max_by_depth[depth] = a.label_max_by_depth[depth].max(label_len);
    a.loc_max_by_depth[depth] = a.loc_max_by_depth[depth].max(loc_len);

    if options.show_tokens {
        for child in visible_children(node, options) {
            match child.elem {
                SyntaxElement::Node(n) => {
                    measure_node(
                        &n,
                        options,
                        kind_name,
                        field_name,
                        depth + 1,
                        child.field,
                        a,
                        visited,
                    );
                }
                SyntaxElement::Token(t) => {
                    let label_len = kind_name(t.kind()).len()
                        + field_label_str(child.field, options, field_name).len();
                    let loc_len = loc_str(t.text_range(), options).len();
                    ensure_len(&mut a.label_max_by_depth, depth + 1);
                    ensure_len(&mut a.loc_max_by_depth, depth + 1);
                    a.label_max_by_depth[depth + 1] =
                        a.label_max_by_depth[depth + 1].max(label_len);
                    a.loc_max_by_depth[depth + 1] = a.loc_max_by_depth[depth + 1].max(loc_len);
                }
            }
        }
    } else {
        for child in node.child_nodes() {
            measure_node(
                &child,
                options,
                kind_name,
                field_name,
                depth + 1,
                None,
                a,
                visited,
            );
        }
    }

    visited.remove(&id);
}

fn connectors(style: TreeDisplayStyle) -> (&'static str, &'static str, &'static str, &'static str) {
    match style {
        TreeDisplayStyle::Unicode => ("└── ", "├── ", "│   ", "    "),
        TreeDisplayStyle::Ascii => ("\\-- ", "|-- ", "|   ", "    "),
    }
}

fn loc_str(range: Span, options: &TreeDisplayOptions) -> String {
    use std::fmt::Write as _;
    let mut s = String::new();
    if options.show_offsets {
        let _ = write!(s, "  @{}", range.start);
    }
    if options.show_ranges {
        let _ = write!(s, "  [{}..{})", range.start, range.end);
    }
    s
}

fn field_label_str(
    field: Option<FieldId>,
    options: &TreeDisplayOptions,
    field_name: &impl Fn(FieldId) -> Option<String>,
) -> String {
    if !options.show_field_labels {
        return String::new();
    }
    let Some(fid) = field else {
        return String::new();
    };
    if let Some(name) = field_name(fid) {
        format!("  {{field:{name}}}")
    } else {
        format!("  {{field:#{fid}}}")
    }
}

fn format_node_header(
    prefix: &str,
    is_last: bool,
    kind_str: &str,
    range: Span,
    field: Option<FieldId>,
    options: &TreeDisplayOptions,
    field_name: &impl Fn(FieldId) -> Option<String>,
    align: Option<&Alignment>,
    depth: usize,
    out: &mut String,
    cycle: bool,
) {
    let (last, mid, _vert, _blank) = connectors(options.style);
    let connector = if is_last { last } else { mid };
    out.push_str(prefix);
    out.push_str(connector);
    out.push_str(kind_str);
    let field_s = field_label_str(field, options, field_name);
    out.push_str(&field_s);

    let label_len = kind_str.len() + field_s.len();
    if let Some(a) = align {
        if let Some(&max) = a.label_max_by_depth.get(depth) {
            if max > label_len {
                out.extend(std::iter::repeat_n(' ', max - label_len));
            }
        }
    }

    let loc = loc_str(range, options);
    out.push_str(&loc);
    if let Some(a) = align {
        if let Some(&max) = a.loc_max_by_depth.get(depth) {
            if max > loc.len() {
                out.extend(std::iter::repeat_n(' ', max - loc.len()));
            }
        }
    }
    if cycle {
        out.push_str(" [cycle]\n");
    } else {
        out.push('\n');
    }
}

fn format_node(
    node: &SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    field_name: &impl Fn(FieldId) -> Option<String>,
    align: Option<&Alignment>,
    prefix: &str,
    is_last: bool,
    field: Option<FieldId>,
    depth: usize,
    out: &mut String,
    visited: &mut HashSet<(usize, u32)>,
) {
    let id = node_id(node);
    let kind_str = kind_name(node.kind());
    if !visited.insert(id) {
        format_node_header(
            prefix,
            is_last,
            &kind_str,
            node.text_range(),
            field,
            options,
            field_name,
            align,
            depth,
            out,
            true,
        );
        return;
    }

    format_node_header(
        prefix,
        is_last,
        &kind_str,
        node.text_range(),
        field,
        options,
        field_name,
        align,
        depth,
        out,
        false,
    );

    let (_last, _mid, vert, blank) = connectors(options.style);
    let new_prefix = if is_last {
        format!("{prefix}{blank}")
    } else {
        format!("{prefix}{vert}")
    };

    format_node_children(
        node,
        options,
        kind_name,
        field_name,
        align,
        &new_prefix,
        depth + 1,
        out,
        visited,
    );
    visited.remove(&id);
}

fn format_node_children(
    node: &SyntaxNode,
    options: &TreeDisplayOptions,
    kind_name: &impl Fn(SyntaxKind) -> String,
    field_name: &impl Fn(FieldId) -> Option<String>,
    align: Option<&Alignment>,
    new_prefix: &str,
    child_depth: usize,
    out: &mut String,
    visited: &mut HashSet<(usize, u32)>,
) {
    if options.show_tokens {
        for child in visible_children(node, options) {
            match child.elem {
                SyntaxElement::Node(n) => {
                    format_node(
                        &n,
                        options,
                        kind_name,
                        field_name,
                        align,
                        new_prefix,
                        child.is_last,
                        child.field,
                        child_depth,
                        out,
                        visited,
                    );
                }
                SyntaxElement::Token(t) => {
                    format_token(
                        &t,
                        options,
                        kind_name,
                        field_name,
                        align,
                        new_prefix,
                        child.is_last,
                        child.field,
                        child_depth,
                        out,
                    );
                }
            }
        }
    } else {
        let child_nodes: Vec<SyntaxNode> = node.child_nodes().collect();
        let last_idx = child_nodes.len().saturating_sub(1);
        for (i, child) in child_nodes.into_iter().enumerate() {
            format_node(
                &child,
                options,
                kind_name,
                field_name,
                align,
                new_prefix,
                i == last_idx,
                None,
                child_depth,
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
    field_name: &impl Fn(FieldId) -> Option<String>,
    align: Option<&Alignment>,
    prefix: &str,
    is_last: bool,
    field: Option<FieldId>,
    depth: usize,
    out: &mut String,
) {
    let kind_str = kind_name(token.kind());
    let (last, mid, _vert, _blank) = connectors(options.style);
    let connector = if is_last { last } else { mid };
    out.push_str(prefix);
    out.push_str(connector);
    out.push_str(&kind_str);
    let field_s = field_label_str(field, options, field_name);
    out.push_str(&field_s);

    let label_len = kind_str.len() + field_s.len();
    if let Some(a) = align {
        if let Some(&max) = a.label_max_by_depth.get(depth) {
            if max > label_len {
                out.extend(std::iter::repeat_n(' ', max - label_len));
            }
        }
    }

    let loc = loc_str(token.text_range(), options);
    out.push_str(&loc);
    if let Some(a) = align {
        if let Some(&max) = a.loc_max_by_depth.get(depth) {
            if max > loc.len() {
                out.extend(std::iter::repeat_n(' ', max - loc.len()));
            }
        }
    }
    let text = token.text();
    if !text.is_empty() {
        let display = format_token_preview(text, options.max_token_text_len);
        out.push_str("  ");
        out.push_str(&display);
    }
    if token.is_trivia() {
        out.push_str(" (trivia)");
    }
    out.push('\n');
}

fn format_token_preview(text: &str, max_len: usize) -> String {
    // Keep output single-line and deterministic.
    let mut one_line = text.replace('\n', "\\n").replace('\r', "\\r");

    // Avoid dumping huge runs of whitespace; still keep it unambiguous.
    // (We don't aggressively normalize spaces because that can hide syntax.)
    if one_line.len() > max_len {
        one_line.truncate(char_boundary_floor(&one_line, max_len));
        one_line.push_str("...");
    }

    format!("{one_line:?}")
}

fn char_boundary_floor(s: &str, mut idx: usize) -> usize {
    idx = idx.min(s.len());
    while idx > 0 && !s.is_char_boundary(idx) {
        idx -= 1;
    }
    idx
}
