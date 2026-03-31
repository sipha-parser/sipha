//! Incremental reparse: reuse unchanged parts of the green tree after a text edit.
//!
//! Full re-parse with reuse: run the parser on the new source, then when building
//! the green tree, reuse `Arc<GreenToken>` and `Arc<GreenNode>` from the old tree
//! when the new parse produces the same span in unchanged regions.
//!
//! Enable the **`incremental`** Cargo feature to use this module.

use std::collections::HashMap;
use std::sync::Arc;

use crate::parse::engine::{Engine, ParseError, ParseOutput};
use crate::parse::insn::ParseGraph;
use crate::tree::green::{GreenElement, GreenNode, GreenToken};
use crate::tree::red::SyntaxNode;
use crate::types::{FieldId, Pos, SyntaxKind, TreeEvent};

/// Stack element when building green tree from events (with or without reuse).
type IncrementalStackEntry = (
    SyntaxKind,
    Option<FieldId>,
    Pos, // NodeOpen `pos` — start byte in the new source
    Vec<(Option<FieldId>, GreenElement)>,
);
/// Root or child element: (optional field id, node or token).
type IncrementalRootElem = (Option<FieldId>, GreenElement);

/// A text edit: replace `old_source[start..end]` with `new_text`.
#[derive(Clone, Debug)]
pub struct TextEdit {
    /// Start byte offset in the old source (inclusive).
    pub start: Pos,
    /// End byte offset in the old source (exclusive).
    pub end: Pos,
    /// Replacement bytes (UTF-8).
    pub new_text: Vec<u8>,
}

impl TextEdit {
    /// Build the new source buffer after applying this edit.
    #[must_use]
    pub fn apply(&self, old_source: &[u8]) -> Vec<u8> {
        let mut out = Vec::with_capacity(
            old_source
                .len()
                .saturating_sub((self.end - self.start) as usize)
                + self.new_text.len(),
        );
        out.extend_from_slice(&old_source[..self.start as usize]);
        out.extend_from_slice(&self.new_text);
        out.extend_from_slice(&old_source[self.end as usize..]);
        out
    }

    /// Apply several **non-overlapping** edits in ascending byte order. Each
    /// `start`/`end` refers to the **original** `old_source` (before any edit).
    ///
    /// # Panics
    ///
    /// Debug builds: panics if edits overlap or are not sorted by `start`.
    #[must_use]
    pub fn apply_edits(old_source: &[u8], edits: &[Self]) -> Vec<u8> {
        if edits.is_empty() {
            return old_source.to_vec();
        }
        let mut sorted: Vec<&Self> = edits.iter().collect();
        sorted.sort_by_key(|e| e.start);
        debug_assert!(
            sorted.windows(2).all(|w| w[0].end <= w[1].start),
            "non-overlapping edits required"
        );
        let mut out = Vec::with_capacity(
            old_source.len() + sorted.iter().map(|e| e.new_text.len()).sum::<usize>()
                - sorted
                    .iter()
                    .map(|e| (e.end - e.start) as usize)
                    .sum::<usize>(),
        );
        let mut pos = 0usize;
        for e in &sorted {
            out.extend_from_slice(&old_source[pos..e.start as usize]);
            out.extend_from_slice(&e.new_text);
            pos = e.end as usize;
        }
        out.extend_from_slice(&old_source[pos..]);
        out
    }
}

/// Result of [`reparse_with_output`]: parse output plus optional red-tree root.
#[derive(Debug)]
pub struct ReparseResult {
    pub parse_output: ParseOutput,
    pub root: Option<SyntaxNode>,
}

/// Statistics about green-tree reuse during incremental rebuild.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ReuseStats {
    /// Total token events that produced a non-empty token.
    pub tokens_total: u32,
    /// Tokens reused from the old green tree (`Arc::ptr_eq`).
    pub tokens_reused: u32,
    /// Total nodes closed (i.e. `TreeEvent::NodeClose` successfully produced a node).
    pub nodes_total: u32,
    /// Nodes reused from the old green tree (`Arc::ptr_eq`).
    pub nodes_reused: u32,
}

/// Sort `edits` by `start` (same order as [`TextEdit::apply_edits`]).
fn sorted_edits_refs<'a>(edits: &'a [TextEdit]) -> Vec<&'a TextEdit> {
    let mut v: Vec<&'a TextEdit> = edits.iter().collect();
    v.sort_by_key(|e| e.start);
    v
}

/// Map a span in the *new* source to the corresponding span in the *old* source
/// if that range lies entirely in an **unchanged** region (same invariants as
/// [`TextEdit::apply_edits`]: non-overlapping edits, `edits_sorted` by `start`).
fn new_span_to_old_multi(
    new_start: Pos,
    new_end: Pos,
    edits_sorted: &[&TextEdit],
    old_len: usize,
) -> Option<(Pos, Pos)> {
    let old_len_u = u32::try_from(old_len).ok()?;
    if edits_sorted.is_empty() {
        if new_end <= new_start {
            return Some((new_start, new_end));
        }
        if new_end <= old_len_u {
            return Some((new_start, new_end));
        }
        return None;
    }
    debug_assert!(
        edits_sorted.windows(2).all(|w| w[0].end <= w[1].start),
        "non-overlapping edits required"
    );

    let mut new_cursor = 0u32;
    let mut old_cursor = 0u32;

    for e in edits_sorted {
        let unchanged_len = e.start.saturating_sub(old_cursor);
        let seg_new_lo = new_cursor;
        let seg_new_hi = new_cursor + unchanged_len;

        if new_start >= seg_new_lo && new_end <= seg_new_hi {
            let old_s = old_cursor + (new_start - seg_new_lo);
            let old_e = old_s + (new_end - new_start);
            return Some((old_s, old_e));
        }

        if new_start < seg_new_hi && new_end > seg_new_lo {
            return None;
        }

        new_cursor += unchanged_len;
        // Old bytes [e.start, e.end) are replaced by new_text; map only `new_cursor` past that.

        let repl_len = u32::try_from(e.new_text.len()).ok()?;
        let repl_lo = new_cursor;

        if new_end > repl_lo && new_start < repl_lo + repl_len {
            return None;
        }

        new_cursor += repl_len;
        old_cursor = e.end;
    }

    let tail_old_len = old_len_u.saturating_sub(old_cursor);
    let seg_new_lo = new_cursor;
    let seg_new_hi = new_cursor + tail_old_len;

    if new_start >= seg_new_lo && new_end <= seg_new_hi {
        let old_s = old_cursor + (new_start - seg_new_lo);
        let old_e = old_s + (new_end - new_start);
        return Some((old_s, old_e));
    }

    None
}

/// Remove and return trailing trivia from `children` (from the end, while elements are trivia).
/// Must match `green::drain_trailing_trivia` so incremental tree structure matches full parse.
fn drain_trailing_trivia(children: &mut Vec<IncrementalRootElem>) -> Vec<IncrementalRootElem> {
    let n = children
        .iter()
        .rev()
        .take_while(|(_, el)| el.is_trivia())
        .count();
    if n == 0 {
        return Vec::new();
    }
    children.drain(children.len() - n..).collect()
}

/// Build a map from (`old_start`, `old_end`) to the green token at that span by walking the tree.
fn old_tree_token_map(root: &GreenNode) -> HashMap<(Pos, Pos), Arc<GreenToken>> {
    let mut map = HashMap::new();
    let mut stack: Vec<(&GreenNode, Pos)> = vec![(root, 0)];
    while let Some((node, offset)) = stack.pop() {
        let mut off = offset;
        for child in &node.children {
            let start = off;
            off += child.text_len();
            match child {
                GreenElement::Token(t) => {
                    map.insert((start, off), Arc::clone(t));
                }
                GreenElement::Node(n) => {
                    stack.push((n.as_ref(), start));
                }
            }
        }
    }
    map
}

fn old_tree_node_map_walk(
    node: &Arc<GreenNode>,
    start: Pos,
    map: &mut HashMap<(Pos, Pos, SyntaxKind), Arc<GreenNode>>,
) {
    let end = start + node.text_len;
    map.insert((start, end, node.kind), Arc::clone(node));
    let mut off = start;
    for child in &node.children {
        match child {
            GreenElement::Token(t) => {
                off += t.text_len;
            }
            GreenElement::Node(n) => {
                old_tree_node_map_walk(n, off, map);
                off += n.text_len;
            }
        }
    }
}

/// Map `(byte_start, byte_end, kind)` to each [`GreenNode`] in the old tree.
///
/// `kind` is included so nested nodes that cover the same byte span (e.g. outer and inner
/// around a single token cluster) do not collide.
fn old_tree_node_map(root: &Arc<GreenNode>) -> HashMap<(Pos, Pos, SyntaxKind), Arc<GreenNode>> {
    let mut map = HashMap::new();
    old_tree_node_map_walk(root, 0, &mut map);
    map
}

#[inline]
fn field_matches(child_fields: Option<&[Option<FieldId>]>, i: usize, f: Option<FieldId>) -> bool {
    child_fields.map_or_else(|| f.is_none(), |cf| cf.get(i).copied().flatten() == f)
}

/// If every child is pointer-identical to `old_node`'s children, reuse `old_node`.
fn try_reuse_green_node(
    kind: SyntaxKind,
    children_with_fields: &[(Option<FieldId>, GreenElement)],
    old_node: &Arc<GreenNode>,
) -> Option<Arc<GreenNode>> {
    if old_node.kind != kind {
        return None;
    }
    if children_with_fields.len() != old_node.children.len() {
        return None;
    }
    for (i, ((f, el), old_el)) in children_with_fields
        .iter()
        .zip(old_node.children.iter())
        .enumerate()
    {
        if !field_matches(old_node.child_fields.as_deref(), i, *f) {
            return None;
        }
        match (el, old_el) {
            (GreenElement::Token(a), GreenElement::Token(b)) if Arc::ptr_eq(a, b) => {}
            (GreenElement::Node(a), GreenElement::Node(b)) if Arc::ptr_eq(a, b) => {}
            _ => return None,
        }
    }
    Some(Arc::clone(old_node))
}

/// Build a green tree from events, reusing tokens and whole nodes from the old tree
/// when the new parse produces the same span in an unchanged region.
///
/// `edits` must be **non-overlapping** in the original `old_source` (same as
/// [`TextEdit::apply_edits`]); order does not matter (sorted internally).
#[must_use]
pub fn build_green_tree_with_reuse(
    new_source: &[u8],
    events: &[TreeEvent],
    old_root: &Arc<GreenNode>,
    old_source: &[u8],
    edits: &[TextEdit],
) -> Option<Arc<GreenNode>> {
    build_green_tree_with_reuse_stats(new_source, events, old_root, old_source, edits).map(|r| r.0)
}

/// Like [`build_green_tree_with_reuse`], but also returns [`ReuseStats`].
#[must_use]
pub fn build_green_tree_with_reuse_stats(
    new_source: &[u8],
    events: &[TreeEvent],
    old_root: &Arc<GreenNode>,
    old_source: &[u8],
    edits: &[TextEdit],
) -> Option<(Arc<GreenNode>, ReuseStats)> {
    let token_map = old_tree_token_map(old_root.as_ref());
    let node_map = old_tree_node_map(old_root);
    let sorted = sorted_edits_refs(edits);
    let old_len = old_source.len();

    let mut stack: Vec<IncrementalStackEntry> = Vec::new();
    let mut roots: Vec<IncrementalRootElem> = Vec::new();
    let mut stats = ReuseStats::default();

    for ev in events {
        match *ev {
            TreeEvent::NodeOpen { kind, field, pos } => {
                // Move trailing trivia from the current top node into the new node as leading trivia.
                // Must match build_green_tree so tree structure (and thus token positions) is identical to full parse.
                let leading = if let Some((_, _, _, children)) = stack.last_mut() {
                    drain_trailing_trivia(children)
                } else {
                    Vec::new()
                };
                stack.push((kind, field, pos, leading));
            }

            TreeEvent::NodeClose { pos: end } => {
                let (kind, my_field, open_start, children_with_fields) = stack.pop()?;
                stats.nodes_total = stats.nodes_total.saturating_add(1);
                let (node, reused) = if let Some((old_s, old_e)) =
                    new_span_to_old_multi(open_start, end, &sorted, old_len)
                {
                    if let Some(old_node) = node_map.get(&(old_s, old_e, kind)) {
                        if let Some(n) = try_reuse_green_node(kind, &children_with_fields, old_node)
                        {
                            (n, true)
                        } else {
                            (
                                GreenNode::new_with_fields(kind, children_with_fields),
                                false,
                            )
                        }
                    } else {
                        (
                            GreenNode::new_with_fields(kind, children_with_fields),
                            false,
                        )
                    }
                } else {
                    (
                        GreenNode::new_with_fields(kind, children_with_fields),
                        false,
                    )
                };
                if reused {
                    stats.nodes_reused = stats.nodes_reused.saturating_add(1);
                }
                push_element(&mut stack, &mut roots, (my_field, GreenElement::Node(node)));
            }

            TreeEvent::Token {
                kind,
                start,
                end,
                is_trivia,
            } => {
                if start == end {
                    continue;
                }
                stats.tokens_total = stats.tokens_total.saturating_add(1);
                let tok_arc = if let Some((old_s, old_e)) =
                    new_span_to_old_multi(start, end, &sorted, old_len)
                {
                    token_map.get(&(old_s, old_e)).cloned()
                } else {
                    None
                };
                let (tok, reused) = match tok_arc.as_ref() {
                    Some(t) if t.kind == kind && t.is_trivia == is_trivia => (Arc::clone(t), true),
                    _ => {
                        let text = new_source
                            .get(start as usize..end as usize)
                            .and_then(|b| std::str::from_utf8(b).ok())
                            .unwrap_or("");
                        (GreenToken::new(kind, text, is_trivia), false)
                    }
                };
                if reused {
                    stats.tokens_reused = stats.tokens_reused.saturating_add(1);
                }
                push_element(&mut stack, &mut roots, (None, GreenElement::Token(tok)));
            }
        }
    }

    if !stack.is_empty() {
        return None;
    }

    match roots.len() {
        0 => None,
        1 => {
            let (_, elem) = roots.remove(0);
            match elem {
                GreenElement::Node(n) => Some((n, stats)),
                GreenElement::Token(t) => {
                    Some((GreenNode::new(t.kind, vec![GreenElement::Token(t)]), stats))
                }
            }
        }
        _ => {
            let children_with_fields: Vec<IncrementalRootElem> = roots.into_iter().collect();
            Some((
                GreenNode::new_with_fields(u16::MAX, children_with_fields),
                stats,
            ))
        }
    }
}

#[inline]
fn push_element(
    stack: &mut [IncrementalStackEntry],
    roots: &mut Vec<IncrementalRootElem>,
    elem: IncrementalRootElem,
) {
    match stack.last_mut() {
        Some((_, _, _, children)) => children.push(elem),
        None => roots.push(elem),
    }
}

/// Reparse after one or more text edits: build new source with [`TextEdit::apply_edits`], run the
/// parser, then build the green tree reusing unchanged tokens and nodes from the old tree.
///
/// Returns the new syntax root, or `None` if the parse produced no root (e.g. empty or error).
///
/// # Errors
///
/// Propagates [`ParseError`] from the parser if the new source fails to parse.
pub fn reparse(
    engine: &mut Engine,
    graph: &ParseGraph<'_>,
    old_source: &[u8],
    old_root: &SyntaxNode,
    edits: &[TextEdit],
) -> Result<Option<SyntaxNode>, ParseError> {
    Ok(reparse_with_output(engine, graph, old_source, old_root, edits)?.root)
}

/// Like [`reparse`], but returns [`ParseOutput`] (events, consumed length) alongside the new root.
///
/// # Errors
///
/// Propagates [`ParseError`] from the parser if the new source fails to parse.
pub fn reparse_with_output(
    engine: &mut Engine,
    graph: &ParseGraph<'_>,
    old_source: &[u8],
    old_root: &SyntaxNode,
    edits: &[TextEdit],
) -> Result<ReparseResult, ParseError> {
    Ok(reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?.0)
}

/// Like [`reparse_with_output`], but also returns [`ReuseStats`].
///
/// # Errors
///
/// Propagates [`ParseError`] from the parser if the new source fails to parse.
pub fn reparse_with_output_and_stats(
    engine: &mut Engine,
    graph: &ParseGraph<'_>,
    old_source: &[u8],
    old_root: &SyntaxNode,
    edits: &[TextEdit],
) -> Result<(ReparseResult, ReuseStats), ParseError> {
    let new_source = TextEdit::apply_edits(old_source, edits);
    let parse_output = engine.parse(graph, &new_source)?;
    let new_green = build_green_tree_with_reuse_stats(
        &new_source,
        &parse_output.tree_events,
        old_root.green(),
        old_source,
        edits,
    );
    let (new_green, stats) = new_green
        .map(|(g, s)| (Some(g), s))
        .unwrap_or((None, ReuseStats::default()));
    Ok((
        ReparseResult {
            parse_output,
            root: new_green.map(SyntaxNode::new_root),
        },
        stats,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree::green::build_green_tree;
    use crate::types::TreeEvent;

    #[test]
    fn text_edit_apply() {
        let old = b"hello world";
        let edit = TextEdit {
            start: 0,
            end: 5,
            new_text: b"hi".to_vec(),
        };
        let new = edit.apply(old);
        assert_eq!(new.as_slice(), b"hi world");
    }

    #[test]
    fn apply_edits_non_overlapping() {
        let old = b"0123456789";
        let edits = [
            TextEdit {
                start: 1,
                end: 2,
                new_text: b"xx".to_vec(),
            },
            TextEdit {
                start: 7,
                end: 8,
                new_text: b"y".to_vec(),
            },
        ];
        let out = TextEdit::apply_edits(old, &edits);
        assert_eq!(out.as_slice(), b"0xx23456y89");
    }

    #[test]
    fn new_span_maps_two_edits() {
        let edits = [
            TextEdit {
                start: 1,
                end: 2,
                new_text: b"xx".to_vec(),
            },
            TextEdit {
                start: 7,
                end: 8,
                new_text: b"y".to_vec(),
            },
        ];
        let sorted = sorted_edits_refs(&edits);
        assert_eq!(new_span_to_old_multi(3, 4, &sorted, 10), Some((2, 3)));
        assert_eq!(new_span_to_old_multi(0, 1, &sorted, 10), Some((0, 1)));
        assert_eq!(new_span_to_old_multi(2, 3, &sorted, 10), None);
    }

    #[test]
    fn reparse_reuses_unchanged_tokens() {
        let old_source = b"ab";
        let events_old = [
            TreeEvent::NodeOpen {
                kind: 1,
                field: None,
                pos: 0,
            },
            TreeEvent::Token {
                kind: 10,
                start: 0,
                end: 1,
                is_trivia: false,
            },
            TreeEvent::Token {
                kind: 10,
                start: 1,
                end: 2,
                is_trivia: false,
            },
            TreeEvent::NodeClose { pos: 2 },
        ];
        let old_root = build_green_tree(old_source, &events_old).expect("build");
        let edit = TextEdit {
            start: 1,
            end: 2,
            new_text: b"xy".to_vec(),
        };
        let new_source = edit.apply(old_source);
        assert_eq!(new_source.as_slice(), b"axy");
        let events_new = [
            TreeEvent::NodeOpen {
                kind: 1,
                field: None,
                pos: 0,
            },
            TreeEvent::Token {
                kind: 10,
                start: 0,
                end: 1,
                is_trivia: false,
            },
            TreeEvent::Token {
                kind: 10,
                start: 1,
                end: 3,
                is_trivia: false,
            },
            TreeEvent::NodeClose { pos: 3 },
        ];
        let new_root = build_green_tree_with_reuse(
            &new_source,
            &events_new,
            &old_root,
            old_source,
            std::slice::from_ref(&edit),
        )
        .expect("build_with_reuse");
        assert_eq!(new_root.text_len, 3);
        let first_tok = match &new_root.children[0] {
            GreenElement::Token(t) => t.as_ref(),
            _ => panic!("expected token"),
        };
        let second_tok = match &new_root.children[1] {
            GreenElement::Token(t) => t.as_ref(),
            _ => panic!("expected token"),
        };
        assert_eq!(first_tok.text(), "a");
        assert_eq!(second_tok.text(), "xy");
        if let (GreenElement::Token(new_t), GreenElement::Token(old_t)) =
            (&new_root.children[0], &old_root.children[0])
        {
            assert!(
                Arc::ptr_eq(new_t, old_t),
                "first token (unchanged span) should be reused"
            );
        }
    }

    #[test]
    fn reparse_reuses_unchanged_inner_node() {
        // outer( prefix_tok, inner( "bc" ) ) — edit only the prefix so inner subtree is identical.
        let old_source = b"abc";
        let events_old = [
            TreeEvent::NodeOpen {
                kind: 1,
                field: None,
                pos: 0,
            },
            TreeEvent::Token {
                kind: 20,
                start: 0,
                end: 1,
                is_trivia: false,
            },
            TreeEvent::NodeOpen {
                kind: 2,
                field: None,
                pos: 1,
            },
            TreeEvent::Token {
                kind: 10,
                start: 1,
                end: 3,
                is_trivia: false,
            },
            TreeEvent::NodeClose { pos: 3 },
            TreeEvent::NodeClose { pos: 3 },
        ];
        let old_root = build_green_tree(old_source, &events_old).expect("build");
        let edit = TextEdit {
            start: 0,
            end: 1,
            new_text: b"x".to_vec(),
        };
        let new_source = edit.apply(old_source);
        assert_eq!(new_source.as_slice(), b"xbc");
        let events_new = [
            TreeEvent::NodeOpen {
                kind: 1,
                field: None,
                pos: 0,
            },
            TreeEvent::Token {
                kind: 20,
                start: 0,
                end: 1,
                is_trivia: false,
            },
            TreeEvent::NodeOpen {
                kind: 2,
                field: None,
                pos: 1,
            },
            TreeEvent::Token {
                kind: 10,
                start: 1,
                end: 3,
                is_trivia: false,
            },
            TreeEvent::NodeClose { pos: 3 },
            TreeEvent::NodeClose { pos: 3 },
        ];
        let new_root = build_green_tree_with_reuse(
            &new_source,
            &events_new,
            &old_root,
            old_source,
            std::slice::from_ref(&edit),
        )
        .expect("build_with_reuse");
        let inner_old = match &old_root.children[1] {
            GreenElement::Node(n) => n,
            _ => panic!("expected inner node"),
        };
        let inner_new = match &new_root.children[1] {
            GreenElement::Node(n) => n,
            _ => panic!("expected inner node"),
        };
        assert!(
            Arc::ptr_eq(inner_old, inner_new),
            "inner node fully unchanged should be reused"
        );
    }
}
