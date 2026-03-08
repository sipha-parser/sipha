//! Incremental reparse: reuse unchanged parts of the green tree after a text edit.
//!
//! Full re-parse with reuse: run the parser on the new source, then when building
//! the green tree, reuse `Arc<GreenToken>` and `Arc<GreenNode>` from the old tree
//! when the new parse produces the same span (in unchanged regions).

use std::collections::HashMap;
use std::sync::Arc;

use crate::engine::{Engine, ParseError};
use crate::green::{GreenElement, GreenNode, GreenToken};
use crate::insn::ParseGraph;
use crate::red::SyntaxNode;
use crate::types::{FieldId, Pos, SyntaxKind, TreeEvent};

/// Stack element when building green tree from events (with or without reuse).
type IncrementalStackEntry = (
    SyntaxKind,
    Option<FieldId>,
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
}

/// Map a span in the *new* source to the corresponding span in the *old* source
/// if that range is unchanged; otherwise return `None`.
#[allow(clippy::cast_possible_truncation)] // const fn; new_text_len is small in practice
const fn new_span_to_old(
    new_start: Pos,
    new_end: Pos,
    edit_start: Pos,
    edit_end: Pos,
    new_text_len: usize,
) -> Option<(Pos, Pos)> {
    let new_text_len_u = new_text_len as u32;
    let delta = (edit_start + new_text_len_u).saturating_sub(edit_end);
    if new_end <= edit_start {
        Some((new_start, new_end))
    } else if new_start >= edit_start + new_text_len_u {
        Some((new_start - delta, new_end - delta))
    } else {
        None
    }
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
fn old_tree_token_map(
    root: &GreenNode,
    _old_source: &[u8],
) -> HashMap<(Pos, Pos), Arc<GreenToken>> {
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

/// Build a green tree from events, reusing tokens (and optionally nodes) from the old tree
/// when the new parse produces the same span in an unchanged region.
#[must_use]
pub fn build_green_tree_with_reuse(
    new_source: &[u8],
    events: &[TreeEvent],
    old_root: &GreenNode,
    old_source: &[u8],
    edit: &TextEdit,
) -> Option<Arc<GreenNode>> {
    let token_map = old_tree_token_map(old_root, old_source);
    let edit_start = edit.start;
    let edit_end = edit.end;
    let new_text_len = edit.new_text.len();

    let mut stack: Vec<IncrementalStackEntry> = Vec::new();
    let mut roots: Vec<IncrementalRootElem> = Vec::new();

    for ev in events {
        match *ev {
            TreeEvent::NodeOpen { kind, field, .. } => {
                // Move trailing trivia from the current top node into the new node as leading trivia.
                // Must match build_green_tree so tree structure (and thus token positions) is identical to full parse.
                let leading = if let Some((_, _, children)) = stack.last_mut() {
                    drain_trailing_trivia(children)
                } else {
                    Vec::new()
                };
                stack.push((kind, field, leading));
            }

            TreeEvent::NodeClose { .. } => {
                let (kind, my_field, children_with_fields) = stack.pop()?;
                let node = GreenNode::new_with_fields(kind, children_with_fields);
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
                let tok_arc = if let Some((old_s, old_e)) =
                    new_span_to_old(start, end, edit_start, edit_end, new_text_len)
                {
                    token_map.get(&(old_s, old_e)).cloned()
                } else {
                    None
                };
                let tok = match tok_arc {
                    Some(t) if t.kind == kind && t.is_trivia == is_trivia => t,
                    _ => {
                        let text = new_source
                            .get(start as usize..end as usize)
                            .and_then(|b| std::str::from_utf8(b).ok())
                            .unwrap_or("");
                        GreenToken::new(kind, text, is_trivia)
                    }
                };
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
                GreenElement::Node(n) => Some(n),
                GreenElement::Token(t) => {
                    Some(GreenNode::new(t.kind, vec![GreenElement::Token(t)]))
                }
            }
        }
        _ => {
            let children_with_fields: Vec<IncrementalRootElem> = roots.into_iter().collect();
            Some(GreenNode::new_with_fields(u16::MAX, children_with_fields))
        }
    }
}

#[inline]
#[allow(clippy::ptr_arg)] // need Vec for .push
fn push_element(
    stack: &mut Vec<IncrementalStackEntry>,
    roots: &mut Vec<IncrementalRootElem>,
    elem: IncrementalRootElem,
) {
    match stack.last_mut() {
        Some((_, _, children)) => children.push(elem),
        None => roots.push(elem),
    }
}

/// Reparse after a text edit: build new source, run the parser, then build the green tree
/// reusing unchanged tokens from the old tree.
///
/// Returns the new syntax root, or `None` if the parse produced no root (e.g. empty or error).
///
/// # Errors
///
/// Propagates [`ParseError`] from the parser if the new source fails to parse.
pub fn reparse(
    engine: &mut Engine,
    graph: &ParseGraph,
    old_source: &[u8],
    old_root: &SyntaxNode,
    edit: &TextEdit,
) -> Result<Option<SyntaxNode>, ParseError> {
    let new_source = edit.apply(old_source);
    let out = engine.parse(graph, &new_source)?;
    let new_green = build_green_tree_with_reuse(
        &new_source,
        &out.tree_events,
        old_root.green(),
        old_source,
        edit,
    );
    Ok(new_green.map(SyntaxNode::new_root))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::green::build_green_tree;
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
        let new_root =
            build_green_tree_with_reuse(&new_source, &events_new, &old_root, old_source, &edit)
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
}
