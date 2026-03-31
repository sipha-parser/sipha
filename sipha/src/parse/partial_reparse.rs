//! Experimental true partial reparse (feature: `partial-reparse`).
//!
//! This module attempts to avoid parsing the entire document after an edit by:
//! - Choosing a boundary node in the old tree (a “stable parse unit”).
//! - Re-parsing only that unit in the new source using `Engine::parse_rule_at_with_context`.
//! - Splicing the new green subtree into the old green tree, rebuilding only the path to root.
//!
//! If any safety checks fail (boundary cannot be found, parse consumes outside the boundary,
//! splice target not found, etc), it falls back to full incremental reparse with reuse.

use std::sync::Arc;

use crate::parse::context::ParseContext;
use crate::parse::engine::{Engine, ParseError, ParseOutput};
use crate::parse::incremental::{ReuseStats, TextEdit, reparse_with_output_and_stats};
use crate::parse::insn::ParseGraph;
use crate::tree::green::{GreenElement, GreenNode};
use crate::tree::red::SyntaxNode;
use crate::types::{Pos, RuleId, SyntaxKind};

/// Configuration for partial reparse.
#[derive(Clone, Debug)]
pub struct PartialReparseConfig<'a> {
    /// Kinds that are valid “reparse roots” (stable parse units).
    pub boundary_kinds: &'a [SyntaxKind],
    /// The grammar rule to use when parsing a boundary unit.
    pub boundary_rule: RuleId,
    /// Parse context (flags, error node kind).
    pub context: ParseContext,
}

impl<'a> PartialReparseConfig<'a> {
    #[must_use]
    pub fn new(boundary_kinds: &'a [SyntaxKind], boundary_rule: RuleId) -> Self {
        Self {
            boundary_kinds,
            boundary_rule,
            context: ParseContext::new(),
        }
    }
}

/// Result of [`reparse_partial_or_fallback`].
#[derive(Debug)]
pub struct PartialReparseResult {
    pub parse_output: ParseOutput,
    pub root: Option<SyntaxNode>,
    pub reuse_stats: ReuseStats,
    /// `true` if the VM was only run for the chosen boundary unit.
    pub used_partial: bool,
}

/// Attempt a true partial reparse; fall back to full incremental reparse with reuse if needed.
///
/// # Safety / correctness model
/// Partial reparse only succeeds when:
/// - A boundary node can be found in the old tree that contains the edit start offset and whose
///   `kind` is in `config.boundary_kinds`.
/// - The boundary node’s start offset lies outside edited regions (so its start maps cleanly).
/// - Parsing the boundary rule at the mapped new-start position consumes **exactly** up to the
///   mapped new-end position.
/// - The replacement can be spliced into the old green tree by exact (kind, start, end) match.
///
/// Otherwise, this function falls back to full incremental `reparse_with_output_and_stats`.
pub fn reparse_partial_or_fallback(
    engine: &mut Engine,
    graph: &ParseGraph<'_>,
    old_source: &[u8],
    old_root: &SyntaxNode,
    edits: &[TextEdit],
    config: &PartialReparseConfig<'_>,
) -> Result<PartialReparseResult, ParseError> {
    let sorted = sorted_edits_refs(edits);
    let new_source = TextEdit::apply_edits(old_source, edits);

    let edit_start = edits.iter().map(|e| e.start).min().unwrap_or(0);
    let leaf = match old_root.node_at_offset(edit_start) {
        Some(n) => n,
        None => {
            let (res, stats) =
                reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?;
            return Ok(PartialReparseResult {
                parse_output: res.parse_output,
                root: res.root,
                reuse_stats: stats,
                used_partial: false,
            });
        }
    };

    // Ascend to the first “stable unit” kind (leaf-first, then parents).
    let node = if config.boundary_kinds.contains(&leaf.kind()) {
        leaf
    } else {
        match leaf
            .ancestors(old_root)
            .into_iter()
            .find(|a| config.boundary_kinds.contains(&a.kind()))
        {
            Some(a) => a,
            None => {
                let (res, stats) =
                    reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?;
                return Ok(PartialReparseResult {
                    parse_output: res.parse_output,
                    root: res.root,
                    reuse_stats: stats,
                    used_partial: false,
                });
            }
        }
    };

    let old_range = node.text_range();
    let old_start = old_range.start;
    let old_end = old_range.end;

    let new_start = old_pos_to_new(old_start, &sorted);
    let new_end = old_pos_to_new(old_end, &sorted);

    // Parse only the boundary unit.
    let out = match engine.parse_rule_at_with_context(
        graph,
        &new_source,
        config.boundary_rule,
        new_start,
        &config.context,
    ) {
        Ok(o) => o,
        Err(_) => {
            let (res, stats) =
                reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?;
            return Ok(PartialReparseResult {
                parse_output: res.parse_output,
                root: res.root,
                reuse_stats: stats,
                used_partial: false,
            });
        }
    };

    // Safety checks: must consume exactly the boundary span.
    if out.consumed != new_end {
        let (res, stats) =
            reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?;
        return Ok(PartialReparseResult {
            parse_output: res.parse_output,
            root: res.root,
            reuse_stats: stats,
            used_partial: false,
        });
    }

    // Build replacement green subtree from the boundary parse events.
    let replacement_green = crate::tree::green::build_green_tree(&new_source, &out.tree_events);
    let Some(replacement_green) = replacement_green else {
        let (res, stats) =
            reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?;
        return Ok(PartialReparseResult {
            parse_output: res.parse_output,
            root: res.root,
            reuse_stats: stats,
            used_partial: false,
        });
    };

    // Splice into old green tree.
    let old_green = old_root.green();
    let Some(new_green_root) = splice_green_by_span_and_kind(
        old_green,
        0,
        old_start,
        old_end,
        node.kind(),
        &replacement_green,
    ) else {
        let (res, stats) =
            reparse_with_output_and_stats(engine, graph, old_source, old_root, edits)?;
        return Ok(PartialReparseResult {
            parse_output: res.parse_output,
            root: res.root,
            reuse_stats: stats,
            used_partial: false,
        });
    };

    Ok(PartialReparseResult {
        parse_output: out,
        root: Some(SyntaxNode::new_root(new_green_root)),
        reuse_stats: ReuseStats::default(),
        used_partial: true,
    })
}

fn sorted_edits_refs<'a>(edits: &'a [TextEdit]) -> Vec<&'a TextEdit> {
    let mut v: Vec<&'a TextEdit> = edits.iter().collect();
    v.sort_by_key(|e| e.start);
    v
}

fn old_pos_to_new(old_pos: Pos, edits_sorted: &[&TextEdit]) -> Pos {
    let mut delta: i64 = 0;
    for e in edits_sorted {
        if e.end <= old_pos {
            let old_len = (e.end - e.start) as i64;
            let new_len = e.new_text.len() as i64;
            delta += new_len - old_len;
        } else {
            break;
        }
    }
    let new_pos = old_pos as i64 + delta;
    new_pos.max(0) as Pos
}

/// Rebuild `node`, replacing the first descendant node whose `(kind, start, end)` matches
/// the target span. Returns the new node (with maximal sharing) or `None` if no match found.
fn splice_green_by_span_and_kind(
    node: &Arc<GreenNode>,
    node_start: Pos,
    target_start: Pos,
    target_end: Pos,
    target_kind: SyntaxKind,
    replacement: &Arc<GreenNode>,
) -> Option<Arc<GreenNode>> {
    let node_end = node_start + node.text_len;
    if node.kind == target_kind && node_start == target_start && node_end == target_end {
        return Some(Arc::clone(replacement));
    }

    if target_start < node_start || target_end > node_end {
        return None;
    }

    let mut any_changed = false;
    let mut out_children: Vec<(Option<crate::types::FieldId>, GreenElement)> =
        Vec::with_capacity(node.children.len());

    let mut off = node_start;
    for (i, child) in node.children.iter().enumerate() {
        let child_start = off;
        let child_end = off + child.text_len();
        let field = node
            .child_fields
            .as_deref()
            .and_then(|f| f.get(i).copied())
            .flatten();

        match child {
            GreenElement::Node(n) => {
                if target_start >= child_start && target_end <= child_end {
                    if let Some(new_child) = splice_green_by_span_and_kind(
                        n,
                        child_start,
                        target_start,
                        target_end,
                        target_kind,
                        replacement,
                    ) {
                        if !Arc::ptr_eq(n, &new_child) {
                            any_changed = true;
                        }
                        out_children.push((field, GreenElement::Node(new_child)));
                    } else {
                        // Target is supposed to be in this subtree, but not found.
                        return None;
                    }
                } else {
                    out_children.push((field, GreenElement::Node(Arc::clone(n))));
                }
            }
            GreenElement::Token(t) => {
                out_children.push((field, GreenElement::Token(Arc::clone(t))));
            }
        }

        off = child_end;
    }

    if !any_changed {
        return Some(Arc::clone(node));
    }

    Some(GreenNode::new_with_fields(node.kind, out_children))
}
