//! Scope extents: map from byte offset to scope for LSP (go-to-def, references, completion).
//!
//! Walks the tree and records (`ScopeId`, extent) for each scope-creating node.
//! Grammar-agnostic: the caller supplies a predicate that identifies which nodes create scopes.

use sipha::red::SyntaxNode;
use sipha::walk::{Visitor, WalkOptions, WalkResult};

/// Build list of (`scope_id`, extent) where extent is (`start_byte`, `end_byte`).
///
/// Root scope is always first with extent `(0, source_len)`. Remaining entries
/// are in walk order, matching `scope_id_sequence`. `is_scope_creating` is
/// called for each node; when it returns true, the next scope ID from
/// `scope_id_sequence` is associated with that node's text range.
///
/// # Type parameter
///
/// `S` is the scope identifier type (e.g. `ScopeId(usize)`). It must be `Copy`
/// so it can be stored in the returned list.
#[must_use]
pub fn build_scope_extents<S: Copy>(
    root: &SyntaxNode,
    root_scope_id: S,
    scope_id_sequence: &[S],
    source_len: usize,
    is_scope_creating: impl Fn(&SyntaxNode) -> bool,
) -> Vec<(S, (u32, u32))> {
    let mut extents = vec![(
        root_scope_id,
        (0u32, u32::try_from(source_len).unwrap_or(u32::MAX)),
    )];
    let mut index = 0usize;
    let mut visitor = ScopeExtentVisitor {
        scope_id_sequence,
        extents: &mut extents,
        index: &mut index,
        is_scope_creating: &is_scope_creating,
    };
    let options = WalkOptions::nodes_only();
    let _ = root.walk(&mut visitor, &options);
    extents
}

/// Find the innermost scope containing the given byte offset.
///
/// Returns the scope ID of the smallest extent that contains `offset`.
/// If no extent contains the offset, returns the root scope (first element's ID) when
/// `extents` is non-empty, otherwise returns `default`.
/// If `extents` is empty (e.g. from a future caller or partial state), returns `default`
/// so that the LSP never panics.
#[must_use]
pub fn scope_at_offset<S: Copy>(extents: &[(S, (u32, u32))], offset: u32, default: S) -> S {
    let mut best: Option<(S, u32)> = None;
    for (scope_id, (start, end)) in extents {
        if *start <= offset && offset < *end {
            let len = end - start;
            if best.is_none_or(|(_, best_len)| len < best_len) {
                best = Some((*scope_id, len));
            }
        }
    }
    best.map(|(id, _)| id)
        .or_else(|| extents.first().map(|(id, _)| *id))
        .unwrap_or(default)
}

struct ScopeExtentVisitor<'a, S, F> {
    scope_id_sequence: &'a [S],
    extents: &'a mut Vec<(S, (u32, u32))>,
    index: &'a mut usize,
    is_scope_creating: &'a F,
}

impl<S: Copy, F: Fn(&SyntaxNode) -> bool> Visitor for ScopeExtentVisitor<'_, S, F> {
    fn enter_node(&mut self, node: &SyntaxNode) -> WalkResult {
        if (self.is_scope_creating)(node) {
            if let Some(&scope_id) = self.scope_id_sequence.get(*self.index) {
                let range = node.text_range();
                self.extents.push((scope_id, (range.start, range.end)));
            }
            *self.index += 1;
        }
        WalkResult::Continue(())
    }
}
