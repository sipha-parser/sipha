use crate::types::{Pos, SyntaxKind, TreeEvent};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// Close open nodes and insert an error node so the event list is well-nested.
pub(super) fn insert_error_node_events(
    tree_events: &mut Vec<TreeEvent>,
    furthest: Pos,
    error_kind: SyntaxKind,
) {
    let mut depth = 0u32;
    for e in tree_events.iter() {
        match e {
            TreeEvent::NodeOpen { .. } => depth = depth.saturating_add(1),
            TreeEvent::NodeClose { .. } => depth = depth.saturating_sub(1),
            TreeEvent::Token { .. } => {}
        }
    }
    for _ in 0..depth {
        tree_events.push(TreeEvent::NodeClose { pos: furthest });
    }
    tree_events.push(TreeEvent::NodeOpen {
        kind: error_kind,
        field: None,
        pos: furthest,
    });
    tree_events.push(TreeEvent::NodeClose { pos: furthest });
}
