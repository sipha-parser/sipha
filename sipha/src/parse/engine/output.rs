use crate::types::{CaptureEvent, Pos, TreeEvent};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::sync::Arc;

#[derive(Debug)]
pub struct ParseOutput {
    /// Number of input bytes consumed (always equals `input.len()` if the
    /// grammar ends with `end_of_input`).
    pub consumed: Pos,
    /// Legacy flat capture events. Use `CaptureNode::build_forest` to get
    /// the capture tree.
    pub events: Vec<CaptureEvent>,
    /// Green/red tree events. Pass to `green::build_green_tree` and wrap
    /// the result in `red::SyntaxNode::new_root`.
    pub tree_events: Vec<TreeEvent>,
}

impl ParseOutput {
    /// Build the green tree from this parse output (requires `std`).
    #[cfg(feature = "std")]
    #[must_use]
    pub fn build_green_tree(&self, input: &[u8]) -> Option<Arc<crate::tree::green::GreenNode>> {
        crate::tree::green::build_green_tree(input, &self.tree_events)
    }

    /// Build the green tree and wrap it in a syntax root (requires `std`).
    #[cfg(feature = "std")]
    #[must_use]
    pub fn syntax_root(&self, input: &[u8]) -> Option<crate::tree::red::SyntaxNode> {
        self.build_green_tree(input)
            .map(crate::tree::red::SyntaxNode::new_root)
    }
}
