use crate::types::{CaptureEvent, Pos, TreeEvent};

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
    /// Build the green tree from this parse output.
    ///
    /// `input` is the raw source bytes used during parsing — token texts are
    /// sliced from it and stored inside each [`GreenToken`](crate::tree::green::GreenToken).
    ///
    /// Returns `None` if `tree_events` is empty or malformed.
    #[must_use]
    pub fn build_green_tree(
        &self,
        input: &[u8],
    ) -> Option<std::sync::Arc<crate::tree::green::GreenNode>> {
        crate::tree::green::build_green_tree(input, &self.tree_events)
    }

    /// Build the green tree and wrap it in a [`crate::tree::red::SyntaxNode`] root.
    #[must_use]
    pub fn syntax_root(&self, input: &[u8]) -> Option<crate::tree::red::SyntaxNode> {
        self.build_green_tree(input)
            .map(crate::tree::red::SyntaxNode::new_root)
    }
}
