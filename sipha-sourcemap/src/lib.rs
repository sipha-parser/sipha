//! Build span mappings from sipha transforms.
//!
//! Run a transform and get back both the new tree and a [`SpanMap`] that maps
//! spans in the new text back to the original source. Use for diagnostics or
//! IDE features after rewriting.

use std::sync::Arc;

use sipha::green::{GreenElement, GreenNode};
use sipha::red::SyntaxNode;
use sipha::transform::Transformer;
use sipha::types::Span;

pub use sipha::source_map::{map_offset, SpanMap, SpanMapping};

/// Run the transformer and build a span mapping from the new tree to the original.
///
/// For each node that was **kept** (not replaced), records a (new_span, old_span)
/// pair. Replaced nodes are not mapped. The new root has offset 0; new_span
/// positions reflect the transformed text.
pub fn transform_with_mapping(
    root: &SyntaxNode,
    transformer: &mut impl Transformer,
) -> (SyntaxNode, SpanMap) {
    let mut map = SpanMap::new();
    let new_green = rebuild_with_map(root, transformer, 0, &mut map);
    map.sort();
    (SyntaxNode::new_root(new_green), map)
}

fn rebuild_with_map(
    node: &SyntaxNode,
    transformer: &mut impl Transformer,
    new_offset: u32,
    map: &mut SpanMap,
) -> Arc<GreenNode> {
    if let Some(replacement) = transformer.transform_node(node) {
        return replacement;
    }
    let old_span = node.text_range();
    let green = node.green();
    let mut new_children = Vec::with_capacity(green.children.len());
    let mut old_off = node.offset();
    let mut new_off = new_offset;
    for child in &green.children {
        match child {
            GreenElement::Node(child_green) => {
                let child_red = SyntaxNode::new(Arc::clone(child_green), old_off);
                let child_new = rebuild_with_map(&child_red, transformer, new_off, map);
                let len = child_new.text_len;
                new_children.push(GreenElement::Node(child_new));
                old_off += child_green.text_len;
                new_off += len;
            }
            GreenElement::Token(t) => {
                new_children.push(GreenElement::Token(Arc::clone(t)));
                let len = t.text_len;
                old_off += len;
                new_off += len;
            }
        }
    }
    let new_span = Span::new(new_offset, new_off);
    map.push(new_span, old_span);
    GreenNode::new(node.kind(), new_children)
}
