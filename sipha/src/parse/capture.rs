//! # Capture Tree
//!
//! Transforms the flat [`CaptureEvent`] log produced by the VM into a
//! typed, tree-structured output that callers can traverse.

use crate::types::{CaptureEvent, Pos, Span, Tag};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

/// A node in the capture tree.
#[derive(Clone, Debug)]
pub struct CaptureNode {
    pub tag: Tag,
    pub span: Span,
    pub children: Vec<Self>,
}

impl CaptureNode {
    /// Build a forest of [`CaptureNode`]s from the flat event log.
    ///
    /// Events form a well-nested sequence of `Open`/`Close` pairs.
    /// Returns the top-level nodes (may be multiple if the grammar emits
    /// sibling captures at the root level).
    ///
    /// # Panics
    ///
    /// Panics if `events` has a `Close` without a matching `Open`, or if
    /// unclosed regions remain at the end (debug builds only for the latter).
    #[must_use]
    pub fn build_forest(events: &[CaptureEvent]) -> Vec<Self> {
        let mut stack: Vec<(Tag, Pos, Vec<Self>)> = Vec::new();
        let mut roots: Vec<Self> = Vec::new();

        for &ev in events {
            match ev {
                CaptureEvent::Open { tag, pos } => {
                    stack.push((tag, pos, Vec::new()));
                }
                CaptureEvent::Close { tag, pos } => {
                    let (open_tag, start, children) = stack
                        .pop()
                        .expect("capture event mismatch: Close without Open");
                    debug_assert_eq!(open_tag, tag, "mismatched capture tags");
                    let node = Self {
                        tag,
                        span: Span::new(start, pos),
                        children,
                    };
                    match stack.last_mut() {
                        Some((_, _, parent_children)) => parent_children.push(node),
                        None => roots.push(node),
                    }
                }
            }
        }

        debug_assert!(stack.is_empty(), "unclosed capture regions");
        roots
    }

    /// Convenience: extract the text of this node from the input buffer.
    #[must_use]
    #[inline]
    pub fn text<'a>(&self, input: &'a [u8]) -> &'a [u8] {
        self.span.as_slice(input)
    }

    /// Depth-first traversal, calling `f` on every node.
    pub fn walk(&self, f: &mut impl FnMut(&Self)) {
        f(self);
        for child in &self.children {
            child.walk(f);
        }
    }

    /// Find the first descendant (or self) with the given `tag`.
    #[must_use]
    pub fn find(&self, tag: Tag) -> Option<&Self> {
        if self.tag == tag {
            return Some(self);
        }
        self.children.iter().find_map(|c| c.find(tag))
    }

    /// Collect all descendants (including self) with the given `tag`.
    #[must_use]
    pub fn find_all(&self, tag: Tag) -> Vec<&Self> {
        let mut result = Vec::new();
        self.collect_all(tag, &mut result);
        result
    }

    fn collect_all<'a>(&'a self, tag: Tag, out: &mut Vec<&'a Self>) {
        if self.tag == tag {
            out.push(self);
        }
        for child in &self.children {
            child.collect_all(tag, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_forest_single_node() {
        let events = [
            CaptureEvent::Open { tag: 1, pos: 0 },
            CaptureEvent::Close { tag: 1, pos: 5 },
        ];
        let forest = CaptureNode::build_forest(&events);
        assert_eq!(forest.len(), 1);
        assert_eq!(forest[0].tag, 1);
        assert_eq!(forest[0].span.start, 0);
        assert_eq!(forest[0].span.end, 5);
        assert!(forest[0].children.is_empty());
    }

    #[test]
    fn build_forest_nested() {
        let events = [
            CaptureEvent::Open { tag: 0, pos: 0 },
            CaptureEvent::Open { tag: 1, pos: 1 },
            CaptureEvent::Close { tag: 1, pos: 4 },
            CaptureEvent::Close { tag: 0, pos: 5 },
        ];
        let forest = CaptureNode::build_forest(&events);
        assert_eq!(forest.len(), 1);
        assert_eq!(forest[0].tag, 0);
        assert_eq!(forest[0].children.len(), 1);
        assert_eq!(forest[0].children[0].tag, 1);
        assert_eq!(forest[0].children[0].span.start, 1);
        assert_eq!(forest[0].children[0].span.end, 4);
    }

    #[test]
    fn capture_node_text() {
        let events = [
            CaptureEvent::Open { tag: 0, pos: 2 },
            CaptureEvent::Close { tag: 0, pos: 7 },
        ];
        let forest = CaptureNode::build_forest(&events);
        let input = b"hello world";
        assert_eq!(forest[0].text(input), b"llo w");
    }
}
