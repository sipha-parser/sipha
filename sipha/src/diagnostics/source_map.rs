//! # Span mapping (source map)
//!
//! Map spans in a transformed tree back to spans in the original source.
//! Use after a transform pass to report diagnostics or selections in original
//! coordinates. Construction of the mapping (e.g. from a [`Transformer`](crate::tree::transform::Transformer))
//! is available with the optional `sourcemap` feature (`transform_with_mapping`).

use crate::types::{Pos, Span};

/// Maps a span in the transformed (new) text to the corresponding span in the
/// original source, if known.
pub trait SpanMapping {
    /// Return the original source span that corresponds to `new_span` in the
    /// transformed output. Returns `None` if no mapping is available (e.g.
    /// synthesized code).
    fn map_span(&self, new_span: Span) -> Option<Span>;
}

/// A span mapping represented as a sorted list of (`new_span`, `old_span`) pairs.
///
/// Lookup returns the old span for the pair whose new span contains the query
/// span's start, or the nearest preceding pair. For exact mapping, use pairs
/// that cover the transformed tree.
#[derive(Clone, Debug, Default)]
pub struct SpanMap {
    /// Pairs (new range, old range). Should be sorted by new range start for
    /// binary search, or built incrementally and sorted once.
    pairs: Vec<(Span, Span)>,
}

impl SpanMap {
    #[inline]
    #[must_use]
    pub const fn new() -> Self {
        Self { pairs: Vec::new() }
    }

    pub fn push(&mut self, new_span: Span, old_span: Span) {
        self.pairs.push((new_span, old_span));
    }

    /// Sort by new span start so that lookup is consistent.
    pub fn sort(&mut self) {
        self.pairs.sort_by_key(|(s, _)| s.start);
    }

    /// Build from a list of (new, old) pairs and sort.
    #[must_use]
    pub fn from_pairs(mut pairs: Vec<(Span, Span)>) -> Self {
        pairs.sort_by_key(|(s, _)| s.start);
        Self { pairs }
    }
}

impl SpanMapping for SpanMap {
    fn map_span(&self, new_span: Span) -> Option<Span> {
        if self.pairs.is_empty() {
            return None;
        }
        let pos = new_span.start;
        let i = match self.pairs.binary_search_by_key(&pos, |(s, _)| s.start) {
            Ok(i) => i,
            Err(0) => return None,
            Err(i) => i - 1,
        };
        let (new, old) = &self.pairs[i];
        if new_span.start >= new.end {
            return None;
        }
        // Linear offset mapping within this segment
        let offset_in_seg = new_span.start - new.start;
        let len = (new_span.end - new_span.start).min(new.end - new_span.start);
        let old_start = old.start + offset_in_seg.min(old.end.saturating_sub(old.start));
        let old_end = (old_start + len).min(old.end);
        Some(Span::new(old_start, old_end))
    }
}

/// Apply a span mapping to an offset: return the original offset for a given
/// offset in the new text.
#[inline]
pub fn map_offset(mapping: &impl SpanMapping, new_offset: Pos) -> Option<Pos> {
    mapping
        .map_span(Span::new(new_offset, new_offset + 1))
        .map(|s| s.start)
}
