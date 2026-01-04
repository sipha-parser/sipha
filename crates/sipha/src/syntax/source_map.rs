//! # Source Map Generation
//!
//! This module provides utilities for generating source maps that track
//! the correspondence between source positions and generated/transformed code.
//!
//! Source maps are essential for:
//! - Debugging transformed code
//! - IDE navigation
//! - Error reporting with accurate positions

use crate::syntax::{TextRange, TextSize};
use std::collections::BTreeMap;

/// A source map that tracks mappings between original and generated positions
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    /// Mappings from generated positions to original positions
    mappings: BTreeMap<TextSize, Mapping>,
    /// Source file names
    sources: Vec<String>,
    /// The generated content (optional, for verification)
    generated_content: Option<String>,
}

/// A single mapping entry
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Mapping {
    /// Position in the generated code
    pub generated_pos: TextSize,
    /// Source file index
    pub source_index: u32,
    /// Position in the original source
    pub original_pos: TextSize,
    /// Original line number (0-based)
    pub original_line: u32,
    /// Original column number (0-based)
    pub original_column: u32,
}

impl SourceMap {
    /// Create a new empty source map
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file to the source map
    pub fn add_source(&mut self, name: impl Into<String>) -> u32 {
        let index = self.sources.len() as u32;
        self.sources.push(name.into());
        index
    }

    /// Add a mapping to the source map
    pub fn add_mapping(&mut self, mapping: Mapping) {
        self.mappings.insert(mapping.generated_pos, mapping);
    }

    /// Add a range mapping (maps a generated range to an original range)
    pub fn add_range_mapping(
        &mut self,
        generated_range: TextRange,
        original_range: TextRange,
        source_index: u32,
        original_source: &str,
    ) {
        // Calculate line/column for the original position
        let (line, column) = Self::pos_to_line_col(original_source, original_range.start());

        self.add_mapping(Mapping {
            generated_pos: generated_range.start(),
            source_index,
            original_pos: original_range.start(),
            original_line: line,
            original_column: column,
        });
    }

    /// Look up the original position for a generated position
    #[must_use]
    pub fn lookup(&self, generated_pos: TextSize) -> Option<&Mapping> {
        // Find the mapping that covers this position
        self.mappings
            .range(..=generated_pos)
            .next_back()
            .map(|(_, m)| m)
    }

    /// Get all mappings
    pub fn mappings(&self) -> impl Iterator<Item = &Mapping> {
        self.mappings.values()
    }

    /// Get the source file names
    #[must_use]
    pub fn sources(&self) -> &[String] {
        &self.sources
    }

    /// Set the generated content
    pub fn set_generated_content(&mut self, content: String) {
        self.generated_content = Some(content);
    }

    /// Get the generated content
    #[must_use]
    pub fn generated_content(&self) -> Option<&str> {
        self.generated_content.as_deref()
    }

    /// Convert a byte position to line/column (0-based)
    fn pos_to_line_col(source: &str, pos: TextSize) -> (u32, u32) {
        let pos: usize = u32::from(pos) as usize;
        let mut line = 0u32;
        let mut line_start: usize = 0;

        for (i, c) in source.char_indices() {
            if i >= pos {
                break;
            }
            if c == '\n' {
                line += 1;
                line_start = i + 1;
            }
        }

        let column = (pos.saturating_sub(line_start)) as u32;
        (line, column)
    }

    /// Generate a JSON source map (v3 format)
    #[must_use]
    pub fn to_json(&self) -> String {
        let sources_json: Vec<_> = self.sources.iter().map(|s| format!("\"{s}\"")).collect();

        // Generate VLQ-encoded mappings
        let mappings_str = self.generate_vlq_mappings();

        format!(
            r#"{{
  "version": 3,
  "sources": [{}],
  "mappings": "{}"
}}"#,
            sources_json.join(", "),
            mappings_str
        )
    }

    /// Generate VLQ-encoded mappings string
    fn generate_vlq_mappings(&self) -> String {
        // Simplified VLQ encoding for source maps
        // Full implementation would need proper VLQ encoding
        let mut result = String::new();
        let mut prev_gen_col = 0u32;
        let mut prev_source = 0u32;
        let mut prev_orig_line = 0u32;
        let mut prev_orig_col = 0u32;

        for mapping in self.mappings.values() {
            // Calculate current generated line
            // For simplicity, assume all on same line (real impl would track newlines)
            let gen_col = u32::from(mapping.generated_pos);

            if !result.is_empty() {
                result.push(',');
            }

            // VLQ encode: [gen_col_delta, source_delta, orig_line_delta, orig_col_delta]
            result.push_str(&vlq_encode(gen_col as i32 - prev_gen_col as i32));
            result.push_str(&vlq_encode(
                mapping.source_index as i32 - prev_source as i32,
            ));
            result.push_str(&vlq_encode(
                mapping.original_line as i32 - prev_orig_line as i32,
            ));
            result.push_str(&vlq_encode(
                mapping.original_column as i32 - prev_orig_col as i32,
            ));

            prev_gen_col = gen_col;
            prev_source = mapping.source_index;
            prev_orig_line = mapping.original_line;
            prev_orig_col = mapping.original_column;
        }

        result
    }
}

/// VLQ encode a single value
fn vlq_encode(value: i32) -> String {
    const BASE64_CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    let mut value = if value < 0 {
        ((-value) << 1) | 1
    } else {
        value << 1
    };

    let mut result = String::new();
    loop {
        let mut digit = value & 0b11111;
        value >>= 5;
        if value > 0 {
            digit |= 0b10_0000; // Set continuation bit
        }
        result.push(BASE64_CHARS[digit as usize] as char);
        if value == 0 {
            break;
        }
    }

    result
}

/// Builder for creating source maps
pub struct SourceMapBuilder {
    source_map: SourceMap,
    current_generated_pos: TextSize,
}

impl Default for SourceMapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceMapBuilder {
    /// Create a new source map builder
    #[must_use]
    pub fn new() -> Self {
        Self {
            source_map: SourceMap::new(),
            current_generated_pos: TextSize::from(0),
        }
    }

    /// Add a source file
    pub fn add_source(&mut self, name: impl Into<String>) -> u32 {
        self.source_map.add_source(name)
    }

    /// Add a mapping at the current generated position
    pub fn add_mapping(
        &mut self,
        source_index: u32,
        original_pos: TextSize,
        original_line: u32,
        original_column: u32,
    ) {
        self.source_map.add_mapping(Mapping {
            generated_pos: self.current_generated_pos,
            source_index,
            original_pos,
            original_line,
            original_column,
        });
    }

    /// Advance the generated position
    pub fn advance(&mut self, len: TextSize) {
        self.current_generated_pos += len;
    }

    /// Finish building and return the source map
    #[must_use]
    pub fn finish(self) -> SourceMap {
        self.source_map
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_map_creation() {
        let mut sm = SourceMap::new();
        let source_idx = sm.add_source("test.rs");
        assert_eq!(source_idx, 0);
        assert_eq!(sm.sources(), &["test.rs"]);
    }

    #[test]
    fn test_mapping_lookup() {
        let mut sm = SourceMap::new();
        let source_idx = sm.add_source("test.rs");

        sm.add_mapping(Mapping {
            generated_pos: TextSize::from(10),
            source_index: source_idx,
            original_pos: TextSize::from(5),
            original_line: 0,
            original_column: 5,
        });

        let mapping = sm.lookup(TextSize::from(10)).unwrap();
        assert_eq!(mapping.original_pos, TextSize::from(5));
    }

    #[test]
    fn test_vlq_encode() {
        assert_eq!(vlq_encode(0), "A");
        assert_eq!(vlq_encode(1), "C");
        assert_eq!(vlq_encode(-1), "D");
    }
}
