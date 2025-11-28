//! Line and column position utilities
//!
//! This module provides utilities for converting byte offsets to line/column positions
//! and vice versa. This is essential for LSP integration and error reporting.

use crate::syntax::TextSize;

/// Represents a line and column position in source text
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineCol {
    /// Zero-based line number
    pub line: u32,
    /// Zero-based column number (in UTF-8 bytes)
    pub column: u32,
}

impl LineCol {
    /// Create a new line/column position
    #[must_use]
    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

/// Efficient line index for converting byte offsets to line/column positions
///
/// This struct caches line start positions for O(log n) binary search lookups.
/// It's more efficient than scanning the text on every conversion when you need
/// to convert multiple offsets.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offsets of line starts (including the start of the first line at 0)
    line_starts: Vec<TextSize>,
    /// Total length of the text in bytes
    text_len: TextSize,
}

impl LineIndex {
    /// Create a new line index from source text
    ///
    /// This scans the text once to build an index of line start positions.
    ///
    /// # Example
    ///
    /// ```rust
    /// use sipha::syntax::line_col::LineIndex;
    /// use sipha::syntax::TextSize;
    ///
    /// let text = "line 1\nline 2\nline 3";
    /// let index = LineIndex::new(text);
    /// let pos = index.line_col(TextSize::from(10));
    /// assert_eq!(pos.line, 1);
    /// ```
    #[must_use]
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![TextSize::zero()];

        // Scan text for line breaks
        let bytes = text.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b'\n' => {
                    // Unix line ending
                    let current_offset = u32::try_from(i).unwrap_or(u32::MAX).saturating_add(1);
                    line_starts.push(TextSize::from(current_offset));
                    i += 1;
                }
                b'\r' => {
                    if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                        // Windows line ending (\r\n)
                        let current_offset = u32::try_from(i).unwrap_or(u32::MAX).saturating_add(2);
                        line_starts.push(TextSize::from(current_offset));
                        i += 2;
                    } else {
                        // Old Mac line ending (just \r)
                        let current_offset = u32::try_from(i).unwrap_or(u32::MAX).saturating_add(1);
                        line_starts.push(TextSize::from(current_offset));
                        i += 1;
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        Self {
            line_starts,
            text_len: TextSize::from(u32::try_from(text.len()).unwrap_or(u32::MAX)),
        }
    }

    /// Convert a byte offset to a line/column position
    ///
    /// Uses binary search for O(log n) performance.
    ///
    /// # Panics
    ///
    /// Panics if `offset` is greater than the text length.
    ///
    /// # Example
    ///
    /// ```rust
    /// use sipha::syntax::line_col::LineIndex;
    /// use sipha::syntax::TextSize;
    ///
    /// let text = "line 1\nline 2";
    /// let index = LineIndex::new(text);
    /// let pos = index.line_col(TextSize::from(10));
    /// assert_eq!(pos.line, 1);
    /// assert_eq!(pos.column, 3); // "n" in "line 2" (offset 10 = position 3 on line 1)
    /// ```
    #[must_use]
    pub fn line_col(&self, offset: TextSize) -> LineCol {
        let offset_u32 = offset.into();
        assert!(
            offset_u32 <= self.text_len.into(),
            "Offset {} exceeds text length {}",
            offset_u32,
            self.text_len.into()
        );

        // Binary search for the line containing this offset
        let line = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,                    // Exact match at line start
            Err(idx) => idx.saturating_sub(1), // Offset is in the previous line
        };

        let line_start = self.line_starts[line];
        let column = offset_u32.saturating_sub(line_start.into());

        LineCol {
            line: u32::try_from(line).unwrap_or(u32::MAX),
            column,
        }
    }

    /// Get the total number of lines in the text
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: uses try_from
    pub fn line_count(&self) -> u32 {
        u32::try_from(self.line_starts.len()).unwrap_or(u32::MAX)
    }

    /// Get the byte offset of the start of a given line
    ///
    /// Returns `None` if the line number is out of bounds.
    #[must_use]
    pub fn line_start(&self, line: u32) -> Option<TextSize> {
        self.line_starts.get(line as usize).copied()
    }
}

/// Convert a byte offset to line/column position without building an index
///
/// This function scans the text from the beginning, so it's O(n) in the worst case.
/// For multiple conversions, use `LineIndex` instead.
///
/// # Example
///
/// ```rust
/// use sipha::syntax::line_col::line_col_from_offset;
/// use sipha::syntax::TextSize;
///
/// let text = "line 1\nline 2";
/// let pos = line_col_from_offset(text, TextSize::from(10));
/// assert_eq!(pos.line, 1);
/// ```
#[must_use]
pub fn line_col_from_offset(text: &str, offset: TextSize) -> LineCol {
    let index = LineIndex::new(text);
    index.line_col(offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_col_unix_line_endings() {
        let text = "line 1\nline 2\nline 3";
        let index = LineIndex::new(text);

        assert_eq!(index.line_col(TextSize::from(0)), LineCol::new(0, 0));
        assert_eq!(index.line_col(TextSize::from(6)), LineCol::new(0, 6));
        assert_eq!(index.line_col(TextSize::from(7)), LineCol::new(1, 0));
        assert_eq!(index.line_col(TextSize::from(13)), LineCol::new(1, 6));
        assert_eq!(index.line_col(TextSize::from(14)), LineCol::new(2, 0));
    }

    #[test]
    fn test_line_col_windows_line_endings() {
        let text = "line 1\r\nline 2\r\nline 3";
        let index = LineIndex::new(text);

        assert_eq!(index.line_col(TextSize::from(0)), LineCol::new(0, 0));
        assert_eq!(index.line_col(TextSize::from(6)), LineCol::new(0, 6));
        assert_eq!(index.line_col(TextSize::from(8)), LineCol::new(1, 0));
        assert_eq!(index.line_col(TextSize::from(14)), LineCol::new(1, 6));
    }

    #[test]
    fn test_line_col_mixed_line_endings() {
        let text = "line 1\nline 2\r\nline 3";
        let index = LineIndex::new(text);

        assert_eq!(index.line_col(TextSize::from(7)), LineCol::new(1, 0));
        assert_eq!(index.line_col(TextSize::from(15)), LineCol::new(2, 0));
    }

    #[test]
    fn test_line_col_empty_text() {
        let text = "";
        let index = LineIndex::new(text);

        assert_eq!(index.line_col(TextSize::from(0)), LineCol::new(0, 0));
        assert_eq!(index.line_count(), 1);
    }

    #[test]
    fn test_line_col_single_line() {
        let text = "single line";
        let index = LineIndex::new(text);

        assert_eq!(index.line_col(TextSize::from(0)), LineCol::new(0, 0));
        assert_eq!(index.line_col(TextSize::from(5)), LineCol::new(0, 5));
        assert_eq!(index.line_count(), 1);
    }

    #[test]
    fn test_line_col_utf8() {
        let text = "café\ncafé";
        let index = LineIndex::new(text);

        // 'é' is 2 bytes in UTF-8
        assert_eq!(index.line_col(TextSize::from(0)), LineCol::new(0, 0));
        assert_eq!(index.line_col(TextSize::from(5)), LineCol::new(0, 5)); // After 'é'
        assert_eq!(index.line_col(TextSize::from(6)), LineCol::new(1, 0));
    }

    #[test]
    fn test_line_col_function() {
        let text = "line 1\nline 2";
        let pos = line_col_from_offset(text, TextSize::from(10));
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 3);
    }

    #[test]
    fn test_line_start() {
        let text = "line 1\nline 2\nline 3";
        let index = LineIndex::new(text);

        assert_eq!(index.line_start(0), Some(TextSize::from(0)));
        assert_eq!(index.line_start(1), Some(TextSize::from(7)));
        assert_eq!(index.line_start(2), Some(TextSize::from(14)));
        assert_eq!(index.line_start(3), None);
    }
}
