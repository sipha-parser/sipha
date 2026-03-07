//! # Line index
//!
//! Maps byte offsets to line/column and provides source snippets for error
//! messages and IDE use.

use crate::types::{Pos, Span};

/// Maps byte offsets to line and column.
///
/// Built once from source; use for diagnostics (line/col, snippets) and
/// formatter/compiler (e.g. "token at line 3").
#[derive(Clone, Debug)]
pub struct LineIndex {
    /// Byte offset of the first character of each line (line 0 = start of file).
    line_starts: Vec<Pos>,
}

impl LineIndex {
    /// Build a line index from the source. Treats `\n` as line break; `\r\n` is
    /// one break (next line starts after `\n`).
    pub fn new(source: &[u8]) -> Self {
        let mut line_starts = vec![0];
        let mut i = 0;
        while i < source.len() {
            if source[i] == b'\n' {
                line_starts.push((i + 1) as Pos);
            } else if source[i] == b'\r' && i + 1 < source.len() && source[i + 1] == b'\n' {
                line_starts.push((i + 2) as Pos);
                i += 1;
            }
            i += 1;
        }
        Self { line_starts }
    }

    /// Number of lines (1-based; empty file = 1 line).
    #[inline]
    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// Line and column for a byte offset (0-based line and column).
    /// Column is the byte offset from the start of the line.
    /// If offset is past end of source, returns the last line and column.
    pub fn line_col(&self, offset: Pos) -> (u32, u32) {
        let offset = offset as usize;
        let line = match self.line_starts.binary_search_by(|&s| s.cmp(&(offset as Pos))) {
            Ok(i) => i,
            Err(i) => i.saturating_sub(1),
        };
        let line = line.min(self.line_starts.len().saturating_sub(1));
        let line_start = self.line_starts[line] as usize;
        let col = offset.saturating_sub(line_start);
        (line as u32, col as u32)
    }

    /// Human-readable line and column (1-based) for display in error messages.
    #[inline]
    pub fn line_col_1based(&self, offset: Pos) -> (u32, u32) {
        let (line, col) = self.line_col(offset);
        (line + 1, col + 1)
    }

    /// Byte offset of the first byte of the given line (0-based line index).
    #[inline]
    pub fn line_start(&self, line: u32) -> Pos {
        self.line_starts
            .get(line as usize)
            .copied()
            .unwrap_or(*self.line_starts.last().unwrap_or(&0))
    }

    /// Byte range [start, end) for the given line (0-based). End is the start
    /// of the next line or end of source.
    pub fn line_range(&self, line: u32, source_len: usize) -> Span {
        let start = self.line_start(line);
        let end = self
            .line_starts
            .get((line + 1) as usize)
            .copied()
            .unwrap_or(source_len as Pos);
        Span::new(start, end.min(source_len as Pos))
    }

    /// Source snippet for error reporting: the line containing `offset` and a
    /// caret line underneath. Uses UTF-8 when possible; invalid UTF-8 is
    /// replaced with replacement character so column alignment is approximate.
    pub fn snippet_at(&self, source: &[u8], offset: Pos) -> String {
        let (line_0, col_0) = self.line_col(offset);
        let (line_1, _col_1) = (line_0 + 1, col_0 + 1);
        let span = self.line_range(line_0, source.len());
        let line_bytes = span.as_slice(source);
        let line_str = String::from_utf8_lossy(line_bytes).into_owned();
        let trimmed = line_str.trim_end_matches(|c| c == '\r' || c == '\n');
        let caret = " ".repeat(col_0.min(trimmed.len() as u32) as usize) + "^";
        format!("  {} | {}\n      | {}", line_1, trimmed, caret)
    }

    /// Full error snippet: "at line:col" plus the line and caret.
    pub fn snippet_with_location(&self, source: &[u8], offset: Pos) -> String {
        let (line_1, col_1) = self.line_col_1based(offset);
        let snip = self.snippet_at(source, offset);
        format!("at {}:{}:\n{}", line_1, col_1, snip)
    }

    /// Line and column for a byte offset with column in UTF-16 code units (0-based).
    ///
    /// Returns `(line_0, utf16_col)`. Use for LSP and editors that expect UTF-16 positions.
    /// Requires the `utf16` feature.
    #[cfg(feature = "utf16")]
    pub fn line_col_utf16(&self, source: &str, offset: Pos) -> (u32, u32) {
        let (line_0, _) = self.line_col(offset);
        let line_start = self.line_start(line_0) as usize;
        let offset_usize = offset as usize;
        if line_start >= source.len() {
            return (line_0, 0);
        }
        let rest = &source[line_start..];
        let mut utf16_col = 0u32;
        for (i, c) in rest.char_indices() {
            if line_start + i >= offset_usize {
                break;
            }
            utf16_col += c.len_utf16() as u32;
        }
        (line_0, utf16_col)
    }

    /// Line and column in UTF-16 (1-based) for display and LSP.
    #[cfg(feature = "utf16")]
    #[inline]
    pub fn line_col_utf16_1based(&self, source: &str, offset: Pos) -> (u32, u32) {
        let (line_0, utf16_col) = self.line_col_utf16(source, offset);
        (line_0 + 1, utf16_col + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_index_single_line() {
        let src = b"hello";
        let idx = LineIndex::new(src);
        assert_eq!(idx.line_count(), 1);
        assert_eq!(idx.line_col(0), (0, 0));
        assert_eq!(idx.line_col(3), (0, 3));
        assert_eq!(idx.line_col_1based(3), (1, 4));
    }

    #[test]
    fn line_index_multiline() {
        let src = b"a\nbb\nccc";
        let idx = LineIndex::new(src);
        assert_eq!(idx.line_count(), 3);
        assert_eq!(idx.line_col(0), (0, 0));
        assert_eq!(idx.line_col(1), (0, 1));
        assert_eq!(idx.line_col(2), (1, 0));
        // offset 5 = first byte of third line "ccc"
        assert_eq!(idx.line_col(5).0, 2, "offset 5 should be on line 2 (third line)");
        assert_eq!(idx.line_col(6), (2, 1));
    }

    #[test]
    fn snippet_at() {
        let src = b"let x = 1\n  foo";
        let idx = LineIndex::new(src);
        let s = idx.snippet_at(src, 7);
        assert!(s.contains("let x = 1"));
        assert!(s.contains("^"));
    }
}
