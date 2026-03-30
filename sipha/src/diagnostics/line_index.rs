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
    #[must_use]
    pub fn new(source: &[u8]) -> Self {
        let mut line_starts = vec![0];
        let mut i = 0;
        while i < source.len() {
            if source[i] == b'\n' {
                line_starts.push(Pos::try_from(i + 1).unwrap_or(0));
            } else if source[i] == b'\r' && i + 1 < source.len() && source[i + 1] == b'\n' {
                line_starts.push(Pos::try_from(i + 2).unwrap_or(0));
                i += 1;
            }
            i += 1;
        }
        Self { line_starts }
    }

    /// Number of lines (1-based; empty file = 1 line).
    #[must_use]
    #[inline]
    pub fn line_count(&self) -> u32 {
        u32::try_from(self.line_starts.len()).unwrap_or(u32::MAX)
    }

    /// Line and column for a byte offset (0-based line and column).
    /// Column is the byte offset from the start of the line.
    /// If offset is past end of source, returns the last line and column.
    #[must_use]
    pub fn line_col(&self, offset: Pos) -> (u32, u32) {
        let offset = offset as usize;
        let offset_pos = Pos::try_from(offset).unwrap_or(Pos::MAX);
        let line = match self.line_starts.binary_search_by(|&s| s.cmp(&offset_pos)) {
            Ok(i) => i,
            Err(i) => i.saturating_sub(1),
        };
        let line = line.min(self.line_starts.len().saturating_sub(1));
        let line_start = self.line_starts[line] as usize;
        let col = offset.saturating_sub(line_start);
        (
            u32::try_from(line).unwrap_or(u32::MAX),
            u32::try_from(col).unwrap_or(u32::MAX),
        )
    }

    /// Human-readable line and column (1-based) for display in error messages.
    #[must_use]
    #[inline]
    pub fn line_col_1based(&self, offset: Pos) -> (u32, u32) {
        let (line, col) = self.line_col(offset);
        (line + 1, col + 1)
    }

    /// Byte offset of the first byte of the given line (0-based line index).
    #[must_use]
    #[inline]
    pub fn line_start(&self, line: u32) -> Pos {
        self.line_starts
            .get(line as usize)
            .copied()
            .unwrap_or_else(|| *self.line_starts.last().unwrap_or(&0))
    }

    /// Byte range [start, end) for the given line (0-based). End is the start
    /// of the next line or end of source.
    #[must_use]
    pub fn line_range(&self, line: u32, source_len: usize) -> Span {
        let start = self.line_start(line);
        let end = self
            .line_starts
            .get((line + 1) as usize)
            .copied()
            .unwrap_or_else(|| Pos::try_from(source_len).unwrap_or(Pos::MAX));
        let source_len_pos = Pos::try_from(source_len).unwrap_or(Pos::MAX);
        Span::new(start, end.min(source_len_pos))
    }

    /// Source snippet for error reporting: the line containing `offset` and a
    /// caret line underneath. Uses UTF-8 when possible; invalid UTF-8 is
    /// replaced with replacement character so column alignment is approximate.
    #[must_use]
    pub fn snippet_at(&self, source: &[u8], offset: Pos) -> String {
        let (line_0, col_0) = self.line_col(offset);
        let line_1 = line_0 + 1;
        let span = self.line_range(line_0, source.len());
        let line_bytes = span.as_slice(source);
        let line_str = String::from_utf8_lossy(line_bytes).into_owned();
        let trimmed = line_str.trim_end_matches(['\r', '\n']);
        let col_limit = u32::try_from(trimmed.len()).unwrap_or(0);
        let caret = " ".repeat(col_0.min(col_limit) as usize) + "^";
        format!("  {line_1} | {trimmed}\n      | {caret}")
    }

    /// Full error snippet: "at line:col" plus the line and caret.
    #[must_use]
    pub fn snippet_with_location(&self, source: &[u8], offset: Pos) -> String {
        let (line_1, col_1) = self.line_col_1based(offset);
        let snip = self.snippet_at(source, offset);
        format!("at {line_1}:{col_1}:\n{snip}")
    }

    /// Line and column for a byte offset with column in UTF-16 code units (0-based).
    ///
    /// Returns `(line_0, utf16_col)`. Use for LSP and editors that expect UTF-16 positions.
    /// Requires the `utf16` feature.
    #[cfg(feature = "utf16")]
    #[must_use]
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
            utf16_col += u32::try_from(c.len_utf16()).unwrap_or(0);
        }
        (line_0, utf16_col)
    }

    /// Line and column in UTF-16 (1-based) for display and LSP.
    #[cfg(feature = "utf16")]
    #[inline]
    #[must_use]
    pub fn line_col_utf16_1based(&self, source: &str, offset: Pos) -> (u32, u32) {
        let (line_0, utf16_col) = self.line_col_utf16(source, offset);
        (line_0 + 1, utf16_col + 1)
    }

    /// Convert (line, character) in UTF-16 code units to byte offset.
    ///
    /// Returns `None` if the line is out of range or the position is past the end of the line.
    /// Useful for LSP (client sends line/character; server needs byte offset).
    #[cfg(feature = "utf16")]
    #[must_use]
    pub fn line_col_utf16_to_byte(&self, source: &str, line: u32, character: u32) -> Option<Pos> {
        let line_start = self.line_start(line) as usize;
        let source_len = source.len();
        let line_span = self.line_range(line, source_len);
        let line_end = line_span.end as usize;
        let line_src = source.get(line_start..line_end)?;
        let mut utf16_col = 0u32;
        for (i, c) in line_src.char_indices() {
            if utf16_col >= character {
                return Pos::try_from(line_start + i).ok();
            }
            utf16_col += u32::try_from(c.len_utf16()).ok()?;
        }
        Pos::try_from(line_start + line_src.len()).ok()
    }

    /// Prefix of the line up to (line, character) in UTF-16 code units.
    ///
    /// Returns `None` if the line is out of range. Useful for completion (prefix before cursor).
    #[cfg(feature = "utf16")]
    #[must_use]
    pub fn line_prefix_utf16(&self, source: &str, line: u32, character: u32) -> Option<String> {
        let line_start = self.line_start(line) as usize;
        let line_span = self.line_range(line, source.len());
        let line_end = line_span.end as usize;
        let line_src = source.get(line_start..line_end)?;
        let mut utf16_count = 0u32;
        let mut end = line_src.len();
        for (i, c) in line_src.char_indices() {
            if utf16_count >= character {
                end = i;
                break;
            }
            utf16_count += u32::try_from(c.len_utf16()).unwrap_or(0);
        }
        Some(line_src[..end].to_string())
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
        assert_eq!(
            idx.line_col(5).0,
            2,
            "offset 5 should be on line 2 (third line)"
        );
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
