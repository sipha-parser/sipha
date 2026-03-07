//! # UTF-16 offset support
//!
//! Helpers to convert byte (UTF-8) positions to UTF-16 code unit offsets.
//! Required for LSP and editors that use UTF-16 (e.g. JavaScript, `VSCode`).
//!
//! Enable the `utf16` feature to use this module.

use crate::types::Span;

/// Number of UTF-16 code units in `s`.
#[inline]
#[must_use] 
pub fn utf16_len(s: &str) -> u32 {
    u32::try_from(s.chars().map(char::len_utf16).sum::<usize>()).unwrap_or(0)
}

/// UTF-16 code unit offset corresponding to `byte_offset` in UTF-8 `s`.
///
/// If `byte_offset` is past the end of `s`, returns the UTF-16 length of `s`.
/// If `byte_offset` is inside a multi-byte character, returns the offset at the
/// start of that character.
#[inline]
#[must_use] 
pub fn byte_offset_to_utf16(s: &str, byte_offset: usize) -> u32 {
    let mut utf16 = 0u32;
    for (i, c) in s.char_indices() {
        if i >= byte_offset {
            break;
        }
        utf16 += u32::try_from(c.len_utf16()).unwrap_or(0);
    }
    utf16
}

/// UTF-16 code unit range `[start, end)` for the byte span.
///
/// Useful for LSP `Range` (character is in UTF-16 code units).
#[inline]
#[must_use] 
pub fn span_to_utf16_range(span: Span, source: &str) -> (u32, u32) {
    let start = byte_offset_to_utf16(source, span.start as usize);
    let end = byte_offset_to_utf16(source, span.end as usize);
    (start, end)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn utf16_len_ascii() {
        assert_eq!(utf16_len("abc"), 3);
    }

    #[test]
    fn utf16_len_emoji() {
        // emoji is 2 UTF-16 code units
        assert_eq!(utf16_len("a\u{1F600}b"), 4);
    }

    #[test]
    fn byte_offset_to_utf16_ascii() {
        let s = "hello";
        assert_eq!(byte_offset_to_utf16(s, 0), 0);
        assert_eq!(byte_offset_to_utf16(s, 5), 5);
    }

    #[test]
    fn byte_offset_to_utf16_emoji() {
        let s = "x\u{1F600}y";
        assert_eq!(byte_offset_to_utf16(s, 0), 0);
        assert_eq!(byte_offset_to_utf16(s, 1), 1);
        assert_eq!(byte_offset_to_utf16(s, 5), 3); // after 4-byte emoji, 1 + 2 utf16
        assert_eq!(byte_offset_to_utf16(s, 6), 4);
    }

    #[test]
    fn span_to_utf16_range_() {
        let s = "a\u{1F600}b";
        let span = Span::new(1, 5);
        assert_eq!(span_to_utf16_range(span, s), (1, 3));
    }
}
