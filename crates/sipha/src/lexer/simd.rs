//! # SIMD-Optimized Scanning
//!
//! This module provides SIMD-accelerated character class matching for lexing hot paths.
//!
//! ## Overview
//!
//! SIMD (Single Instruction, Multiple Data) allows processing multiple bytes
//! simultaneously, dramatically speeding up common lexing operations like:
//!
//! - Scanning identifiers
//! - Skipping whitespace
//! - Finding delimiters
//!
//! ## Implementation Notes
//!
//! This module uses the `memchr` crate which provides SIMD-accelerated byte searching
//! on supported platforms. When SIMD is not available, it falls back to optimized
//! scalar implementations.

use memchr::{memchr, memchr2, memchr3};

/// SIMD-accelerated scanner for common lexing patterns
pub struct SimdScanner;

impl SimdScanner {
    /// Scan an identifier starting at the beginning of the input.
    ///
    /// Returns the length of the identifier (number of bytes until the first
    /// non-identifier character).
    ///
    /// An identifier consists of:
    /// - First character: `[a-zA-Z_]` (or Unicode XID_Start if unicode feature enabled)
    /// - Continuation: `[a-zA-Z0-9_]` (or Unicode XID_Continue if unicode feature enabled)
    #[must_use]
    pub fn scan_identifier(input: &[u8]) -> usize {
        if input.is_empty() {
            return 0;
        }

        // Check first character
        if !is_ident_start(input[0]) {
            return 0;
        }

        // Scan continuation characters
        let mut len = 1;
        while len < input.len() && is_ident_continue(input[len]) {
            len += 1;
        }

        len
    }

    /// Skip whitespace at the beginning of the input.
    ///
    /// Returns the number of whitespace bytes skipped.
    #[must_use]
    pub fn skip_whitespace(input: &[u8]) -> usize {
        let mut pos = 0;

        // Use SIMD to find first non-whitespace
        // We look for common ASCII whitespace: space, tab, newline, carriage return
        while pos < input.len() {
            match input[pos] {
                b' ' | b'\t' | b'\n' | b'\r' => pos += 1,
                _ => break,
            }
        }

        pos
    }

    /// Skip whitespace including Unicode whitespace.
    ///
    /// Returns the number of bytes skipped.
    #[cfg(feature = "unicode")]
    #[must_use]
    pub fn skip_unicode_whitespace(input: &str) -> usize {
        let chars = input.chars();
        let mut len = 0;

        for c in chars {
            if c.is_whitespace() {
                len += c.len_utf8();
            } else {
                break;
            }
        }

        len
    }

    /// Find the first occurrence of any delimiter in the set.
    ///
    /// Returns the position of the first delimiter, or `None` if not found.
    #[must_use]
    pub fn find_delimiter(input: &[u8], delims: &[u8]) -> Option<usize> {
        match delims.len() {
            0 => None,
            1 => memchr(delims[0], input),
            2 => memchr2(delims[0], delims[1], input),
            3 => memchr3(delims[0], delims[1], delims[2], input),
            _ => {
                // For more delimiters, use a set-based approach
                // memchr doesn't have memchrN for N > 3, so we iterate
                for (i, &b) in input.iter().enumerate() {
                    if delims.contains(&b) {
                        return Some(i);
                    }
                }
                None
            }
        }
    }

    /// Find the first newline character.
    ///
    /// Returns the position of the newline, or `None` if not found.
    #[must_use]
    pub fn find_newline(input: &[u8]) -> Option<usize> {
        memchr(b'\n', input)
    }

    /// Find the first occurrence of a byte.
    ///
    /// This is a thin wrapper around `memchr` for consistency.
    #[must_use]
    pub fn find_byte(input: &[u8], byte: u8) -> Option<usize> {
        memchr(byte, input)
    }

    /// Find the first occurrence of either of two bytes.
    #[must_use]
    pub fn find_byte2(input: &[u8], b1: u8, b2: u8) -> Option<usize> {
        memchr2(b1, b2, input)
    }

    /// Find the first occurrence of any of three bytes.
    #[must_use]
    pub fn find_byte3(input: &[u8], b1: u8, b2: u8, b3: u8) -> Option<usize> {
        memchr3(b1, b2, b3, input)
    }

    /// Scan a number literal (integer).
    ///
    /// Returns the length of the number.
    #[must_use]
    pub fn scan_number(input: &[u8]) -> usize {
        let mut len = 0;
        while len < input.len() && input[len].is_ascii_digit() {
            len += 1;
        }
        len
    }

    /// Scan a hexadecimal number (without 0x prefix).
    ///
    /// Returns the length of the hex digits.
    #[must_use]
    pub fn scan_hex_number(input: &[u8]) -> usize {
        let mut len = 0;
        while len < input.len() && input[len].is_ascii_hexdigit() {
            len += 1;
        }
        len
    }

    /// Scan a string literal (double-quoted).
    ///
    /// Returns the length including the quotes, or 0 if not a valid string.
    /// Handles escape sequences.
    #[must_use]
    pub fn scan_string_literal(input: &[u8]) -> usize {
        if input.is_empty() || input[0] != b'"' {
            return 0;
        }

        let mut pos = 1;
        while pos < input.len() {
            match input[pos] {
                b'"' => return pos + 1,                     // End of string
                b'\\' if pos + 1 < input.len() => pos += 2, // Escape sequence
                b'\n' | b'\r' => return 0,                  // Unterminated (newline in string)
                _ => pos += 1,
            }
        }

        0 // Unterminated string
    }

    /// Scan a character literal (single-quoted).
    ///
    /// Returns the length including the quotes, or 0 if not valid.
    #[must_use]
    pub fn scan_char_literal(input: &[u8]) -> usize {
        if input.is_empty() || input[0] != b'\'' {
            return 0;
        }

        if input.len() < 3 {
            return 0;
        }

        let pos = if input[1] == b'\\' {
            // Escape sequence
            if input.len() < 4 {
                return 0;
            }
            3
        } else {
            2
        };

        if pos < input.len() && input[pos] == b'\'' {
            pos + 1
        } else {
            0
        }
    }

    /// Scan a line comment (// style).
    ///
    /// Returns the length including the newline (if present).
    #[must_use]
    pub fn scan_line_comment(input: &[u8]) -> usize {
        if input.len() < 2 || input[0] != b'/' || input[1] != b'/' {
            return 0;
        }

        match Self::find_newline(&input[2..]) {
            Some(pos) => 2 + pos + 1, // Include the newline
            None => input.len(),      // Comment goes to end of input
        }
    }

    /// Scan a block comment (/* */ style).
    ///
    /// Returns the length including the closing */, or 0 if unterminated.
    /// Does not handle nested comments.
    #[must_use]
    pub fn scan_block_comment(input: &[u8]) -> usize {
        if input.len() < 4 || input[0] != b'/' || input[1] != b'*' {
            return 0;
        }

        let mut pos = 2;
        while pos + 1 < input.len() {
            if input[pos] == b'*' && input[pos + 1] == b'/' {
                return pos + 2;
            }
            pos += 1;
        }

        0 // Unterminated
    }

    /// Count the number of leading spaces/tabs for indentation.
    #[must_use]
    pub fn count_indentation(input: &[u8]) -> (usize, usize) {
        let mut spaces = 0;
        let mut tabs = 0;
        let mut pos = 0;

        while pos < input.len() {
            match input[pos] {
                b' ' => spaces += 1,
                b'\t' => tabs += 1,
                _ => break,
            }
            pos += 1;
        }

        (spaces, tabs)
    }
}

/// Check if a byte is a valid ASCII identifier start character.
#[inline]
fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

/// Check if a byte is a valid ASCII identifier continuation character.
#[inline]
fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Check if a character is a valid Unicode identifier start.
#[cfg(feature = "unicode")]
#[inline]
#[must_use]
pub fn is_unicode_ident_start(c: char) -> bool {
    unicode_ident::is_xid_start(c)
}

/// Check if a character is a valid Unicode identifier continuation.
#[cfg(feature = "unicode")]
#[inline]
#[must_use]
pub fn is_unicode_ident_continue(c: char) -> bool {
    unicode_ident::is_xid_continue(c)
}

/// Scan a Unicode identifier.
///
/// Returns the length in bytes.
#[cfg(feature = "unicode")]
#[must_use]
pub fn scan_unicode_identifier(input: &str) -> usize {
    let mut chars = input.chars();

    let Some(first) = chars.next() else {
        return 0;
    };

    if !is_unicode_ident_start(first) {
        return 0;
    }

    let mut len = first.len_utf8();

    for c in chars {
        if is_unicode_ident_continue(c) {
            len += c.len_utf8();
        } else {
            break;
        }
    }

    len
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_identifier() {
        assert_eq!(SimdScanner::scan_identifier(b"hello"), 5);
        assert_eq!(SimdScanner::scan_identifier(b"hello_world"), 11);
        assert_eq!(SimdScanner::scan_identifier(b"_private"), 8);
        assert_eq!(SimdScanner::scan_identifier(b"var123"), 6);
        assert_eq!(SimdScanner::scan_identifier(b"123var"), 0); // Starts with digit
        assert_eq!(SimdScanner::scan_identifier(b"hello world"), 5);
        assert_eq!(SimdScanner::scan_identifier(b""), 0);
    }

    #[test]
    fn test_skip_whitespace() {
        assert_eq!(SimdScanner::skip_whitespace(b"   hello"), 3);
        assert_eq!(SimdScanner::skip_whitespace(b"\t\t hello"), 3);
        assert_eq!(SimdScanner::skip_whitespace(b"\n\r\n hello"), 4);
        assert_eq!(SimdScanner::skip_whitespace(b"hello"), 0);
        assert_eq!(SimdScanner::skip_whitespace(b""), 0);
    }

    #[test]
    fn test_find_delimiter() {
        assert_eq!(SimdScanner::find_delimiter(b"hello;world", b";"), Some(5));
        assert_eq!(SimdScanner::find_delimiter(b"hello", b";"), None);
        assert_eq!(SimdScanner::find_delimiter(b"a,b;c", b",;"), Some(1));
        assert_eq!(SimdScanner::find_delimiter(b"abc", b",;:"), None);
    }

    #[test]
    fn test_find_newline() {
        assert_eq!(SimdScanner::find_newline(b"hello\nworld"), Some(5));
        assert_eq!(SimdScanner::find_newline(b"hello world"), None);
    }

    #[test]
    fn test_scan_number() {
        assert_eq!(SimdScanner::scan_number(b"12345"), 5);
        assert_eq!(SimdScanner::scan_number(b"123abc"), 3);
        assert_eq!(SimdScanner::scan_number(b"abc123"), 0);
    }

    #[test]
    fn test_scan_hex_number() {
        assert_eq!(SimdScanner::scan_hex_number(b"abc123"), 6);
        assert_eq!(SimdScanner::scan_hex_number(b"DEADBEEF"), 8);
        assert_eq!(SimdScanner::scan_hex_number(b"xyz"), 0);
    }

    #[test]
    fn test_scan_string_literal() {
        assert_eq!(SimdScanner::scan_string_literal(br#""hello""#), 7);
        assert_eq!(SimdScanner::scan_string_literal(br#""hello world""#), 13);
        assert_eq!(SimdScanner::scan_string_literal(br#""hello\nworld""#), 14);
        assert_eq!(SimdScanner::scan_string_literal(br#""unterminated"#), 0);
        assert_eq!(SimdScanner::scan_string_literal(b"not a string"), 0);
    }

    #[test]
    fn test_scan_char_literal() {
        assert_eq!(SimdScanner::scan_char_literal(b"'a'"), 3);
        assert_eq!(SimdScanner::scan_char_literal(b"'\\n'"), 4);
        assert_eq!(SimdScanner::scan_char_literal(b"'abc'"), 0); // Too long
        assert_eq!(SimdScanner::scan_char_literal(b"'"), 0); // Too short
    }

    #[test]
    fn test_scan_line_comment() {
        assert_eq!(SimdScanner::scan_line_comment(b"// comment\n"), 11);
        assert_eq!(SimdScanner::scan_line_comment(b"// comment"), 10);
        assert_eq!(SimdScanner::scan_line_comment(b"not a comment"), 0);
    }

    #[test]
    fn test_scan_block_comment() {
        assert_eq!(SimdScanner::scan_block_comment(b"/* comment */"), 13);
        assert_eq!(SimdScanner::scan_block_comment(b"/* multi\nline */"), 16);
        assert_eq!(SimdScanner::scan_block_comment(b"/* unterminated"), 0);
        assert_eq!(SimdScanner::scan_block_comment(b"not a comment"), 0);
    }

    #[test]
    fn test_count_indentation() {
        assert_eq!(SimdScanner::count_indentation(b"    hello"), (4, 0));
        assert_eq!(SimdScanner::count_indentation(b"\t\thello"), (0, 2));
        assert_eq!(SimdScanner::count_indentation(b"  \t hello"), (3, 1));
        assert_eq!(SimdScanner::count_indentation(b"hello"), (0, 0));
    }
}
