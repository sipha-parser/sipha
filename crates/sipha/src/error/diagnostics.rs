//! # Diagnostic Utilities
//!
//! Enhanced diagnostic utilities for better error messages and suggestions.
//!
//! This module provides utilities for:
//! - Generating "Did you mean?" suggestions
//! - Context-aware error reporting
//! - Error message formatting with surrounding code
//! - Similarity-based token matching

use crate::error::ParseError;
use crate::syntax::{TextRange, line_col::LineIndex};
use std::fmt::Write;

#[cfg(test)]
use crate::syntax::TextSize;

/// Generate a "Did you mean?" suggestion for an unexpected token
///
/// This function uses string similarity to suggest the most likely intended token
/// from a list of expected tokens.
///
/// # Example
///
/// ```rust,no_run
/// use sipha::error::diagnostics::did_you_mean;
///
/// let actual = "identifer";
/// let expected = vec!["identifier", "number", "string"];
/// if let Some(suggestion) = did_you_mean(actual, &expected) {
///     println!("Did you mean: {}?", suggestion);
/// }
/// ```
pub fn did_you_mean(actual: &str, expected: &[String]) -> Option<String> {
    if expected.is_empty() {
        return None;
    }

    let actual_lower = actual.to_lowercase();
    let mut best_match: Option<(&String, f64)> = None;
    let threshold = 0.6; // Minimum similarity threshold

    for candidate in expected {
        let similarity = string_similarity(&actual_lower, &candidate.to_lowercase());
        if similarity >= threshold {
            match best_match {
                None => best_match = Some((candidate, similarity)),
                Some((_, best_sim)) if similarity > best_sim => {
                    best_match = Some((candidate, similarity));
                }
                _ => {}
            }
        }
    }

    best_match.map(|(suggestion, _)| suggestion.clone())
}

/// Generate multiple suggestions for an unexpected token
///
/// Returns up to `max_suggestions` tokens sorted by similarity.
pub fn suggest_tokens(actual: &str, expected: &[String], max_suggestions: usize) -> Vec<String> {
    if expected.is_empty() {
        return Vec::new();
    }

    let actual_lower = actual.to_lowercase();
    let mut candidates: Vec<(&String, f64)> = expected
        .iter()
        .map(|candidate| {
            let similarity = string_similarity(&actual_lower, &candidate.to_lowercase());
            (candidate, similarity)
        })
        .filter(|(_, sim)| *sim >= 0.4) // Lower threshold for multiple suggestions
        .collect();

    candidates.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    candidates
        .into_iter()
        .take(max_suggestions)
        .map(|(s, _)| s.clone())
        .collect()
}

/// Extract context around an error location
///
/// Returns (before_context, error_span, after_context) with up to `context_chars`
/// characters before and after the error.
pub fn extract_context(
    source: &str,
    error_span: TextRange,
    context_chars: usize,
) -> Option<(String, String, String)> {
    let start = usize::from(error_span.start());
    let end = usize::from(error_span.end());

    if start > source.len() || end > source.len() || start > end {
        return None;
    }

    let context_start = start.saturating_sub(context_chars);
    let context_end = (end + context_chars).min(source.len());

    let before = source[context_start..start].to_string();
    let error = source[start..end].to_string();
    let after = source[end..context_end].to_string();

    Some((before, error, after))
}

/// Format an error with surrounding context
pub fn format_error_with_context(error: &ParseError, source: &str, context_chars: usize) -> String {
    let mut result = String::new();

    // Main error message
    write!(result, "{error}").unwrap();

    // Add context if available
    if let Some(span) = error.span()
        && let Some((before, error_text, after)) = extract_context(source, span, context_chars)
    {
        result.push_str("\n\nContext:\n");
        result.push_str("  ...");
        result.push_str(&before);
        result.push('[');
        result.push_str(&error_text);
        result.push(']');
        result.push_str(&after);
        result.push_str("...");
    }

    // Add suggestions
    if let Some(expected) = error.expected_tokens()
        && !expected.is_empty()
    {
        result.push_str("\n\nExpected one of: ");
        result.push_str(&expected.join(", "));
    }

    result
}

/// Format error with line/column information
pub fn format_error_with_location(
    error: &ParseError,
    source: &str,
    filename: Option<&str>,
) -> String {
    let mut result = String::new();

    if let Some(span) = error.span() {
        let line_index = LineIndex::new(source);
        let start_line_col = line_index.line_col(span.start());
        if let Some(filename) = filename {
            write!(
                result,
                "{}:{}:{}: ",
                filename,
                start_line_col.line + 1,
                start_line_col.column + 1
            )
            .unwrap();
        } else {
            write!(
                result,
                "{}:{}: ",
                start_line_col.line + 1,
                start_line_col.column + 1
            )
            .unwrap();
        }
    }

    write!(result, "{error}").unwrap();
    result
}

/// Calculate string similarity using Levenshtein distance
///
/// Returns a value between 0.0 (completely different) and 1.0 (identical).
fn string_similarity(s1: &str, s2: &str) -> f64 {
    if s1 == s2 {
        return 1.0;
    }

    if s1.is_empty() || s2.is_empty() {
        return 0.0;
    }

    let distance = levenshtein_distance(s1, s2);
    let max_len = s1.len().max(s2.len());
    1.0 - (distance as f64 / max_len as f64)
}

/// Calculate Levenshtein distance between two strings
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1_chars: Vec<char> = s1.chars().collect();
    let s2_chars: Vec<char> = s2.chars().collect();
    let s1_len = s1_chars.len();
    let s2_len = s2_chars.len();

    if s1_len == 0 {
        return s2_len;
    }
    if s2_len == 0 {
        return s1_len;
    }

    let mut matrix = vec![vec![0; s2_len + 1]; s1_len + 1];

    // Initialize first row and column
    for (i, row) in matrix.iter_mut().enumerate().take(s1_len + 1) {
        row[0] = i;
    }
    for (j, cell) in matrix[0].iter_mut().enumerate().take(s2_len + 1) {
        *cell = j;
    }

    // Fill the matrix
    for i in 1..=s1_len {
        for j in 1..=s2_len {
            let cost = usize::from(s1_chars[i - 1] != s2_chars[j - 1]);
            matrix[i][j] = (matrix[i - 1][j] + 1)
                .min(matrix[i][j - 1] + 1)
                .min(matrix[i - 1][j - 1] + cost);
        }
    }

    matrix[s1_len][s2_len]
}

/// Helper trait for ParseError to get expected tokens
trait ParseErrorExt {
    fn expected_tokens(&self) -> Option<&[String]>;
}

impl ParseErrorExt for ParseError {
    fn expected_tokens(&self) -> Option<&[String]> {
        match self {
            ParseError::UnexpectedToken { expected, .. } => Some(expected),
            ParseError::UnexpectedEof { expected, .. } => Some(expected),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_did_you_mean() {
        let actual = "identifer";
        let expected = vec!["identifier".to_string(), "number".to_string()];
        let suggestion = did_you_mean(actual, &expected);
        assert_eq!(suggestion, Some("identifier".to_string()));
    }

    #[test]
    fn test_string_similarity() {
        assert_eq!(string_similarity("hello", "hello"), 1.0);
        assert!(string_similarity("hello", "hell") > 0.5);
        assert!(string_similarity("hello", "world") < 0.5);
    }

    #[test]
    fn test_extract_context() {
        let source = "This is a test string";
        let span = TextRange::new(TextSize::from(10), TextSize::from(14)); // "test"
        let (before, error, after) = extract_context(source, span, 5).unwrap();
        assert_eq!(before, "is a ");
        assert_eq!(error, "test");
        assert_eq!(after, " stri");
    }

    #[test]
    fn test_extract_context_at_start() {
        let source = "Hello world";
        let span = TextRange::new(TextSize::from(0), TextSize::from(5)); // "Hello"
        let (before, error, after) = extract_context(source, span, 5).unwrap();
        assert_eq!(before, "");
        assert_eq!(error, "Hello");
        assert_eq!(after, " worl"); // 5 chars after
    }

    #[test]
    fn test_extract_context_at_end() {
        let source = "Hello world";
        let span = TextRange::new(TextSize::from(6), TextSize::from(11)); // "world"
        let (before, error, after) = extract_context(source, span, 5).unwrap();
        assert_eq!(before, "ello "); // 5 chars before (from position 1 to 6)
        assert_eq!(error, "world");
        assert_eq!(after, "");
    }

    #[test]
    fn test_extract_context_small_source() {
        let source = "Hi";
        let span = TextRange::new(TextSize::from(0), TextSize::from(2)); // "Hi"
        let (before, error, after) = extract_context(source, span, 5).unwrap();
        assert_eq!(before, "");
        assert_eq!(error, "Hi");
        assert_eq!(after, "");
    }

    #[test]
    fn test_extract_context_invalid_span() {
        let source = "Hello";
        let span = TextRange::new(TextSize::from(10), TextSize::from(15)); // Out of bounds
        let result = extract_context(source, span, 5);
        assert!(result.is_none(), "Should return None for invalid span");
    }

    #[test]
    fn test_extract_context_zero_context() {
        let source = "This is a test string";
        let span = TextRange::new(TextSize::from(10), TextSize::from(14)); // "test"
        let (before, error, after) = extract_context(source, span, 0).unwrap();
        assert_eq!(before, "");
        assert_eq!(error, "test");
        assert_eq!(after, "");
    }
}
