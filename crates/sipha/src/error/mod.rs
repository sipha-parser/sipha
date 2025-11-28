//! # Error Types
//!
//! Error types and diagnostics for parsing and lexing.
//!
//! ## Overview
//!
//! This module provides comprehensive error types for:
//!
//! - **Parse errors**: Syntax errors, unexpected tokens, ambiguity
//! - **Lexer errors**: Invalid characters, unterminated strings, etc.
//! - **Warnings**: Recoverable issues and deprecations
//! - **Diagnostics**: Rich error reporting with source locations
//!
//! ## Error Types
//!
//! - [`ParseError`]: Errors during parsing (unexpected tokens, invalid syntax, etc.)
//! - [`LexerError`]: Errors during tokenization
//! - [`ParseWarning`]: Warnings that don't prevent parsing
//!
//! ## Usage
//!
//! ```rust,no_run
//! use sipha::error::{ParseError, ParseResult};
//! use sipha::grammar::{Token, NonTerminal};
//! use sipha::syntax::SyntaxKind;
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MyTokenKind {
//!     Ident,
//!     Number,
//! }
//!
//! impl SyntaxKind for MyTokenKind {
//!     fn is_terminal(self) -> bool {
//!         true
//!     }
//!     
//!     fn is_trivia(self) -> bool {
//!         false
//!     }
//! }
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! struct MyToken;
//!
//! impl Token for MyToken {
//!     type Kind = MyTokenKind;
//!     fn kind(&self) -> Self::Kind {
//!         MyTokenKind::Ident
//!     }
//! }
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MyNonTerminal {
//!     Expr,
//! }
//!
//! impl NonTerminal for MyNonTerminal {
//!     fn name(&self) -> &str {
//!         "Expr"
//!     }
//! }
//!
//! // Example usage (types would need to be properly instantiated)
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! # let parser: std::marker::PhantomData<()> = std::marker::PhantomData;
//! # let tokens: &[MyToken] = &[];
//! # let entry = MyNonTerminal::Expr;
//! // let result: ParseResult<MyToken, MyNonTerminal> = parser.parse(&tokens, entry);
//!
//! // if !result.errors.is_empty() {
//! //     for error in &result.errors {
//! //         eprintln!("Error at {:?}: {}", error.span(), error);
//! //     }
//! // }
//! # Ok(())
//! # }
//! ```
//!
//! ## Diagnostics Support
//!
//! When the `diagnostics` feature is enabled, errors integrate with [`miette`]
//! for rich error reporting with source code snippets and suggestions.

#[cfg(feature = "backend-glr")]
use crate::backend::glr::ParseForest;
use crate::syntax::TextRange;
use thiserror::Error;

#[cfg(feature = "diagnostics")]
use miette::Diagnostic;

#[derive(Debug, Error)]
#[cfg_attr(feature = "diagnostics", derive(Diagnostic))]
pub enum ParseError {
    #[error("Unexpected token")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(parser::unexpected_token)))]
    UnexpectedToken {
        #[cfg_attr(feature = "diagnostics", label("Unexpected token"))]
        span: TextRange,
        expected: Vec<String>,
    },

    #[error("Invalid syntax")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(parser::invalid_syntax)))]
    InvalidSyntax {
        #[cfg_attr(feature = "diagnostics", label)]
        span: TextRange,
        message: String,
    },

    #[error("Unexpected end of file")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(parser::unexpected_eof)))]
    UnexpectedEof {
        #[cfg_attr(feature = "diagnostics", label("Expected"))]
        span: TextRange,
        expected: Vec<String>,
    },

    #[error("Ambiguous parse")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(parser::ambiguous)))]
    Ambiguity {
        #[cfg_attr(feature = "diagnostics", label("Ambiguous here"))]
        span: TextRange,
        alternatives: Vec<String>,
    },
}

/// Lexer error with location information
#[derive(Debug, Clone, Error)]
#[cfg_attr(feature = "diagnostics", derive(Diagnostic))]
#[error("{kind}")]
pub struct LexerError {
    #[cfg_attr(feature = "diagnostics", label)]
    pub span: TextRange,
    #[source]
    pub kind: LexerErrorKind,
}

/// Types of lexer errors
#[derive(Debug, Clone, Error)]
#[cfg_attr(feature = "diagnostics", derive(Diagnostic))]
pub enum LexerErrorKind {
    #[error("Unexpected character: '{char}'")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::unexpected_char)))]
    UnexpectedChar { char: char },

    #[error("Unterminated string literal")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::unterminated_string)))]
    UnterminatedString,

    #[error("Invalid escape sequence: {escape}")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::invalid_escape)))]
    InvalidEscape { escape: String },

    #[error("Invalid number format: {reason}")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::invalid_number)))]
    InvalidNumber { reason: String },

    #[error("Unexpected end of file")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::unexpected_eof)))]
    UnexpectedEof,
}

impl LexerErrorKind {
    /// Create an unexpected character error
    #[must_use]
    pub const fn unexpected_char(char: char) -> Self {
        Self::UnexpectedChar { char }
    }

    /// Create an unterminated string error
    #[must_use]
    pub const fn unterminated_string() -> Self {
        Self::UnterminatedString
    }

    /// Create an invalid escape sequence error
    #[must_use]
    pub const fn invalid_escape(escape: String) -> Self {
        Self::InvalidEscape { escape }
    }

    /// Create an invalid number format error
    #[must_use]
    pub const fn invalid_number(reason: String) -> Self {
        Self::InvalidNumber { reason }
    }

    /// Create an unexpected EOF error
    #[must_use]
    pub const fn unexpected_eof() -> Self {
        Self::UnexpectedEof
    }
}

impl ParseError {
    /// Get the span (location) of this error
    #[must_use]
    pub const fn span(&self) -> TextRange {
        match self {
            Self::UnexpectedToken { span, .. }
            | Self::InvalidSyntax { span, .. }
            | Self::UnexpectedEof { span, .. }
            | Self::Ambiguity { span, .. } => *span,
        }
    }

    /// Create an unexpected token error with improved message formatting
    #[must_use]
    pub const fn unexpected_token(span: TextRange, expected: Vec<String>) -> Self {
        Self::UnexpectedToken { span, expected }
    }

    /// Create an unexpected token error with a single expected token (convenience method)
    #[must_use]
    pub fn unexpected_token_single(span: TextRange, expected: String) -> Self {
        Self::UnexpectedToken {
            span,
            expected: vec![expected],
        }
    }

    /// Create an invalid syntax error
    #[must_use]
    pub const fn invalid_syntax(span: TextRange, message: String) -> Self {
        Self::InvalidSyntax { span, message }
    }

    /// Create an invalid syntax error with a suggestion
    #[must_use]
    pub fn invalid_syntax_with_suggestion(
        span: TextRange,
        message: impl Into<String>,
        suggestion: &str,
    ) -> Self {
        let mut msg = message.into();
        msg.push_str(". Suggestion: ");
        msg.push_str(suggestion);
        Self::InvalidSyntax { span, message: msg }
    }

    /// Create an unexpected EOF error
    #[must_use]
    pub const fn unexpected_eof(span: TextRange, expected: Vec<String>) -> Self {
        Self::UnexpectedEof { span, expected }
    }

    /// Create an unexpected EOF error with a single expected token (convenience method)
    #[must_use]
    pub fn unexpected_eof_single(span: TextRange, expected: String) -> Self {
        Self::UnexpectedEof {
            span,
            expected: vec![expected],
        }
    }

    /// Create an ambiguity error
    #[must_use]
    pub const fn ambiguity(span: TextRange, alternatives: Vec<String>) -> Self {
        Self::Ambiguity { span, alternatives }
    }

    /// Format expected tokens as a human-readable string
    #[must_use]
    pub fn format_expected(&self) -> String {
        match self {
            Self::UnexpectedToken { expected, .. } | Self::UnexpectedEof { expected, .. } => {
                Self::format_expected_list(expected)
            }
            _ => String::new(),
        }
    }

    /// Format a list of expected tokens as a human-readable string
    #[must_use]
    pub fn format_expected_list(expected: &[String]) -> String {
        match expected.len() {
            0 => "nothing".to_string(),
            1 => expected[0].clone(),
            2 => format!("{} or {}", expected[0], expected[1]),
            _ => {
                let mut result = expected[..expected.len() - 1]
                    .iter()
                    .map(std::string::String::as_str)
                    .collect::<Vec<_>>()
                    .join(", ");
                result.push_str(", or ");
                result.push_str(&expected[expected.len() - 1]);
                result
            }
        }
    }

    /// Get a "did you mean" suggestion for unexpected tokens
    ///
    /// This compares the expected tokens with common typos or similar tokens
    /// to provide helpful suggestions.
    ///
    /// # Panics
    ///
    /// This function may panic if `best_match` is `None` when calling `unwrap()`.
    /// This should not happen in normal usage as the code checks `is_none()` first.
    #[must_use]
    pub fn did_you_mean(&self, actual: &str) -> Option<String> {
        match self {
            Self::UnexpectedToken { expected, .. } | Self::UnexpectedEof { expected, .. } => {
                // Simple string similarity check (Levenshtein-like)
                let mut best_match: Option<(&String, f64)> = None;
                for candidate in expected {
                    let similarity = string_similarity(actual, candidate);
                    if similarity > 0.6 {
                        // Threshold for similarity
                        if best_match.is_none() || similarity > best_match.unwrap().1 {
                            best_match = Some((candidate, similarity));
                        }
                    }
                }
                best_match.map(|(suggestion, _)| format!("Did you mean '{suggestion}'?"))
            }
            _ => None,
        }
    }

    /// Get surrounding context for error messages
    ///
    /// This can be used to show code snippets around the error location.
    /// Returns a tuple of (`before_context`, `error_span`, `after_context`).
    #[must_use]
    pub fn get_context(&self, source: &str) -> Option<(String, String, String)> {
        let span = self.span();
        let start = span.start().into() as usize;
        let end = span.end().into() as usize;

        if end > source.len() {
            return None;
        }

        // Get context before (up to 20 characters)
        let context_before_start = start.saturating_sub(20);
        let before = source[context_before_start..start].to_string();

        // Get error span
        let error_span = source[start..end.min(source.len())].to_string();

        // Get context after (up to 20 characters)
        let after_end = (end + 20).min(source.len());
        let after = source[end..after_end].to_string();

        Some((before, error_span, after))
    }

    /// Format error with enhanced context and suggestions
    #[must_use]
    pub fn format_with_context(&self, source: &str, actual_token: Option<&str>) -> String {
        use std::fmt::Write;
        let mut result = String::new();

        // Add main error message
        let _ = write!(result, "{self}");

        // Add context if available
        if let Some((before, error_span, after)) = self.get_context(source) {
            result.push_str("\n\nContext:\n");
            let _ = write!(result, "  ...{before}[{error_span}]{after}...");
        }

        // Add "did you mean" suggestion if available
        if let Some(actual) = actual_token
            && let Some(suggestion) = self.did_you_mean(actual)
        {
            result.push_str("\n\n");
            result.push_str(&suggestion);
        }

        // Add expected tokens
        if let Some(expected_str) = match self {
            Self::UnexpectedToken { expected, .. } | Self::UnexpectedEof { expected, .. } => {
                if expected.is_empty() {
                    None
                } else {
                    Some(format!("Expected: {}", self.format_expected()))
                }
            }
            _ => None,
        } {
            result.push('\n');
            result.push_str(&expected_str);
        }

        result
    }
}

/// Simple string similarity metric (0.0 to 1.0)
/// Uses a basic character overlap approach
fn string_similarity(s1: &str, s2: &str) -> f64 {
    if s1 == s2 {
        return 1.0;
    }

    let s1_lower = s1.to_lowercase();
    let s2_lower = s2.to_lowercase();

    // Count common characters
    let mut common = 0;
    let s1_chars: Vec<char> = s1_lower.chars().collect();
    let s2_chars: Vec<char> = s2_lower.chars().collect();

    let min_len = s1_chars.len().min(s2_chars.len());
    let max_len = s1_chars.len().max(s2_chars.len());

    if max_len == 0 {
        return 1.0;
    }

    for i in 0..min_len {
        if s1_chars[i] == s2_chars[i] {
            common += 1;
        }
    }

    // Simple similarity: common characters / max length
    // Note: usize to f64 may lose precision on 64-bit systems, but acceptable for similarity calculation
    f64::from(common) / f64::from(u32::try_from(max_len).unwrap_or(u32::MAX))
}

impl LexerError {
    /// Create a new lexer error
    #[must_use]
    pub const fn new(span: TextRange, kind: LexerErrorKind) -> Self {
        Self { span, kind }
    }

    /// Get the span (location) of this error
    #[must_use]
    pub const fn span(&self) -> TextRange {
        self.span
    }

    /// Get the kind of lexer error
    #[must_use]
    pub const fn kind(&self) -> &LexerErrorKind {
        &self.kind
    }
}

impl From<LexerError> for ParseError {
    /// Convert a lexer error to a parse error with full context.
    ///
    /// This preserves the original error information and adds context
    /// that the error occurred during tokenization.
    fn from(err: LexerError) -> Self {
        Self::invalid_syntax(
            err.span,
            format!("Lexer error at {:?}: {}", err.span, err.kind),
        )
    }
}

#[derive(Debug, Clone)]
pub struct ParseWarning {
    pub span: TextRange,
    pub message: String,
    pub severity: Severity,
}

impl ParseWarning {
    /// Create a new parse warning
    #[must_use]
    pub const fn new(span: TextRange, message: String, severity: Severity) -> Self {
        Self {
            span,
            message,
            severity,
        }
    }

    /// Create a warning-level parse warning
    #[must_use]
    pub const fn warning(span: TextRange, message: String) -> Self {
        Self::new(span, message, Severity::Warning)
    }

    /// Create an info-level parse warning
    #[must_use]
    pub const fn info(span: TextRange, message: String) -> Self {
        Self::new(span, message, Severity::Info)
    }

    /// Create a hint-level parse warning
    #[must_use]
    pub const fn hint(span: TextRange, message: String) -> Self {
        Self::new(span, message, Severity::Hint)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Warning,
    Info,
    Hint,
}

#[derive(Debug, Default)]
pub struct ParseMetrics {
    pub tokens_consumed: usize,
    pub nodes_created: usize,
    pub errors_recovered: usize,
    pub parse_time: std::time::Duration,
    pub cache_hits: usize,
}

pub struct ParseResult<T, N>
where
    T: crate::grammar::Token,
{
    pub root: std::sync::Arc<crate::syntax::GreenNode<T::Kind>>,
    pub errors: Vec<ParseError>,
    pub warnings: Vec<ParseWarning>,
    pub metrics: ParseMetrics,
    #[cfg(feature = "backend-glr")]
    pub forest: Option<ParseForest<T::Kind>>,
    pub(crate) _phantom: std::marker::PhantomData<N>,
}

impl<T, N> ParseResult<T, N>
where
    T: crate::grammar::Token,
{
    /// Create a new `ParseResult` with the given root node, errors, warnings, and metrics.
    ///
    /// # Example
    /// ```
    /// # use sipha::error::{ParseResult, ParseError, ParseMetrics};
    /// # use sipha::syntax::{GreenNodeBuilder, SyntaxKind, TextSize};
    /// # use sipha::grammar::{Token, NonTerminal};
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum TestKind { Root }
    /// # impl SyntaxKind for TestKind {
    /// #     fn is_terminal(self) -> bool { true }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # struct TestToken;
    /// # impl Token for TestToken {
    /// #     type Kind = TestKind;
    /// #     fn kind(&self) -> Self::Kind { TestKind::Root }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # struct TestNonTerminal;
    /// # impl NonTerminal for TestNonTerminal {
    /// #     fn name(&self) -> &str { "test" }
    /// # }
    /// # let mut builder = GreenNodeBuilder::new();
    /// # builder.start_node(TestKind::Root);
    /// # let root = builder.finish().unwrap();
    /// # let result: ParseResult<TestToken, TestNonTerminal> = ParseResult::new(root, vec![], vec![], ParseMetrics::default());
    /// ```
    pub const fn new(
        root: std::sync::Arc<crate::syntax::GreenNode<T::Kind>>,
        errors: Vec<ParseError>,
        warnings: Vec<ParseWarning>,
        metrics: ParseMetrics,
    ) -> Self {
        Self {
            root,
            errors,
            warnings,
            metrics,
            #[cfg(feature = "backend-glr")]
            forest: None,
            _phantom: std::marker::PhantomData,
        }
    }

    #[cfg(feature = "backend-glr")]
    /// Access the parse forest for ambiguous GLR parses, when available.
    #[must_use]
    pub const fn forest(&self) -> Option<&ParseForest<T::Kind>> {
        self.forest.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{TextRange, TextSize};

    #[test]
    fn test_parse_error_unexpected_token() {
        let range = TextRange::new(TextSize::from(10), TextSize::from(15));
        let error = ParseError::UnexpectedToken {
            span: range,
            expected: vec!["identifier".to_string(), "number".to_string()],
        };

        let error_str = format!("{error}");
        assert!(error_str.contains("Unexpected token"));
    }

    #[test]
    fn test_parse_error_invalid_syntax() {
        let range = TextRange::new(TextSize::from(0), TextSize::from(5));
        let error = ParseError::InvalidSyntax {
            span: range,
            message: "Invalid expression".to_string(),
        };

        let error_str = format!("{error}");
        assert!(error_str.contains("Invalid syntax"));
    }

    #[test]
    fn test_parse_error_unexpected_eof() {
        let range = TextRange::new(TextSize::from(100), TextSize::from(100));
        let error = ParseError::UnexpectedEof {
            span: range,
            expected: vec!["}".to_string()],
        };

        let error_str = format!("{error}");
        assert!(error_str.contains("Unexpected end of file"));
    }

    #[test]
    fn test_parse_error_ambiguity() {
        let range = TextRange::new(TextSize::from(20), TextSize::from(25));
        let error = ParseError::Ambiguity {
            span: range,
            alternatives: vec!["expr1".to_string(), "expr2".to_string()],
        };

        let error_str = format!("{error}");
        assert!(error_str.contains("Ambiguous parse"));
    }

    #[test]
    fn test_lexer_error_kind() {
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));
        let error = LexerError {
            span: range,
            kind: LexerErrorKind::UnexpectedChar { char: '!' },
        };

        assert_eq!(error.span, range);
        match error.kind {
            LexerErrorKind::UnexpectedChar { char } => assert_eq!(char, '!'),
            _ => panic!("Wrong error kind"),
        }
    }

    #[test]
    fn test_lexer_error_kinds() {
        let range = TextRange::new(TextSize::from(0), TextSize::from(1));

        let errors = [
            LexerError {
                span: range,
                kind: LexerErrorKind::UnexpectedChar { char: 'x' },
            },
            LexerError {
                span: range,
                kind: LexerErrorKind::UnterminatedString,
            },
            LexerError {
                span: range,
                kind: LexerErrorKind::InvalidEscape {
                    escape: "\\z".to_string(),
                },
            },
            LexerError {
                span: range,
                kind: LexerErrorKind::InvalidNumber {
                    reason: "Invalid format".to_string(),
                },
            },
            LexerError {
                span: range,
                kind: LexerErrorKind::UnexpectedEof,
            },
        ];

        assert_eq!(errors.len(), 5);
    }

    #[test]
    fn test_lexer_error_to_parse_error() {
        let range = TextRange::new(TextSize::from(10), TextSize::from(15));
        let lexer_error = LexerError {
            span: range,
            kind: LexerErrorKind::UnexpectedChar { char: '#' },
        };

        let parse_error: ParseError = lexer_error.into();
        match parse_error {
            ParseError::InvalidSyntax { span, message } => {
                assert_eq!(span, range);
                assert!(message.contains("Lexer error"));
            }
            _ => panic!("Expected InvalidSyntax"),
        }
    }

    #[test]
    fn test_parse_warning() {
        let range = TextRange::new(TextSize::from(0), TextSize::from(10));
        let warning = ParseWarning {
            span: range,
            message: "Deprecated syntax".to_string(),
            severity: Severity::Warning,
        };

        assert_eq!(warning.span, range);
        assert_eq!(warning.severity, Severity::Warning);
        assert_eq!(warning.message, "Deprecated syntax");
    }

    #[test]
    fn test_severity() {
        assert_eq!(Severity::Warning, Severity::Warning);
        assert_ne!(Severity::Warning, Severity::Info);
        assert_ne!(Severity::Info, Severity::Hint);
    }

    #[test]
    fn test_parse_metrics_default() {
        let metrics = ParseMetrics::default();
        assert_eq!(metrics.tokens_consumed, 0);
        assert_eq!(metrics.nodes_created, 0);
        assert_eq!(metrics.errors_recovered, 0);
        assert_eq!(metrics.cache_hits, 0);
        assert_eq!(metrics.parse_time, std::time::Duration::ZERO);
    }

    #[test]
    fn test_parse_metrics() {
        let metrics = ParseMetrics {
            tokens_consumed: 100,
            nodes_created: 50,
            errors_recovered: 2,
            cache_hits: 10,
            parse_time: std::time::Duration::from_millis(5),
        };

        assert_eq!(metrics.tokens_consumed, 100);
        assert_eq!(metrics.nodes_created, 50);
        assert_eq!(metrics.errors_recovered, 2);
        assert_eq!(metrics.cache_hits, 10);
        assert_eq!(metrics.parse_time.as_millis(), 5);
    }
}
