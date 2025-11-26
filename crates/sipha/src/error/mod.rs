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

use thiserror::Error;
use crate::syntax::TextRange;

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
    UnexpectedChar { 
        char: char 
    },
    
    #[error("Unterminated string literal")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::unterminated_string)))]
    UnterminatedString,
    
    #[error("Invalid escape sequence: {escape}")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::invalid_escape)))]
    InvalidEscape { 
        escape: String 
    },
    
    #[error("Invalid number format: {reason}")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::invalid_number)))]
    InvalidNumber { 
        reason: String 
    },
    
    #[error("Unexpected end of file")]
    #[cfg_attr(feature = "diagnostics", diagnostic(code(lexer::unexpected_eof)))]
    UnexpectedEof,
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
}

impl LexerError {
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
        Self::InvalidSyntax {
            span: err.span,
            message: format!("Lexer error at {:?}: {}", err.span, err.kind),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseWarning {
    pub span: TextRange,
    pub message: String,
    pub severity: Severity,
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
            _phantom: std::marker::PhantomData,
        }
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
                kind: LexerErrorKind::InvalidEscape { escape: "\\z".to_string() },
            },
            LexerError {
                span: range,
                kind: LexerErrorKind::InvalidNumber { reason: "Invalid format".to_string() },
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

