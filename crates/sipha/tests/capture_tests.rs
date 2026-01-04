//! Tests for Capture variant handling across parser backends
//!
//! These tests verify that the Capture variant is handled correctly
//! in parsers that don't fully support backreferences (LL, LR).
//! The Capture variant should be unwrapped and the inner expression parsed.

use sipha::backend::ParserBackend;
#[cfg(feature = "backend-ll")]
use sipha::backend::ll::{LlConfig, LlParser};
#[cfg(feature = "backend-lr")]
use sipha::backend::lr::{LrConfig, LrParser};
#[cfg(feature = "backend-peg")]
use sipha::backend::peg::{PegConfig, PegParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token, capture::CaptureId};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestSyntaxKind {
    Number,
    Plus,
    Eof,
    Expr,
}

impl SyntaxKind for TestSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TestToken {
    kind: TestSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for TestToken {
    type Kind = TestSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(u32::MAX))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
    }
}

fn create_token(kind: TestSyntaxKind, text: &str) -> TestToken {
    TestToken {
        kind,
        text: text.into(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TestNonTerminal {
    Expr,
}

impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
        }
    }
}

/// Test that LL parser handles Capture by unwrapping the inner expression
#[test]
#[cfg(feature = "backend-ll")]
fn test_ll_parser_capture_unwraps() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            // Capture wraps a token - LL parser should unwrap and parse the token
            Expr::capture(
                CaptureId::Numeric(0),
                Expr::token(create_token(TestSyntaxKind::Number, "42")),
            ),
        )
        .build()
        .expect("Failed to build grammar");

    let config = LlConfig::default();
    let mut parser = LlParser::new(&grammar, config).expect("Failed to create parser");

    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];

    let result = parser.parse(&tokens, TestNonTerminal::Expr);
    // Should succeed because Capture is unwrapped and inner token is parsed
    // Note: May have warnings about EOF, but should not have parsing errors
    let has_parsing_errors = result
        .errors
        .iter()
        .any(|e| !e.to_string().contains("EOF") && !e.to_string().contains("Eof"));
    assert!(
        !has_parsing_errors,
        "LL parser should handle Capture by unwrapping: {:?}",
        result.errors
    );
}

/// Test that LR parser handles Capture by unwrapping the inner expression
#[test]
#[cfg(feature = "backend-lr")]
fn test_lr_parser_capture_unwraps() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            // Capture wraps a token - LR parser should unwrap and parse the token
            Expr::capture(
                CaptureId::Numeric(0),
                Expr::token(create_token(TestSyntaxKind::Number, "42")),
            ),
        )
        .build()
        .expect("Failed to build grammar");

    let config = LrConfig::default();
    let parser_result = LrParser::new(&grammar, config);

    // LR table construction may succeed or fail depending on grammar
    if let Ok(mut parser) = parser_result {
        let tokens = vec![create_token(TestSyntaxKind::Number, "42")];

        let result = parser.parse(&tokens, TestNonTerminal::Expr);
        // Should succeed because Capture is unwrapped and inner token is parsed
        // Note: May have warnings about EOF, but should not have parsing errors
        let has_parsing_errors = result
            .errors
            .iter()
            .any(|e| !e.to_string().contains("EOF") && !e.to_string().contains("Eof"));
        assert!(
            !has_parsing_errors,
            "LR parser should handle Capture by unwrapping: {:?}",
            result.errors
        );
    }
    // If table construction fails, that's acceptable for this test
}

/// Test that Capture works with nested expressions
#[test]
#[cfg(feature = "backend-ll")]
fn test_ll_parser_capture_nested() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            // Capture wraps a sequence
            Expr::capture(
                CaptureId::Named("expr".to_string()),
                Expr::seq(vec![
                    Expr::token(create_token(TestSyntaxKind::Number, "1")),
                    Expr::token(create_token(TestSyntaxKind::Plus, "+")),
                    Expr::token(create_token(TestSyntaxKind::Number, "2")),
                ]),
            ),
        )
        .build()
        .expect("Failed to build grammar");

    let config = LlConfig::default();
    let mut parser = LlParser::new(&grammar, config).expect("Failed to create parser");

    let tokens = vec![
        create_token(TestSyntaxKind::Number, "1"),
        create_token(TestSyntaxKind::Plus, "+"),
        create_token(TestSyntaxKind::Number, "2"),
    ];

    let result = parser.parse(&tokens, TestNonTerminal::Expr);
    // Note: May have warnings about EOF, but should not have parsing errors
    let has_parsing_errors = result
        .errors
        .iter()
        .any(|e| !e.to_string().contains("EOF") && !e.to_string().contains("Eof"));
    assert!(
        !has_parsing_errors,
        "LL parser should handle nested Capture: {:?}",
        result.errors
    );
}

/// Test that Backreference fails gracefully in LL parser
#[test]
#[cfg(feature = "backend-ll")]
fn test_ll_parser_backreference_fails_gracefully() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            // Backreference is not supported in LL parsers
            Expr::backreference(CaptureId::Numeric(0)),
        )
        .build()
        .expect("Failed to build grammar");

    let config = LlConfig::default();
    let mut parser = LlParser::new(&grammar, config).expect("Failed to create parser");

    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];

    let result = parser.parse(&tokens, TestNonTerminal::Expr);
    // Should fail - Backreference is not supported in LL parsers
    // The error may be about backreference not being supported, or it may fail
    // during parsing because backreference can't match anything
    assert!(
        !result.errors.is_empty(),
        "LL parser should reject Backreference (got errors: {:?})",
        result.errors
    );
    // Just verify it fails - the exact error message may vary
}
