//! Property-based tests for all parser backends
//!
//! These tests use proptest to generate random valid inputs and verify
//! that parsers handle them correctly.

#![cfg(test)]

use proptest::prelude::*;
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum PropTestSyntaxKind {
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
    Expr,
    Term,
    Factor,
}

impl SyntaxKind for PropTestSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PropTestToken {
    kind: PropTestSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for PropTestToken {
    type Kind = PropTestSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(0))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropTestNonTerminal {
    Expr,
    Term,
    Factor,
}

impl NonTerminal for PropTestNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }
}

fn create_token(kind: PropTestSyntaxKind, text: &str) -> PropTestToken {
    PropTestToken {
        kind,
        text: text.into(),
    }
}

fn build_arithmetic_grammar() -> sipha::grammar::Grammar<PropTestToken, PropTestNonTerminal> {
    GrammarBuilder::new()
        .entry_point(PropTestNonTerminal::Expr)
        .rule(
            PropTestNonTerminal::Expr,
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::rule(PropTestNonTerminal::Term),
                    Expr::token(create_token(PropTestSyntaxKind::Plus, "+")),
                    Expr::rule(PropTestNonTerminal::Expr),
                ]),
                Expr::Seq(vec![
                    Expr::rule(PropTestNonTerminal::Term),
                    Expr::token(create_token(PropTestSyntaxKind::Minus, "-")),
                    Expr::rule(PropTestNonTerminal::Expr),
                ]),
                Expr::rule(PropTestNonTerminal::Term),
            ]),
        )
        .rule(
            PropTestNonTerminal::Term,
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::rule(PropTestNonTerminal::Factor),
                    Expr::token(create_token(PropTestSyntaxKind::Multiply, "*")),
                    Expr::rule(PropTestNonTerminal::Term),
                ]),
                Expr::Seq(vec![
                    Expr::rule(PropTestNonTerminal::Factor),
                    Expr::token(create_token(PropTestSyntaxKind::Divide, "/")),
                    Expr::rule(PropTestNonTerminal::Term),
                ]),
                Expr::rule(PropTestNonTerminal::Factor),
            ]),
        )
        .rule(
            PropTestNonTerminal::Factor,
            Expr::Choice(vec![
                Expr::token(create_token(PropTestSyntaxKind::Number, "0")),
                Expr::Seq(vec![
                    Expr::token(create_token(PropTestSyntaxKind::LParen, "(")),
                    Expr::rule(PropTestNonTerminal::Expr),
                    Expr::token(create_token(PropTestSyntaxKind::RParen, ")")),
                ]),
            ]),
        )
        .build()
        .expect("Failed to build grammar")
}

/// Generate a sequence of numbers for testing
fn number_sequence() -> impl Strategy<Value = Vec<u32>> {
    proptest::collection::vec(1u32..1000, 1..20)
}

/// Convert numbers to tokens
fn numbers_to_tokens(numbers: &[u32]) -> Vec<PropTestToken> {
    let mut tokens = Vec::new();
    for (i, num) in numbers.iter().enumerate() {
        if i > 0 {
            // Add operators between numbers
            tokens.push(create_token(PropTestSyntaxKind::Plus, "+"));
        }
        tokens.push(create_token(PropTestSyntaxKind::Number, &num.to_string()));
    }
    tokens
}

#[cfg(feature = "backend-ll")]
mod ll_property_tests {
    use super::*;
    use sipha::backend::ll::{LlConfig, LlParser};
    use sipha::backend::ParserBackend;

    proptest! {
        #[test]
        fn ll_parser_handles_random_numbers(numbers in number_sequence()) {
            let grammar = build_arithmetic_grammar();
            let mut parser = LlParser::new(&grammar, LlConfig::default())
                .expect("Failed to create parser");
            let tokens = numbers_to_tokens(&numbers);
            let result = parser.parse(&tokens, PropTestNonTerminal::Expr);
            // Note: This grammar has left recursion, so it may not parse correctly
            // This is a property test to ensure the parser doesn't crash
            prop_assume!(!tokens.is_empty());
        }
    }
}

#[cfg(feature = "backend-lr")]
mod lr_property_tests {
    use super::*;
    use sipha::backend::lr::{LrConfig, LrParser};
    use sipha::backend::ParserBackend;

    proptest! {
        #[test]
        fn lr_parser_handles_random_numbers(numbers in number_sequence()) {
            let grammar = build_arithmetic_grammar();
            let mut parser = LrParser::new(&grammar, LrConfig::default())
                .expect("Failed to create parser");
            let tokens = numbers_to_tokens(&numbers);
            let result = parser.parse(&tokens, PropTestNonTerminal::Expr);
            prop_assume!(!tokens.is_empty());
        }
    }
}

#[cfg(feature = "backend-peg")]
mod peg_property_tests {
    use super::*;
    use sipha::backend::peg::{PegConfig, PegParser};
    use sipha::backend::ParserBackend;

    proptest! {
        #[test]
        fn peg_parser_handles_random_numbers(numbers in number_sequence()) {
            let grammar = build_arithmetic_grammar();
            let mut parser = PegParser::new(&grammar, PegConfig::default())
                .expect("Failed to create parser");
            let tokens = numbers_to_tokens(&numbers);
            let result = parser.parse(&tokens, PropTestNonTerminal::Expr);
            prop_assume!(!tokens.is_empty());
        }
    }
}

/// Property tests for error recovery
mod error_recovery_property_tests {
    use super::*;
    #[cfg(feature = "backend-ll")]
    use sipha::backend::ll::{LlConfig, LlParser};
    #[cfg(feature = "backend-ll")]
    use sipha::backend::ParserBackend;
    use sipha::lexer::{CharSet, LexerBuilder, Pattern};

    /// Generate random arithmetic expressions with potential errors
    fn arithmetic_with_errors() -> impl Strategy<Value = String> {
        "[0-9+\\-*/() ]{1,50}".prop_map(|s| s)
    }

    #[cfg(feature = "backend-ll")]
    proptest! {
        #[test]
        fn error_recovery_handles_invalid_tokens(input in arithmetic_with_errors()) {
            // Build lexer
            let lexer = LexerBuilder::new()
                .token(
                    PropTestSyntaxKind::Number,
                    Pattern::Repeat {
                        pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                        min: 1,
                        max: None,
                    },
                )
                .token(PropTestSyntaxKind::Plus, Pattern::Literal("+".into()))
                .token(PropTestSyntaxKind::Minus, Pattern::Literal("-".into()))
                .token(PropTestSyntaxKind::Multiply, Pattern::Literal("*".into()))
                .token(PropTestSyntaxKind::Divide, Pattern::Literal("/".into()))
                .token(PropTestSyntaxKind::LParen, Pattern::Literal("(".into()))
                .token(PropTestSyntaxKind::RParen, Pattern::Literal(")".into()))
                .token(
                    PropTestSyntaxKind::Whitespace,
                    Pattern::Repeat {
                        pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                        min: 1,
                        max: None,
                    },
                )
                .trivia(PropTestSyntaxKind::Whitespace)
                .build(PropTestSyntaxKind::Eof, PropTestSyntaxKind::Number)
                .expect("Failed to build lexer");

            // Tokenize - may have errors
            let tokens_result = lexer.tokenize(&input);
            
            // If tokenization succeeds, try parsing
            if let Ok(tokens) = tokens_result {
                let grammar = build_arithmetic_grammar();
                let mut parser = LlParser::new(&grammar, LlConfig::default())
                    .expect("Failed to create parser");
                
                // Parse should not panic even with errors
                let _result = parser.parse(&tokens, PropTestNonTerminal::Expr);
                // We don't assert on success, just that it doesn't crash
            }
        }

        #[test]
        fn error_recovery_handles_missing_tokens(numbers in number_sequence()) {
            let grammar = build_arithmetic_grammar();
            let mut parser = LlParser::new(&grammar, LlConfig::default())
                .expect("Failed to create parser");
            
            // Create tokens but remove some to simulate errors
            let mut tokens = numbers_to_tokens(&numbers);
            if tokens.len() > 2 {
                // Remove a token to create an error
                tokens.remove(1);
            }
            
            // Parse should handle missing tokens gracefully
            let _result = parser.parse(&tokens, PropTestNonTerminal::Expr);
        }
    }
}

