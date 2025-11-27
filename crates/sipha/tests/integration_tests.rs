//! Integration tests for end-to-end parsing workflows

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum CalcSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals
    Expr,
    Term,
    Factor,
}

impl SyntaxKind for CalcSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CalcToken {
    kind: CalcSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for CalcToken {
    type Kind = CalcSyntaxKind;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum CalcNonTerminal {
    Expr,
    Term,
    Factor,
}

impl NonTerminal for CalcNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }
}

fn create_calc_token(kind: CalcSyntaxKind, text: &str, _offset: u32) -> CalcToken {
    CalcToken {
        kind,
        text: text.into(),
    }
}

#[test]
fn test_lexer_grammar_integration() {
    // Build lexer
    let lexer = LexerBuilder::new()
        .token(
            CalcSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(CalcSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(CalcSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(CalcSyntaxKind::Multiply, Pattern::Literal("*".into()))
        .token(CalcSyntaxKind::Divide, Pattern::Literal("/".into()))
        .token(CalcSyntaxKind::LParen, Pattern::Literal("(".into()))
        .token(CalcSyntaxKind::RParen, Pattern::Literal(")".into()))
        .token(
            CalcSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(CalcSyntaxKind::Whitespace)
        .build(CalcSyntaxKind::Eof, CalcSyntaxKind::Number)
        .expect("Failed to build lexer");

    // Tokenize input
    let tokens_result = lexer.tokenize("42 + 10");
    assert!(tokens_result.is_ok(), "Should tokenize expression");

    // Build grammar
    let grammar = GrammarBuilder::new()
        .entry_point(CalcNonTerminal::Expr)
        .rule(
            CalcNonTerminal::Expr,
            Expr::token(create_calc_token(CalcSyntaxKind::Number, "42", 0)),
        )
        .build()
        .expect("Failed to build grammar");

    // Verify grammar structure
    assert_eq!(grammar.entry_point(), &CalcNonTerminal::Expr);
    assert!(grammar.get_rule(&CalcNonTerminal::Expr).is_some());
}

#[test]
fn test_error_handling() {
    use sipha::error::{LexerError, LexerErrorKind, ParseError};

    // Test ParseError creation
    let parse_error = ParseError::UnexpectedToken {
        span: TextRange::new(TextSize::from(0), TextSize::from(5)),
        expected: vec!["number".to_string(), "identifier".to_string()],
    };

    assert_eq!(
        parse_error.span(),
        TextRange::new(TextSize::from(0), TextSize::from(5))
    );

    // Test LexerError creation
    let lexer_error = LexerError {
        span: TextRange::new(TextSize::from(10), TextSize::from(11)),
        kind: LexerErrorKind::UnexpectedChar { char: '!' },
    };

    assert_eq!(
        lexer_error.span(),
        TextRange::new(TextSize::from(10), TextSize::from(11))
    );

    // Test conversion
    let parse_error_from_lexer: ParseError = lexer_error.into();
    match parse_error_from_lexer {
        ParseError::InvalidSyntax { span, .. } => {
            assert_eq!(span, TextRange::new(TextSize::from(10), TextSize::from(11)));
        }
        _ => panic!("Expected InvalidSyntax"),
    }
}
