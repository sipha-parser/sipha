//! Tests for lexer functionality

use sipha::lexer::{LexerBuilder, Pattern, CharSet};
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestTokenKind {
    Ident,
    Number,
    Plus,
    Minus,
    Whitespace,
    Eof,
}

impl SyntaxKind for TestTokenKind {
    fn is_terminal(self) -> bool {
        true // All variants are terminals in this test
    }
    
    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[test]
fn test_lexer_builder_basic() {
    let lexer = LexerBuilder::new()
        .token(TestTokenKind::Ident, Pattern::Any)
        .token(TestTokenKind::Number, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::digits())),
            min: 1,
            max: None,
        })
        .token(TestTokenKind::Plus, Pattern::Literal("+".into()))
        .token(TestTokenKind::Minus, Pattern::Literal("-".into()))
        .token(TestTokenKind::Whitespace, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
            min: 1,
            max: None,
        })
        .trivia(TestTokenKind::Whitespace)
        .build(TestTokenKind::Eof, TestTokenKind::Ident);
    
    assert!(lexer.is_ok(), "Lexer should be built successfully");
}

#[test]
fn test_lexer_tokenize_simple() {
    let lexer = LexerBuilder::new()
        .token(TestTokenKind::Number, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::digits())),
            min: 1,
            max: None,
        })
        .token(TestTokenKind::Plus, Pattern::Literal("+".into()))
        .token(TestTokenKind::Whitespace, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
            min: 1,
            max: None,
        })
        .trivia(TestTokenKind::Whitespace)
        .build(TestTokenKind::Eof, TestTokenKind::Number)
        .expect("Failed to build lexer");
    
    let result = lexer.tokenize("42");
    assert!(result.is_ok(), "Should tokenize simple number");
    
    if let Ok(tokens) = result {
        assert!(!tokens.is_empty(), "Should produce at least one token");
    }
}

#[test]
fn test_lexer_tokenize_with_whitespace() {
    let lexer = LexerBuilder::new()
        .token(TestTokenKind::Number, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::digits())),
            min: 1,
            max: None,
        })
        .token(TestTokenKind::Plus, Pattern::Literal("+".into()))
        .token(TestTokenKind::Whitespace, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
            min: 1,
            max: None,
        })
        .trivia(TestTokenKind::Whitespace)
        .build(TestTokenKind::Eof, TestTokenKind::Number)
        .expect("Failed to build lexer");
    
    let result = lexer.tokenize("42 + 10");
    assert!(result.is_ok(), "Should tokenize expression with whitespace");
}

#[test]
fn test_lexer_keywords() {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum KeywordKind {
        If,
        Then,
        Else,
        Ident,
        Whitespace,
        Eof,
    }
    
    impl SyntaxKind for KeywordKind {
        fn is_terminal(self) -> bool {
            true // All variants are terminals
        }
        
        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
        }
    }
    
    let lexer = LexerBuilder::new()
        .token(KeywordKind::Ident, Pattern::Any)
        .keyword("if", KeywordKind::If)
        .keyword("then", KeywordKind::Then)
        .keyword("else", KeywordKind::Else)
        .token(KeywordKind::Whitespace, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
            min: 1,
            max: None,
        })
        .trivia(KeywordKind::Whitespace)
        .build(KeywordKind::Eof, KeywordKind::Ident);
    
    assert!(lexer.is_ok(), "Should build lexer with keywords");
}
