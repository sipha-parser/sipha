//! Tests for lexer functionality

use sipha::lexer::{CharSet, LexerBuilder, Pattern};
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
        .token(
            TestTokenKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(TestTokenKind::Plus, Pattern::Literal("+".into()))
        .token(TestTokenKind::Minus, Pattern::Literal("-".into()))
        .token(
            TestTokenKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(TestTokenKind::Whitespace)
        .build(TestTokenKind::Eof, TestTokenKind::Ident);

    assert!(lexer.is_ok(), "Lexer should be built successfully");
}

#[test]
fn test_lexer_tokenize_simple() {
    let lexer = LexerBuilder::new()
        .token(
            TestTokenKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(TestTokenKind::Plus, Pattern::Literal("+".into()))
        .token(
            TestTokenKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
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
        .token(
            TestTokenKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(TestTokenKind::Plus, Pattern::Literal("+".into()))
        .token(
            TestTokenKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
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
        .token(
            KeywordKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(KeywordKind::Whitespace)
        .build(KeywordKind::Eof, KeywordKind::Ident);

    assert!(lexer.is_ok(), "Should build lexer with keywords");
}

#[test]
fn test_lexer_regex_patterns() {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum RegexKind {
        Number,
        Word,
        Ident,
        Whitespace,
        Eof,
    }

    impl SyntaxKind for RegexKind {
        fn is_terminal(self) -> bool {
            true
        }

        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
        }
    }

    let lexer = LexerBuilder::new()
        .token(RegexKind::Number, Pattern::Regex(r"\d+".into()))
        .token(RegexKind::Word, Pattern::Regex(r"\w+".into()))
        .token(RegexKind::Ident, Pattern::Any)
        .token(RegexKind::Whitespace, Pattern::Regex(r"\s+".into()))
        .trivia(RegexKind::Whitespace)
        .build(RegexKind::Eof, RegexKind::Ident)
        .expect("Failed to build lexer");

    let result = lexer.tokenize("123 abc_123");
    assert!(result.is_ok(), "Should tokenize with regex patterns");

    if let Ok(tokens) = result {
        // Should have Number, Word, and Eof tokens
        assert!(tokens.len() >= 2, "Should produce at least 2 tokens");
    }
}

#[test]
fn test_lexer_pattern_any() {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum AnyKind {
        Ident,
        Number,
        Eof,
    }

    impl SyntaxKind for AnyKind {
        fn is_terminal(self) -> bool {
            true
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    let lexer = LexerBuilder::new()
        .token(AnyKind::Number, Pattern::Regex(r"\d+".into()))
        .token(AnyKind::Ident, Pattern::Any)
        .build(AnyKind::Eof, AnyKind::Ident)
        .expect("Failed to build lexer");

    let result = lexer.tokenize("hello world 42");
    assert!(result.is_ok(), "Should tokenize with Pattern::Any");

    if let Ok(tokens) = result {
        // Should have Ident tokens for "hello" and "world", Number for "42", and Eof
        assert!(tokens.len() >= 3, "Should produce at least 3 tokens");
    }
}

#[test]
fn test_lexer_regex_character_class() {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum CharClassKind {
        Letters,
        Digits,
        Eof,
    }

    impl SyntaxKind for CharClassKind {
        fn is_terminal(self) -> bool {
            true
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    let lexer = LexerBuilder::new()
        .token(CharClassKind::Letters, Pattern::Regex("[a-zA-Z]+".into()))
        .token(CharClassKind::Digits, Pattern::Regex("[0-9]+".into()))
        .build(CharClassKind::Eof, CharClassKind::Letters)
        .expect("Failed to build lexer");

    let result = lexer.tokenize("hello123world");
    assert!(result.is_ok(), "Should tokenize with character class regex");

    if let Ok(tokens) = result {
        // Should have Letters, Digits, Letters, and Eof tokens
        assert!(tokens.len() >= 3, "Should produce at least 3 tokens");
    }
}
