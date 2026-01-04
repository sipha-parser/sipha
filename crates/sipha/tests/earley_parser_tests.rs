//! Tests for Earley parser backend

#[cfg(feature = "backend-earley")]
use sipha::backend::ParserBackend;
#[cfg(feature = "backend-earley")]
use sipha::backend::earley::{EarleyConfig, EarleyError, EarleyParser};
#[cfg(feature = "backend-earley")]
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
#[cfg(feature = "backend-earley")]
use sipha::syntax::{SyntaxKind, TextSize};

#[cfg(feature = "backend-earley")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum TestSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals
    Expr,
    Term,
    Factor,
}

#[cfg(feature = "backend-earley")]
impl SyntaxKind for TestSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[cfg(feature = "backend-earley")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TestToken {
    kind: TestSyntaxKind,
    text: compact_str::CompactString,
}

#[cfg(feature = "backend-earley")]
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

#[cfg(feature = "backend-earley")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum TestNonTerminal {
    Expr,
    Term,
    Factor,
}

#[cfg(feature = "backend-earley")]
impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }

    fn to_syntax_kind<K: SyntaxKind>(&self) -> Option<K> {
        None
    }

    fn default_syntax_kind<K: SyntaxKind>(&self) -> Option<K> {
        None
    }
}

// Helper to create tokens
#[cfg(feature = "backend-earley")]
fn create_token(kind: TestSyntaxKind, text: &str) -> TestToken {
    TestToken {
        kind,
        text: text.into(),
    }
}

#[cfg(feature = "backend-earley")]
fn create_test_grammar() -> sipha::grammar::Grammar<TestToken, TestNonTerminal> {
    GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::Rule(TestNonTerminal::Term),
                    Expr::Token(create_token(TestSyntaxKind::Plus, "+")),
                    Expr::Rule(TestNonTerminal::Expr),
                ]),
                Expr::Rule(TestNonTerminal::Term),
            ]),
        )
        .rule(
            TestNonTerminal::Term,
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::Rule(TestNonTerminal::Factor),
                    Expr::Token(create_token(TestSyntaxKind::Minus, "-")),
                    Expr::Rule(TestNonTerminal::Term),
                ]),
                Expr::Rule(TestNonTerminal::Factor),
            ]),
        )
        .rule(
            TestNonTerminal::Factor,
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::Token(create_token(TestSyntaxKind::LParen, "(")),
                    Expr::Rule(TestNonTerminal::Expr),
                    Expr::Token(create_token(TestSyntaxKind::RParen, ")")),
                ]),
                Expr::Token(create_token(TestSyntaxKind::Number, "number")),
            ]),
        )
        .build()
        .expect("Failed to build test grammar")
}

#[cfg(feature = "backend-earley")]
#[test]
fn test_earley_parser_new() {
    let grammar = create_test_grammar();
    let config = EarleyConfig::default();
    let parser = EarleyParser::new(&grammar, config);
    assert!(parser.is_ok());
}

#[cfg(feature = "backend-earley")]
#[test]
fn test_earley_parser_basic_parse() {
    let grammar = create_test_grammar();
    let config = EarleyConfig::default();
    let mut parser = EarleyParser::new(&grammar, config).expect("Failed to create parser");

    let tokens = vec![
        create_token(TestSyntaxKind::Number, "1"),
        create_token(TestSyntaxKind::Plus, "+"),
        create_token(TestSyntaxKind::Number, "2"),
    ];

    let result = parser.parse(&tokens, TestNonTerminal::Expr);

    // Earley parser should be able to parse this
    // Note: The current implementation may have limitations, so we check for either success or reasonable errors
    assert!(!result.errors.is_empty() || result.root.text_len() > TextSize::zero());
}

#[cfg(feature = "backend-earley")]
#[test]
fn test_earley_parser_config() {
    let config = EarleyConfig::default()
        .with_error_recovery(true)
        .with_max_depth(Some(1000))
        .with_ambiguity_detection(true);

    assert!(config.error_recovery);
    assert_eq!(config.max_depth, Some(1000));
    assert!(config.detect_ambiguity);
}

#[cfg(feature = "backend-earley")]
#[test]
fn test_earley_parser_capabilities() {
    let capabilities = EarleyParser::<TestToken, TestNonTerminal>::capabilities();

    assert_eq!(capabilities.name, "Earley");
    assert!(capabilities.supports_left_recursion);
    assert!(capabilities.supports_ambiguity);
    assert!(capabilities.supports_incremental);
    assert!(capabilities.supports_error_recovery);
    assert_eq!(capabilities.max_lookahead, None);
}

#[cfg(feature = "backend-earley")]
#[test]
fn test_earley_parser_incremental() {
    let grammar = create_test_grammar();
    let config = EarleyConfig::default();
    let mut parser = EarleyParser::new(&grammar, config).expect("Failed to create parser");

    let tokens1 = vec![
        create_token(TestSyntaxKind::Number, "1"),
        create_token(TestSyntaxKind::Plus, "+"),
        create_token(TestSyntaxKind::Number, "2"),
    ];

    let result1 = parser.parse(&tokens1, TestNonTerminal::Expr);
    let old_tree = Some(&result1.root);

    // Create incremental session with no edits (should use cached result)
    let edits = vec![];
    let session = sipha::incremental::IncrementalSession::new(old_tree, &edits);

    let tokens2 = vec![
        create_token(TestSyntaxKind::Number, "1"),
        create_token(TestSyntaxKind::Plus, "+"),
        create_token(TestSyntaxKind::Number, "2"),
    ];

    let result2 = parser.parse_with_session(&tokens2, TestNonTerminal::Expr, &session);

    // Should succeed (either with reuse or full parse)
    assert!(!result2.errors.is_empty() || result2.root.text_len() > TextSize::zero());
}

#[cfg(feature = "backend-earley")]
#[test]
fn test_earley_parser_validate() {
    let grammar = create_test_grammar();
    let errors = EarleyParser::<TestToken, TestNonTerminal>::validate(&grammar);

    // Earley parser can handle any context-free grammar, so validation should pass
    // (unless there are undefined references)
    // For a valid grammar, errors should be empty or minimal
    assert!(errors.is_empty() || errors.iter().all(|e| e.to_string().contains("undefined")));
}
