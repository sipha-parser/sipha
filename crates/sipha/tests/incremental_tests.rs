#![cfg(feature = "backend-ll")]

use sipha::backend::ParserBackend;
use sipha::backend::ll::{LlConfig, LlParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::incremental::TextEdit;
use sipha::syntax::{SyntaxKind, TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestToken {
    Ident,
    Number,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestNonTerminal {
    Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestSyntaxKind {
    Ident,
    Number,
}

impl SyntaxKind for TestSyntaxKind {
    fn is_terminal(self) -> bool {
        matches!(self, Self::Ident | Self::Number)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

impl Token for TestToken {
    type Kind = TestSyntaxKind;

    fn kind(&self) -> Self::Kind {
        match self {
            Self::Ident => TestSyntaxKind::Ident,
            Self::Number => TestSyntaxKind::Number,
        }
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(1)
    }

    fn text(&self) -> compact_str::CompactString {
        match self {
            Self::Ident => "a".into(),
            Self::Number => "1".into(),
        }
    }
}

impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &'static str {
        "Expr"
    }
}

#[test]
fn ll_incremental_matches_full_parse() {
    let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::choice([
                Expr::token(TestToken::Ident),
                Expr::token(TestToken::Number),
            ]),
        )
        .build()
        .expect("grammar should build");

    let tokens_before = vec![TestToken::Ident];
    let tokens_after = vec![TestToken::Number];

    let mut parser = LlParser::new(&grammar, LlConfig::default()).expect("parser");
    let first = parser.parse(&tokens_before, TestNonTerminal::Expr);

    let edits = [TextEdit {
        range: TextRange::new(TextSize::zero(), TextSize::from(1)),
        new_text: "1".into(),
    }];

    let incremental = parser.parse_incremental(
        &tokens_after,
        Some(first.root.as_ref()),
        &edits,
        TestNonTerminal::Expr,
    );

    let mut fresh = LlParser::new(&grammar, LlConfig::default()).expect("parser");
    let baseline = fresh.parse(&tokens_after, TestNonTerminal::Expr);

    assert!(incremental.errors.is_empty());
    assert!(baseline.errors.is_empty());
    assert_eq!(incremental.root, baseline.root);
}

#[test]
fn test_cache_population() {
    use sipha::incremental::IncrementalParser;

    let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
        .build()
        .expect("grammar should build");

    let parser = LlParser::new(&grammar, LlConfig::default()).expect("parser");
    let mut incremental_parser = IncrementalParser::new(parser);

    // Test that cache is created
    let tokens = vec![TestToken::Number];
    let result = incremental_parser
        .parser_mut()
        .parse(&tokens, TestNonTerminal::Expr);
    assert!(result.errors.is_empty());

    // Test cache population with grammar
    let parser2 = LlParser::new(&grammar, LlConfig::default()).expect("parser");
    let mut incremental_parser2 = IncrementalParser::new(parser2);
    let result2 = incremental_parser2.parse_incremental_with_grammar(
        &tokens,
        None,
        &[],
        TestNonTerminal::Expr,
        &grammar,
    );
    assert!(result2.errors.is_empty());
}

#[test]
fn test_cache_hit_behavior() {
    use sipha::incremental::IncrementalParser;

    let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
        .build()
        .expect("grammar should build");

    let parser = LlParser::new(&grammar, LlConfig::default()).expect("parser");
    let mut incremental_parser = IncrementalParser::new(parser);

    // First parse - populate cache
    let tokens1 = vec![TestToken::Number];
    let result1 = incremental_parser.parse_incremental_with_grammar(
        &tokens1,
        None,
        &[],
        TestNonTerminal::Expr,
        &grammar,
    );
    assert!(result1.errors.is_empty());

    // Second parse with no edits - should potentially use cache
    // Note: Cache lookup happens during parsing, so we can't directly verify cache hits
    // but we can verify the parse succeeds and produces the same result
    let result2 = incremental_parser.parse_incremental_with_grammar(
        &tokens1,
        Some(&result1.root),
        &[],
        TestNonTerminal::Expr,
        &grammar,
    );
    assert!(result2.errors.is_empty());
    assert_eq!(result1.root, result2.root);
}

#[test]
fn test_cache_invalidation_on_edits() {
    use sipha::incremental::{IncrementalParser, TextEdit};
    use sipha::syntax::{TextRange, TextSize};

    // Use a grammar that accepts both Ident and Number
    let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::choice([
                Expr::token(TestToken::Ident),
                Expr::token(TestToken::Number),
            ]),
        )
        .build()
        .expect("grammar should build");

    let parser = LlParser::new(&grammar, LlConfig::default()).expect("parser");
    let mut incremental_parser = IncrementalParser::new(parser);

    // First parse - populate cache
    let tokens1 = vec![TestToken::Number];
    let result1 = incremental_parser.parse_incremental_with_grammar(
        &tokens1,
        None,
        &[],
        TestNonTerminal::Expr,
        &grammar,
    );
    assert!(result1.errors.is_empty());

    // Second parse with edit - cache should be invalidated for affected region
    let tokens2 = vec![TestToken::Ident];
    let edits = vec![TextEdit {
        range: TextRange::new(TextSize::zero(), TextSize::from(1)),
        new_text: "a".into(),
    }];
    let result2 = incremental_parser.parse_incremental_with_grammar(
        &tokens2,
        Some(&result1.root),
        &edits,
        TestNonTerminal::Expr,
        &grammar,
    );
    assert!(result2.errors.is_empty());
    // Result should be different due to edit
    assert_ne!(result1.root, result2.root);
}
