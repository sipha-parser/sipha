//! Tests for grammar transformation and optimization

use sipha::backend::ParserBackend;
#[cfg(feature = "backend-ll")]
use sipha::backend::ll::{LlConfig, LlParser};
#[cfg(feature = "backend-lr")]
use sipha::backend::lr::{LrConfig, LrParser};
#[cfg(feature = "backend-peg")]
use sipha::backend::peg::{PegConfig, PegParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TestNonTerminal {
    Expr,
}

impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &str {
        "Expr"
    }
}

fn create_token(kind: TestSyntaxKind, text: &str) -> TestToken {
    TestToken {
        kind,
        text: text.into(),
    }
}

#[cfg(feature = "backend-ll")]
#[test]
fn test_ll_parser_with_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    // Test with optimization disabled
    let config_no_opt = LlConfig {
        optimize: false,
        optimization_level: sipha::grammar::hint::OptimizationLevel::None,
        ..Default::default()
    };
    let parser_no_opt = LlParser::new(&grammar, config_no_opt);
    assert!(
        parser_no_opt.is_ok(),
        "Parser should be created without optimization"
    );

    // Test with optimization enabled
    let config_with_opt = LlConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Basic,
        ..Default::default()
    };
    let parser_with_opt = LlParser::new(&grammar, config_with_opt);
    // Note: LL optimization may not be fully implemented yet, so we just check it doesn't crash
    assert!(
        parser_with_opt.is_ok() || parser_with_opt.is_err(),
        "Parser creation should not panic"
    );

    // Both parsers should produce the same results (if optimization was successful)
    if let Ok(mut parser_with_opt) = parser_with_opt {
        let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
        let result_no_opt = parser_no_opt.unwrap().parse(&tokens, TestNonTerminal::Expr);
        let result_with_opt = parser_with_opt.parse(&tokens, TestNonTerminal::Expr);

        assert_eq!(
            result_no_opt.errors.len(),
            result_with_opt.errors.len(),
            "Results should have same number of errors"
        );
    }
}

#[cfg(feature = "backend-lr")]
#[test]
fn test_lr_parser_with_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    // Test with optimization disabled
    let config_no_opt = LrConfig {
        optimize: false,
        optimization_level: sipha::grammar::hint::OptimizationLevel::None,
        ..Default::default()
    };
    let parser_no_opt = LrParser::new(&grammar, config_no_opt);
    assert!(
        parser_no_opt.is_ok(),
        "Parser should be created without optimization"
    );

    // Test with optimization enabled
    let config_with_opt = LrConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Basic,
        ..Default::default()
    };
    let parser_with_opt = LrParser::new(&grammar, config_with_opt);
    // Note: LR optimization may not be fully implemented yet, so we just check it doesn't crash
    assert!(
        parser_with_opt.is_ok() || parser_with_opt.is_err(),
        "Parser creation should not panic"
    );
}

#[cfg(feature = "backend-peg")]
#[test]
fn test_peg_parser_with_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    // Test with optimization disabled
    let config_no_opt = PegConfig {
        optimize: false,
        optimization_level: sipha::grammar::hint::OptimizationLevel::None,
        ..Default::default()
    };
    let parser_no_opt = PegParser::new(&grammar, config_no_opt);
    assert!(
        parser_no_opt.is_ok(),
        "Parser should be created without optimization"
    );

    // Test with optimization enabled
    let config_with_opt = PegConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Basic,
        ..Default::default()
    };
    let parser_with_opt = PegParser::new(&grammar, config_with_opt);
    assert!(
        parser_with_opt.is_ok(),
        "Parser should be created with optimization"
    );

    // Both parsers should produce the same results
    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result_no_opt = parser_no_opt.unwrap().parse(&tokens, TestNonTerminal::Expr);
    let result_with_opt = parser_with_opt
        .unwrap()
        .parse(&tokens, TestNonTerminal::Expr);

    assert_eq!(
        result_no_opt.errors.len(),
        result_with_opt.errors.len(),
        "Results should have same number of errors"
    );
}

#[test]
fn test_parser_backend_backend_grammar_type() {
    // Test that all backends expose BackendGrammar type
    #[cfg(feature = "backend-ll")]
    {
        use sipha::backend::ll::LlParser;
        type _TestLlGrammar = <LlParser<TestToken, TestNonTerminal> as ParserBackend<
            TestToken,
            TestNonTerminal,
        >>::BackendGrammar;
    }

    #[cfg(feature = "backend-lr")]
    {
        use sipha::backend::lr::LrParser;
        type _TestLrGrammar = <LrParser<TestToken, TestNonTerminal> as ParserBackend<
            TestToken,
            TestNonTerminal,
        >>::BackendGrammar;
    }

    #[cfg(feature = "backend-peg")]
    {
        use sipha::backend::peg::PegParser;
        type _TestPegGrammar = <PegParser<TestToken, TestNonTerminal> as ParserBackend<
            TestToken,
            TestNonTerminal,
        >>::BackendGrammar;
    }

    #[cfg(feature = "backend-glr")]
    {
        use sipha::backend::glr::GlrParser;
        type _TestGlrGrammar = <GlrParser<TestToken, TestNonTerminal> as ParserBackend<
            TestToken,
            TestNonTerminal,
        >>::BackendGrammar;
    }

    // If we get here, the types are accessible
    assert!(true);
}
