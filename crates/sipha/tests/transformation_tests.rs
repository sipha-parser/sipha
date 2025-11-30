//! Tests for grammar transformation and optimization

use sipha::backend::ParserBackend;
#[cfg(feature = "backend-ll")]
use sipha::backend::ll::{LlConfig, LlParser};
#[cfg(feature = "backend-lr")]
use sipha::backend::lr::{LrConfig, LrParser};
#[cfg(feature = "backend-peg")]
use sipha::backend::peg::{PegConfig, PegParser};
#[cfg(feature = "backend-pratt")]
use sipha::backend::pratt::{PrattConfig, PrattParser};
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
    // Both should produce a root node (even if empty)
    assert!(
        result_no_opt.root.kind() == result_with_opt.root.kind(),
        "Results should have compatible root nodes"
    );
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
    // Both should produce a root node (even if empty)
    assert!(
        result_no_opt.root.kind() == result_with_opt.root.kind(),
        "Results should have compatible root nodes"
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

#[cfg(feature = "backend-pratt")]
#[test]
fn test_pratt_parser_with_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    // Test with optimization disabled
    let config_no_opt = PrattConfig {
        optimize: false,
        optimization_level: sipha::grammar::hint::OptimizationLevel::None,
        ..Default::default()
    };
    let parser_no_opt = PrattParser::new(&grammar, config_no_opt);
    assert!(
        parser_no_opt.is_ok(),
        "Parser should be created without optimization"
    );

    // Test with basic optimization enabled
    let config_basic = PrattConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Basic,
        ..Default::default()
    };
    let parser_basic = PrattParser::new(&grammar, config_basic);
    assert!(
        parser_basic.is_ok(),
        "Parser should be created with basic optimization"
    );

    // Test with aggressive optimization enabled
    let config_aggressive = PrattConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Aggressive,
        ..Default::default()
    };
    let parser_aggressive = PrattParser::new(&grammar, config_aggressive);
    assert!(
        parser_aggressive.is_ok(),
        "Parser should be created with aggressive optimization"
    );

    // All parsers should produce the same results
    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result_no_opt = parser_no_opt.unwrap().parse(&tokens, TestNonTerminal::Expr);
    let result_basic = parser_basic.unwrap().parse(&tokens, TestNonTerminal::Expr);
    let result_aggressive = parser_aggressive
        .unwrap()
        .parse(&tokens, TestNonTerminal::Expr);

    assert_eq!(
        result_no_opt.errors.len(),
        result_basic.errors.len(),
        "Basic optimization should produce same number of errors"
    );
    assert_eq!(
        result_no_opt.errors.len(),
        result_aggressive.errors.len(),
        "Aggressive optimization should produce same number of errors"
    );
}

#[cfg(feature = "backend-ll")]
#[test]
fn test_ll_parser_with_aggressive_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    let config = LlConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Aggressive,
        ..Default::default()
    };
    let parser = LlParser::new(&grammar, config);
    assert!(
        parser.is_ok(),
        "Parser should be created with aggressive optimization"
    );

    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result = parser.unwrap().parse(&tokens, TestNonTerminal::Expr);
    assert_eq!(result.errors.len(), 0, "Should parse without errors");
}

#[cfg(feature = "backend-lr")]
#[test]
fn test_lr_parser_with_aggressive_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    let config = LrConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Aggressive,
        ..Default::default()
    };
    let parser = LrParser::new(&grammar, config);
    assert!(
        parser.is_ok(),
        "Parser should be created with aggressive optimization"
    );

    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result = parser.unwrap().parse(&tokens, TestNonTerminal::Expr);
    assert_eq!(result.errors.len(), 0, "Should parse without errors");
}

#[cfg(feature = "backend-peg")]
#[test]
fn test_peg_parser_with_aggressive_optimization() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    let config = PegConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Aggressive,
        ..Default::default()
    };
    let parser = PegParser::new(&grammar, config);
    assert!(
        parser.is_ok(),
        "Parser should be created with aggressive optimization"
    );

    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result = parser.unwrap().parse(&tokens, TestNonTerminal::Expr);
    assert_eq!(result.errors.len(), 0, "Should parse without errors");
}

#[cfg(feature = "backend-lr")]
#[test]
fn test_lr_production_inlining() {
    // Create a grammar with small productions that should be inlined
    // Expr -> Term
    // Term -> Number
    // This should result in Expr -> Number after inlining
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum InlineTestNonTerminal {
        Expr,
        Term,
    }

    impl NonTerminal for InlineTestNonTerminal {
        fn name(&self) -> &str {
            match self {
                Self::Expr => "Expr",
                Self::Term => "Term",
            }
        }
    }

    let grammar = GrammarBuilder::new()
        .entry_point(InlineTestNonTerminal::Expr)
        .rule(
            InlineTestNonTerminal::Expr,
            Expr::rule(InlineTestNonTerminal::Term),
        )
        .rule(
            InlineTestNonTerminal::Term,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    // Test with aggressive optimization (includes inlining)
    let config = LrConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Aggressive,
        ..Default::default()
    };
    let parser = LrParser::new(&grammar, config);
    assert!(
        parser.is_ok(),
        "Parser should be created with aggressive optimization (including inlining)"
    );

    // Verify it still parses correctly after inlining
    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result = parser.unwrap().parse(&tokens, InlineTestNonTerminal::Expr);
    assert_eq!(
        result.errors.len(),
        0,
        "Should parse correctly after inlining"
    );
}

#[cfg(feature = "backend-pratt")]
#[test]
fn test_pipeline_caching() {
    use sipha::backend::pipeline::GrammarTransformPipeline;
    use sipha::backend::pratt::PrattTransformer;
    use sipha::backend::traits::TransformConfig;

    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    let config = TransformConfig {
        optimize: false,
        optimization_level: sipha::grammar::hint::OptimizationLevel::None,
        cache: true,
        backend_options: std::collections::HashMap::new(),
    };

    // First transformation - should not be cached
    let transformed1 =
        GrammarTransformPipeline::transform::<TestToken, TestNonTerminal, PrattTransformer>(
            &grammar, &config,
        );
    assert!(transformed1.is_ok(), "First transformation should succeed");

    // Store in cache
    if let Ok(grammar1) = &transformed1 {
        GrammarTransformPipeline::store_cached::<TestToken, TestNonTerminal, PrattTransformer>(
            &grammar, &config, grammar1,
        );
    }

    // Second transformation with same grammar and config - should hit cache
    if let Some(cached) =
        GrammarTransformPipeline::get_cached::<TestToken, TestNonTerminal, PrattTransformer>(
            &grammar, &config,
        )
    {
        // Cache hit - verify it's the same type
        assert_eq!(
            cached.entry_point,
            transformed1.as_ref().unwrap().entry_point
        );
    }

    // Different config - should miss cache
    let config2 = TransformConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Basic,
        cache: true,
        backend_options: std::collections::HashMap::new(),
    };
    let cache_miss =
        GrammarTransformPipeline::get_cached::<TestToken, TestNonTerminal, PrattTransformer>(
            &grammar, &config2,
        );
    assert!(cache_miss.is_none(), "Different config should miss cache");
}

#[cfg(feature = "backend-glr")]
#[test]
fn test_glr_parser_with_optimization() {
    use sipha::backend::glr::{GlrConfig, GlrParser};

    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number, "42")),
        )
        .build()
        .expect("Failed to build grammar");

    // Test with optimization disabled
    let config_no_opt = GlrConfig {
        optimize: false,
        optimization_level: sipha::grammar::hint::OptimizationLevel::None,
        ..Default::default()
    };
    let parser_no_opt = GlrParser::new(&grammar, config_no_opt);
    assert!(
        parser_no_opt.is_ok(),
        "Parser should be created without optimization"
    );

    // Test with basic optimization enabled
    let config_basic = GlrConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Basic,
        ..Default::default()
    };
    let parser_basic = GlrParser::new(&grammar, config_basic);
    assert!(
        parser_basic.is_ok(),
        "Parser should be created with basic optimization"
    );

    // Test with aggressive optimization enabled
    let config_aggressive = GlrConfig {
        optimize: true,
        optimization_level: sipha::grammar::hint::OptimizationLevel::Aggressive,
        ..Default::default()
    };
    let parser_aggressive = GlrParser::new(&grammar, config_aggressive);
    assert!(
        parser_aggressive.is_ok(),
        "Parser should be created with aggressive optimization"
    );

    // All parsers should produce the same results
    let tokens = vec![create_token(TestSyntaxKind::Number, "42")];
    let result_no_opt = parser_no_opt.unwrap().parse(&tokens, TestNonTerminal::Expr);
    let result_basic = parser_basic.unwrap().parse(&tokens, TestNonTerminal::Expr);
    let result_aggressive = parser_aggressive
        .unwrap()
        .parse(&tokens, TestNonTerminal::Expr);

    assert_eq!(
        result_no_opt.errors.len(),
        result_basic.errors.len(),
        "Basic optimization should produce same number of errors"
    );
    assert_eq!(
        result_no_opt.errors.len(),
        result_aggressive.errors.len(),
        "Aggressive optimization should produce same number of errors"
    );
}
