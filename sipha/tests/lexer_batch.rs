#![cfg(feature = "std")]

use sipha::parse::lexer_batch::{LexerKeywordSpec, LexerTokenLiteralSpec};
use sipha::prelude::*;

#[derive(Clone, Copy)]
struct TestLex(SyntaxKind);

impl IntoSyntaxKind for TestLex {
    fn into_syntax_kind(self) -> SyntaxKind {
        self.0
    }
}

impl LexKind for TestLex {
    fn display_name(self) -> &'static str {
        "TEST_LEX"
    }
}

#[derive(Clone, Copy)]
struct TestRule(SyntaxKind);

impl IntoSyntaxKind for TestRule {
    fn into_syntax_kind(self) -> SyntaxKind {
        self.0
    }
}

impl RuleKind for TestRule {
    fn display_name(self) -> &'static str {
        "TEST_RULE"
    }
}

#[test]
fn lexer_rule_keywords_batch_registers_named_rules() {
    let mut g = GrammarBuilder::new();
    // Rule 0 must be the entry rule (`parse` uses `graph.start()`).
    g.parser_rule("start", |g| {
        g.node(TestRule(1), |g| {
            g.call("ka");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule_keywords_batch(&[
        LexerKeywordSpec::new("ka", TestLex(10), b"alpha"),
        LexerKeywordSpec::new("kb", TestLex(11), b"beta"),
    ]);

    let built = g.finish().expect("grammar");
    let graph = built.as_graph();
    let mut engine = Engine::new();
    let out = engine.parse(&graph, b"alpha").expect("parse");
    assert_eq!(out.consumed, b"alpha".len() as u32);
}

#[test]
fn lexer_rule_token_literals_batch_honors_require_flags() {
    let flag_on: FlagId = 0;
    const PLUS_REQ_FLAGS: &[FlagId] = &[0];

    let mut g = GrammarBuilder::new();
    g.parser_rule("start", |g| {
        g.node(TestRule(2), |g| {
            g.call("plus_tok");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule_token_literals_batch(&[LexerTokenLiteralSpec::new("plus_tok", TestLex(20), b"+")
        .with_flags(PLUS_REQ_FLAGS)]);

    let built = g.finish().expect("grammar");
    let graph = built.as_graph();

    let ctx = ParseContext::new();
    let mut engine = Engine::new();
    let err = engine
        .parse_with_context(&graph, b"+", &ctx)
        .expect_err("flag should be required");
    assert!(
        matches!(err, ParseError::NoMatch(_)),
        "expected NoMatch when required flag clear: {err:?}"
    );

    let ctx = ParseContext::new().with_set(flag_on);
    let mut engine = Engine::new();
    let out = engine
        .parse_with_context(&graph, b"+", &ctx)
        .expect("parse with flag");
    assert_eq!(out.consumed, 1);
}
