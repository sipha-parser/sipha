#![cfg(feature = "std")]

mod common;

use common::{expr_grammar, sexp};
use sipha::prelude::*;

fn parse_doc(graph: &ParseGraph<'_>, src: &[u8]) -> ParsedDoc {
    let mut engine = Engine::new();
    let out = engine.parse(graph, src).expect("parse should succeed");
    ParsedDoc::from_slice(src, &out).expect("should produce syntax root")
}

#[test]
fn parses_expression_and_builds_tree() {
    let built = expr_grammar();
    let graph = built.as_graph();
    let doc = parse_doc(&graph, b"1+2*3");

    // Multiplication binds tighter than addition, so the tree should reflect:
    // expr: mul ('+' mul)*
    // mul: atom ('*' atom)*
    let got = sexp(doc.root());
    assert!(
        got.contains("ROOT") && got.contains("NUMBER"),
        "expected a structured tree, got: {got}"
    );
}

#[test]
fn trivia_is_skipped_between_tokens_in_parser_rules() {
    let built = expr_grammar();
    let graph = built.as_graph();
    let doc = parse_doc(&graph, b"1 + 2 * ( 3 + 4 )");
    let got = sexp(doc.root());

    // If trivia skipping is working, we should still get a successful parse and a semantic tree.
    assert!(got.contains("LPAREN") && got.contains("RPAREN"), "{got}");
}
