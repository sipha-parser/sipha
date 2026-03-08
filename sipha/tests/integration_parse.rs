//! Integration tests for the sipha parser: full parse path from grammar build to parse output.

use sipha::engine::ParseError;
use sipha::prelude::*;

fn minimal_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.byte(b'a');
    g.end_of_input();
    g.accept();
    g.finish().expect("minimal grammar valid")
}

#[test]
fn parse_minimal_success() {
    let built = minimal_grammar();
    let graph = built.as_graph();
    let mut engine = Engine::new();
    let out = engine.parse(&graph, b"a").expect("parse ok");
    assert_eq!(out.consumed, 1);
}

#[test]
fn parse_minimal_fail_returns_no_match_diagnostic() {
    let built = minimal_grammar();
    let graph = built.as_graph();
    let mut engine = Engine::new();
    let err = engine.parse(&graph, b"b").unwrap_err();
    let ParseError::NoMatch(diag) = err else {
        panic!("expected NoMatch");
    };
    assert_eq!(diag.furthest, 0);
    assert!(!diag.expected.is_empty());
}

#[test]
fn parse_sequence_and_choice() {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.literal(b"ab");
        g.choice(
            |g| {
                g.literal(b"c");
            },
            |g| {
                g.literal(b"d");
            },
        );
        g.end_of_input();
        g.accept();
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();

    let out = engine.parse(&graph, b"abc").expect("parse abc");
    assert_eq!(out.consumed, 3);

    let out = engine.parse(&graph, b"abd").expect("parse abd");
    assert_eq!(out.consumed, 3);

    let err = engine.parse(&graph, b"abx").unwrap_err();
    let ParseError::NoMatch(d) = err else {
        panic!("expected NoMatch");
    };
    assert_eq!(d.furthest, 2);
}

#[test]
fn parse_repetition() {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.byte(b'a');
        g.zero_or_more(|g| {
        g.byte(b'b');
    });
        g.end_of_input();
        g.accept();
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();

    let out = engine.parse(&graph, b"a").expect("parse a");
    assert_eq!(out.consumed, 1);

    let out = engine.parse(&graph, b"abbb").expect("parse abbb");
    assert_eq!(out.consumed, 4);
}
