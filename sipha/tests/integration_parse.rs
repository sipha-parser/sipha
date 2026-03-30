//! Integration tests for the sipha parser: full parse path from grammar build to parse output.

use sipha::parse::engine::ParseError;
use sipha::prelude::*;

fn minimal_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.byte(b'a');
        g.end_of_input();
        g.accept();
    });
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

#[test]
fn parse_separated_and_delimited() {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.separated(
            |g| {
                g.byte(b'x');
            },
            |g| {
                g.byte(b',');
            },
        );
        g.end_of_input();
        g.accept();
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();

    let out = engine.parse(&graph, b"").expect("empty ok");
    assert_eq!(out.consumed, 0);

    let out = engine.parse(&graph, b"x").expect("single x");
    assert_eq!(out.consumed, 1);

    let out = engine.parse(&graph, b"x,x,x").expect("three xs");
    assert_eq!(out.consumed, 5);

    let mut g2 = GrammarBuilder::new();
    g2.rule("start", |g| {
        g.delimited(
            |g| {
                g.byte(b'(');
            },
            |g| {
                g.byte(b'z');
            },
            |g| {
                g.byte(b')');
            },
        );
        g.end_of_input();
        g.accept();
    });
    let built2 = g2.finish().expect("grammar valid");
    let graph2 = built2.as_graph();
    assert!(engine.parse(&graph2, b"(z)").is_ok());
    assert!(engine.parse(&graph2, b"z").is_err());
}

#[test]
fn parse_not_followed_by() {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.byte(b'a');
        g.not_followed_by(|g| {
            g.byte(b'b');
        });
        g.end_of_input();
        g.accept();
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();

    let out = engine.parse(&graph, b"a").expect("a alone");
    assert_eq!(out.consumed, 1);

    assert!(engine.parse(&graph, b"ab").is_err());
}

#[test]
fn parse_byte_dispatch_rules() {
    let mut g = GrammarBuilder::new();
    // First defined rule is the parse entry point (`ParseGraph::start`).
    g.rule("start", |g| {
        g.byte_dispatch_rules(&[(b'a', "ra"), (b'b', "rb")], Some("rf"));
        g.end_of_input();
        g.accept();
    });
    g.rule("ra", |g| {
        g.byte(b'a');
    });
    g.rule("rb", |g| {
        g.byte(b'b');
    });
    g.rule("rf", |g| {
        g.byte(b'c');
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();

    assert!(engine.parse(&graph, b"a").is_ok());
    assert!(engine.parse(&graph, b"b").is_ok());
    assert!(engine.parse(&graph, b"c").is_ok());
    assert!(engine.parse(&graph, b"d").is_err());
}

#[test]
fn parse_choices_macro_and_choice5() {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        sipha::choices!(
            g,
            |g| {
                g.literal(b"a");
            },
            |g| {
                g.literal(b"b");
            },
            |g| {
                g.literal(b"c");
            },
            |g| {
                g.literal(b"d");
            },
            |g| {
                g.literal(b"e");
            },
        );
        g.end_of_input();
        g.accept();
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();
    assert!(engine.parse(&graph, b"a").is_ok());
    assert!(engine.parse(&graph, b"e").is_ok());

    let mut g2 = GrammarBuilder::new();
    g2.rule("start", |g| {
        g.choice5(
            |g| {
                g.byte(b'1');
            },
            |g| {
                g.byte(b'2');
            },
            |g| {
                g.byte(b'3');
            },
            |g| {
                g.byte(b'4');
            },
            |g| {
                g.byte(b'5');
            },
        );
        g.end_of_input();
        g.accept();
    });
    let built2 = g2.finish().expect("grammar valid");
    let graph2 = built2.as_graph();
    assert!(engine.parse(&graph2, b"3").is_ok());
    assert!(engine.parse(&graph2, b"9").is_err());
}

#[test]
fn parse_expr_helpers_postfix_and_separated_list() {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.call("expr");
        g.end_of_input();
        g.accept();
    });
    sipha::parse::expr::postfix_chain(&mut g, "expr", "atom", "bang");
    g.rule("atom", |g| {
        g.byte(b'n');
    });
    g.rule("bang", |g| {
        g.byte(b'!');
    });
    let built = g.finish().expect("grammar valid");
    let graph = built.as_graph();
    let mut engine = Engine::new();
    let out = engine.parse(&graph, b"n!!").expect("n!!");
    assert_eq!(out.consumed, 3);

    let mut g2 = GrammarBuilder::new();
    g2.rule("start", |g| {
        g.call("list");
        g.end_of_input();
        g.accept();
    });
    sipha::parse::expr::separated_rule_list(&mut g2, "list", "lit", "comma");
    g2.rule("lit", |g| {
        g.byte(b'x');
    });
    g2.rule("comma", |g| {
        g.byte(b',');
    });
    let built2 = g2.finish().expect("grammar valid");
    let graph2 = built2.as_graph();
    assert!(engine.parse(&graph2, b"").is_ok());
    assert!(engine.parse(&graph2, b"x,x").is_ok());
}
