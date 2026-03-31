#![no_main]

use libfuzzer_sys::fuzz_target;
use once_cell::sync::Lazy;
use sipha::prelude::*;
use sipha::SyntaxKinds;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
enum K {
    Root,
    Expr,
    BinExpr,
    ParenExpr,
    Number,
    Plus,
    Star,
    LParen,
    RParen,
    Ws,
}

fn expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.lexer_rule("ws", |g| {
        g.trivia(K::Ws, |g| {
            g.zero_or_more(|g| g.class(classes::WHITESPACE));
        });
    });

    g.lexer_rule("number", |g| {
        g.token(K::Number, |g| {
            g.one_or_more(|g| g.class(classes::DIGIT));
        });
    });

    g.parser_rule("expr", |g| {
        g.node(K::Expr, |g| {
            g.call("mul");
            g.zero_or_more(|g| {
                g.token(K::Plus, |g| g.byte(b'+'));
                g.call("mul");
            });
        });
    });

    g.parser_rule("mul", |g| {
        g.node(K::BinExpr, |g| {
            g.call("atom");
            g.zero_or_more(|g| {
                g.token(K::Star, |g| g.byte(b'*'));
                g.call("atom");
            });
        });
    });

    g.parser_rule("atom", |g| {
        g.choice(|g| g.call("number"), |g| {
            g.node(K::ParenExpr, |g| {
                g.token(K::LParen, |g| g.byte(b'('));
                g.call("expr");
                g.token(K::RParen, |g| g.byte(b')'));
            });
        });
    });

    g.parser_rule("start", |g| {
        g.node(K::Root, |g| {
            g.call("expr");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.finish().unwrap()
}

static BUILT: Lazy<BuiltGraph> = Lazy::new(expr_grammar);

fn map_input(data: &[u8]) -> Vec<u8> {
    // Restrict to a small ASCII subset so the grammar has a chance to make progress.
    let mut buf = Vec::with_capacity(data.len());
    for &b in data {
        let mapped = match b % 8 {
            0 => b'0' + (b % 10),
            1 => b'+',
            2 => b'*',
            3 => b'(',
            4 => b')',
            5 => b' ',
            6 => b'\n',
            _ => b'\t',
        };
        buf.push(mapped);
    }
    buf
}

fuzz_target!(|data: &[u8]| {
    let input = map_input(data);
    let graph = BUILT.as_graph();

    let mut e1 = Engine::new();
    let r1 = e1.parse(&graph, &input);

    let mut e2 = Engine::new().with_memo();
    let r2 = e2.parse(&graph, &input);

    match (r1, r2) {
        (Ok(o1), Ok(o2)) => {
            // Both should accept and consume the full input for this grammar.
            // If this fails, it indicates a semantic mismatch between memoised and non-memoised execution.
            assert_eq!(o1.consumed, o2.consumed);
            assert_eq!(o1.consumed as usize, input.len());
        }
        (Err(_), Err(_)) => {}
        _ => panic!("memoised vs non-memoised parse disagreement"),
    }
});

