use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
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

fn build_expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.lexer_rule("ws", |g| {
        g.trivia(K::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("number", |g| {
        g.token(K::Number, |g| {
            g.one_or_more(|g| {
                g.class(classes::DIGIT);
            });
        });
    });

    g.parser_rule("expr", |g| {
        g.node(K::Expr, |g| {
            g.call("mul");
            g.zero_or_more(|g| {
                g.token(K::Plus, |g| {
                    g.byte(b'+');
                });
                g.call("mul");
            });
        });
    });

    g.parser_rule("mul", |g| {
        g.node(K::BinExpr, |g| {
            g.call("atom");
            g.zero_or_more(|g| {
                g.token(K::Star, |g| {
                    g.byte(b'*');
                });
                g.call("atom");
            });
        });
    });

    g.parser_rule("atom", |g| {
        g.choice(
            |g| {
                g.call("number");
            },
            |g| {
                g.node(K::ParenExpr, |g| {
                    g.token(K::LParen, |g| {
                        g.byte(b'(');
                    });
                    g.call("expr");
                    g.token(K::RParen, |g| {
                        g.byte(b')');
                    });
                });
            },
        );
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

fn bench_expr_parse(c: &mut Criterion) {
    let built = build_expr_grammar();
    let graph = built.as_graph();

    let inputs: Vec<Vec<u8>> = [
        "1+2*3",
        "1 + 2 * (3 + 4) * 5 + 6",
        "1+2+3+4+5+6+7+8+9+10+11+12+13+14+15",
        "(((1)))+(((2)))+(((3)))+(((4)))",
    ]
    .into_iter()
    .map(|s| s.as_bytes().to_vec())
    .collect();

    c.bench_function("expr/parse", |b| {
        b.iter_batched(
            || {
                let engine = Engine::new();
                (engine, &graph)
            },
            |(mut engine, graph)| {
                for input in &inputs {
                    let out = engine.parse(graph, input).unwrap();
                    criterion::black_box(out.consumed);
                }
            },
            BatchSize::SmallInput,
        )
    });

    c.bench_function("expr/parse_memo", |b| {
        b.iter_batched(
            || {
                let engine = Engine::new().with_memo();
                (engine, &graph)
            },
            |(mut engine, graph)| {
                for input in &inputs {
                    let out = engine.parse(graph, input).unwrap();
                    criterion::black_box(out.consumed);
                }
            },
            BatchSize::SmallInput,
        )
    });

    c.bench_function("expr/parse_and_build_tree", |b| {
        b.iter_batched(
            || {
                let engine = Engine::new();
                (engine, &graph)
            },
            |(mut engine, graph)| {
                for input in &inputs {
                    let out = engine.parse(graph, input).unwrap();
                    let root = out.syntax_root(input).unwrap();
                    criterion::black_box(root.text_len());
                }
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, bench_expr_parse);
criterion_main!(benches);
