use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
enum Lex {
    Number,
    Plus,
    Star,
    LParen,
    RParen,
    Ws,
}

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        match self {
            Lex::Number => "NUMBER",
            Lex::Plus => "PLUS",
            Lex::Star => "STAR",
            Lex::LParen => "LPAREN",
            Lex::RParen => "RPAREN",
            Lex::Ws => "WS",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, RuleKinds)]
#[sipha(lex = Lex)]
#[repr(u16)]
enum Rule {
    Root,
    Expr,
    BinExpr,
    ParenExpr,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::Expr => "EXPR",
            Rule::BinExpr => "BIN_EXPR",
            Rule::ParenExpr => "PAREN_EXPR",
        }
    }
}

fn build_expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.lexer_rule("ws", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("number", |g| {
        g.token(Lex::Number, |g| {
            g.one_or_more(|g| {
                g.class(classes::DIGIT);
            });
        });
    });

    g.parser_rule("expr", |g| {
        g.node(Rule::Expr, |g| {
            g.call("mul");
            g.zero_or_more(|g| {
                g.token(Lex::Plus, |g| {
                    g.byte(b'+');
                });
                g.call("mul");
            });
        });
    });

    g.parser_rule("mul", |g| {
        g.node(Rule::BinExpr, |g| {
            g.call("atom");
            g.zero_or_more(|g| {
                g.token(Lex::Star, |g| {
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
                g.node(Rule::ParenExpr, |g| {
                    g.token(Lex::LParen, |g| {
                        g.byte(b'(');
                    });
                    g.call("expr");
                    g.token(Lex::RParen, |g| {
                        g.byte(b')');
                    });
                });
            },
        );
    });

    g.parser_rule("start", |g| {
        g.node(Rule::Root, |g| {
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
        );
    });
}

criterion_group!(benches, bench_expr_parse);
criterion_main!(benches);
