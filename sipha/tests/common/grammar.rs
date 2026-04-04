use sipha::SyntaxKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
pub enum K {
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

pub fn expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.allow_rule_cycles(true);
    g.set_trivia_rule("ws");

    g.parser_rule("start", |g| {
        g.node(K::Root, |g| {
            g.call("expr");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

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
                g.class_with_label(classes::DIGIT, "digit");
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

    g.finish().expect("expr grammar should build")
}
