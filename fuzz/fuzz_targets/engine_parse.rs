#![no_main]

use libfuzzer_sys::fuzz_target;
use once_cell::sync::Lazy;
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

fn expr_grammar() -> BuiltGraph {
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
        g.choice(|g| g.call("number"), |g| {
            g.node(Rule::ParenExpr, |g| {
                g.token(Lex::LParen, |g| {
                    g.byte(b'(');
                });
                g.call("expr");
                g.token(Lex::RParen, |g| {
                    g.byte(b')');
                });
            });
        });
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

static BUILT: Lazy<BuiltGraph> = Lazy::new(expr_grammar);

fuzz_target!(|data: &[u8]| {
    // Restrict to a small ASCII subset so the grammar has a chance to make progress.
    // This keeps the fuzzer focused on VM correctness, backtracking, and error paths.
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

    let graph = BUILT.as_graph();
    let mut engine = Engine::new();
    let _ = engine.parse(&graph, &buf);
});
