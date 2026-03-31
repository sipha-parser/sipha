#![cfg(feature = "std")]

use sipha::prelude::*;
use sipha::SyntaxKinds;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
pub enum K {
    Root,
    // Nodes
    Expr,
    BinExpr,
    ParenExpr,
    // Tokens
    Number,
    Plus,
    Star,
    LParen,
    RParen,
    Ws,
}

pub fn kind_to_name(k: SyntaxKind) -> Option<&'static str> {
    K::from_syntax_kind(k).map(|k| match k {
        K::Root => "ROOT",
        K::Expr => "EXPR",
        K::BinExpr => "BIN_EXPR",
        K::ParenExpr => "PAREN_EXPR",
        K::Number => "NUMBER",
        K::Plus => "PLUS",
        K::Star => "STAR",
        K::LParen => "LPAREN",
        K::RParen => "RPAREN",
        K::Ws => "WS",
    })
}

pub fn expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.allow_rule_cycles(true);
    g.set_trivia_rule("ws");

    // Start rule must be rule 0 (Engine always enters rule 0).
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

pub fn parse_doc(graph: &ParseGraph<'_>, src: &[u8]) -> ParsedDoc {
    let mut engine = Engine::new();
    let out = engine.parse(graph, src).expect("parse should succeed");
    ParsedDoc::from_slice(src, &out).expect("should produce syntax root")
}

pub fn sexp(node: &SyntaxNode) -> String {
    let opts = sipha::tree::sexp::SexpOptions {
        include_trivia: false,
        kind_to_name: Some(kind_to_name),
        max_token_len: None,
    };
    sipha::tree::sexp::syntax_node_to_sexp(node, &opts)
}
