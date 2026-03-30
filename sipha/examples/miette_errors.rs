//! # Miette error reporting
//!
//! Run with: `cargo run --example miette_errors --features miette`
//!
//! Demonstrates converting a parse failure into a [`miette::Report`] for
//! pretty-printed diagnostics with source snippets.
//!
//! Syntax kinds are defined as an enum so we don't need to manage numeric constants.

use sipha::prelude::*;
use sipha::types::classes;
use sipha::SyntaxKinds;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SyntaxKinds)]
#[repr(u16)]
enum Kind {
    Root,
    Stmt,
    TokKeyword,
    TokIdent,
    TokSemicolon,
    TokWs,
}

fn build_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.rule("root", |g| {
        g.node(Kind::Root, |g| {
            g.trivia(Kind::TokWs, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
            g.zero_or_more(|g| {
                g.call("stmt");
            });
        });
        g.end_of_input();
        g.accept();
    });

    g.rule("stmt", |g| {
        g.node(Kind::Stmt, |g| {
            g.token(Kind::TokKeyword, |g| {
                g.choice(
                    |g| {
                        g.literal(b"let");
                    },
                    |g| {
                        g.choice(
                            |g| {
                                g.literal(b"const");
                            },
                            |g| {
                                g.literal(b"var");
                            },
                        )
                    },
                );
            });
            g.trivia(Kind::TokWs, |g| {
                g.one_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
            g.token(Kind::TokIdent, |g| {
                g.call("ident");
            });
            g.trivia(Kind::TokWs, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
            g.token(Kind::TokSemicolon, |g| {
                g.byte(b';');
            });
            g.trivia(Kind::TokWs, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
        });
    });

    g.rule("ident", |g| {
        g.char_range('a', 'z');
        g.zero_or_more(|g| {
            g.choice(
                |g| {
                    g.char_range('a', 'z');
                },
                |g| {
                    g.char_range('A', 'Z');
                },
            );
        });
    });

    g.finish().unwrap()
}

fn main() {
    let built = build_grammar();
    let graph = built.as_graph();
    let mut engine = Engine::new();

    // Intentionally invalid: missing semicolon after "foo"
    let source = "let foo\n  bar";
    let source_bytes = source.as_bytes();

    if let Err(e) = engine.parse(&graph, source_bytes) {
        if let Some(report) =
            e.to_miette_report(source, "script.txt", Some(&graph.literals), Some(&graph))
        {
            eprintln!("{report:?}");
        } else {
            eprintln!("{e}");
        }
    }
}
