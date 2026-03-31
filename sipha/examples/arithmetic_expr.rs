use sipha::prelude::*;
use sipha::SyntaxKinds;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
enum K {
    Root,
    Expr,
    Term,
    Factor,
    Number,
    Plus,
    Star,
    LParen,
    RParen,
    Ws,
}

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    K::from_syntax_kind(k).map(|k| match k {
        K::Root => "ROOT",
        K::Expr => "EXPR",
        K::Term => "TERM",
        K::Factor => "FACTOR",
        K::Number => "NUMBER",
        K::Plus => "PLUS",
        K::Star => "STAR",
        K::LParen => "LPAREN",
        K::RParen => "RPAREN",
        K::Ws => "WS",
    })
}

fn grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");
    // This grammar has an intentional rule cycle: expr → term → factor → expr.
    // Cycles are rejected by default unless explicitly enabled.
    g.allow_rule_cycles(true);
    // Emit TracePoint instructions only when explicitly enabled.
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.context_rule("start", |g| {
            g.node(K::Root, |g| {
                g.call("expr");
                g.skip();
            });
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

    // expr := term ("+" term)*
    g.parser_rule("expr", |g| {
        g.context_rule("expression", |g| {
            g.trace("enter expr");
            g.node(K::Expr, |g| {
                g.call("term");
                g.zero_or_more(|g| {
                    g.token(K::Plus, |g| {
                        g.byte(b'+');
                    });
                    g.call("term");
                });
            });
        });
    });

    // term := factor ("*" factor)*
    g.parser_rule("term", |g| {
        g.context_rule("term", |g| {
            g.node(K::Term, |g| {
                g.call("factor");
                g.zero_or_more(|g| {
                    g.token(K::Star, |g| {
                        g.byte(b'*');
                    });
                    g.call("factor");
                });
            });
        });
    });

    // factor := number | "(" expr ")"
    g.parser_rule("factor", |g| {
        g.context_rule("factor", |g| {
            g.node(K::Factor, |g| {
                g.choice(
                    |g| {
                        g.call("number");
                    },
                    |g| {
                        g.token(K::LParen, |g| {
                            g.byte(b'(');
                        });
                        g.call("expr");
                        g.hint("did you forget a closing ')' ?");
                        g.token(K::RParen, |g| {
                            g.byte(b')');
                        });
                    },
                );
            });
        });
    });

    g.finish().unwrap()
}

fn main() {
    let built = grammar();
    let graph = built.as_graph();

    let src = b"1 + 2*(3+4)";
    let mut engine = Engine::new();
    #[cfg(feature = "trace")]
    if std::env::var("SIPHA_TRACE").is_ok() {
        let mut tracer = sipha::parse::engine::PrintTracer::default();
        let err = engine
            .parse_with_tracer(&graph, src, &ParseContext::new(), &mut tracer)
            .unwrap_err();
        if let ParseError::NoMatch(d) = err {
            let li = LineIndex::new(src);
            eprintln!(
                "{}",
                d.format_with_source(src, &li, Some(&graph.literals), Some(&graph))
            );
        }
        return;
    }

    let out = engine.parse(&graph, src).unwrap_or_else(|e| {
        if let ParseError::NoMatch(d) = e {
            let li = LineIndex::new(src);
            eprintln!(
                "{}",
                d.format_with_source(src, &li, Some(&graph.literals), Some(&graph))
            );
        } else {
            eprintln!("{e}");
        }
        std::process::exit(1);
    });
    let doc = ParsedDoc::from_slice(src, &out).unwrap();

    let sexp = sipha::tree::sexp::syntax_node_to_sexp(
        doc.root(),
        &sipha::tree::sexp::SexpOptions {
            include_trivia: false,
            kind_to_name: Some(kind_name),
            max_token_len: None,
        },
    );
    println!("{sexp}");
}
