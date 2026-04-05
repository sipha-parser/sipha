use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
enum Lex {
    Key,
    Value,
    LBracket,
    RBracket,
    Eq,
    Newline,
    Ws,
    Comment,
}

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        match self {
            Lex::Key => "KEY",
            Lex::Value => "VALUE",
            Lex::LBracket => "LBRACKET",
            Lex::RBracket => "RBRACKET",
            Lex::Eq => "EQ",
            Lex::Newline => "NEWLINE",
            Lex::Ws => "WS",
            Lex::Comment => "COMMENT",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, RuleKinds)]
#[sipha(lex = Lex)]
#[repr(u16)]
enum Rule {
    Root,
    Header,
    Pair,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::Header => "HEADER",
            Rule::Pair => "PAIR",
        }
    }
}

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Root)]
struct Root(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Header)]
struct Header(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Pair)]
struct Pair(SyntaxNode);

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    Lex::from_syntax_kind(k)
        .map(LexKind::display_name)
        .or_else(|| Rule::from_syntax_kind(k).map(RuleKind::display_name))
}

fn grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("trivia");
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.context_rule("ini-file", |g| {
            g.node(Rule::Root, |g| {
                g.zero_or_more(|g| {
                    g.call("item");
                });
                g.skip();
            });
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("newline", |g| {
        g.token(Lex::Newline, |g| {
            g.byte(b'\n');
        });
    });

    // Whitespace and comments are trivia.
    g.lexer_rule("trivia", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| {
                g.choice(
                    |g| {
                        g.one_or_more(|g| {
                            g.class(classes::WHITESPACE);
                        });
                    },
                    |g| {
                        // Comment: '#' until newline or EOI.
                        g.trivia(Lex::Comment, |g| {
                            g.byte(b'#');
                            g.zero_or_more(|g| {
                                g.neg_lookahead(|g| {
                                    g.byte(b'\n');
                                });
                                g.class(CharClass::ANY);
                            });
                        });
                    },
                );
            });
        });
    });

    g.lexer_rule("key", |g| {
        g.token(Lex::Key, |g| {
            g.class_with_label(classes::IDENT_START, "ident-start");
            g.zero_or_more(|g| {
                g.class_with_label(classes::IDENT_CONT, "ident-cont");
            });
        });
    });

    g.lexer_rule("value", |g| {
        g.token(Lex::Value, |g| {
            // Value: all bytes until newline or comment start.
            g.zero_or_more(|g| {
                g.neg_lookahead(|g| {
                    g.choice(
                        |g| {
                            g.byte(b'\n');
                        },
                        |g| {
                            g.byte(b'#');
                        },
                    );
                });
                g.class(CharClass::ANY);
            });
        });
    });

    g.parser_rule("header", |g| {
        g.context_rule("ini-header", |g| {
            g.node(Rule::Header, |g| {
                g.trace("header");
                g.token(Lex::LBracket, |g| {
                    g.byte(b'[');
                });
                g.call("key");
                g.hint("missing closing ']' for section header?");
                g.token(Lex::RBracket, |g| {
                    g.byte(b']');
                });
            });
        });
    });

    g.parser_rule("pair", |g| {
        g.context_rule("ini-pair", |g| {
            g.node(Rule::Pair, |g| {
                g.call("key");
                g.token(Lex::Eq, |g| {
                    g.byte(b'=');
                });
                g.hint("missing value after '='?");
                g.call("value");
            });
        });
    });

    g.parser_rule("item", |g| {
        g.choice(
            |g| {
                g.call("header");
            },
            |g| {
                g.call("pair");
            },
        );
        g.optional(|g| {
            g.call("newline");
        });
    });

    g.finish().unwrap()
}

fn main() {
    let built = grammar();
    let graph = built.as_graph();

    let src = br#"
# a comment
[core]
repo = sipha

[ui]
theme = dark # inline comment
"#;

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

    let out = engine.parse(&graph, src).unwrap();
    let doc = ParsedDoc::from_slice(src, &out).unwrap();
    println!("{}", doc.source_str());

    // Typed CST: find all pairs and headers with typed wrappers.
    let root: Root = doc.root().ast().unwrap();
    let pair_count = root
        .syntax()
        .descendant_nodes()
        .filter_map(|n| n.ast::<Pair>())
        .count();
    let header_count = root
        .syntax()
        .descendant_nodes()
        .filter_map(|n| n.ast::<Header>())
        .count();
    println!("headers={header_count} pairs={pair_count}");

    println!(
        "{}",
        sipha::tree::tree_display::format_syntax_tree(
            doc.root(),
            &TreeDisplayOptions::default(),
            |k| kind_name(k).unwrap_or("?").to_string()
        )
    );
}
