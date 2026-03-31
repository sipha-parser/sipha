use sipha::prelude::*;
use sipha::SyntaxKinds;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
enum K {
    Root,
    Stmt,
    Ident,
    Number,
    Let,
    Eq,
    Semi,
    Ws,
}

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = K::Root)]
struct Root(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = K::Stmt)]
struct Stmt(SyntaxNode);

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    K::from_syntax_kind(k).map(|k| match k {
        K::Root => "ROOT",
        K::Stmt => "STMT",
        K::Ident => "IDENT",
        K::Number => "NUMBER",
        K::Let => "LET",
        K::Eq => "EQ",
        K::Semi => "SEMI",
        K::Ws => "WS",
    })
}

fn grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.node(K::Root, |g| {
            g.zero_or_more(|g| {
                g.recover_until("semi", |g| {
                    g.call("stmt");
                });
                g.optional(|g| {
                    g.call("semi");
                });
            });
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

    g.lexer_rule("ident", |g| {
        g.token(K::Ident, |g| {
            g.class(classes::IDENT_START);
            g.zero_or_more(|g| {
                g.class(classes::IDENT_CONT);
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

    g.parser_rule("semi", |g| {
        g.token(K::Semi, |g| {
            g.byte(b';');
        });
    });

    g.parser_rule("stmt", |g| {
        g.context_rule("statement", |g| {
            g.node(K::Stmt, |g| {
                g.trace("stmt");
                g.keyword(K::Let, b"let");
                g.hint("expected an identifier after 'let'");
                g.call("ident");
                g.token(K::Eq, |g| {
                    g.byte(b'=');
                });
                g.hint("expected a number after '='");
                g.call("number");
            });
        });
    });

    g.finish().unwrap()
}

fn main() {
    let built = grammar();
    let graph = built.as_graph();

    let src = b"let a=1; let =2; let c=; let d=4;";
    let mut engine = Engine::new();
    let ctx = ParseContext::new().with_error_node_kind(K::Stmt as SyntaxKind);

    #[cfg(feature = "trace")]
    if std::env::var("SIPHA_TRACE").is_ok() {
        let mut tracer = sipha::parse::engine::PrintTracer::default();
        let _ = engine.parse_with_tracer(&graph, src, &ctx, &mut tracer);
    }

    match engine.parse_recovering_multi_with_context(&graph, src, &ctx, 16) {
        Ok(out) => {
            if let Some(doc) = ParsedDoc::from_slice(src, &out) {
                // Typed CST: collect recovered statements (including error nodes).
                let root: Root = doc.root().ast().unwrap();
                let stmts: Vec<Stmt> =
                    sipha::tree::ast::AstNodeExt::children::<Stmt>(root.syntax()).collect();
                println!("stmts={}", stmts.len());

                println!(
                    "{}",
                    sipha::tree::tree_display::format_syntax_tree(
                        doc.root(),
                        &TreeDisplayOptions::default(),
                        |k| kind_name(k).unwrap_or("?").to_string()
                    )
                );
                return;
            }

            // If recovery succeeded but produced malformed/empty tree events,
            // force an early stop so the engine can synthesize a well-nested
            // partial tree with an inserted error node.
            if let Err(multi) = engine.parse_recovering_multi_with_context(&graph, src, &ctx, 1) {
                let li = LineIndex::new(src);
                for (i, e) in multi.errors.iter().enumerate() {
                    if let ParseError::NoMatch(d) = e {
                        let s = d.format_with_source(src, &li, Some(&graph.literals), Some(&graph));
                        println!("error {i}:\n{s}\n");
                    } else {
                        println!("error {i}: {e}");
                    }
                }
                if let Some(doc) = ParsedDoc::from_slice(src, &multi.partial) {
                    // Typed CST on partial tree as well.
                    if let Some(root) = doc.root().ast::<Root>() {
                        let stmts: Vec<Stmt> = sipha::tree::ast::AstNodeExt::children::<Stmt>(
                            root.syntax(),
                        )
                        .collect();
                        println!("partial_stmts={}", stmts.len());
                    }
                    println!(
                        "{}",
                        sipha::tree::tree_display::format_syntax_tree(
                            doc.root(),
                            &TreeDisplayOptions::default(),
                            |k| kind_name(k).unwrap_or("?").to_string()
                        )
                    );
                }
            }
        }
        Err(multi) => {
            let li = LineIndex::new(src);
            for (i, e) in multi.errors.iter().enumerate() {
                if let ParseError::NoMatch(d) = e {
                    let s = d.format_with_source(src, &li, Some(&graph.literals), Some(&graph));
                    println!("error {i}:\n{s}\n");
                } else {
                    println!("error {i}: {e}");
                }
            }
            if let Some(doc) = ParsedDoc::from_slice(src, &multi.partial) {
                if let Some(root) = doc.root().ast::<Root>() {
                    let stmts: Vec<Stmt> =
                        sipha::tree::ast::AstNodeExt::children::<Stmt>(root.syntax()).collect();
                    println!("partial_stmts={}", stmts.len());
                }
                println!(
                    "{}",
                    sipha::tree::tree_display::format_syntax_tree(
                        doc.root(),
                        &TreeDisplayOptions::default(),
                        |k| kind_name(k).unwrap_or("?").to_string()
                    )
                );
            }
        }
    }
}
