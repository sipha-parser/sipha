use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, sipha::SyntaxKinds)]
#[repr(u16)]
enum K {
    Root,
    Ws,
}

fn main() {
    let src = b"aX";
    let mut engine = Engine::new();

    // Grammar that *fails* on `aX` (expects 'b' as the second byte).
    let built_err = {
        let mut g = GrammarBuilder::new();
        g.set_trivia_rule("ws");
        g.parser_rule("start", |g| {
            g.node(K::Root, |g| {
                g.byte(b'a');
                g.byte(b'b');
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
        g.finish().expect("valid grammar")
    };
    let graph_err = built_err.as_graph();

    // Grammar that can *succeed* on the same source, so we can build a ParsedDoc for
    // UTF-16 conversions even when the "real" parse failed.
    let built_ok = {
        let mut g = GrammarBuilder::new();
        g.set_trivia_rule("ws");
        g.parser_rule("start", |g| {
            g.node(K::Root, |g| {
                g.byte(b'a');
                g.any_char();
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
        g.finish().expect("valid grammar")
    };
    let graph_ok = built_ok.as_graph();

    // This fails at the second byte: expected 'b'.
    let err = engine.parse(&graph_err, src).unwrap_err();

    // Build a `ParsedDoc` from a successful parse of the same buffer.
    let out_ok = engine.parse(&graph_ok, src).unwrap();
    let doc = ParsedDoc::from_slice(src, &out_ok).unwrap();

    if let Some(lsp_diag) = sipha::diagnostics::lsp::parse_error_to_lsp(&doc, &err) {
        // Just show the structure so you can see the range/severity/message.
        println!("{lsp_diag:?}");
    }
}

