#![cfg(feature = "std")]

use sipha::SyntaxKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
enum K {
    Root,
    Host,
    Body,
    Embedded,
    JsonRoot,
    JsonString,
    JsonLBrace,
    JsonRBrace,
    JsonColon,
    Ws,
}

fn host_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.parser_rule("start", |g| {
        g.node(K::Root, |g| {
            g.node(K::Host, |g| {
                g.call("body");
            });
        });
        g.accept();
    });

    g.lexer_rule("ws", |g| {
        g.trivia(K::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("body", |g| {
        g.token(K::Body, |g| {
            g.literal(b"{\"a\":\"b\"}");
        });
    });

    g.finish().unwrap()
}

fn json_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.parser_rule("start", |g| {
        g.node(K::JsonRoot, |g| {
            g.token(K::JsonLBrace, |g| {
                g.byte(b'{');
            });
            g.call("jstring");
            g.token(K::JsonColon, |g| {
                g.byte(b':');
            });
            g.call("jstring");
            g.token(K::JsonRBrace, |g| {
                g.byte(b'}');
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

    g.lexer_rule("jstring", |g| {
        g.token(K::JsonString, |g| {
            g.byte(b'"');
            g.zero_or_more(|g| {
                g.neg_lookahead(|g| {
                    g.byte(b'"');
                });
                g.class(CharClass::ANY);
            });
            g.byte(b'"');
        });
    });

    g.finish().unwrap()
}

#[test]
fn can_splice_embedded_tree_events() {
    let host = host_grammar();
    let json = json_grammar();
    let src = b"{\"a\":\"b\"}";

    let mut engine = Engine::new();
    let host_out = engine.parse(&host.as_graph(), src).unwrap();

    let (rewritten, errors) = apply_sublanguages(
        &mut engine,
        src,
        &host_out.tree_events,
        &[SubLanguage {
            host_kind: K::Host as SyntaxKind,
            span: EmbeddedSpan::TokenKind(K::Body as SyntaxKind),
            embedded: json.as_graph(),
            wrapper_kind: Some(K::Embedded as SyntaxKind),
            error_kind: Some(K::Embedded as SyntaxKind),
        }],
    );

    assert!(errors.is_empty(), "{errors:#?}");
    assert!(
        rewritten.iter().any(
            |e| matches!(e, TreeEvent::NodeOpen { kind, .. } if *kind == K::Embedded as SyntaxKind)
        ),
        "expected an Embedded wrapper node to be inserted"
    );
}
