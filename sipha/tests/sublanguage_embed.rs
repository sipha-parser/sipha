#![cfg(feature = "std")]

use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
enum Lex {
    Body,
    JsonString,
    JsonLBrace,
    JsonRBrace,
    JsonColon,
    Ws,
}

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        match self {
            Lex::Body => "BODY",
            Lex::JsonString => "JSON_STRING",
            Lex::JsonLBrace => "JSON_LBRACE",
            Lex::JsonRBrace => "JSON_RBRACE",
            Lex::JsonColon => "JSON_COLON",
            Lex::Ws => "WS",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, RuleKinds)]
#[sipha(lex = Lex)]
#[repr(u16)]
enum Rule {
    Root,
    Host,
    Embedded,
    JsonRoot,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::Host => "HOST",
            Rule::Embedded => "EMBEDDED",
            Rule::JsonRoot => "JSON_ROOT",
        }
    }
}

fn host_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.parser_rule("start", |g| {
        g.node(Rule::Root, |g| {
            g.node(Rule::Host, |g| {
                g.call("body");
            });
        });
        g.accept();
    });

    g.lexer_rule("ws", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("body", |g| {
        g.token(Lex::Body, |g| {
            g.literal(b"{\"a\":\"b\"}");
        });
    });

    g.finish().unwrap()
}

fn json_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.parser_rule("start", |g| {
        g.node(Rule::JsonRoot, |g| {
            g.token(Lex::JsonLBrace, |g| {
                g.byte(b'{');
            });
            g.call("jstring");
            g.token(Lex::JsonColon, |g| {
                g.byte(b':');
            });
            g.call("jstring");
            g.token(Lex::JsonRBrace, |g| {
                g.byte(b'}');
            });
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("ws", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("jstring", |g| {
        g.token(Lex::JsonString, |g| {
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
            host_kind: Rule::Host.into_syntax_kind(),
            span: EmbeddedSpan::TokenKind(Lex::Body.into_syntax_kind()),
            embedded: json.as_graph(),
            wrapper_kind: Some(Rule::Embedded.into_syntax_kind()),
            error_kind: Some(Rule::Embedded.into_syntax_kind()),
        }],
    );

    assert!(errors.is_empty(), "{errors:#?}");
    assert!(
        rewritten.iter().any(|e| matches!(
            e,
            TreeEvent::NodeOpen { kind, .. } if *kind == Rule::Embedded.into_syntax_kind()
        )),
        "expected an Embedded wrapper node to be inserted"
    );
}
