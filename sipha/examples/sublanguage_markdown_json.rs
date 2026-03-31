use sipha::prelude::*;
use sipha::SyntaxKinds;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SyntaxKinds)]
#[repr(u16)]
enum MdK {
    Root,
    CodeFence,
    CodeLang,
    CodeBody,
    Text,
    Ticks,
    Newline,
    Ws,
    // embedded wrapper
    Embedded,
    // json
    JsonRoot,
    JsonObj,
    JsonString,
    JsonLBrace,
    JsonRBrace,
    JsonColon,
    JsonComma,
    JsonWs,
}

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    MdK::from_syntax_kind(k).map(|k| match k {
        MdK::Root => "ROOT",
        MdK::CodeFence => "CODE_FENCE",
        MdK::CodeLang => "CODE_LANG",
        MdK::CodeBody => "CODE_BODY",
        MdK::Text => "TEXT",
        MdK::Ticks => "TICKS",
        MdK::Newline => "NEWLINE",
        MdK::Ws => "WS",
        MdK::Embedded => "EMBEDDED",
        MdK::JsonRoot => "JSON_ROOT",
        MdK::JsonObj => "JSON_OBJ",
        MdK::JsonString => "JSON_STRING",
        MdK::JsonLBrace => "JSON_LBRACE",
        MdK::JsonRBrace => "JSON_RBRACE",
        MdK::JsonColon => "JSON_COLON",
        MdK::JsonComma => "JSON_COMMA",
        MdK::JsonWs => "JSON_WS",
    })
}

fn markdown_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.node(MdK::Root, |g| {
            g.zero_or_more(|g| {
                g.call("chunk");
            });
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("newline", |g| {
        g.token(MdK::Newline, |g| {
            g.byte(b'\n');
        });
    });

    g.lexer_rule("ws", |g| {
        g.trivia(MdK::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("ticks", |g| {
        g.token(MdK::Ticks, |g| {
            g.literal(b"```");
        });
    });

    g.lexer_rule("lang_json", |g| {
        g.token(MdK::CodeLang, |g| {
            g.literal(b"json");
        });
    });

    // Code body: everything until a line that starts with ```
    // (toy grammar for demo purposes).
    g.lexer_rule("code_body", |g| {
        g.token(MdK::CodeBody, |g| {
            g.zero_or_more(|g| {
                // Stop before a closing fence.
                g.neg_lookahead(|g| {
                    g.literal(b"```");
                });
                g.class(CharClass::ANY);
            });
        });
    });

    g.lexer_rule("text", |g| {
        g.token(MdK::Text, |g| {
            g.one_or_more(|g| {
                g.neg_lookahead(|g| {
                    g.literal(b"```");
                });
                g.class(CharClass::ANY);
            });
        });
    });

    g.parser_rule("code_fence_json", |g| {
        g.context_rule("markdown-code-fence", |g| {
            g.node(MdK::CodeFence, |g| {
                g.trace("code_fence_json");
                g.call("ticks");
                g.call("lang_json");
                g.call("newline");
                g.call("code_body");
                g.hint("missing closing ``` fence?");
                g.call("ticks");
            });
        });
    });

    g.parser_rule("chunk", |g| {
        g.choice(
            |g| {
                g.call("code_fence_json");
            },
            |g| {
                g.call("text");
            },
        );
    });

    g.finish().unwrap()
}

fn json_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("jws");
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.node(MdK::JsonRoot, |g| {
            g.call("object");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("jws", |g| {
        g.trivia(MdK::JsonWs, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("jstring", |g| {
        g.token(MdK::JsonString, |g| {
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

    g.parser_rule("lbrace", |g| {
        g.token(MdK::JsonLBrace, |g| {
            g.byte(b'{');
        });
    });
    g.parser_rule("rbrace", |g| {
        g.token(MdK::JsonRBrace, |g| {
            g.byte(b'}');
        });
    });
    g.parser_rule("colon", |g| {
        g.token(MdK::JsonColon, |g| {
            g.byte(b':');
        });
    });
    g.parser_rule("comma", |g| {
        g.token(MdK::JsonComma, |g| {
            g.byte(b',');
        });
    });

    g.parser_rule("pair", |g| {
        g.node(MdK::JsonObj, |g| {
            g.call("jstring");
            g.call("colon");
            g.call("jstring");
        });
    });

    g.parser_rule("object", |g| {
        g.context_rule("json-object", |g| {
            g.node(MdK::JsonObj, |g| {
                g.trace("json object");
                g.call("lbrace");
                g.optional(|g| {
                    g.call("pair");
                    g.zero_or_more(|g| {
                        g.call("comma");
                        g.call("pair");
                    });
                });
                g.hint("missing closing '}'?");
                g.call("rbrace");
            });
        });
    });

    g.finish().unwrap()
}

fn main() {
    let md = markdown_grammar();
    let json = json_grammar();
    let src = b"hello\n```json\n{\"a\":\"b\"}\n```\nbye\n";

    let mut engine = Engine::new();
    let host_out = engine.parse(&md.as_graph(), src).unwrap();

    let (tree_events, errors) = apply_sublanguages(
        &mut engine,
        src,
        &host_out.tree_events,
        &[SubLanguage {
            host_kind: MdK::CodeFence as SyntaxKind,
            span: EmbeddedSpan::TokenKind(MdK::CodeBody as SyntaxKind),
            embedded: json.as_graph(),
            wrapper_kind: Some(MdK::Embedded as SyntaxKind),
            error_kind: Some(MdK::Embedded as SyntaxKind),
        }],
    );

    assert!(errors.is_empty(), "{errors:#?}");
    let green = sipha::tree::green::build_green_tree(src, &tree_events).unwrap();
    let root = sipha::tree::red::SyntaxNode::new_root(green);
    println!(
        "{}",
        sipha::tree::tree_display::format_syntax_tree(&root, &TreeDisplayOptions::default(), |k| {
            kind_name(k).unwrap_or("?").to_string()
        })
    );
}
