use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    EmbeddedWrapper,
    // json
    Embedded(JsonK),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, sipha::SyntaxKinds)]
#[repr(u16)]
enum JsonK {
    Root,
    Obj,
    String,
    LBrace,
    RBrace,
    Colon,
    Comma,
    Ws,
}

impl sipha::types::IntoSyntaxKind for MdK {
    #[inline]
    fn into_syntax_kind(self) -> sipha::types::SyntaxKind {
        const BASE_COUNT: u16 = 9; // MdK unit variants, up to EmbeddedWrapper
        match self {
            MdK::Root => 0,
            MdK::CodeFence => 1,
            MdK::CodeLang => 2,
            MdK::CodeBody => 3,
            MdK::Text => 4,
            MdK::Ticks => 5,
            MdK::Newline => 6,
            MdK::Ws => 7,
            MdK::EmbeddedWrapper => 8,
            MdK::Embedded(jk) => BASE_COUNT + jk.into_syntax_kind(),
        }
    }
}

impl sipha::types::FromSyntaxKind for MdK {
    fn from_syntax_kind(k: sipha::types::SyntaxKind) -> Option<Self> {
        const BASE_COUNT: u16 = 9; // MdK unit variants, up to EmbeddedWrapper
        match k {
            0 => Some(MdK::Root),
            1 => Some(MdK::CodeFence),
            2 => Some(MdK::CodeLang),
            3 => Some(MdK::CodeBody),
            4 => Some(MdK::Text),
            5 => Some(MdK::Ticks),
            6 => Some(MdK::Newline),
            7 => Some(MdK::Ws),
            8 => Some(MdK::EmbeddedWrapper),
            _ => JsonK::from_syntax_kind(k - BASE_COUNT).map(MdK::Embedded),
        }
    }
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
        MdK::EmbeddedWrapper => "EMBEDDED",
        MdK::Embedded(JsonK::Root) => "JSON_ROOT",
        MdK::Embedded(JsonK::Obj) => "JSON_OBJ",
        MdK::Embedded(JsonK::String) => "JSON_STRING",
        MdK::Embedded(JsonK::LBrace) => "JSON_LBRACE",
        MdK::Embedded(JsonK::RBrace) => "JSON_RBRACE",
        MdK::Embedded(JsonK::Colon) => "JSON_COLON",
        MdK::Embedded(JsonK::Comma) => "JSON_COMMA",
        MdK::Embedded(JsonK::Ws) => "JSON_WS",
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
        g.node(MdK::Embedded(JsonK::Root), |g| {
            g.call("object");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("jws", |g| {
        g.trivia(MdK::Embedded(JsonK::Ws), |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("jstring", |g| {
        g.token(MdK::Embedded(JsonK::String), |g| {
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
        g.token(MdK::Embedded(JsonK::LBrace), |g| {
            g.byte(b'{');
        });
    });
    g.parser_rule("rbrace", |g| {
        g.token(MdK::Embedded(JsonK::RBrace), |g| {
            g.byte(b'}');
        });
    });
    g.parser_rule("colon", |g| {
        g.token(MdK::Embedded(JsonK::Colon), |g| {
            g.byte(b':');
        });
    });
    g.parser_rule("comma", |g| {
        g.token(MdK::Embedded(JsonK::Comma), |g| {
            g.byte(b',');
        });
    });

    g.parser_rule("pair", |g| {
        g.node(MdK::Embedded(JsonK::Obj), |g| {
            g.call("jstring");
            g.call("colon");
            g.call("jstring");
        });
    });

    g.parser_rule("object", |g| {
        g.context_rule("json-object", |g| {
            g.node(MdK::Embedded(JsonK::Obj), |g| {
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
            host_kind: MdK::CodeFence.into_syntax_kind(),
            span: EmbeddedSpan::TokenKind(MdK::CodeBody.into_syntax_kind()),
            embedded: json.as_graph(),
            wrapper_kind: Some(MdK::EmbeddedWrapper.into_syntax_kind()),
            error_kind: Some(MdK::EmbeddedWrapper.into_syntax_kind()),
        }],
    );

    assert!(errors.is_empty(), "{errors:#?}");
    let green = sipha::tree::green::build_green_tree(src, &tree_events).unwrap();
    let root = sipha::tree::red::SyntaxNode::new_root(green);
    let options = TreeDisplayOptions {
        show_ranges: true,
        show_offsets: true,
        align_columns: true,
        ..TreeDisplayOptions::default()
    };
    println!(
        "{}",
        sipha::tree::tree_display::format_syntax_tree(&root, &options, |k| {
            kind_name(k).unwrap_or("?").to_string()
        })
    );
}
