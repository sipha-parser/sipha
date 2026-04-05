use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
enum Lex {
    Newline,
    Ws,
    Text,
    Ticks,
    CodeLang,
    CodeBody,
    JString,
    JLBrace,
    JRBrace,
    JColon,
    JComma,
    JWs,
}

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        match self {
            Lex::Newline => "NEWLINE",
            Lex::Ws => "WS",
            Lex::Text => "TEXT",
            Lex::Ticks => "TICKS",
            Lex::CodeLang => "CODE_LANG",
            Lex::CodeBody => "CODE_BODY",
            Lex::JString => "JSON_STRING",
            Lex::JLBrace => "JSON_LBRACE",
            Lex::JRBrace => "JSON_RBRACE",
            Lex::JColon => "JSON_COLON",
            Lex::JComma => "JSON_COMMA",
            Lex::JWs => "JSON_WS",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, RuleKinds)]
#[sipha(lex = Lex)]
#[repr(u16)]
enum Rule {
    Root,
    CodeFence,
    EmbeddedWrapper,
    JsonRoot,
    JsonObj,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::CodeFence => "CODE_FENCE",
            Rule::EmbeddedWrapper => "EMBEDDED",
            Rule::JsonRoot => "JSON_ROOT",
            Rule::JsonObj => "JSON_OBJ",
        }
    }
}

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    Lex::from_syntax_kind(k)
        .map(LexKind::display_name)
        .or_else(|| Rule::from_syntax_kind(k).map(RuleKind::display_name))
}

fn markdown_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.node(Rule::Root, |g| {
            g.zero_or_more(|g| {
                g.call("chunk");
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

    g.lexer_rule("ws", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("ticks", |g| {
        g.token(Lex::Ticks, |g| {
            g.literal(b"```");
        });
    });

    g.lexer_rule("lang_json", |g| {
        g.token(Lex::CodeLang, |g| {
            g.literal(b"json");
        });
    });

    // Code body: everything until a line that starts with ```
    // (toy grammar for demo purposes).
    g.lexer_rule("code_body", |g| {
        g.token(Lex::CodeBody, |g| {
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
        g.token(Lex::Text, |g| {
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
            g.node(Rule::CodeFence, |g| {
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
        g.node(Rule::JsonRoot, |g| {
            g.call("object");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("jws", |g| {
        g.trivia(Lex::JWs, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("jstring", |g| {
        g.token(Lex::JString, |g| {
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
        g.token(Lex::JLBrace, |g| {
            g.byte(b'{');
        });
    });
    g.parser_rule("rbrace", |g| {
        g.token(Lex::JRBrace, |g| {
            g.byte(b'}');
        });
    });
    g.parser_rule("colon", |g| {
        g.token(Lex::JColon, |g| {
            g.byte(b':');
        });
    });
    g.parser_rule("comma", |g| {
        g.token(Lex::JComma, |g| {
            g.byte(b',');
        });
    });

    g.parser_rule("pair", |g| {
        g.node(Rule::JsonObj, |g| {
            g.call("jstring");
            g.call("colon");
            g.call("jstring");
        });
    });

    g.parser_rule("object", |g| {
        g.context_rule("json-object", |g| {
            g.node(Rule::JsonObj, |g| {
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
            host_kind: Rule::CodeFence.into_syntax_kind(),
            span: EmbeddedSpan::TokenKind(Lex::CodeBody.into_syntax_kind()),
            embedded: json.as_graph(),
            wrapper_kind: Some(Rule::EmbeddedWrapper.into_syntax_kind()),
            error_kind: Some(Rule::EmbeddedWrapper.into_syntax_kind()),
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
