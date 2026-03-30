use sipha::prelude::*;
use sipha::types::classes;

// ─── JSONC fuzz grammar: syntax kinds (same numbering as `sipha/examples/json_jsonc.rs`) ───

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsoncNode {
    Document = 0,
    Object = 1,
    Member = 2,
    Array = 3,
}

impl IntoSyntaxKind for JsoncNode {
    #[inline]
    fn into_syntax_kind(self) -> SyntaxKind {
        self as SyntaxKind
    }
}

impl FromSyntaxKind for JsoncNode {
    fn from_syntax_kind(k: SyntaxKind) -> Option<Self> {
        match k {
            0 => Some(Self::Document),
            1 => Some(Self::Object),
            2 => Some(Self::Member),
            3 => Some(Self::Array),
            _ => None,
        }
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsoncTok {
    String = 10,
    Number = 11,
    True = 12,
    False = 13,
    Null = 14,
    LBrace = 15,
    RBrace = 16,
    LBracket = 17,
    RBracket = 18,
    Colon = 19,
    Comma = 20,
}

impl IntoSyntaxKind for JsoncTok {
    #[inline]
    fn into_syntax_kind(self) -> SyntaxKind {
        self as SyntaxKind
    }
}

impl FromSyntaxKind for JsoncTok {
    fn from_syntax_kind(k: SyntaxKind) -> Option<Self> {
        match k {
            10 => Some(Self::String),
            11 => Some(Self::Number),
            12 => Some(Self::True),
            13 => Some(Self::False),
            14 => Some(Self::Null),
            15 => Some(Self::LBrace),
            16 => Some(Self::RBrace),
            17 => Some(Self::LBracket),
            18 => Some(Self::RBracket),
            19 => Some(Self::Colon),
            20 => Some(Self::Comma),
            _ => None,
        }
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JsoncTrivia {
    Ws = 30,
    LineComment = 31,
    BlockComment = 32,
}

impl IntoSyntaxKind for JsoncTrivia {
    #[inline]
    fn into_syntax_kind(self) -> SyntaxKind {
        self as SyntaxKind
    }
}

impl FromSyntaxKind for JsoncTrivia {
    fn from_syntax_kind(k: SyntaxKind) -> Option<Self> {
        match k {
            30 => Some(Self::Ws),
            31 => Some(Self::LineComment),
            32 => Some(Self::BlockComment),
            _ => None,
        }
    }
}

pub fn tiny_ab_star() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.node(1, |g| {
            g.zero_or_more(|g| {
                g.token(10, |g| {
                    g.choice(
                        |g| {
                            g.byte(b'a');
                        },
                        |g| {
                            g.byte(b'b');
                        },
                    );
                });
            });
        });
        g.end_of_input();
        g.accept();
    });
    g.finish().expect("grammar should be valid")
}

pub fn json_rfc8259() -> BuiltGraph {
    // Mostly copied from `sipha/examples/json_grammar.rs`, but trimmed for fuzzing.
    const T_VALUE: Tag = 0;
    const T_OBJECT: Tag = 1;
    const T_ARRAY: Tag = 2;
    const T_STRING: Tag = 3;
    const T_NUMBER: Tag = 4;
    const T_BOOL: Tag = 5;
    const T_NULL: Tag = 6;
    const T_MEMBER: Tag = 7;

    let mut g = GrammarBuilder::new();
    g.allow_rule_cycles(true);

    g.rule("json", |g| {
        g.call("ws");
        g.call("value");
        g.call("ws");
        g.end_of_input();
        g.accept();
    });

    g.rule("value", |g| {
        g.capture(T_VALUE, |g| {
            let class_quote = CharClass::from_byte(b'"');
            let class_lbrace = CharClass::from_byte(b'{');
            let class_lbrack = CharClass::from_byte(b'[');
            let class_number = classes::DIGIT.with_byte(b'-');
            let class_t = CharClass::from_byte(b't');
            let class_f = CharClass::from_byte(b'f');
            let class_n = CharClass::from_byte(b'n');

            g.byte_dispatch(
                vec![
                    (class_quote, Box::new(|g| {
                        g.call("string");
                    })),
                    (class_lbrace, Box::new(|g| {
                        g.call("object");
                    })),
                    (class_lbrack, Box::new(|g| {
                        g.call("array");
                    })),
                    (class_number, Box::new(|g| {
                        g.call("number");
                    })),
                    (
                        class_t,
                        Box::new(|g| {
                            g.capture(T_BOOL, |g| {
                                g.literal(b"true");
                            });
                        }),
                    ),
                    (
                        class_f,
                        Box::new(|g| {
                            g.capture(T_BOOL, |g| {
                                g.literal(b"false");
                            });
                        }),
                    ),
                    (
                        class_n,
                        Box::new(|g| {
                            g.capture(T_NULL, |g| {
                                g.literal(b"null");
                            });
                        }),
                    ),
                ],
                None,
            );
        });
    });

    g.rule("object", |g| {
        g.capture(T_OBJECT, |g| {
            g.byte(b'{');
            g.call("ws");
            g.optional(|g| {
                g.call("member");
                g.zero_or_more(|g| {
                    g.call("ws");
                    g.byte(b',');
                    g.call("ws");
                    g.call("member");
                });
            });
            g.call("ws");
            g.byte(b'}');
        });
    });

    g.rule("member", |g| {
        g.capture(T_MEMBER, |g| {
            g.call("string");
            g.call("ws");
            g.byte(b':');
            g.call("ws");
            g.call("value");
        });
    });

    g.rule("array", |g| {
        g.capture(T_ARRAY, |g| {
            g.byte(b'[');
            g.call("ws");
            g.optional(|g| {
                g.call("value");
                g.zero_or_more(|g| {
                    g.call("ws");
                    g.byte(b',');
                    g.call("ws");
                    g.call("value");
                });
            });
            g.call("ws");
            g.byte(b']');
        });
    });

    g.rule("string", |g| {
        g.capture(T_STRING, |g| {
            g.byte(b'"');
            g.zero_or_more(|g| {
                g.neg_lookahead(|g| {
                    g.byte(b'"');
                });
                g.choice(
                    |g| {
                        g.call("escape");
                    },
                    |g| {
                        g.class(CharClass::ANY);
                    },
                );
            });
            g.byte(b'"');
        });
    });

    g.rule("escape", |g| {
        g.byte(b'\\');
        g.choice(
            |g| {
                g.class(
                    CharClass::EMPTY
                        .with_byte(b'"')
                        .with_byte(b'\\')
                        .with_byte(b'/')
                        .with_byte(b'b')
                        .with_byte(b'f')
                        .with_byte(b'n')
                        .with_byte(b'r')
                        .with_byte(b't'),
                );
            },
            |g| {
                g.byte(b'u');
                g.repeat(4u32, |g| {
                    g.class(classes::HEX_DIGIT);
                });
            },
        );
    });

    g.rule("number", |g| {
        g.capture(T_NUMBER, |g| {
            g.optional(|g| {
                g.byte(b'-');
            });
            g.choice(
                |g| {
                    g.byte(b'0');
                },
                |g| {
                    g.byte_range(b'1', b'9');
                    g.zero_or_more(|g| {
                        g.class(classes::DIGIT);
                    });
                },
            );
            g.optional(|g| {
                g.byte(b'.');
                g.one_or_more(|g| {
                    g.class(classes::DIGIT);
                });
            });
            g.optional(|g| {
                g.choice(
                    |g| {
                        g.byte(b'e');
                    },
                    |g| {
                        g.byte(b'E');
                    },
                );
                g.optional(|g| {
                    g.choice(
                        |g| {
                            g.byte(b'+');
                        },
                        |g| {
                            g.byte(b'-');
                        },
                    );
                });
                g.one_or_more(|g| {
                    g.class(classes::DIGIT);
                });
            });
        });
    });

    g.rule("ws", |g| {
        g.zero_or_more(|g| {
            g.class(classes::WHITESPACE);
        });
    });

    g.finish().expect("JSON grammar must be valid")
}

pub fn jsonc_trivia_skip() -> BuiltGraph {
    // Trimmed from `sipha/examples/json_jsonc.rs` to exercise trivia skipping + comments.

    const STRING_CHAR: CharClass = CharClass::EMPTY
        .with_range(b'\x00', b'\x1F')
        .with_byte(b'"')
        .with_byte(b'\\')
        .complement();

    const ESCAPE_SIMPLE: CharClass = CharClass::EMPTY
        .with_byte(b'"')
        .with_byte(b'\\')
        .with_byte(b'/')
        .with_byte(b'b')
        .with_byte(b'f')
        .with_byte(b'n')
        .with_byte(b'r')
        .with_byte(b't');

    fn emit_shared_rules(g: &mut GrammarBuilder) {
        g.parser_rule("document", |g| {
            g.node(JsoncNode::Document, |g| {
                g.call("value");
                g.skip();
            });
            g.end_of_input();
            g.accept();
        });

        g.parser_rule("value", |g| {
            g.byte_dispatch(
                vec![
                    (CharClass::from_byte(b'"'), Box::new(|g| {
                        g.call("string");
                    })),
                    (CharClass::from_byte(b'{'), Box::new(|g| {
                        g.call("object");
                    })),
                    (CharClass::from_byte(b'['), Box::new(|g| {
                        g.call("array");
                    })),
                    (
                        classes::DIGIT.with_byte(b'-'),
                        Box::new(|g| {
                            g.call("number");
                        }),
                    ),
                    (
                        CharClass::from_byte(b't'),
                        Box::new(|g| {
                            g.token(JsoncTok::True, |g| {
                                g.literal(b"true");
                            });
                        }),
                    ),
                    (
                        CharClass::from_byte(b'f'),
                        Box::new(|g| {
                            g.token(JsoncTok::False, |g| {
                                g.literal(b"false");
                            });
                        }),
                    ),
                    (
                        CharClass::from_byte(b'n'),
                        Box::new(|g| {
                            g.token(JsoncTok::Null, |g| {
                                g.literal(b"null");
                            });
                        }),
                    ),
                ],
                None,
            );
        });

        g.parser_rule("object", |g| {
            g.node(JsoncNode::Object, |g| {
                g.token(JsoncTok::LBrace, |g| {
                    g.byte(b'{');
                });
                g.optional(|g| {
                    g.call("member");
                    g.zero_or_more(|g| {
                        g.token(JsoncTok::Comma, |g| {
                            g.byte(b',');
                        });
                        g.call("member");
                    });
                });
                g.token(JsoncTok::RBrace, |g| {
                    g.byte(b'}');
                });
            });
        });

        g.parser_rule("member", |g| {
            g.node(JsoncNode::Member, |g| {
                g.call("string");
                g.token(JsoncTok::Colon, |g| {
                    g.byte(b':');
                });
                g.call("value");
            });
        });

        g.parser_rule("array", |g| {
            g.node(JsoncNode::Array, |g| {
                g.token(JsoncTok::LBracket, |g| {
                    g.byte(b'[');
                });
                g.optional(|g| {
                    g.call("value");
                    g.zero_or_more(|g| {
                        g.token(JsoncTok::Comma, |g| {
                            g.byte(b',');
                        });
                        g.call("value");
                    });
                });
                g.token(JsoncTok::RBracket, |g| {
                    g.byte(b']');
                });
            });
        });

        g.lexer_rule("string", |g| {
            g.token(JsoncTok::String, |g| {
                g.byte(b'"');
                g.zero_or_more(|g| {
                    g.neg_lookahead(|g| {
                        g.byte(b'"');
                    });
                    g.choice(
                        |g| {
                            g.call("escape");
                        },
                        |g| {
                            g.class(STRING_CHAR);
                        },
                    );
                });
                g.byte(b'"');
            });
        });

        g.lexer_rule("escape", |g| {
            g.byte(b'\\');
            g.choice(
                |g| {
                    g.class(ESCAPE_SIMPLE);
                },
                |g| {
                    g.byte(b'u');
                    g.repeat(4u32, |g| {
                        g.class(classes::HEX_DIGIT);
                    });
                },
            );
        });

        g.lexer_rule("number", |g| {
            g.token(JsoncTok::Number, |g| {
                g.optional(|g| {
                    g.byte(b'-');
                });
                g.choice(
                    |g| {
                        g.byte(b'0');
                    },
                    |g| {
                        g.byte_range(b'1', b'9');
                        g.zero_or_more(|g| {
                            g.class(classes::DIGIT);
                        });
                    },
                );
                g.optional(|g| {
                    g.byte(b'.');
                    g.one_or_more(|g| {
                        g.class(classes::DIGIT);
                    });
                });
                g.optional(|g| {
                    g.choice(
                        |g| {
                            g.byte(b'e');
                        },
                        |g| {
                            g.byte(b'E');
                        },
                    );
                    g.optional(|g| {
                        g.choice(
                            |g| {
                                g.byte(b'+');
                            },
                            |g| {
                                g.byte(b'-');
                            },
                        );
                    });
                    g.one_or_more(|g| {
                        g.class(classes::DIGIT);
                    });
                });
            });
        });
    }

    let mut g = GrammarBuilder::new();
    g.allow_rule_cycles(true);
    g.set_trivia_rule("ws");
    emit_shared_rules(&mut g);

    g.lexer_rule("ws", |g| {
        g.zero_or_more(|g| {
            g.choice(
                |g| {
                    g.trivia(JsoncTrivia::Ws, |g| {
                        g.one_or_more(|g| {
                            g.class(classes::WHITESPACE);
                        });
                    });
                },
                |g| {
                    g.choice(
                        |g| {
                            g.call("line_comment");
                        },
                        |g| {
                            g.call("block_comment");
                        },
                    );
                },
            );
        });
    });

    g.lexer_rule("line_comment", |g| {
        g.trivia(JsoncTrivia::LineComment, |g| {
            g.literal(b"//");
            g.zero_or_more(|g| {
                g.neg_lookahead(|g| {
                    g.byte(b'\n');
                });
                g.class(CharClass::ANY);
            });
            g.optional(|g| {
                g.byte(b'\n');
            });
        });
    });

    g.lexer_rule("block_comment", |g| {
        g.trivia(JsoncTrivia::BlockComment, |g| {
            g.literal(b"/*");
            g.zero_or_more(|g| {
                g.neg_lookahead(|g| {
                    g.literal(b"*/");
                });
                g.class(CharClass::ANY);
            });
            g.literal(b"*/");
        });
    });

    g.finish().expect("JSONC grammar must be valid")
}

pub fn unicode_tag_like() -> BuiltGraph {
    // Based on the `grammar_tag()` from `sipha/examples/unicode_and_repeat.rs`.
    let mut g = GrammarBuilder::new();

    g.rule("start", |g| {
        g.byte(b'[');
        g.repeat(1..=4u32, |g| {
            g.neg_lookahead(|g| {
                g.byte(b':');
            });
            g.neg_lookahead(|g| {
                g.byte(b']');
            });
            g.any_char();
        });
        g.byte(b':');
        g.call("label");
        g.byte(b']');
        g.end_of_input();
        g.accept();
    });

    g.rule("label", |g| {
        g.call("segment");
        g.zero_or_more(|g| {
            g.byte(b'-');
            g.call("segment");
        });
    });

    g.rule("segment", |g| {
        g.repeat(3..=12u32, |g| {
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

    g.finish().expect("unicode tag grammar must be valid")
}

