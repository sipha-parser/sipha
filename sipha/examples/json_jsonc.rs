//! # JSON and JSONC grammars — with automatic trivia skipping
//!
//! Two closely related grammars built from a shared rule-set, demonstrating
//! the builder's **trivia-skip system**.
//!
//! ## The problem: whitespace everywhere
//!
//! In a naive grammar, every rule that cares about structure must manually
//! skip whitespace between each element:
//!
//! ```rust,ignore
//! // Manual approach — tedious and error-prone:
//! g.rule("object", |g| {
//!     g.byte(b'{');
//!     g.call("ws");     // ← must not forget
//!     g.call("member");
//!     g.call("ws");     // ← must not forget
//!     g.byte(b',');
//!     g.call("ws");     // ← must not forget
//!     ...
//! });
//! ```
//!
//! ## The solution: trivia-skip mode
//!
//! Register the whitespace rule once with [`set_trivia_rule`], then
//! declare structural rules as [`parser_rule`].  Inside parser rules,
//! every [`call`], [`token`], and [`byte_dispatch`] automatically emits
//! a skip before itself:
//!
//! ```rust,ignore
//! g.set_trivia_rule("ws");
//!
//! g.parser_rule("object", |g| {
//!     g.node(NODE_OBJECT, |g| {
//!         g.token(TOK_LBRACE, |g| { g.byte(b'{'); });   // auto: ws + {
//!         g.optional(|g| {
//!             g.call("member");                     // auto: ws + member
//!             g.zero_or_more(|g| {
//!                 g.token(TOK_COMMA, |g| { g.byte(b','); });  // auto: ws + ,
//!                 g.call("member");                 // auto: ws + member
//!             });
//!         });
//!         g.token(TOK_RBRACE, |g| { g.byte(b'}'); });   // auto: ws + }
//!     });
//! });
//! ```
//!
//! Token-matching rules (strings, numbers, the `ws` rule itself) are declared
//! as [`lexer_rule`] — no skipping is injected inside them.
//!
//! ## Rule classification
//!
//! | Rule | Kind | Why |
//! |------|------|-----|
//! | `document`, `value`, `object`, `member`, `array` | [`parser_rule`] | Structural; trivia between elements |
//! | `string`, `number`, `escape` | [`lexer_rule`] | Byte-level; interior must not be interrupted |
//! | `ws`, `line_comment`, `block_comment` | [`lexer_rule`] | *Are* trivia; must not skip themselves |
//!
//! ## JSONC extension
//!
//! JSONC (JSON with Comments) differs from JSON in exactly one rule: `ws`.
//! All structural rules are shared verbatim via [`emit_shared_rules`].
//!
//! - JSON  `ws` ← `[ \t\n\r]*`
//! - JSONC `ws` ← `(spaces | // … | /* … */)*`
//!
//! ## Grammar (EBNF approximation)
//!
//! ```text
//! document   ←  value ws EOF
//! value      ←  string | object | array | number | 'true' | 'false' | 'null'
//! object     ←  '{' (member (',' member)*)? '}'
//! member     ←  string ':' value
//! array      ←  '[' (value (',' value)*)? ']'
//! string     ←  '"' (escape | STRING_CHAR)* '"'
//! number     ←  '-'? ('0' | [1-9][0-9]*) ('.' [0-9]+)? ([eE][+-]?[0-9]+)?
//! escape     ←  '\' (["\\/bfnrt] | 'u' HEX{4})
//!
//! ws (JSON)  ←  [ \t\n\r]*
//! ws (JSONC) ←  ([ \t\n\r]+ | '//' [^\n]* '\n'? | '/*' (!'*/' .)* '*/')*
//! ```
//!
//! [`set_trivia_rule`]: sipha::builder::GrammarBuilder::set_trivia_rule
//! [`parser_rule`]: sipha::builder::GrammarBuilder::parser_rule
//! [`lexer_rule`]: sipha::builder::GrammarBuilder::lexer_rule
//! [`call`]: sipha::builder::GrammarBuilder::call
//! [`token`]: sipha::builder::GrammarBuilder::token
//! [`byte_dispatch`]: sipha::builder::GrammarBuilder::byte_dispatch

use sipha::prelude::*;
use sipha::types::classes;

// ─── SyntaxKind constants ─────────────────────────────────────────────────────

// Inner nodes — structural.
const NODE_DOCUMENT: SyntaxKind = 0;
const NODE_OBJECT: SyntaxKind = 1;
const NODE_MEMBER: SyntaxKind = 2;
const NODE_ARRAY: SyntaxKind = 3;

// Leaf tokens — semantic.
const TOK_STRING: SyntaxKind = 10;
const TOK_NUMBER: SyntaxKind = 11;
const TOK_TRUE: SyntaxKind = 12;
const TOK_FALSE: SyntaxKind = 13;
const TOK_NULL: SyntaxKind = 14;
const TOK_LBRACE: SyntaxKind = 15;
const TOK_RBRACE: SyntaxKind = 16;
const TOK_LBRACKET: SyntaxKind = 17;
const TOK_RBRACKET: SyntaxKind = 18;
const TOK_COLON: SyntaxKind = 19;
const TOK_COMMA: SyntaxKind = 20;

// Leaf tokens — trivia (whitespace and comments).
const TRIVIA_WS: SyntaxKind = 30;
const TRIVIA_LINE_COMMENT: SyntaxKind = 31;
const TRIVIA_BLOCK_COMMENT: SyntaxKind = 32;

// ─── CharClass constants ──────────────────────────────────────────────────────

/// Bytes valid inside a JSON string without escaping.
///
/// RFC 8259: any Unicode character except `"`, `\`, and control chars
/// (U+0000–U+001F).  Bytes 0x80–0xFF (multi-byte UTF-8 units) are allowed.
const STRING_CHAR: CharClass = CharClass::EMPTY
    .with_range(b'\x00', b'\x1F') // control chars — must escape
    .with_byte(b'"') // string terminator
    .with_byte(b'\\') // escape leader
    .complement();

/// Single-character escape codes that follow `\`.
const ESCAPE_SIMPLE: CharClass = CharClass::EMPTY
    .with_byte(b'"')
    .with_byte(b'\\')
    .with_byte(b'/')
    .with_byte(b'b')
    .with_byte(b'f')
    .with_byte(b'n')
    .with_byte(b'r')
    .with_byte(b't');

// ─── Shared structural rules ──────────────────────────────────────────────────

/// Emit all grammar rules **except** `ws` (and its JSONC sub-rules).
///
/// All rules here are [`parser_rule`]s or [`lexer_rule`]s; after
/// [`set_trivia_rule`] is called, `call`/`token`/`byte_dispatch` inside parser
/// rules automatically inject a skip before themselves.
///
/// Forward references to `ws` are resolved by [`finish`].
///
/// [`parser_rule`]: GrammarBuilder::parser_rule
/// [`lexer_rule`]: GrammarBuilder::lexer_rule
/// [`set_trivia_rule`]: GrammarBuilder::set_trivia_rule
/// [`finish`]: GrammarBuilder::finish
fn emit_shared_rules(g: &mut GrammarBuilder) {
    // ── document — parser rule ────────────────────────────────────────────────
    //
    // auto-skip fires before `call("value")`, so leading trivia is consumed.
    // Then an explicit `skip()` handles trailing trivia before EOF.
    g.parser_rule("document", |g| {
        g.node(NODE_DOCUMENT, |g| {
            g.call("value"); // auto: ws → value
            g.skip(); // trailing ws before end-of-input
        });
        g.end_of_input();
        g.accept();
    });

    // ── value — parser rule ───────────────────────────────────────────────────
    //
    // ByteDispatch already receives the auto-skip before the table lookup, so
    // trivia is consumed once and then the byte is peeked.  Arm bodies run
    // without auto-skip (they're inside the dispatch, which has already skipped).
    g.parser_rule("value", |g| {
        g.byte_dispatch(
            vec![
                // auto: ws → dispatch
                (
                    CharClass::EMPTY.with_byte(b'"'),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.call("string");
                    }),
                ),
                (
                    CharClass::EMPTY.with_byte(b'{'),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.call("object");
                    }),
                ),
                (
                    CharClass::EMPTY.with_byte(b'['),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.call("array");
                    }),
                ),
                (
                    classes::DIGIT.with_byte(b'-'),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.call("number");
                    }),
                ),
                (
                    CharClass::EMPTY.with_byte(b't'),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.token(TOK_TRUE, |g| {
                            g.literal(b"true");
                        });
                    }),
                ),
                (
                    CharClass::EMPTY.with_byte(b'f'),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.token(TOK_FALSE, |g| {
                            g.literal(b"false");
                        });
                    }),
                ),
                (
                    CharClass::EMPTY.with_byte(b'n'),
                    Box::new(|g: &mut GrammarBuilder| {
                        g.token(TOK_NULL, |g| {
                            g.literal(b"null");
                        });
                    }),
                ),
            ],
            None,
        );
    });

    // ── object — parser rule ──────────────────────────────────────────────────
    //
    // No manual ws calls — auto-skip fires before every token/call:
    //   token(LBRACE) → ws + {
    //   call("member") → ws + member
    //   token(COMMA)   → ws + ,
    //   token(RBRACE)  → ws + }
    g.parser_rule("object", |g| {
        g.node(NODE_OBJECT, |g| {
            g.token(TOK_LBRACE, |g| {
                g.byte(b'{');
            }); // auto: ws + {
            g.optional(|g| {
                g.call("member"); // auto: ws + member
                g.zero_or_more(|g| {
                    g.token(TOK_COMMA, |g| {
                        g.byte(b',');
                    }); // auto: ws + ,
                    g.call("member"); // auto: ws + member
                });
            });
            g.token(TOK_RBRACE, |g| {
                g.byte(b'}');
            }); // auto: ws + }
        });
    });

    // ── member — parser rule ──────────────────────────────────────────────────
    //
    //   call("string") → ws + string
    //   token(COLON)   → ws + :
    //   call("value")  → ws + value
    //
    // `member` is called from `object`, which is itself a parser rule, so
    // the leading ws before `member` is injected by the call site, not here.
    g.parser_rule("member", |g| {
        g.node(NODE_MEMBER, |g| {
            g.call("string"); // auto: ws + string
            g.token(TOK_COLON, |g| {
                g.byte(b':');
            }); // auto: ws + :
            g.call("value"); // auto: ws + value
        });
    });

    // ── array — parser rule ───────────────────────────────────────────────────
    //
    //   token(LBRACKET) → ws + [
    //   call("value")   → ws + value
    //   token(COMMA)    → ws + ,
    //   token(RBRACKET) → ws + ]
    g.parser_rule("array", |g| {
        g.node(NODE_ARRAY, |g| {
            g.token(TOK_LBRACKET, |g| {
                g.byte(b'[');
            }); // auto: ws + [
            g.optional(|g| {
                g.call("value"); // auto: ws + value
                g.zero_or_more(|g| {
                    g.token(TOK_COMMA, |g| {
                        g.byte(b',');
                    }); // auto: ws + ,
                    g.call("value"); // auto: ws + value
                });
            });
            g.token(TOK_RBRACKET, |g| {
                g.byte(b']');
            }); // auto: ws + ]
        });
    });

    // ── string — lexer rule ───────────────────────────────────────────────────
    //
    // Lexer rule: no auto-skip inside; byte-level pattern must be uninterrupted.
    // `neg_lookahead` + `choice` ensures `\` enters the escape branch.
    g.lexer_rule("string", |g| {
        g.token(TOK_STRING, |g| {
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

    // ── escape — lexer rule ───────────────────────────────────────────────────
    //
    // Called from inside the `string` token body, so its bytes become part
    // of TOK_STRING.  Not wrapped in its own token or node.
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

    // ── number — lexer rule ───────────────────────────────────────────────────
    //
    // `-`? (`0` | [1-9][0-9]*) (`.` [0-9]+)? ([eE][+-]?[0-9]+)?
    g.lexer_rule("number", |g| {
        g.token(TOK_NUMBER, |g| {
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

// ─── JSON grammar ─────────────────────────────────────────────────────────────

/// Build the strict JSON grammar (RFC 8259).
///
/// - Trivia rule: `ws` matching `[ \t\n\r]*`
/// - All structural rules are parser rules; `ws`, `string`, `number`, `escape`
///   are lexer rules.
#[must_use]
pub fn build_json_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();

    // Register ws as the trivia rule.  After this, parser_rule bodies will
    // auto-inject a ws-skip before every call/token/byte_dispatch.
    g.set_trivia_rule("ws");

    emit_shared_rules(&mut g);

    // ws — lexer rule: [ \t\n\r]*
    //
    // Must be a lexer rule: it IS the trivia rule, so it must not skip itself.
    g.lexer_rule("ws", |g| {
        g.trivia(TRIVIA_WS, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.finish().expect("JSON grammar must be valid")
}

// ─── JSONC grammar ────────────────────────────────────────────────────────────

/// Build the JSONC grammar (JSON + `//` line comments + `/* */` block comments).
///
/// - Trivia rule: `ws` matching whitespace runs, line comments, and block comments.
/// - All structural rules are identical to [`build_json_grammar`].
/// - `ws`, `line_comment`, `block_comment` are lexer rules.
#[must_use]
pub fn build_jsonc_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();

    g.set_trivia_rule("ws");

    emit_shared_rules(&mut g);

    // ws — lexer rule: (spaces+ | line_comment | block_comment)*
    //
    // Each whitespace run and each comment produces its own trivia token,
    // letting tooling (formatters, linters) inspect them individually.
    g.lexer_rule("ws", |g| {
        g.zero_or_more(|g| {
            g.choice(
                |g| {
                    g.trivia(TRIVIA_WS, |g| {
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

    // line_comment — lexer rule: '//' [^\n]* '\n'?
    g.lexer_rule("line_comment", |g| {
        g.trivia(TRIVIA_LINE_COMMENT, |g| {
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

    // block_comment — lexer rule: '/*' (!'*/' .)* '*/'
    g.lexer_rule("block_comment", |g| {
        g.trivia(TRIVIA_BLOCK_COMMENT, |g| {
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

// ─── Display helpers ──────────────────────────────────────────────────────────

const fn kind_name(k: SyntaxKind) -> &'static str {
    match k {
        NODE_DOCUMENT => "Document",
        NODE_OBJECT => "Object",
        NODE_MEMBER => "Member",
        NODE_ARRAY => "Array",
        TOK_STRING => "String",
        TOK_NUMBER => "Number",
        TOK_TRUE => "True",
        TOK_FALSE => "False",
        TOK_NULL => "Null",
        TOK_LBRACE => "LBrace",
        TOK_RBRACE => "RBrace",
        TOK_LBRACKET => "LBracket",
        TOK_RBRACKET => "RBracket",
        TOK_COLON => "Colon",
        TOK_COMMA => "Comma",
        TRIVIA_WS => "Whitespace",
        TRIVIA_LINE_COMMENT => "LineComment",
        TRIVIA_BLOCK_COMMENT => "BlockComment",
        _ => "?",
    }
}

fn preview(s: &str) -> String {
    let s = s.replace('\n', "↵").replace('\r', "");
    if s.len() <= 24 {
        s
    } else {
        format!("{}…", &s[..21])
    }
}

fn print_tree(node: &SyntaxNode, indent: usize) {
    let r = node.text_range();
    println!(
        "{}{} ({}..{})",
        "  ".repeat(indent),
        kind_name(node.kind()),
        r.start,
        r.end
    );
    for child in node.children() {
        match child {
            SyntaxElement::Node(n) => print_tree(&n, indent + 1),
            SyntaxElement::Token(t) => {
                let r = t.text_range();
                let glyph = if t.is_trivia() { "~" } else { "·" };
                println!(
                    "{}  {} {} {:?} ({}..{})",
                    "  ".repeat(indent),
                    glyph,
                    kind_name(t.kind()),
                    preview(t.text()),
                    r.start,
                    r.end
                );
            }
        }
    }
}

// ─── Tree-walking helpers ─────────────────────────────────────────────────────

fn string_content(tok: &SyntaxToken) -> &str {
    let t = tok.text();
    if t.len() >= 2 && t.starts_with('"') && t.ends_with('"') {
        &t[1..t.len() - 1]
    } else {
        t
    }
}

fn object_members(obj: &SyntaxNode) -> Vec<(String, String)> {
    obj.find_all_nodes(NODE_MEMBER)
        .into_iter()
        .map(|member| {
            let sem: Vec<SyntaxToken> = member.non_trivia_tokens().collect();
            let key = sem
                .first()
                .map(|t| string_content(t).to_string())
                .unwrap_or_default();
            let scalar_value = sem.get(2).map(|t| t.text().to_string());
            let node_value = member
                .child_nodes()
                .last()
                .map(|n| format!("<{}>", kind_name(n.kind())));
            (key, node_value.or(scalar_value).unwrap_or_default())
        })
        .collect()
}

// ─── Test helpers ─────────────────────────────────────────────────────────────

fn check(pass: &mut usize, fail: &mut usize, ok: bool, label: &str) {
    if ok {
        *pass += 1;
        println!("  [✓] {label}");
    } else {
        *fail += 1;
        println!("  [✗] {label}");
    }
}

// ─── Main ─────────────────────────────────────────────────────────────────────

fn main() {
    let json_bg = build_json_grammar();
    let jsonc_bg = build_jsonc_grammar();
    let json_g = json_bg.as_graph();
    let jsonc_g = jsonc_bg.as_graph();
    let mut engine = Engine::new();

    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut c = |ok: bool, label: &str| check(&mut pass, &mut fail, ok, label);

    // ── 1. Show what auto-skip removes from the grammar definition ────────────

    println!("══ 1. Grammar size comparison ═══════════════════════════");
    println!();
    println!("   Without auto-skip (manual ws calls), the JSON grammar");
    println!("   needs a g.call(\"ws\") between every structural element:");
    println!();
    println!("     object: {{ + ws + member + ws + , + ws + member + ... + ws + }}");
    println!("     member: string + ws + : + ws + value");
    println!("     array:  [ + ws + value + ws + , + ws + value + ... + ws + ]");
    println!("     root:   ws + value + ws + EOF");
    println!();
    println!("   With parser_rule + set_trivia_rule, those ws calls vanish.");
    println!("   The grammar now reads exactly like its EBNF spec.");
    println!();

    // Count ws-related instructions in the built grammar.
    // (Each skip() emits a Call instruction to the ws rule.)
    let total_insns = json_bg.insns.len();
    println!("   Total instructions in JSON grammar: {total_insns}");
    println!();

    // ── 2. Parse clean JSON — verify tree is identical ────────────────────────

    let src_json: &[u8] = br#"{
  "name": "Alice",
  "scores": [10, 20, 30],
  "active": true
}"#;

    println!("══ 2. JSON tree ════════════════════════════════════════");
    println!("   Input: {}", std::str::from_utf8(src_json).unwrap());
    println!();

    let out = engine.parse(&json_g, src_json).expect("valid JSON");
    let root = out.syntax_root(src_json).expect("tree");
    print_tree(&root, 0);

    println!();
    let doc_members = root
        .find_node(NODE_OBJECT)
        .map(|o| object_members(&o))
        .unwrap_or_default();
    println!("   Members:");
    for (k, v) in &doc_members {
        println!("     {k:10} → {v}");
    }

    c(doc_members.len() == 3, "JSON object has 3 members");
    c(
        doc_members[0] == ("name".into(), "\"Alice\"".into()),
        "member[0]: name = \"Alice\"",
    );
    c(
        doc_members[1] == ("scores".into(), "<Array>".into()),
        "member[1]: scores = <Array>",
    );
    c(
        doc_members[2] == ("active".into(), "true".into()),
        "member[2]: active = true",
    );

    let trivia: Vec<_> = root
        .descendant_tokens()
        .into_iter()
        .filter(sipha::red::SyntaxToken::is_trivia)
        .collect();
    c(!trivia.is_empty(), "whitespace trivia tokens are present");
    println!("   Trivia tokens: {}", trivia.len());

    println!();

    // ── 3. Parse JSONC with line and block comments ───────────────────────────

    let src_jsonc: &[u8] = br#"{
  // Who this record belongs to.
  "name": "Alice",

  /* Scores from the last three rounds.
     Higher is better. */
  "scores": [10, 20, 30],

  "active": true  // currently enrolled
}"#;

    println!("══ 3. JSONC tree (with comments) ═══════════════════════");
    println!();

    let out_c = engine.parse(&jsonc_g, src_jsonc).expect("valid JSONC");
    let root_c = out_c.syntax_root(src_jsonc).expect("tree");
    print_tree(&root_c, 0);

    println!();

    let all_trivia: Vec<SyntaxToken> = root_c
        .descendant_tokens()
        .into_iter()
        .filter(sipha::red::SyntaxToken::is_trivia)
        .collect();
    let ws_count = all_trivia.iter().filter(|t| t.kind() == TRIVIA_WS).count();
    let line_count = all_trivia
        .iter()
        .filter(|t| t.kind() == TRIVIA_LINE_COMMENT)
        .count();
    let block_count = all_trivia
        .iter()
        .filter(|t| t.kind() == TRIVIA_BLOCK_COMMENT)
        .count();

    println!("   Trivia breakdown:");
    println!("     {ws_count:2} whitespace runs");
    println!("     {line_count:2} line comments");
    println!("     {block_count:2} block comments");
    println!();

    c(line_count == 2, "2 line comments");
    c(block_count == 1, "1 block comment");

    let block = all_trivia
        .iter()
        .find(|t| t.kind() == TRIVIA_BLOCK_COMMENT)
        .unwrap();
    c(
        block.text().starts_with("/*") && block.text().ends_with("*/"),
        "block comment delimiters correct",
    );
    c(
        block.text().contains("Higher is better"),
        "block comment body preserved verbatim",
    );

    let members_c = root_c
        .find_node(NODE_OBJECT)
        .map(|o| object_members(&o))
        .unwrap_or_default();
    c(
        members_c == doc_members,
        "JSONC semantic members match JSON semantic members",
    );

    println!();

    // ── 4. Grammar boundary ───────────────────────────────────────────────────

    println!("══ 4. Grammar boundary ══════════════════════════════════");

    let plain = b"{ \"x\": 1 }";
    let with_line_comment = b"{ \"x\": 1 } // trailing";
    let with_block_comment = b"{ /* preamble */ \"x\": 1 }";

    c(
        engine.parse(&json_g, plain).is_ok(),
        "plain JSON: accepted by JSON grammar",
    );
    c(
        engine.parse(&jsonc_g, plain).is_ok(),
        "plain JSON: accepted by JSONC grammar",
    );
    c(
        engine.parse(&json_g, with_line_comment).is_err(),
        "// comment: rejected by JSON grammar",
    );
    c(
        engine.parse(&jsonc_g, with_line_comment).is_ok(),
        "// comment: accepted by JSONC grammar",
    );
    c(
        engine.parse(&json_g, with_block_comment).is_err(),
        "/* */ comment: rejected by JSON grammar",
    );
    c(
        engine.parse(&jsonc_g, with_block_comment).is_ok(),
        "/* */ comment: accepted by JSONC grammar",
    );

    println!();

    // ── 5. no_skip demo: tight operator sequences ─────────────────────────────
    //
    // Demonstrate that inside a parser_rule you can suppress auto-skip for
    // regions that must not contain trivia.  We use the JSON number rule as a
    // proxy: it's a lexer rule so auto-skip is off inside it by default, but
    // if it were a parser rule `no_skip` would be how you protect the interior.

    println!("══ 5. no_skip: protected regions ════════════════════════");
    println!("   Inside a parser_rule, wrap tight byte sequences in no_skip()");
    println!("   to prevent trivia injection between their components.");
    println!("   Example use-cases: '..' range operators, '::' paths, '--' decrements.");
    println!();

    // We can verify it works by building a tiny ad-hoc grammar:
    {
        let mut g2 = GrammarBuilder::new();
        g2.set_trivia_rule("ws");

        // A parser rule where `::` must be tight (no ws between the two colons).
        g2.parser_rule("path", |g| {
            g.node(1, |g| {
                g.token(10, |g| {
                    // ident: auto ws + ident
                    g.one_or_more(|g| {
                        g.class(classes::IDENT_CONT);
                    });
                });
                g.no_skip(|g| {
                    // protect "::": no ws injection inside
                    g.token(11, |g| {
                        g.literal(b"::");
                    });
                });
                g.token(10, |g| {
                    // ident: auto ws + ident
                    g.one_or_more(|g| {
                        g.class(classes::IDENT_CONT);
                    });
                });
            });
            g.skip();
            g.end_of_input();
            g.accept();
        });

        g2.lexer_rule("ws", |g| {
            g.trivia(30, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
        });

        let bg2 = g2.finish().unwrap();
        let gr2 = bg2.as_graph();

        c(
            engine.parse(&gr2, b"foo::bar").is_ok(),
            "foo::bar     accepted",
        );
        c(
            engine.parse(&gr2, b"  foo :: bar").is_err(),
            "foo :: bar   rejected (space in ::)",
        );
        c(
            engine.parse(&gr2, b"  foo::bar  ").is_ok(),
            "leading/trailing ws accepted (skip() around path)",
        );
    }

    println!();

    // ── 6. collect_text — tree is self-contained after source is dropped ──────

    println!("══ 6. collect_text (self-contained tree) ════════════════");
    {
        let src_owned = br#"{"key": "value"}"#.to_vec();
        let out_owned = engine.parse(&json_g, &src_owned).unwrap();
        let root_owned = out_owned.syntax_root(&src_owned).unwrap();
        drop(src_owned);

        let reconstructed = root_owned.collect_text();
        c(
            reconstructed == "{\"key\": \"value\"}",
            "collect_text reconstructs original after source is dropped",
        );
        println!("   Reconstructed: {reconstructed:?}");
    }
    println!();

    // ── 7. Correctness table ──────────────────────────────────────────────────

    println!("══ 7. Correctness table ══════════════════════════════════");
    println!("   {:45}  {:5}  {:5}", "input", "JSON", "JSONC");
    println!("   {}", "─".repeat(60));

    type Row<'a> = (&'a [u8], bool, bool);
    let cases: &[Row] = &[
        (b"null", true, true),
        (b"true", true, true),
        (b"false", true, true),
        (b"42", true, true),
        (b"-3.14e+10", true, true),
        (b"\"hello\"", true, true),
        (b"\"esc \\n \\u0041\"", true, true),
        (b"[]", true, true),
        (b"[1, 2, 3]", true, true),
        (b"{}", true, true),
        (b"{\"a\": 1, \"b\": [true, null]}", true, true),
        (b"  [ 1, {\"k\": false} ]  ", true, true),
        // JSONC extensions
        (b"1 // end", false, true),
        (b"// comment\n42", false, true),
        (b"[1, // item\n 2]", false, true),
        (b"{\"a\":/*b*/1}", false, true),
        (b"/* lead */true/* trail */", false, true),
        // Invalid in both
        (b"", false, false),
        (b"invalid", false, false),
        (b"{\"x\": }", false, false),
        (b"[1, 2,]", false, false),
        (b"{\"k\": 1 \"j\": 2}", false, false),
    ];

    let mut table_pass = 0;
    let mut table_fail = 0;
    for &(input, json_ok, jsonc_ok) in cases {
        let got_json = engine.parse(&json_g, input).is_ok();
        let got_jsonc = engine.parse(&jsonc_g, input).is_ok();
        let ok = got_json == json_ok && got_jsonc == jsonc_ok;
        let mark = if ok { "✓" } else { "✗" };
        let label = std::str::from_utf8(input).unwrap_or("<binary>");
        println!(
            "   [{mark}] {:<45}  {:5}  {:5}",
            if label.len() > 44 {
                &label[..44]
            } else {
                label
            },
            if got_json { "✓" } else { "✗" },
            if got_jsonc { "✓" } else { "✗" }
        );
        if ok {
            table_pass += 1;
        } else {
            table_fail += 1;
        }
    }

    if table_fail == 0 {
        c(true, &format!("all {table_pass} table cases correct"));
    } else {
        c(false, &format!("{table_fail} table cases incorrect"));
    }

    // ── Summary ───────────────────────────────────────────────────────────────
    println!();
    println!(
        "══ Results: {pass} passed, {fail} failed out of {}. ═════════",
        pass + fail
    );
    if fail > 0 {
        std::process::exit(1);
    }
}
