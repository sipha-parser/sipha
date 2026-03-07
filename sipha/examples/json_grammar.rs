//! Demonstrates all four parser extensions on a JSON grammar:
//!
//! 1. SIMD literal matching (transparent, used for "true"/"false"/"null")
//! 2. Structured error diagnostics (`ParseError::NoMatch` with expected tokens)
//! 3. Packrat memoisation (`Engine::with_memo()`)
//! 4. O(1) byte dispatch (`byte_dispatch` on JSON value rule)

use sipha::prelude::*;
use sipha::types::classes;

// ─── Tag constants ────────────────────────────────────────────────────────────

const T_VALUE:  Tag = 0;
const T_OBJECT: Tag = 1;
const T_ARRAY:  Tag = 2;
const T_STRING: Tag = 3;
const T_NUMBER: Tag = 4;
const T_BOOL:   Tag = 5;
const T_NULL:   Tag = 6;
const T_MEMBER: Tag = 7;

// ─── Build the grammar ───────────────────────────────────────────────────────

fn build_json_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();

    // ── json (root) ──────────────────────────────────────────────────────────
    g.begin_rule("json");
    g.call("ws");
    g.call("value");
    g.call("ws");
    g.end_of_input();
    g.accept();

    // ── value — uses ByteDispatch for O(1) first-byte dispatch ───────────────
    //
    //   Instead of a chain of 7 Choice/Commit pairs (O(7) on the last branch),
    //   we emit one ByteDispatch table that jumps directly to the right arm.
    g.begin_rule("value");
    g.capture(T_VALUE, |g| {
        // Class sets for the dispatch table.
        let class_quote  = CharClass::EMPTY.with_byte(b'"');
        let class_lbrace = CharClass::EMPTY.with_byte(b'{');
        let class_lbrack = CharClass::EMPTY.with_byte(b'[');
        let class_number = classes::DIGIT.with_byte(b'-');
        let class_t      = CharClass::EMPTY.with_byte(b't');
        let class_f      = CharClass::EMPTY.with_byte(b'f');
        let class_n      = CharClass::EMPTY.with_byte(b'n');

        g.byte_dispatch(vec![
            // ── string ──────────────────────────────────────────────
            (class_quote,  Box::new(|g: &mut GrammarBuilder| { g.call("string"); })),
            // ── object ──────────────────────────────────────────────
            (class_lbrace, Box::new(|g: &mut GrammarBuilder| { g.call("object"); })),
            // ── array ───────────────────────────────────────────────
            (class_lbrack, Box::new(|g: &mut GrammarBuilder| { g.call("array");  })),
            // ── number ──────────────────────────────────────────────
            (class_number, Box::new(|g: &mut GrammarBuilder| { g.call("number"); })),
            // ── true ────────────────────────────────────────────────
            (class_t,      Box::new(|g: &mut GrammarBuilder| {
                g.capture(T_BOOL, |g| { g.literal(b"true"); });
            })),
            // ── false ───────────────────────────────────────────────
            (class_f,      Box::new(|g: &mut GrammarBuilder| {
                g.capture(T_BOOL, |g| { g.literal(b"false"); });
            })),
            // ── null ────────────────────────────────────────────────
            (class_n,      Box::new(|g: &mut GrammarBuilder| {
                g.capture(T_NULL, |g| { g.literal(b"null"); });
            })),
        ], None);
    });
    g.end_rule();

    // ── object ───────────────────────────────────────────────────────────────
    g.begin_rule("object");
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
    g.end_rule();

    // ── member ───────────────────────────────────────────────────────────────
    g.begin_rule("member");
    g.capture(T_MEMBER, |g| {
        g.call("string");
        g.call("ws");
        g.byte(b':');
        g.call("ws");
        g.call("value");
    });
    g.end_rule();

    // ── array ────────────────────────────────────────────────────────────────
    g.begin_rule("array");
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
    g.end_rule();

    // ── string ───────────────────────────────────────────────────────────────
    g.begin_rule("string");
    g.capture(T_STRING, |g| {
        g.byte(b'"');
        g.zero_or_more(|g| {
            g.neg_lookahead(|g| { g.byte(b'"'); });
            g.choice(
                |g| { g.call("escape"); },
                |g| { g.class(CharClass::ANY); },
            );
        });
        g.byte(b'"');
    });
    g.end_rule();

    // ── escape ────────────────────────────────────────────────────────────────
    g.begin_rule("escape");
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
            g.class(classes::HEX_DIGIT);
            g.class(classes::HEX_DIGIT);
            g.class(classes::HEX_DIGIT);
            g.class(classes::HEX_DIGIT);
        },
    );
    g.end_rule();

    // ── number ────────────────────────────────────────────────────────────────
    g.begin_rule("number");
    g.capture(T_NUMBER, |g| {
        g.optional(|g| { g.byte(b'-'); });
        g.choice(
            |g| { g.byte(b'0'); },
            |g| {
                g.byte_range(b'1', b'9');
                g.zero_or_more(|g| { g.class(classes::DIGIT); });
            },
        );
        g.optional(|g| {
            g.byte(b'.');
            g.one_or_more(|g| { g.class(classes::DIGIT); });
        });
        g.optional(|g| {
            g.choice(|g| { g.byte(b'e'); }, |g| { g.byte(b'E'); });
            g.optional(|g| {
                g.choice(|g| { g.byte(b'+'); }, |g| { g.byte(b'-'); });
            });
            g.one_or_more(|g| { g.class(classes::DIGIT); });
        });
    });
    g.end_rule();

    // ── ws ────────────────────────────────────────────────────────────────────
    g.begin_rule("ws");
    g.zero_or_more(|g| { g.class(classes::WHITESPACE); });
    g.end_rule();

    g.finish().expect("grammar must be valid")
}

// ─── Main ────────────────────────────────────────────────────────────────────

fn main() {
    let graph_data = build_json_grammar();
    let graph      = graph_data.as_graph();

    println!("=== Extension 1: SIMD literal matching ===");
    println!("  Transparent — Insn::Literal uses SSE2/AVX2 on x86_64.");
    println!("  Literals like \"true\", \"false\", \"null\" are compared directly.");
    println!();

    // ── Extension 2: Structured error diagnostics ─────────────────────────────
    println!("=== Extension 2: Structured error diagnostics ===");
    {
        let mut engine = Engine::new();
        let inputs: &[(&[u8], &str)] = &[
            (b"",         "empty input"),
            (b"invalid",  "unknown token"),
            (b"{\"x\": }", "missing value after colon"),
            (b"[1, 2, ]", "trailing comma"),
        ];
        for (input, desc) in inputs {
            if let Err(ParseError::NoMatch(diag)) = engine.parse(&graph, input) {
                // Show the rich error from ErrorContext
                let ctx = engine.error_context();
                println!(
                    "  [{desc}] byte {}: expected {} token(s) → {:?}",
                    diag.furthest,
                    ctx.expected.len(),
                    ctx.expected
                        .iter()
                        .map(|e| e.display(Some(&graph.literals), Some(graph.rule_names), Some(graph.expected_labels)))
                        .collect::<Vec<_>>()
                );
            }
        }
    }
    println!();

    // ── Extension 3: Packrat memoisation ─────────────────────────────────────
    println!("=== Extension 3: Packrat memoisation ===");
    {
        let json = b"{ \"name\": \"Alice\", \"scores\": [10, 20, 30], \"active\": true }";

        // Parse WITHOUT memo.
        let mut plain = Engine::new();
        let _ok = plain.parse(&graph, json).expect("valid JSON");

        // Parse WITH memo.
        let mut memo_engine = Engine::new().with_memo();
        let out = memo_engine.parse(&graph, json).expect("valid JSON");
        println!(
            "  Consumed {} bytes. Memo entries cached: {}",
            out.consumed,
            memo_engine.memo_len().unwrap()
        );

        let tree = CaptureNode::build_forest(&out.events);
        println!(
            "  Top-level captures: {}  (tag={})",
            tree.len(),
            tree[0].tag
        );
    }
    println!();

    // ── Extension 4: O(1) byte dispatch ──────────────────────────────────────
    println!("=== Extension 4: O(1) ByteDispatch ===");
    {
        println!(
            "  Grammar has {} jump table(s).",
            graph_data.jump_tables.len()
        );
        // Verify the table dispatches correctly for each first byte.
        let table = &graph_data.jump_tables[0];
        let byte_targets: Vec<(char, u32)> = b"\"{[tfn0-"
            .iter()
            .map(|&b| (b as char, table[b as usize]))
            .collect();
        for (ch, target) in &byte_targets {
            println!("    '{ch}' → insn {target}");
        }
    }
    println!();

    // ── Full correctness suite ────────────────────────────────────────────────
    println!("=== Correctness suite ===");
    let mut engine = Engine::new().with_memo();
    let cases: &[(&[u8], bool)] = &[
        (b"null",                             true),
        (b"true",                             true),
        (b"false",                            true),
        (b"42",                               true),
        (b"-3.14e+10",                        true),
        (b"\"hello\"",                        true),
        (b"\"escape \\n \\u0041\"",           true),
        (b"[]",                               true),
        (b"[1, 2, 3]",                        true),
        (b"{}",                               true),
        (b"{\"a\": 1, \"b\": [true, null]}",  true),
        (b"  [ 1 , { \"k\" : false } ]  ",    true),
        (b"",                                 false),
        (b"invalid",                          false),
        (b"{\"x\": }",                        false),
    ];
    let mut passed = 0;
    for &(input, expect_ok) in cases {
        let ok = engine.parse(&graph, input).is_ok();
        let check = ok == expect_ok;
        println!("  [{}] {:?}", if check { "✓" } else { "✗" },
                 std::str::from_utf8(input).unwrap_or("<binary>"));
        if check { passed += 1; }
    }
    println!("\n  {passed}/{} passed.", cases.len());

    // ── Codegen preview ───────────────────────────────────────────────────────
    println!("\n=== Codegen preview (first 50 lines) ===");
    let mut src = String::new();
    sipha::codegen::emit_rust(&graph_data, &mut src).unwrap();
    for line in src.lines().take(50) {
        println!("  {line}");
    }
    println!("  ... ({} total lines)", src.lines().count());
}
