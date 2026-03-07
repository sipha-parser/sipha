//! # Context-flags example (multi-word edition)
//!
//! Demonstrates the flag system with flags spread across **multiple words**:
//!
//! - Flags 0..63  live in word 0  (conventional range)
//! - Flag  64     lives in word 1  (second word)
//! - Flag  128    lives in word 2  (third word)
//!
//! The grammar is a small imperative language:
//!
//! ```text
//! program    <- (item ws)*  !.
//! item       <- fn_decl / stmt
//! fn_decl    <- 'fn' ws1 ident '(' ')' ws PushFlags(+IN_FN, -IN_LOOP) block PopFlags
//! stmt       <- loop_stmt / fn_decl / return_stmt / break_stmt / cont_stmt / expr_stmt
//! loop_stmt  <- 'loop' ws PushFlags(+IN_LOOP) block PopFlags
//! return_stmt<- IfFlag(IN_FN) 'return' (ws1 expr)? ws ';'
//! break_stmt <- IfFlag(IN_LOOP) 'break' ws ';'
//! cont_stmt  <- IfFlag(IN_LOOP) 'continue' ws ';'
//! ```
//!
//! Some flags are intentionally placed in different words to verify that the
//! multi-word snapshot+restore mechanism works across word boundaries.

use sipha::prelude::*;
use sipha::types::classes;

// ─── Flag constants ───────────────────────────────────────────────────────────
// Spread deliberately across multiple 64-bit words.

const FLAG_IN_LOOP:   FlagId = 0;   // word 0, bit 0
const FLAG_IN_FN:     FlagId = 1;   // word 0, bit 1
const FLAG_STRICT:    FlagId = 64;  // word 1, bit 0  ← second word
const FLAG_ASYNC:     FlagId = 128; // word 2, bit 0  ← third word

// ─── Tags ─────────────────────────────────────────────────────────────────────

const T_PROGRAM:  Tag = 0;
const T_FN_DECL:  Tag = 1;
const T_LOOP:     Tag = 2;
const T_RETURN:   Tag = 3;
const T_BREAK:    Tag = 4;
const T_CONTINUE: Tag = 5;
const T_EXPR:     Tag = 6;

// ─── Grammar ──────────────────────────────────────────────────────────────────

fn build_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();

    // ── program
    g.begin_rule("program");
    g.capture(T_PROGRAM, |g| {
        g.call("ws");
        g.zero_or_more(|g| { g.call("item"); g.call("ws"); });
        g.end_of_input();
    });
    g.accept();

    // ── item <- fn_decl / stmt
    g.begin_rule("item");
    g.choice(|g| { g.call("fn_decl"); }, |g| { g.call("stmt"); });
    g.end_rule();

    // ── fn_decl
    //    Sets IN_FN (word 0), clears IN_LOOP (word 0) — same word, one snapshot entry.
    g.begin_rule("fn_decl");
    g.capture(T_FN_DECL, |g| {
        g.literal(b"fn");
        g.call("ws1");
        g.call("ident_raw");
        g.call("ws");
        g.byte(b'('); g.call("ws"); g.byte(b')');
        g.call("ws");
        g.byte(b'{'); g.call("ws");
        g.with_flags(
            &[FLAG_IN_FN],    // set: IN_FN
            &[FLAG_IN_LOOP],  // clear: IN_LOOP (nested fn resets loop ctx)
            |g| {
                g.zero_or_more(|g| { g.call("stmt"); g.call("ws"); });
            },
        );
        g.byte(b'}');
    });
    g.end_rule();

    // ── stmt
    g.begin_rule("stmt");
    g.choice(
        |g| { g.call("loop_stmt");   },
        |g| g.choice(
            |g| { g.call("fn_decl");     },   // nested fn allowed anywhere
            |g| g.choice(
                |g| { g.call("return_stmt"); },
                |g| g.choice(
                    |g| { g.call("break_stmt");  },
                    |g| g.choice(
                        |g| { g.call("cont_stmt");   },
                        |g| { g.call("expr_stmt");   },
                    ),
                ),
            ),
        ),
    );
    g.end_rule();

    // ── loop_stmt
    //    Sets IN_LOOP (word 0 bit 0) — single snapshot entry.
    g.begin_rule("loop_stmt");
    g.capture(T_LOOP, |g| {
        g.literal(b"loop");
        g.call("ws");
        g.byte(b'{'); g.call("ws");
        g.with_flags(
            &[FLAG_IN_LOOP],  // set
            &[],              // clear nothing
            |g| {
                g.zero_or_more(|g| { g.call("stmt"); g.call("ws"); });
            },
        );
        g.byte(b'}');
    });
    g.end_rule();

    // ── return_stmt — only valid in a function
    g.begin_rule("return_stmt");
    g.if_flag(FLAG_IN_FN);
    g.capture(T_RETURN, |g| {
        g.literal(b"return");
        g.optional(|g| { g.call("ws1"); g.call("expr"); });
        g.call("ws"); g.byte(b';');
    });
    g.end_rule();

    // ── break_stmt — only valid in a loop
    g.begin_rule("break_stmt");
    g.if_flag(FLAG_IN_LOOP);
    g.capture(T_BREAK, |g| {
        g.literal(b"break");
        g.call("ws"); g.byte(b';');
    });
    g.end_rule();

    // ── cont_stmt — only valid in a loop
    g.begin_rule("cont_stmt");
    g.if_flag(FLAG_IN_LOOP);
    g.capture(T_CONTINUE, |g| {
        g.literal(b"continue");
        g.call("ws"); g.byte(b';');
    });
    g.end_rule();

    // ── expr_stmt
    g.begin_rule("expr_stmt");
    g.call("expr"); g.call("ws"); g.byte(b';');
    g.end_rule();

    // ── expr <- integer / ident
    g.begin_rule("expr");
    g.capture(T_EXPR, |g| {
        g.choice(
            |g| { g.one_or_more(|g| { g.class(classes::DIGIT); }); },
            |g| { g.call("ident_raw"); },
        );
    });
    g.end_rule();

    // ── ident_raw (keyword-guarded)
    g.begin_rule("ident_raw");
    for kw in &[b"loop" as &[u8], b"fn", b"return", b"break", b"continue"] {
        let kw = *kw;
        g.neg_lookahead(move |g| { g.literal(kw); g.call("non_ident"); });
    }
    g.class(classes::IDENT_START);
    g.zero_or_more(|g| { g.class(classes::IDENT_CONT); });
    g.end_rule();

    // ── non_ident — word boundary: succeed only when next char is not ident-continuing
    g.begin_rule("non_ident");
    g.neg_lookahead(|g| { g.class(classes::IDENT_CONT); });
    g.end_rule();

    // ── ws1 / ws
    g.begin_rule("ws1");
    g.one_or_more(|g| { g.class(classes::WHITESPACE); });
    g.end_rule();
    g.begin_rule("ws");
    g.zero_or_more(|g| { g.class(classes::WHITESPACE); });
    g.end_rule();

    g.finish().expect("grammar valid")
}

// ─── Test runner ─────────────────────────────────────────────────────────────

fn check(engine: &mut Engine, graph: &sipha::insn::ParseGraph,
         src: &str, ctx: &ParseContext, expect_ok: bool, desc: &str) -> bool
{
    let result = engine.parse_with_context(graph, src.as_bytes(), ctx);
    let ok = result.is_ok();
    let correct = ok == expect_ok;
    let sym = if correct { "✓" } else { "✗" };
    if ok {
        let out = result.unwrap();
        println!("  [{sym}] {desc} → OK ({} bytes consumed)", out.consumed);
    } else {
        let ectx = engine.error_context();
        let exp: Vec<String> = ectx.expected.iter()
            .map(|e| e.display(Some(&graph.literals), Some(&graph.rule_names), Some(&graph.expected_labels))).collect();
        println!("  [{sym}] {desc} → ERR at byte {}: {exp:?}", ectx.furthest);
    }
    correct
}

fn main() {
    let graph_data = build_grammar();
    let graph      = graph_data.as_graph();
    let mut engine = Engine::new();

    println!("=== Flag-mask table (sparse word entries) ===");
    for (i, e) in graph_data.flag_mask_data.iter().enumerate() {
        println!("  entry[{i}]: word={} set={:#066b} clear={:#066b}",
                 e.word, e.set_bits, e.clear_bits);
    }
    println!("  {} mask(s) interned, {} total word entries",
             graph_data.flag_mask_offsets.len() - 1,
             graph_data.flag_mask_data.len());
    println!();

    // ── Default context (no flags set) ────────────────────────────────────────
    let no_ctx = ParseContext::new();

    // ── Strict-mode context (FLAG_STRICT in word 1) ───────────────────────────
    // Demonstrates that the engine auto-grows the flag bank for higher words.
    let strict_ctx = ParseContext::new().with_set(FLAG_STRICT);
    assert_eq!(strict_ctx.num_words(), 2,
        "FLAG_STRICT=64 lives in word 1 → 2 words needed");

    // ── Async context (FLAG_ASYNC in word 2) ─────────────────────────────────
    let async_ctx = ParseContext::new().with_set(FLAG_ASYNC);
    assert_eq!(async_ctx.num_words(), 3,
        "FLAG_ASYNC=128 lives in word 2 → 3 words needed");

    println!("=== Correctness suite ===");
    let mut pass = 0; let mut fail = 0;
    let mut c = |src, ctx: &ParseContext, ok, desc| {
        if check(&mut engine, &graph, src, ctx, ok, desc) { pass += 1 } else { fail += 1 }
    };

    // Valid programs
    c("42;",                                    &no_ctx, true,  "expression statement");
    c("loop { break; }",                        &no_ctx, true,  "break inside loop");
    c("loop { continue; }",                     &no_ctx, true,  "continue inside loop");
    c("fn foo() { return 1; }",                 &no_ctx, true,  "return inside fn");
    c("fn foo() { loop { break; } return; }",   &no_ctx, true,  "break in loop, return in fn");
    c("loop { loop { break; } break; }",        &no_ctx, true,  "nested loops");
    c("fn foo() { loop { return 0; } }",        &no_ctx, true,  "return in loop inside fn");
    c("loop { fn inner() { 0; } break; }",      &no_ctx, true,  "IN_LOOP restored after inner fn");

    // Context violations
    c("break;",                                 &no_ctx, false, "INVALID: break outside loop");
    c("continue;",                              &no_ctx, false, "INVALID: continue outside loop");
    c("return 0;",                              &no_ctx, false, "INVALID: return outside fn");
    c("fn foo() { break; }",                    &no_ctx, false, "INVALID: break in fn not loop");

    // Caller-supplied context (IN_FN pre-set from outside the grammar)
    let in_fn_ctx = ParseContext::new().with_set(FLAG_IN_FN);
    c("return 42;", &in_fn_ctx, true,  "return with caller-supplied IN_FN context");
    c("return 42;", &no_ctx,    false, "return without IN_FN context");

    // Multi-word: FLAG_STRICT and FLAG_ASYNC are in words 1 and 2.
    // The grammar doesn't currently USE these flags, but the engine must
    // preserve them across PushFlags/PopFlags.
    let complex_ctx = ParseContext::new()
        .with_set(FLAG_STRICT)   // word 1
        .with_set(FLAG_ASYNC);   // word 2
    c("fn foo() { loop { break; } return; }", &complex_ctx, true,
        "multi-word context (STRICT+ASYNC) preserved through parse");

    // Verify that after the parse with complex_ctx, words 1 and 2 are still set.
    let _ = engine.parse_with_context(&graph, b"42;", &complex_ctx);
    assert!(complex_ctx.has(FLAG_STRICT), "FLAG_STRICT should still be set in context");
    assert!(complex_ctx.has(FLAG_ASYNC),  "FLAG_ASYNC should still be set in context");
    println!("  [✓] multi-word context not mutated by parse");
    pass += 1;

    println!();
    println!("Results: {pass} passed, {fail} failed out of {}.", pass + fail);

    if fail > 0 { std::process::exit(1); }

    // ── Codegen preview — show flag-relevant lines ────────────────────────────
    println!();
    println!("=== Generated flag instructions ===");
    let mut src = String::new();
    sipha::codegen::emit_rust(&graph_data, &mut src).unwrap();
    for line in src.lines().filter(|l| {
        l.contains("PushFlags") || l.contains("PopFlags") ||
        l.contains("IfFlag")    || l.contains("FLAG_MASK")
    }) {
        println!("  {}", line.trim());
    }
}
