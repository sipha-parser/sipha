//! # Unicode characters + ranged repetition example
//!
//! Demonstrates:
//!
//! 1. **`AnyChar`** — consume any valid UTF-8 codepoint (1–4 bytes)
//! 2. **`Char`** — match exactly one Unicode codepoint
//! 3. **`CharRange`** — match a codepoint in an inclusive range
//! 4. **`Repeat::Exact(n)` / `n`** — exactly n repetitions
//! 5. **`Repeat::AtLeast(n)` / `n..`** — at least n repetitions
//! 6. **`Repeat::AtMost(n)` / `..=n`** — at most n repetitions
//! 7. **`Repeat::Between(m,n)` / `m..=n`** — between m and n repetitions
//!
//! Each test suite uses a small purpose-built grammar so every start rule
//! properly ends with `Accept`.

use sipha::prelude::*;

fn check(
    engine:    &mut Engine,
    graph:     &sipha::insn::ParseGraph,
    input:     &str,
    expect_ok: bool,
    desc:      &str,
) -> bool {
    let result = engine.parse(graph, input.as_bytes());
    let ok = result.is_ok();
    let sym = if ok == expect_ok { "✓" } else { "✗" };
    if ok {
        println!("  [{sym}] {desc:<58} → OK  ({} bytes)", result.unwrap().consumed);
    } else {
        let ec = engine.error_context();
        let exp: Vec<_> = ec.expected.iter()
            .map(|e| e.display(Some(&graph.literals), Some(graph.rule_names), Some(graph.expected_labels))).collect();
        println!("  [{sym}] {desc:<58} → ERR byte {}: {exp:?}", ec.furthest);
    }
    ok == expect_ok
}

// ─── Grammar builders ─────────────────────────────────────────────────────────

/// `AnyChar` — matches any valid UTF-8 codepoint, then EOF.
fn grammar_any_char() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.any_char();
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// `Char('€')` — matches exactly U+20AC, then EOF.
fn grammar_exact_char(c: char) -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.char(c);
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// `CharRange('α','ω')` — matches any Greek lowercase codepoint, then EOF.
fn grammar_char_range(lo: char, hi: char) -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.char_range(lo, hi);
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// `AnyChar{1,4}` — matches 1 to 4 codepoints, then EOF.
fn grammar_any_char_repeat() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.repeat(1..=4u32, |g| { g.any_char(); });
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// Exact(n) — byte 'x' repeated exactly n times.
fn grammar_exact(n: u32) -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.repeat(n, |g| { g.byte(b'x'); });
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// AtLeast(n) — byte 'y' repeated at least n times.
fn grammar_at_least(n: u32) -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.repeat(n.., |g| { g.byte(b'y'); });
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// AtMost(n) — byte 'z' repeated at most n times.
fn grammar_at_most(n: u32) -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.repeat(..=n, |g| { g.byte(b'z'); });
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// Between(lo, hi) — byte 'a' repeated between lo and hi times.
fn grammar_between(lo: u32, hi: u32) -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.repeat(lo..=hi, |g| { g.byte(b'a'); });
    g.end_of_input();
    g.accept();
    g.finish().unwrap()
}

/// Full tag grammar: `[emoji{1,4}:label{3,12}]`
fn grammar_tag() -> BuiltGraph {
    let mut g = GrammarBuilder::new();

    // start <- '[' emoji{1,4} ':' label ']' EOF
    g.begin_rule("start");
    g.byte(b'[');
    // 1 to 4 emoji (any non-ASCII codepoint, or any char that isn't control bytes)
    g.repeat(1..=4u32, |g| {
        g.neg_lookahead(|g| { g.byte(b':'); });
        g.neg_lookahead(|g| { g.byte(b']'); });
        g.any_char();
    });
    g.byte(b':');
    g.call("label");
    g.byte(b']');
    g.end_of_input();
    g.accept();

    // label <- segment ('-' segment)*
    g.begin_rule("label");
    g.call("segment");
    g.zero_or_more(|g| { g.byte(b'-'); g.call("segment"); });
    g.end_rule();

    // segment <- alpha{3,12}
    g.begin_rule("segment");
    g.repeat(3..=12u32, |g| {
        g.choice(
            |g| { g.char_range('a', 'z'); },
            |g| { g.char_range('A', 'Z'); },
        );
    });
    g.end_rule();

    g.finish().unwrap()
}

// ─── Main ─────────────────────────────────────────────────────────────────────

fn main() {
    let mut engine = Engine::new();
    let mut pass = 0usize;
    let mut fail = 0usize;

    macro_rules! c {
        ($g:expr, $inp:expr, $ok:expr, $desc:expr) => {{
            let bg = $g;
            let gr = bg.as_graph();
            if check(&mut engine, &gr, $inp, $ok, $desc) { pass += 1 } else { fail += 1 }
        }};
    }

    // ── AnyChar ───────────────────────────────────────────────────────────────
    println!("─── AnyChar: match any valid UTF-8 codepoint ───");
    c!(grammar_any_char(), "A",          true,  "ASCII 'A' (1 byte)");
    c!(grammar_any_char(), "é",          true,  "U+00E9 é (2 bytes)");
    c!(grammar_any_char(), "中",         true,  "U+4E2D 中 (3 bytes)");
    c!(grammar_any_char(), "🎉",         true,  "U+1F389 🎉 (4 bytes)");
    c!(grammar_any_char(), "AB",         false, "two chars — only one expected before EOF");
    c!(grammar_any_char(), "",           false, "empty input");
    // Invalid UTF-8: bare 0x80 continuation byte
    {
        let bg = grammar_any_char(); let gr = bg.as_graph();
        let bad = b"\x80";
        let ok = engine.parse(&gr, bad).is_ok();
        let sym = if ok { "✗" } else { "✓" };
        println!("  [{sym}] {:<58} → {}", "invalid UTF-8 (0x80 continuation as lead)", if ok { "OK (wrong!)" } else { "ERR (correct)" });
        if ok { fail += 1 } else { pass += 1 }
    }

    println!();

    // ── Char ──────────────────────────────────────────────────────────────────
    println!("─── Char: match exact Unicode codepoint ───");
    c!(grammar_exact_char('€'), "€",  true,  "U+20AC € matches €");
    c!(grammar_exact_char('€'), "$",  false, "U+0024 $ does not match €");
    c!(grammar_exact_char('€'), "£",  false, "U+00A3 £ does not match €");
    c!(grammar_exact_char('α'), "α",  true,  "U+03B1 α matches α");
    c!(grammar_exact_char('α'), "Α",  false, "U+0391 Α (capital) does not match α");
    c!(grammar_exact_char('🦀'), "🦀", true,  "U+1F980 🦀 matches 🦀");
    c!(grammar_exact_char('🦀'), "🎉", false, "U+1F389 🎉 does not match 🦀");

    println!();

    // ── CharRange ─────────────────────────────────────────────────────────────
    println!("─── CharRange: match codepoint in [lo, hi] ───");
    // Greek lowercase α (U+03B1) .. ω (U+03C9)
    c!(grammar_char_range('α','ω'), "α",  true,  "α = U+03B1 in range");
    c!(grammar_char_range('α','ω'), "ω",  true,  "ω = U+03C9 in range (upper bound)");
    c!(grammar_char_range('α','ω'), "μ",  true,  "μ = U+03BC in range");
    c!(grammar_char_range('α','ω'), "Α",  false, "Α = U+0391 below range");
    c!(grammar_char_range('α','ω'), "ϊ",  false, "ϊ = U+03CA above range");
    c!(grammar_char_range('α','ω'), "A",  false, "A = U+0041 not Greek");
    // Emoji range (all emoji in 1F600..1F64F smiley block)
    c!(grammar_char_range('\u{1F600}','\u{1F64F}'), "😀", true,  "😀 U+1F600 in smiley block");
    c!(grammar_char_range('\u{1F600}','\u{1F64F}'), "🙏", true,  "🙏 U+1F64F in smiley block (upper)");
    c!(grammar_char_range('\u{1F600}','\u{1F64F}'), "🎉", false, "🎉 U+1F389 not in smiley block");

    println!();

    // ── AnyChar with Repeat(1..=4) ─────────────────────────────────────────────
    println!("─── AnyChar{{1,4}}: 1 to 4 codepoints ───");
    c!(grammar_any_char_repeat(), "",    false, "empty (need at least 1)");
    c!(grammar_any_char_repeat(), "x",   true,  "1 char");
    c!(grammar_any_char_repeat(), "ab",  true,  "2 chars");
    c!(grammar_any_char_repeat(), "🎉xy", true, "4 chars (emoji + ASCII)");
    c!(grammar_any_char_repeat(), "12345", false, "5 chars (over max 4)");

    println!();

    // ── Repeat::Exact ─────────────────────────────────────────────────────────
    println!("─── Repeat::Exact(n) ───");
    c!(grammar_exact(0), "",     true,  "Exact(0): empty");
    c!(grammar_exact(0), "x",   false, "Exact(0): one x rejected");
    c!(grammar_exact(3), "xx",  false, "Exact(3): 2 (too few)");
    c!(grammar_exact(3), "xxx", true,  "Exact(3): 3 (exact)");
    c!(grammar_exact(3), "xxxx",false, "Exact(3): 4 (trailing rejected by EOF)");

    println!();

    // ── Repeat::AtLeast ───────────────────────────────────────────────────────
    println!("─── Repeat::AtLeast(n) — n.. ───");
    c!(grammar_at_least(0), "",       true,  "AtLeast(0): 0 (= zero_or_more)");
    c!(grammar_at_least(0), "yyy",    true,  "AtLeast(0): 3");
    c!(grammar_at_least(1), "",       false, "AtLeast(1): 0 (too few)");
    c!(grammar_at_least(1), "y",      true,  "AtLeast(1): 1 (= one_or_more min)");
    c!(grammar_at_least(1), "yyyyyy", true,  "AtLeast(1): 6");
    c!(grammar_at_least(3), "yy",     false, "AtLeast(3): 2 (too few)");
    c!(grammar_at_least(3), "yyy",    true,  "AtLeast(3): 3 (min)");
    c!(grammar_at_least(3), "yyyyyy", true,  "AtLeast(3): 6");

    println!();

    // ── Repeat::AtMost ────────────────────────────────────────────────────────
    println!("─── Repeat::AtMost(n) — ..=n ───");
    c!(grammar_at_most(0), "",     true,  "AtMost(0): 0");
    c!(grammar_at_most(0), "z",    false, "AtMost(0): 1 (trailing rejected)");
    c!(grammar_at_most(4), "",     true,  "AtMost(4): 0");
    c!(grammar_at_most(4), "z",    true,  "AtMost(4): 1");
    c!(grammar_at_most(4), "zzzz", true,  "AtMost(4): 4 (max)");
    c!(grammar_at_most(4), "zzzzz",false, "AtMost(4): 5 (trailing rejected)");

    println!();

    // ── Repeat::Between ───────────────────────────────────────────────────────
    println!("─── Repeat::Between(m, n) — m..=n ───");
    c!(grammar_between(2,5), "a",      false, "Between(2,5): 1 (< min)");
    c!(grammar_between(2,5), "aa",     true,  "Between(2,5): 2 (min)");
    c!(grammar_between(2,5), "aaa",    true,  "Between(2,5): 3");
    c!(grammar_between(2,5), "aaaaa",  true,  "Between(2,5): 5 (max)");
    c!(grammar_between(2,5), "aaaaaa", false, "Between(2,5): 6 (> max, trailing rejected)");
    // Degenerate: Between(n,n) == Exact(n)
    c!(grammar_between(3,3), "aaa",    true,  "Between(3,3) == Exact(3): 3");
    c!(grammar_between(3,3), "aa",     false, "Between(3,3) == Exact(3): 2");
    // Between(0,n) == AtMost(n)
    c!(grammar_between(0,3), "",       true,  "Between(0,3) == AtMost(3): 0");
    c!(grammar_between(0,3), "aaa",    true,  "Between(0,3) == AtMost(3): 3");
    c!(grammar_between(0,3), "aaaa",   false, "Between(0,3) == AtMost(3): 4 (rejected)");

    println!();

    // ── Mixed: tag grammar ────────────────────────────────────────────────────
    println!("─── Tag grammar: [emoji{{1,4}}:label{{3..12}}] ───");
    let tag_bg = grammar_tag();
    let tag_g  = tag_bg.as_graph();
    macro_rules! ct {
        ($inp:expr, $ok:expr, $desc:expr) => {{
            if check(&mut engine, &tag_g, $inp, $ok, $desc) { pass += 1 } else { fail += 1 }
        }};
    }
    ct!("[🎉:wow]",               true,  "one emoji, 3-letter label");
    ct!("[😀😎:cool]",            true,  "two emoji");
    ct!("[🦀🦀🦀:rustacean]",    true,  "three crab emoji");
    ct!("[🎸🥁🎺🎻:jazz]",       true,  "four emoji (max)");
    ct!("[🎸🥁🎺🎻🎹:jazz]",    false, "five emoji → ':' not found where expected");
    ct!("[⭐:ab]",               false, "label too short (2 < 3)");
    ct!("[⭐:abc]",              true,  "label exactly 3");
    ct!("[⭐:abcdefghijkl]",    true,  "label exactly 12");
    ct!("[⭐:abcdefghijklm]",   false, "label 13 chars (> max 12)");
    ct!("[🌍:hello-world]",      true,  "compound label");
    ct!("[💡:FooBar]",           true,  "mixed-case label");
    ct!("[🔥:café]",             false, "non-ASCII 'é' in label (rejects)");

    println!();
    println!("Results: {pass} passed, {fail} failed out of {}.", pass + fail);
    if fail > 0 { std::process::exit(1); }

    // ── Instruction shape of Between(2,5) ─────────────────────────────────────
    println!();
    println!("─── Between(2,5) expands to {} instructions: ───", {
        let bg = grammar_between(2,5);
        bg.insns.len()
    });
    let bg = grammar_between(2,5);
    for (i, insn) in bg.insns.iter().enumerate() {
        println!("  [{i:02}] {insn:?}");
    }
    // 2 mandatory Byte('a') + 3 optional Choice/Byte/Commit + EndOfInput + Accept
    // = 2 + 3*3 + 1 + 1 = 13 instructions
}
