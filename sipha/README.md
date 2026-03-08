# sipha

[![Release to crates.io](https://github.com/sipha-parser/sipha/actions/workflows/release.yml/badge.svg)](https://github.com/sipha-parser/sipha/actions/workflows/release.yml)

A PEG (Parsing Expression Grammar) parser with a stack-based VM, green/red syntax trees, and optional packrat memoisation.

## Features

- **Grammar builder** — Compose rules with combinators: `byte`, `class`, `literal`, `choice`, `optional`, `zero_or_more`, `node`, `token`, etc.
- **Green/red trees** — Immutable green tree plus position-aware red layer; trivia-aware iterators.
- **Structured errors** — `ParseError::NoMatch` carries a `Diagnostic` with furthest position and expected tokens.
- **Packrat memoisation** — `Engine::with_memo()` for guaranteed O(n) on grammars with backtracking.
- **SIMD literals** — Fast literal matching for long tokens (SSE2/AVX2 on x86_64).
- **Byte dispatch** — O(1) first-byte dispatch for rules like JSON `value`.

## Quick example

```rust
use sipha::prelude::*;

fn main() -> Result<(), sipha::engine::ParseError> {
    let mut g = GrammarBuilder::new();
    g.begin_rule("start");
    g.byte(b'a');
    g.zero_or_more(|g| g.byte(b'b'));
    g.end_of_input();
    g.accept();
    let built = g.finish().expect("valid grammar");
    let graph = built.as_graph();

    let mut engine = Engine::new();
    let out = engine.parse(&graph, b"abbb")?;
    assert_eq!(out.consumed, 4);
    Ok(())
}
```

## Parsing with a syntax tree

Use `node` and `token` in the builder to emit green/red tree events, then build the tree from the parse output:

```rust
use sipha::prelude::*;

let mut g = GrammarBuilder::new();
g.begin_rule("root");
g.node(1, |g| {
    g.token(10, |g| g.byte(b'x'));
});
g.end_of_input();
g.accept();
let built = g.finish().unwrap();
let graph = built.as_graph();

let mut engine = Engine::new();
let out = engine.parse(&graph, b"x").unwrap();
let root = out.syntax_root(b"x").unwrap();
assert_eq!(root.kind(), 1);
```

## Error handling

On parse failure, `ParseError::NoMatch(diagnostic)` contains the furthest byte reached and the set of expected tokens:

```rust
if let Err(ParseError::NoMatch(d)) = engine.parse(&graph, input) {
    eprintln!("parse error at byte {}: expected {}", d.furthest, d.message(Some(&graph.literals)));
    for e in &d.expected {
        println!("  - {}", e.display(Some(&graph.literals)));
    }
}
```

## Grammar macro (sipha-macros)

The **sipha-macros** crate provides a `sipha_grammar!` macro so you can write rules in a PEG-style DSL:

```rust
use sipha::prelude::*;
use sipha_macros::sipha_grammar;

let built = sipha_grammar! {
    @trivia ws;
    @start start;
    #[lexer] ws = (" " | "\t" | "\n")*;
    #[parser] start = "a" "b"+;
};
let graph = built.as_graph();
```

See the [sipha-macros README](../sipha-macros/README.md) for full syntax. Run: `cargo run --example macro_grammar`

## Examples

- `examples/macro_grammar.rs` — Grammar defined with `sipha_grammar!`.
- `examples/json_grammar.rs` — Full JSON with SIMD literals, diagnostics, memo, and byte dispatch.
- `examples/green_red_tree.rs` — Green/red tree and trivia.
- `examples/context_flags.rs` — Parse context flags.
- `examples/unicode_and_repeat.rs` — Unicode terminals and repetition.

Run with: `cargo run --example json_grammar`

## Safety note

`BuiltGraph::as_graph()` returns a `ParseGraph` that borrows from the `BuiltGraph` via a `'static` cast. Keep the `BuiltGraph` alive and do not mutate it while any `ParseGraph` (or references derived from it) is in use.
