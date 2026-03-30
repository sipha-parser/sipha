# sipha

[![Release to crates.io](https://github.com/sipha-parser/sipha/actions/workflows/release.yml/badge.svg)](https://github.com/sipha-parser/sipha/actions/workflows/release.yml)

A PEG (Parsing Expression Grammar) parser with a stack-based VM, green/red syntax trees, and optional packrat memoisation.

**More documentation:** [API on docs.rs](https://docs.rs/sipha) · [Cookbook](docs/COOKBOOK.md) (patterns for real grammars) · [Repository architecture](../ARCHITECTURE.md) (internals).

## Features

- **Grammar builder** — Compose rules with combinators: `byte`, `class`, `literal`, `choice`, `optional`, `zero_or_more`, `node`, `token`, etc.
- **Green/red trees** — Immutable green tree plus position-aware red layer; trivia-aware iterators.
- **Structured errors** — `ParseError::NoMatch` carries a `Diagnostic` with furthest position and expected tokens.
- **Packrat memoisation** — `Engine::with_memo()` for guaranteed O(n) on grammars with backtracking.
- **SIMD literals** — Fast literal matching for long tokens (SSE2/AVX2 on x86_64).
- **Byte dispatch** — O(1) first-byte dispatch for rules like JSON `value`.

### Cargo features

**Default:** `walk` (red-tree visitor). Enable extras with `sipha = { version = "2", features = ["…"] }` (combine as needed).

| Feature | Depends on | Description |
|---------|------------|-------------|
| `walk` | — | Red-tree [`Visitor`](https://docs.rs/sipha/latest/sipha/tree/walk/index.html) traversal (on by default). |
| `emit` | `walk` | Serialize syntax trees to text (`tree::emit`). |
| `transform` | — | Tree transforms (`tree::transform`). Required for `sourcemap`. |
| `miette` | — | Pretty diagnostics via [miette](https://docs.rs/miette); see `examples/miette_errors.rs`. |
| `utf16` | — | UTF-16 line/column helpers in `diagnostics::utf16` (e.g. LSP). |
| `analysis` | `walk` | Scope extents and definition collection: [`sipha::extras::analysis`](https://docs.rs/sipha/latest/sipha/extras/analysis/index.html). |
| `display` | — | PEG text and DOT export: [`sipha::extras::display`](https://docs.rs/sipha/latest/sipha/extras/display/index.html). |
| `sourcemap` | `transform` | Span mapping after transform: [`sipha::extras::sourcemap`](https://docs.rs/sipha/latest/sipha/extras/sourcemap/index.html). |
| `fmt` | `emit` | Formatting presets: [`sipha::extras::fmt`](https://docs.rs/sipha/latest/sipha/extras/fmt/index.html). |
| `diff` | `emit` | Tree diff, S-expression test helpers, [`assert_parse!`](https://docs.rs/sipha/latest/sipha/macro.assert_parse.html): [`sipha::extras::diff`](https://docs.rs/sipha/latest/sipha/extras/diff/index.html). |
| `incremental` | — | Incremental reparse: [`sipha::parse::incremental`](https://docs.rs/sipha/latest/sipha/parse/incremental/index.html) ([`reparse`](https://docs.rs/sipha/latest/sipha/parse/incremental/fn.reparse.html), [`build_green_tree_with_reuse`](https://docs.rs/sipha/latest/sipha/parse/incremental/fn.build_green_tree_with_reuse.html), [`TextEdit`](https://docs.rs/sipha/latest/sipha/parse/incremental/struct.TextEdit.html), [`TextEdit::apply_edits`](https://docs.rs/sipha/latest/sipha/parse/incremental/struct.TextEdit.html#method.apply_edits)). APIs take `edits: &[TextEdit]`; use [`std::slice::from_ref`](https://doc.rust-lang.org/stable/std/slice/fn.from_ref.html) for a single edit. |

`use sipha::prelude::*` re-exports the same symbols as the nested `sipha::extras::*` modules when the matching feature is enabled, so you can import helpers from the prelude without typing `extras` every time. With **`incremental`**, the prelude also re-exports [`reparse`](https://docs.rs/sipha/latest/sipha/parse/incremental/fn.reparse.html), [`TextEdit`](https://docs.rs/sipha/latest/sipha/parse/incremental/struct.TextEdit.html), and related helpers.

The **sipha-macros** crate stays separate (procedural macros must live in their own crate).

## Quick example

```rust
use sipha::prelude::*;

fn main() -> Result<(), sipha::parse::engine::ParseError> {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.byte(b'a');
        g.zero_or_more(|g| g.byte(b'b'));
        g.end_of_input();
        g.accept();
    });
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
g.rule("root", |g| {
    g.node(1, |g| {
        g.token(10, |g| g.byte(b'x'));
    });
    g.end_of_input();
    g.accept();
});
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

Run from this repo: `cargo run -p sipha --example <name>` (add `--features …` when noted).

| Example | Topic |
|---------|--------|
| `macro_grammar` | `sipha_grammar!` DSL |
| `json_grammar` | Full JSON: SIMD literals, diagnostics, memo, byte dispatch |
| `json_jsonc` | JSON with JSONC-style extensions |
| `green_red_tree` | Green/red tree construction and trivia |
| `context_flags` | Parse context flags |
| `unicode_and_repeat` | Unicode terminals and repetition |
| `parsed_doc_errors` | [`ParsedDoc`](https://docs.rs/sipha/latest/sipha/diagnostics/parsed_doc/struct.ParsedDoc.html) and line-indexed diagnostics |
| `miette_errors` | Pretty errors (`--features miette`) |
| `display_grammar` | PEG and DOT export (`--features display`) |
| `incremental_reparse` | Reparse with green reuse (`--features incremental`) |

## Safety note

`BuiltGraph::as_graph()` returns a `ParseGraph<'_>` that borrows all tables from the `BuiltGraph`. The graph must not outlive the `BuiltGraph`. Generated static grammars from `codegen::emit_rust` use `ParseGraph<'static>` with real `&'static` slices.
