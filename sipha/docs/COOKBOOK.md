# sipha Cookbook

This document is intentionally example-first. Most sections show a “pattern” and a small runnable snippet.

## Trivia (whitespace/comments) that never breaks parsing

If you call `GrammarBuilder::set_trivia_rule("ws")`, the builder will **auto-inject** `skip()` before:

- `call(...)`
- `token(...)`
- `byte_dispatch(...)`

…but only inside `parser_rule(...)` bodies.

**Rule of thumb:** your trivia rule must succeed even when there is *no* trivia, so prefer `zero_or_more`.

```rust
use sipha::prelude::*;
use sipha::types::LexKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, sipha::LexKinds)]
#[repr(u16)]
enum Lex { Ws }

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        "WS"
    }
}

let mut g = GrammarBuilder::new();
g.set_trivia_rule("ws");
g.lexer_rule("ws", |g| {
    g.trivia(Lex::Ws, |g| {
        g.zero_or_more(|g| {
            g.class(classes::WHITESPACE);
        });
    });
});
```

## Parser rules vs lexer rules

- **`lexer_rule`**: “tight” byte-level patterns like identifiers, numbers, string bodies, comments.
- **`parser_rule`**: structural composition of tokens + subrules, with trivia auto-skip.

Inside `token(...)` and `trivia(...)` bodies, trivia auto-skip is suppressed automatically (token interiors should be byte-level).

## Type-safe rule names (`GrammarRuleName`)

[`GrammarRuleName`](https://docs.rs/sipha/latest/sipha/types/trait.GrammarRuleName.html) maps a **copy** type (usually a unit enum) to the `&str` registered with `parser_rule` / `lexer_rule`. Use [`GrammarBuilder::call_rule`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.call_rule), [`set_trivia_rule_name`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.set_trivia_rule_name), and [`recover_until_rule`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.recover_until_rule) so call sites do not spell raw rule strings. Forward references still resolve at `finish()` like `call("…")`.

```rust
use sipha::prelude::*;
use sipha::types::GrammarRuleName;

#[derive(Clone, Copy)]
enum R {
    Ws,
    Start,
}

impl GrammarRuleName for R {
    fn rule_name(self) -> &'static str {
        match self {
            R::Ws => "ws",
            R::Start => "start",
        }
    }
}

// g.set_trivia_rule_name(R::Ws);
// g.call_rule(R::Start);
```

This names **PEG rules** in the bytecode graph, not CST [`RuleKind`](https://docs.rs/sipha/latest/sipha/types/trait.RuleKind.html) variants.

## Trailing trivia before EOF

`end_of_input()` is **not** preceded by an automatic skip. If your grammar allows trailing spaces/comments, do:

```rust
g.parser_rule("start", |g| {
    g.call("top_level");
    g.skip();
    g.end_of_input();
    g.accept();
});
```

## Building a `ParsedDoc`

After a successful parse, build a single “document handle” that gives you:

- root node
- source bytes
- a `LineIndex` for formatting diagnostics with snippets

```rust
let out = engine.parse(&graph, source_bytes)?;
let doc = ParsedDoc::from_slice(source_bytes, &out).unwrap();
let root: &SyntaxNode = doc.root();
```

## LSP integration (UTF-16 positions + diagnostics)

Sipha uses **byte offsets** (`Pos`) and byte spans (`Span`) internally. LSP uses
**UTF-16 code units** for `Position.character`, so you need conversions when
implementing an LSP server.

Enable features:

```toml
[dependencies]
sipha = { version = "3", features = ["lsp"] }
```

Then you can:

- Convert byte spans to LSP ranges (UTF-16):

```rust
use sipha::prelude::*;

let range: Range = sipha::diagnostics::lsp::span_to_range(&doc, Span::new(0, 1));
```

- Convert parse errors into LSP diagnostics:

```rust
use sipha::prelude::*;

let err: ParseError = /* engine.parse(...).unwrap_err() */;
if let Some(diag) = sipha::diagnostics::lsp::parse_error_to_lsp(&doc, &err) {
    // send `diag` to the client
}
```

- Convert an LSP cursor position back to a byte offset:

```rust
use sipha::prelude::*;

let pos = Position { line: 0, character: 3 };
let byte_offset: Option<Pos> = sipha::diagnostics::lsp::position_to_byte_offset(&doc, pos);
```

## S-expression snapshots for tests

For grammar tests, comparing the full pretty tree output can be noisy. Use the built-in S-expression serializer:

```rust
let sexp = sipha::tree::sexp::syntax_node_to_sexp(
    doc.root(),
    &sipha::tree::sexp::SexpOptions::semantic_only(),
);
```

If you implement [`LexKind`](https://docs.rs/sipha/latest/sipha/types/trait.LexKind.html) / [`RuleKind`](https://docs.rs/sipha/latest/sipha/types/trait.RuleKind.html) (the `LexKinds` / `RuleKinds` derives do not fill these in), you can pass a `kind_to_name` mapping so the output uses readable names instead of numeric kinds.

## Multi-error recovery (panic-free parsing)

If your grammar uses `recover_until("sync_rule", ...)`, you can parse while collecting multiple errors:

```rust
let res = engine.parse_recovering_multi(&graph, src, 32);
match res {
    Ok(out) => { /* no errors */ }
    Err(multi) => {
        for e in &multi.errors {
            // e: ParseError
        }
        // multi.partial: ParseOutput with whatever was built before/around recovery points
    }
}
```

## Embedding sub-languages

You can parse the “host” language, then post-process its `TreeEvent`s to parse marked spans with an embedded grammar and splice the embedded tree under a host node.

See the runnable example `examples/sublanguage_markdown_json.rs`.

