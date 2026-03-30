# Sipha cookbook

Short recipes for common grammar patterns. For API details, see [docs.rs/sipha](https://docs.rs/sipha) (this file lives in the repo as `sipha/docs/COOKBOOK.md` and is not embedded on docs.rs).

## 1. Expression grammar with precedence

Use the [`sipha::parse::expr`](https://docs.rs/sipha/latest/sipha/parse/expr/index.html) module helpers to build left- or right-associative infix levels without repeating the same pattern:

- **Left-associative** (e.g. mul/div, add/sub): `lower ( op lower )*` with each `op lower` wrapped in a node.

```rust
use sipha::prelude::*;
use sipha::parse::expr;

// After defining primary, postfix, unary, and token rules:
expr::left_assoc_infix_level(g, "expr_mul", "expr_power", &["op_star", "op_slash", "op_percent"], Kind::NodeBinaryExpr);
expr::left_assoc_infix_level(g, "expr_add", "expr_mul", &["op_plus", "op_minus"], Kind::NodeBinaryExpr);
```

- **Right-associative** (e.g. power, assignment): `lower op level | lower`.

```rust
expr::right_assoc_infix_level(g, "expr_power", "unary", "op_power", Kind::NodeBinaryExpr);
```

Define your base level (primary, postfix, unary) and token rules first; then chain levels from highest precedence (e.g. power) down to lowest (e.g. assignment).

## 2. Error recovery with `recover_until`

When a rule fails, you can skip input until a sync point and then continue. Use `GrammarBuilder::recover_until`:

```rust
g.parser_rule("program", |g| {
    g.recover_until("semicolon", |g| {
        g.zero_or_more(|g| {
            g.call("statement");
        });
    });
});
```

Here, if `statement` fails, the engine skips bytes until the `semicolon` rule matches (or end-of-input), then continues parsing. For multi-error collection, use `Engine::parse_recovering_multi` so each failure is recorded and parsing continues.

## 3. Byte dispatch for keywords / values

For a rule with many alternatives that start with different bytes (e.g. JSON value: `{`, `[`, `"`, …), use [`GrammarBuilder::byte_dispatch`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.byte_dispatch) with `(CharClass, closure)` arms, or the helpers below when each arm is a single leading byte or a plain rule call.

**Rule calls only** — [`byte_dispatch_rules`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.byte_dispatch_rules):

```rust
g.byte_dispatch_rules(
    &[
        (b'{', "object"),
        (b'[', "array"),
        (b'"', "string"),
    ],
    Some("value_fallback"),
);
```

**Custom body per byte** — [`byte_dispatch_bytes`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.byte_dispatch_bytes) (each arm is one byte and a closure; in debug builds duplicate bytes panic):

```rust
use sipha::parse::builder::GrammarChoiceFn;

g.byte_dispatch_bytes(
    vec![
        (b'{', Box::new(|g| g.call("object"))),
        (b'[', Box::new(|g| g.call("array"))),
    ],
    Some(Box::new(|g| g.call("other"))),
);
```

Inside [`parser_rule`](crate::parse::builder::GrammarBuilder::parser_rule), trivia is skipped once before the dispatch table lookup. This avoids a long chain of choices and improves performance.

The **first** rule you define is the parse entry point (see [`ParseGraph::start`](crate::parse::insn::ParseGraph::start)); define your top-level rule (e.g. `start`) before helper rules when those helpers are only reached via `call` from it.

## 4. Trivia and parser vs lexer rules

- **Trivia**: Register a single rule for whitespace/comments with `set_trivia_rule`. Then use `parser_rule` for structural rules; the builder will automatically emit a trivia-skip before each `call`, `token`, and `byte_dispatch`.

- **Parser rules**: Use `parser_rule` for rules that should have trivia skipped between tokens (e.g. statements, expressions).

- **Lexer rules**: Use `lexer_rule` for token-level rules (strings, numbers, identifiers, whitespace). No trivia is injected inside them, so the byte-level pattern stays contiguous.

Example: define `ws` as a lexer rule, call `set_trivia_rule("ws")`, then define `object`, `array`, etc. as parser rules so whitespace is allowed between every token.

## Reference: full grammar example

For an end-to-end grammar in-tree, see `examples/json_grammar.rs` (byte dispatch, memo, SIMD literals, diagnostics). For a macro-defined grammar, see `examples/macro_grammar.rs`. Larger language front ends typically split lexer-style rules, expression precedence, and statement/recovery rules across modules in the same way as the cookbook sections above.
