# Sipha cookbook

Short recipes for common grammar patterns.

## 1. Expression grammar with precedence

Use the [`expr`](../src/expr.rs) module helpers to build left- or right-associative infix levels without repeating the same pattern:

- **Left-associative** (e.g. mul/div, add/sub): `lower ( op lower )*` with each `op lower` wrapped in a node.

```rust
use sipha::prelude::*;
use sipha::expr;

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

For a rule with many alternatives that start with different bytes (e.g. JSON value: `{`, `[`, `"`, digit, `t`/`f`/`n`), use [`byte_dispatch`](crate::builder::GrammarBuilder::byte_dispatch) to jump in O(1) by first byte:

```rust
g.byte_dispatch(vec![
    (b'{', "object"),
    (b'[', "array"),
    (b'"', "string"),
    // ...
]);
```

Inside each branch, call the corresponding rule. This avoids a long chain of choices and improves performance.

## 4. Trivia and parser vs lexer rules

- **Trivia**: Register a single rule for whitespace/comments with `set_trivia_rule`. Then use `parser_rule` for structural rules; the builder will automatically emit a trivia-skip before each `call`, `token`, and `byte_dispatch`.

- **Parser rules**: Use `parser_rule` for rules that should have trivia skipped between tokens (e.g. statements, expressions).

- **Lexer rules**: Use `lexer_rule` for token-level rules (strings, numbers, identifiers, whitespace). No trivia is injected inside them, so the byte-level pattern stays contiguous.

Example: define `ws` as a lexer rule, call `set_trivia_rule("ws")`, then define `object`, `array`, etc. as parser rules so whitespace is allowed between every token.

## Reference: full grammar example

For a full grammar (token stream, expressions, statements, recovery), see the **leekscript-rs** crate in this workspace: token stream in `grammar::token_stream`, expressions and precedence in `grammar::expressions`, statements in `grammar::statements`, and recovery in the program rule.
