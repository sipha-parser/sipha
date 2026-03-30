# sipha-macros

Procedural macros for defining [sipha](https://docs.rs/sipha) PEG grammars via a DSL. The **sipha** crate holds the runtime; this crate only expands `sipha_grammar!` into `GrammarBuilder` calls.

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
sipha = "3"
sipha-macros = "3"
```

Then use the `sipha_grammar!` macro:

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

## Directives

- **`@trivia rule_name;`** — Set the trivia rule (e.g. whitespace). Parser rules will skip trivia before calls and tokens.
- **`@start rule_name;`** — Set the start rule. This rule is emitted first and gets `skip()`, `end_of_input()`, and `accept()` appended so it becomes the grammar entry point.

## Rule attributes

- **`#[parser] name = expr;`** — Parser rule (auto trivia skip before calls/tokens).
- **`#[lexer] name = expr;`** — Lexer rule (no trivia skip).
- **`name = expr;`** — Neutral rule (inherits context).

## Expression syntax

| Syntax        | Meaning                    |
|---------------|----------------------------|
| `a \| b`      | Choice (try `a`, then `b`) |
| `a b`         | Sequence                   |
| `e?`          | Optional (zero or one)     |
| `e*`          | Zero or more               |
| `e+`          | One or more                |
| `&e`          | Positive lookahead         |
| `!e`          | Negative lookahead         |
| `( e )`       | Grouping                   |
| `"literal"`   | Byte literal               |
| `rulename`    | Call another rule          |
| `#[node(KIND)] e`   | Wrap in syntax node  |
| `#[token(KIND)] e`  | Wrap in token        |
| `#[trivia(KIND)] e`| Wrap in trivia token |
| `#[capture(TAG)] e` | Legacy capture       |
| `#[no_skip] e`     | No trivia skip       |

## See also

- [sipha crate README](../sipha/README.md) (overview and features)
- [Sipha cookbook](../sipha/docs/COOKBOOK.md) (builder patterns without the macro)
