# PEG Parsing Example

This example demonstrates using the PEG parser with ordered choice and memoization.

## Overview

We'll parse an expression grammar using PEG's ordered choice semantics and demonstrate the benefits of memoization (packrat parsing).

## Ordered Choice Semantics

PEG uses ordered choice where the first matching alternative wins:

```rust,ignore
// Grammar: Expr -> Expr + Term | Term
// In PEG: First alternative tried first
// If "Expr + Term" matches, "Term" is never tried
```

This makes precedence natural - lower precedence operators are tried first.

## Setup

```rust,ignore
use sipha::backend::peg::{PegParser, PegConfig};
use sipha::backend::ParserBackend;

let config = PegConfig {
    enable_memoization: true,    // Enable packrat parsing
    max_memo_size: 10000,        // Cache size limit
    error_recovery: true,
    max_errors: 100,
    max_backtrack_depth: 1000,
};

let mut parser = PegParser::new(&grammar, config)
    .expect("Failed to create PEG parser");
```

## Basic Parsing

```rust,ignore
# use sipha::backend::peg::{PegParser, PegConfig};
# use sipha::backend::ParserBackend;
# type MyToken = ();
# type MyNonTerminal = ();
# struct Grammar;
# let grammar = Grammar;
# let config = PegConfig::default();
# let mut parser = PegParser::new(&grammar, config).expect("Failed to create PEG parser");
# let tokens: Vec<MyToken> = vec![];
let result = parser.parse(&tokens, MyNonTerminal::Expr);

if result.errors.is_empty() {
    println!("Parse successful!");
    println!("Tokens consumed: {}", result.metrics.tokens_consumed);
    println!("Nodes created: {}", result.metrics.nodes_created);
} else {
    for error in &result.errors {
        eprintln!("Error: {error}");
    }
}
```

## Memoization (Packrat Parsing)

Memoization caches parse results for O(n) performance:

```rust,ignore
# use sipha::backend::peg::{PegParser, PegConfig};
# type MyToken = ();
# type MyNonTerminal = ();
# struct Grammar;
# let grammar = Grammar;
let config_with_memo = PegConfig {
    enable_memoization: true,
    ..Default::default()
};

let config_without_memo = PegConfig {
    enable_memoization: false,
    ..Default::default()
};

// With memoization: O(n) time, O(n) space
let parser_with = PegParser::new(&grammar, config_with_memo)?;

// Without memoization: May be exponential for some grammars
let parser_without = PegParser::new(&grammar, config_without_memo)?;
```

## Incremental Parsing

PEG works well with incremental parsing thanks to memoization:

```rust,ignore
# use sipha::incremental::IncrementalParser;
# use sipha::backend::peg::{PegParser, PegConfig};
# type MyToken = ();
# type MyNonTerminal = ();
# struct Grammar;
# let grammar = Grammar;
# let parser = PegParser::new(&grammar, PegConfig::default()).expect("parser");
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::syntax::{TextRange, TextSize};

let mut incremental = IncrementalParser::new(parser);

// Initial parse
let result1 = incremental.parse_incremental(
    &tokens,
    None,
    &[],
    MyNonTerminal::Expr,
    Some(&grammar),
);

// After edit - benefits from memoization cache
let edits = vec![TextEdit::replace(
    TextRange::new(TextSize::from(0), TextSize::from(1)),
    "new_text".into(),
)];
let result2 = incremental.parse_incremental(
    &new_tokens,
    Some(&result1.root),
    &edits,
    MyNonTerminal::Expr,
    Some(&grammar),
);
```

## Ordered Choice for Precedence

PEG's ordered choice makes operator precedence natural:

```rust,ignore
// Lower precedence tried first
Expr -> Expr + Term | Term

// Higher precedence tried later  
Term -> Term * Factor | Factor

// This naturally gives * higher precedence than +
```

## Performance Comparison

The example demonstrates performance with and without memoization:

- **With memoization**: Linear time O(n)
- **Without memoization**: May be exponential for some grammars
- **Incremental**: Fast for small edits (benefits from cache)

## Complete Example

See [`examples/peg_parsing.rs`](../../crates/sipha/examples/peg_parsing.rs) for the complete working example.

Run it with:

```bash
cargo run --example peg_parsing --features backend-peg
```

## Expected Output

The example demonstrates:

1. Building a lexer and grammar
2. Parsing with ordered choice
3. Memoization benefits
4. Incremental parsing performance
5. Performance comparison with/without memoization

## Next Steps

- See [PEG Parser](../backends/peg-parser.md) for detailed documentation
- Check [Choosing a Backend](../backends/choosing.md) to see when to use PEG
- Explore [Incremental Parsing](../incremental-parsing/overview.md) for more details

