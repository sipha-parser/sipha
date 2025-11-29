# LL(k) Parser

The LL(k) parser is a top-down predictive parser with configurable lookahead.

## Overview

LL(k) parsing works by:

1. **Predicting**: Using lookahead to predict which production to use
2. **Matching**: Matching tokens against predicted productions
3. **Recursing**: Recursively parsing subexpressions

## Configuration

Configure the LL parser with `LlConfig`:

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};

let config = LlConfig {
    lookahead: 1,              // k value for LL(k)
    left_recursion: true,      // Enable left-recursion elimination
    error_recovery: true,      // Enable error recovery
    ..Default::default()
};

let mut parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");
```

### Lookahead

The `lookahead` parameter controls how many tokens to look ahead:

- **LL(1)**: One token lookahead (most common)
- **LL(k)**: k tokens lookahead (for ambiguous grammars)

Higher lookahead values:
- Handle more ambiguous grammars
- Require larger parse tables
- May be slower

### Left Recursion

Left recursion elimination allows parsing left-recursive grammars:

```rust,ignore
// Without elimination: Expr -> Expr + Term (left-recursive)
// With elimination: Automatically transformed
```

### Error Recovery

Error recovery strategies:

- **Synchronization tokens**: Skip to known recovery points
- **Delimited recovery**: Skip to matching delimiters
- **Best-effort parsing**: Continue parsing despite errors

## Usage

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;

let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");

let result = parser.parse(&tokens, MyNonTerminal::Expr);
```

## Incremental Parsing

The LL parser supports incremental parsing:

```rust,ignore
use sipha::incremental::IncrementalParser;

let mut incremental = IncrementalParser::new(parser);
let result = incremental.parse_incremental(
    &tokens,
    old_tree,
    &edits,
    entry_point,
    Some(&grammar),
);
```

## Grammar Requirements

LL parsers require:

- **No left recursion** (unless elimination is enabled)
- **No ambiguity** (unless lookahead > 1)
- **FIRST/FOLLOW sets**: Computed automatically

## Parse Table

The LL parser builds a parse table:

- **Rows**: Non-terminals
- **Columns**: Terminals (FIRST sets)
- **Entries**: Production rules to apply

## Performance

LL parsing performance:

- **Time**: O(n) where n is input length
- **Space**: O(grammar size) for parse table
- **Incremental**: Fast for small edits

## When to Use

Use LL parser when:

- Grammar is LL(k) compatible
- Need predictable performance
- Want simple error messages
- Building interactive tools

## Limitations

LL parser limitations:

- **Left recursion**: Requires elimination (may change parse tree)
- **Ambiguity**: Requires higher lookahead
- **Left associativity**: May require grammar transformation

## Next Steps

- See [Choosing a Backend](choosing.md) for comparison
- Check [Examples](../examples/basic-arithmetic.md) for usage

