# LR Parser

The LR parser is a bottom-up shift-reduce parser that builds parse trees from leaves to root.

## Overview

LR parsing works by:

1. **Shifting**: Moving tokens onto the stack
2. **Reducing**: Replacing matched rules with non-terminals
3. **Building**: Constructing the parse tree bottom-up

## Configuration

Configure the LR parser with `LrConfig`:

```rust
use sipha::backend::lr::{LrParser, LrConfig};

let config = LrConfig {
    use_lalr: true,        // Use LALR instead of canonical LR
    error_recovery: true,  // Enable error recovery
    ..Default::default()
};

let mut parser = LrParser::new(&grammar, config)
    .expect("Failed to create parser");
```

### LALR vs Canonical LR

- **LALR**: Smaller tables, faster, handles most grammars
- **Canonical LR**: Larger tables, more precise, handles edge cases

For most grammars, LALR is sufficient and faster.

## Usage

```rust
use sipha::backend::lr::{LrParser, LrConfig};
use sipha::backend::ParserBackend;

let config = LrConfig::default();
let mut parser = LrParser::new(&grammar, config)
    .expect("Failed to create parser");

let result = parser.parse(&tokens, MyNonTerminal::Expr);
```

## Parse Table

The LR parser builds a parse table:

- **States**: Parser states (items)
- **Actions**: Shift, reduce, accept, error
- **Gotos**: State transitions for non-terminals

### Table Construction

1. **Build items**: Create LR items from grammar
2. **Build states**: Group items into states
3. **Compute actions**: Determine shift/reduce actions
4. **Resolve conflicts**: Handle shift/reduce and reduce/reduce conflicts

## Grammar Requirements

LR parsers require:

- **No ambiguity**: Must be unambiguous (or use GLR)
- **No conflicts**: No shift/reduce or reduce/reduce conflicts

## Error Recovery

LR parsers support error recovery:

- **Synchronization**: Skip to known recovery points
- **Delimited recovery**: Skip to matching delimiters
- **Best-effort**: Continue parsing despite errors

## Performance

LR parsing performance:

- **Time**: O(n) where n is input length
- **Space**: O(grammar size) for parse table
- **Table size**: LALR tables are smaller than canonical LR

## When to Use

Use LR parser when:

- Grammar is LR compatible
- Need efficient parsing
- Want good error recovery
- Building batch parsers

## Limitations

LR parser limitations:

- **Ambiguity**: Cannot handle ambiguous grammars (use GLR)
- **Table size**: Large grammars may have large tables
- **Left recursion**: Must be handled in grammar

## Next Steps

- See [GLR Parser](glr-parser.md) for ambiguous grammars
- Check [Choosing a Backend](choosing.md) for comparison

