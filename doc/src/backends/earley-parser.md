# Earley Parser Backend

The Earley parser is a chart-based parsing algorithm that can handle any context-free grammar, including ambiguous and left-recursive grammars.

## Features

- **Universal Grammar Support**: Handles any context-free grammar
- **Left Recursion**: Supports left-recursive grammars without transformation
- **Ambiguity Detection**: Can detect and report ambiguous parses
- **Error Recovery**: Configurable error recovery strategies

## Algorithm

The Earley parser uses a chart-based algorithm:

1. **Predictor**: Adds items for all productions of a non-terminal
2. **Scanner**: Advances items when terminals match
3. **Completer**: Advances items when non-terminals complete

## Usage

```rust
use sipha::backend::earley::{EarleyParser, EarleyConfig};
use sipha::backend::ParserBackend;

let config = EarleyConfig::default();
let mut parser = EarleyParser::new(&grammar, config)?;
let result = parser.parse(&tokens, entry_point);
```

## Configuration

```rust
let config = EarleyConfig::new()
    .with_error_recovery(true)
    .with_max_depth(Some(10000))
    .with_ambiguity_detection(true);
```

## When to Use

Use the Earley parser when:

- Your grammar is not LL or LR compatible
- You need to handle left recursion
- You want to detect ambiguities
- Grammar compatibility is more important than performance

## Performance

The Earley parser has O(n³) worst-case complexity for ambiguous grammars, but O(n²) for unambiguous grammars. It's generally slower than LL or LR parsers but more flexible.

## Limitations

- Incremental parsing support is planned but not yet implemented
- Tree extraction from the chart is simplified in the current implementation
- Performance may be slower than specialized parsers for specific grammar types

