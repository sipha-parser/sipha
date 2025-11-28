# GLR Parser

The GLR (Generalized LR) parser extends LR parsing to handle ambiguous grammars by maintaining multiple parser stacks and forking on conflicts.

## Overview

GLR parsing works by:

1. **Building LR table**: Uses LR(1) state machine
2. **Maintaining stacks**: Keeps multiple parser stacks
3. **Forking on conflicts**: Creates new stacks for ambiguous choices
4. **Merging stacks**: Merges stacks when they converge
5. **Returning forest**: Returns parse forest for ambiguous results

## When to Use GLR

Use the GLR backend when:

- Your grammar has inherent ambiguities (e.g., C++ template syntax)
- You need to handle multiple valid parse trees
- You want to disambiguate at parse time using precedence/associativity rules
- Parsing complex languages with inherent ambiguities

## Configuration

Configure the GLR parser with `GlrConfig`:

```rust
use sipha::backend::glr::{GlrParser, GlrConfig};

let config = GlrConfig {
    max_stacks: 100,           // Maximum number of stacks
    disambiguation: None,      // Disambiguation strategy
    ..Default::default()
};

let mut parser = GlrParser::new(&grammar, config)
    .expect("Failed to create GLR parser");
```

## Usage

```rust
use sipha::backend::glr::{GlrParser, GlrConfig};
use sipha::backend::ParserBackend;

let config = GlrConfig::default();
let mut parser = GlrParser::new(&grammar, config)
    .expect("Failed to create GLR parser");

let result = parser.parse(&tokens, entry_point);
```

## Handling Ambiguity

GLR parsers can produce parse forests when multiple valid parse trees exist:

```rust
if let Some(forest) = result.forest {
    // Multiple parse trees exist - disambiguate
    let disambiguated = forest.disambiguate(|alternatives| {
        // Custom disambiguation logic
        alternatives.first().cloned()
    });
}
```

## Disambiguation

Sipha provides several disambiguation strategies:

### Precedence-based

Resolve conflicts using operator precedence:

```rust
use sipha::backend::glr::disambiguate_by_precedence;

let disambiguated = forest.disambiguate(disambiguate_by_precedence(|op| {
    match op {
        MySyntaxKind::Multiply | MySyntaxKind::Divide => 2,
        MySyntaxKind::Plus | MySyntaxKind::Minus => 1,
        _ => 0,
    }
}));
```

### Associativity-based

Resolve conflicts using operator associativity:

```rust
use sipha::backend::glr::disambiguate_by_associativity;

let disambiguated = forest.disambiguate(disambiguate_by_associativity(|op| {
    match op {
        MySyntaxKind::Minus => Associativity::Left,
        MySyntaxKind::Power => Associativity::Right,
        _ => Associativity::None,
    }
}));
```

### Custom Strategies

Implement your own disambiguation logic:

```rust
let disambiguated = forest.disambiguate(|alternatives| {
    // Choose based on custom criteria
    alternatives
        .iter()
        .min_by_key(|tree| compute_complexity(tree))
        .cloned()
});
```

## Parse Forest

A parse forest represents multiple parse trees:

```rust
pub struct ParseForest<K: SyntaxKind> {
    roots: Vec<Arc<GreenNode<K>>>,
}
```

The forest can be:

- **Queried**: Find all parse trees
- **Disambiguated**: Choose one tree
- **Traversed**: Visit all alternatives

## Incremental Parsing

The GLR parser supports incremental parsing:

```rust
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

## Performance

GLR parsing performance:

- **Time**: O(n³) in worst case (ambiguous grammars)
- **Space**: O(n²) for parse forest
- **Stacks**: Number of stacks can grow with ambiguity

For unambiguous grammars, GLR performs similarly to LR.

## Grammar Requirements

GLR parsers can handle:

- **Ambiguous grammars**: Multiple valid parse trees
- **Non-deterministic grammars**: Grammars with conflicts
- **Complex languages**: Languages like C++ with inherent ambiguities

## Limitations

GLR parser limitations:

- **Performance**: Slower for highly ambiguous grammars
- **Memory**: Parse forests can be large
- **Complexity**: More complex than LL or LR

## Example: Ambiguous Expression

```rust
// Grammar: Expr -> Expr + Expr | Expr * Expr | Number
// Input: "1 + 2 * 3"
// Ambiguous: Could be (1 + 2) * 3 or 1 + (2 * 3)

let result = glr_parser.parse(&tokens, MyNonTerminal::Expr);

if let Some(forest) = result.forest {
    // Two parse trees exist
    assert_eq!(forest.roots.len(), 2);
    
    // Disambiguate using precedence
    let disambiguated = forest.disambiguate(disambiguate_by_precedence(|op| {
        match op {
            MySyntaxKind::Multiply => 2,
            MySyntaxKind::Plus => 1,
            _ => 0,
        }
    }));
    
    // Result: 1 + (2 * 3) = 7
}
```

## Next Steps

- See [Choosing a Backend](choosing.md) for comparison
- Check [Examples](../examples/glr-example.md) for complete examples

