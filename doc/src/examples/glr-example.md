# GLR Parsing Example

This example demonstrates using the GLR parser to handle ambiguous grammars.

## Overview

We'll parse an ambiguous expression grammar and disambiguate using precedence rules.

## Ambiguous Grammar

```rust
// Grammar: Expr -> Expr + Expr | Expr * Expr | Number
// Input: "1 + 2 * 3"
// Ambiguous: Could be (1 + 2) * 3 or 1 + (2 * 3)
```

## Setup

```rust
use sipha::backend::glr::{GlrParser, GlrConfig};
use sipha::backend::glr::disambiguate_by_precedence;

let config = GlrConfig::default();
let mut parser = GlrParser::new(&grammar, config)
    .expect("Failed to create GLR parser");
```

## Parsing

```rust
let result = parser.parse(&tokens, MyNonTerminal::Expr);

// Check for ambiguity
if let Some(forest) = result.forest {
    println!("Ambiguous: {} parse trees", forest.roots.len());
    
    // Disambiguate using precedence
    let disambiguated = forest.disambiguate(disambiguate_by_precedence(|op| {
        match op {
            MySyntaxKind::Multiply | MySyntaxKind::Divide => 2,
            MySyntaxKind::Plus | MySyntaxKind::Minus => 1,
            _ => 0,
        }
    }));
    
    // Use disambiguated tree
    let root = SyntaxNode::new_root(disambiguated);
} else {
    // Unambiguous - use result directly
    let root = SyntaxNode::new_root(result.root);
}
```

## Disambiguation Strategies

### Precedence

```rust
use sipha::backend::glr::disambiguate_by_precedence;

let disambiguated = forest.disambiguate(disambiguate_by_precedence(|op| {
    match op {
        MySyntaxKind::Multiply => 2,
        MySyntaxKind::Plus => 1,
        _ => 0,
    }
}));
```

### Associativity

```rust
use sipha::backend::glr::disambiguate_by_associativity;
use sipha::backend::glr::Associativity;

let disambiguated = forest.disambiguate(disambiguate_by_associativity(|op| {
    match op {
        MySyntaxKind::Minus => Associativity::Left,
        MySyntaxKind::Power => Associativity::Right,
        _ => Associativity::None,
    }
}));
```

### Custom

```rust
let disambiguated = forest.disambiguate(|alternatives| {
    // Choose based on custom criteria
    alternatives
        .iter()
        .min_by_key(|tree| compute_complexity(tree))
        .cloned()
});
```

## Complete Example

See [`examples/glr_parsing.rs`](../../crates/sipha/examples/glr_parsing.rs) for the complete working example.

## Next Steps

- See [GLR Parser](../backends/glr-parser.md) for detailed documentation
- Check [Real-World Patterns](real-world.md) for more examples

