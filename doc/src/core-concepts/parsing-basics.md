# Parsing Basics

This chapter explains how parsing works in Sipha, from creating a parser to understanding parse results.

## Overview

Parsing is the process of converting a stream of tokens into a syntax tree according to a grammar. Sipha provides multiple parsing backends, each with different characteristics and capabilities.

## Creating a Parser

First, create a parser from a grammar:

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;

let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");
```

Different backends have different configuration options:

- **LL parser**: `LlConfig` with lookahead settings
- **LR parser**: `LrConfig` with LALR vs canonical LR options
- **GLR parser**: `GlrConfig` with ambiguity handling options

## Parsing Input

Parse a stream of tokens:

```rust,ignore
let result = parser.parse(&tokens, MyNonTerminal::Expr);
```

The `parse` method takes:
- **Tokens**: A slice of tokens from the lexer
- **Entry point**: The non-terminal to start parsing from

## Parse Results

The `parse` method returns a `ParseResult`:

```rust,ignore
pub struct ParseResult<T, N> {
    pub root: GreenNode<T::Kind>,
    pub errors: Vec<ParseError<T, N>>,
    pub warnings: Vec<ParseWarning<T, N>>,
    pub metrics: ParseMetrics,
    #[cfg(feature = "backend-glr")]
    pub forest: Option<ParseForest<T::Kind>>,
}
```

### Root Node

The `root` field contains the syntax tree root. Even if there are errors, the parser attempts to produce a tree:

```rust,ignore
let root = SyntaxNode::new_root(result.root.clone());
println!("Root kind: {:?}", root.kind());
```

### Errors

The `errors` field contains parsing errors:

```rust,ignore
if !result.errors.is_empty() {
    for error in &result.errors {
        eprintln!("Parse error at {:?}: {}", error.span, error.message);
    }
}
```

Common error types:

- **Unexpected token**: Expected one token but found another
- **Unexpected EOF**: Reached end of input unexpectedly
- **Missing token**: Expected a token that wasn't found
- **Invalid grammar**: Grammar-related errors

### Warnings

The `warnings` field contains non-fatal issues:

```rust,ignore
for warning in &result.warnings {
    eprintln!("Warning: {}", warning.message);
}
```

### Metrics

The `metrics` field contains parsing statistics:

```rust,ignore
println!("Tokens consumed: {}", result.metrics.tokens_consumed);
println!("Nodes created: {}", result.metrics.nodes_created);
println!("Parse time: {:?}", result.metrics.parse_time);
```

### Parse Forest (GLR)

When using the GLR backend, the `forest` field may contain multiple parse trees for ambiguous input:

```rust,ignore
#[cfg(feature = "backend-glr")]
if let Some(forest) = result.forest {
    // Handle ambiguity
    let disambiguated = forest.disambiguate(|alternatives| {
        // Choose one alternative
        alternatives.first().cloned()
    });
}
```

## Error Recovery

Sipha parsers attempt to recover from errors and continue parsing:

1. **Synchronization tokens**: Skip to known recovery points
2. **Delimited recovery**: Skip to matching delimiters
3. **Best-effort parsing**: Continue parsing despite errors

See [Error Handling](../error-handling/overview.md) for more details.

## Working with Syntax Trees

After parsing, work with the syntax tree:

```rust,ignore
use sipha::syntax::SyntaxNode;

let root = SyntaxNode::new_root(result.root.clone());

// Traverse children
for child in root.children() {
    println!("Child: {:?}", child.kind());
}

// Find specific nodes
if let Some(expr) = root.descendants().find(|n| n.kind() == MySyntaxKind::Expr) {
    println!("Found expression");
}
```

See [Syntax Trees](syntax-trees/working-with-trees.md) for more details.

## Incremental Parsing

For interactive applications, use incremental parsing:

```rust,ignore
use sipha::incremental::{IncrementalParser, TextEdit};

let mut incremental_parser = IncrementalParser::new(parser);

// Initial parse
let result1 = incremental_parser.parse_incremental(
    &tokens,
    None,
    &[],
    MyNonTerminal::Expr,
    Some(&grammar),
);

// After an edit
let edits = vec![TextEdit::replace(range, "new text".into())];
let result2 = incremental_parser.parse_incremental(
    &new_tokens,
    Some(&result1.root),
    &edits,
    MyNonTerminal::Expr,
    Some(&grammar),
);
```

See [Incremental Parsing](incremental-parsing/overview.md) for more details.

## Backend Selection

Choose a backend based on your needs:

- **LL(k)**: Good for most grammars, supports left-recursion elimination
- **LR**: Efficient for many grammar types, good error recovery
- **GLR**: Handles ambiguous grammars, ideal for complex languages

See [Choosing a Backend](backends/choosing.md) for guidance.

## Complete Example

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;
use sipha::syntax::SyntaxNode;

// Create parser
let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");

// Parse
let result = parser.parse(&tokens, MyNonTerminal::Expr);

// Check for errors
if !result.errors.is_empty() {
    eprintln!("Parsing failed with {} errors", result.errors.len());
    for error in &result.errors {
        eprintln!("  {:?}", error);
    }
}

// Work with syntax tree
let root = SyntaxNode::new_root(result.root.clone());
println!("Parse successful! Root: {:?}", root.kind());
println!("Created {} nodes in {:?}", 
    result.metrics.nodes_created, 
    result.metrics.parse_time);
```

## Next Steps

- Learn about [Incremental Parsing](../incremental-parsing/overview.md) for interactive applications
- Explore [Parsing Backends](../backends/overview.md) to understand different algorithms
- Check out [Syntax Trees](../syntax-trees/working-with-trees.md) for tree manipulation

## See Also

- [Error Handling](../error-handling/overview.md) - Comprehensive error handling guide
- [Cheat Sheet](../reference/cheat-sheet.md) - Quick reference for common patterns
- [Examples](../examples/basic-arithmetic.md) - Working code examples

