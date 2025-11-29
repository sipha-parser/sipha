# Incremental Parsing Usage

This chapter shows how to use incremental parsing in your applications.

## Basic Usage

### Initial Parse

Start with an initial parse:

```rust,ignore
use sipha::incremental::IncrementalParser;
use sipha::backend::ll::{LlParser, LlConfig};

let config = LlConfig::default();
let parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");

let mut incremental_parser = IncrementalParser::new(parser);

// Initial parse
let result1 = incremental_parser.parse_incremental(
    &tokens,
    None,        // No old tree
    &[],         // No edits
    MyNonTerminal::Expr,
    Some(&grammar), // Populate cache
);
```

### After an Edit

When text is edited, re-parse incrementally:

```rust,ignore
use sipha::incremental::TextEdit;
use sipha::syntax::{TextRange, TextSize};

// User changed "42" to "100" at position 0
let edits = vec![TextEdit::replace(
    TextRange::new(TextSize::from(0), TextSize::from(2)),
    "100".into(),
)];

// Re-tokenize (or update tokens incrementally)
let new_tokens = lexer.tokenize(&new_text)
    .expect("Failed to tokenize");

// Incremental re-parse
let result2 = incremental_parser.parse_incremental(
    &new_tokens,
    Some(&result1.root), // Previous tree
    &edits,              // Text edits
    MyNonTerminal::Expr,
    Some(&grammar),      // Update cache
);
```

## Text Edits

### Creating Edits

```rust,ignore
use sipha::incremental::TextEdit;
use sipha::syntax::{TextRange, TextSize};

// Replace a range
let edit1 = TextEdit::replace(
    TextRange::new(TextSize::from(10), TextSize::from(20)),
    "new text".into(),
);

// Insert at a position
let edit2 = TextEdit::insert(
    TextSize::from(5),
    "inserted".into(),
);

// Delete a range
let edit3 = TextEdit::delete(
    TextRange::new(TextSize::from(0), TextSize::from(5)),
);
```

### Multiple Edits

Handle multiple edits at once:

```rust,ignore
let edits = vec![
    TextEdit::replace(range1, "text1".into()),
    TextEdit::replace(range2, "text2".into()),
];

let result = incremental_parser.parse_incremental(
    &tokens,
    Some(&old_tree),
    &edits,
    entry_point,
    Some(&grammar),
);
```

## Reuse Budget

Control how many nodes to consider for reuse:

```rust,ignore
use sipha::incremental::{IncrementalSession, ReuseBudget};

// Fixed budget
let budget = ReuseBudget::Fixed(500);
let session = IncrementalSession::new_with_budget(
    Some(&old_tree),
    &edits,
    budget,
);

// Heuristic budget (default)
let budget = ReuseBudget::Heuristic {
    max_depth: 20,
    max_nodes: 1000,
};
```

## Cache Management

### Automatic Cache Population

The cache is populated automatically when you provide a grammar:

```rust,ignore
let result = incremental_parser.parse_incremental(
    &tokens,
    old_tree,
    &edits,
    entry_point,
    Some(&grammar), // Cache populated automatically
);
```

### Manual Cache Access

Access the cache directly:

```rust,ignore
let session = IncrementalSession::with_cache(
    Some(&old_tree),
    &edits,
    &incremental_parser.cache, // Access cache
);
```

## Backend Integration

Incremental parsing works with all backends that support it:

```rust,ignore
// LL parser
let parser = LlParser::new(&grammar, config)?;
let mut incremental = IncrementalParser::new(parser);

// GLR parser
let parser = GlrParser::new(&grammar, config)?;
let mut incremental = IncrementalParser::new(parser);
```

## Error Handling

Handle errors during incremental parsing:

```rust,ignore
let result = incremental_parser.parse_incremental(
    &tokens,
    Some(&old_tree),
    &edits,
    entry_point,
    Some(&grammar),
);

if !result.errors.is_empty() {
    // Handle errors
    for error in &result.errors {
        eprintln!("Error: {:?}", error);
    }
}
```

## Best Practices

1. **Always provide grammar**: This enables cache population
2. **Batch edits**: Process multiple edits together when possible
3. **Reuse old tree**: Always pass the previous tree for best performance
4. **Monitor metrics**: Check `ParseMetrics` to understand performance
5. **Handle errors gracefully**: Incremental parsing may produce errors

## Example: Language Server

Here's a complete example for a language server:

```rust,ignore
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::syntax::{TextRange, TextSize};

struct LanguageServer {
    parser: IncrementalParser<LlParser, MyToken, MyNonTerminal>,
    grammar: Grammar<MyToken, MyNonTerminal>,
    lexer: CompiledLexer<MySyntaxKind>,
    current_tree: Option<GreenNode<MySyntaxKind>>,
}

impl LanguageServer {
    fn new() -> Self {
        let grammar = build_grammar();
        let parser = LlParser::new(&grammar, Default::default()).unwrap();
        let lexer = build_lexer();
        
        Self {
            parser: IncrementalParser::new(parser),
            grammar,
            lexer,
            current_tree: None,
        }
    }
    
    fn on_text_changed(&mut self, edits: Vec<TextEdit>, new_text: &str) {
        // Tokenize new text
        let tokens = self.lexer.tokenize(new_text).unwrap();
        
        // Incremental parse
        let result = self.parser.parse_incremental(
            &tokens,
            self.current_tree.as_ref(),
            &edits,
            MyNonTerminal::Expr,
            Some(&self.grammar),
        );
        
        // Update current tree
        self.current_tree = Some(result.root.clone());
        
        // Report errors
        if !result.errors.is_empty() {
            self.report_errors(&result.errors);
        }
    }
}
```

## Next Steps

- Read [Implementation Details](implementation.md) for advanced topics
- See [Examples](../examples/incremental-example.md) for complete examples

