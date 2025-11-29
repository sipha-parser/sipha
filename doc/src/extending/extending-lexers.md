# Extending Lexers

This chapter shows how to extend lexers with custom tokenization logic.

## Custom Tokenization

### Custom Matchers

Use custom matchers for complex patterns:

```rust,ignore
let lexer = LexerBuilder::new()
    .custom_token(MySyntaxKind::String, |text, pos| {
        // Custom string matching
        // ...
    })
    .build(MySyntaxKind::Eof, MySyntaxKind::Ident)
    .expect("Failed to build lexer");
```

See [Custom Patterns](../advanced/custom-patterns.md) for details.

## Extending Pattern Types

### Adding New Pattern Types

To add new pattern types, extend the `Pattern` enum:

```rust,ignore
pub enum Pattern {
    Literal(CompactString),
    CharClass(CharSet),
    Repeat { pattern: Box<Pattern>, min: usize, max: Option<usize> },
    Regex(CompactString),
    Any,
    // Add new pattern types
    Custom(CustomPattern),
}
```

### Implementing Pattern Matching

Implement pattern matching logic:

```rust,ignore
impl Pattern {
    fn matches(&self, text: &str, pos: usize) -> Option<usize> {
        match self {
            Pattern::Custom(custom) => custom.matches(text, pos),
            // ... other patterns
        }
    }
}
```

## Lexer Hooks

### Pre-processing

Add pre-processing hooks:

```rust,ignore
pub struct LexerBuilder<K: SyntaxKind> {
    // ...
    preprocessors: Vec<Box<dyn Fn(&str) -> String>>,
}

impl<K: SyntaxKind> LexerBuilder<K> {
    pub fn preprocess(mut self, f: impl Fn(&str) -> String + 'static) -> Self {
        self.preprocessors.push(Box::new(f));
        self
    }
}
```

### Post-processing

Add post-processing hooks:

```rust,ignore
impl<K: SyntaxKind> LexerBuilder<K> {
    pub fn postprocess(mut self, f: impl Fn(&mut Vec<Token<K>>) + 'static) -> Self {
        self.postprocessors.push(Box::new(f));
        self
    }
}
```

## Incremental Lexing

### Token Updates

Update tokens incrementally:

```rust,ignore
pub struct IncrementalLexer<K: SyntaxKind> {
    lexer: CompiledLexer<K>,
    old_tokens: Vec<Token<K>>,
}

impl<K: SyntaxKind> IncrementalLexer<K> {
    pub fn tokenize_incremental(
        &self,
        text: &str,
        edits: &[TextEdit],
    ) -> Vec<Token<K>> {
        // Re-tokenize only affected regions
        // Reuse unchanged tokens
        // ...
    }
}
```

## Best Practices

1. **Use custom matchers**: For complex patterns, use custom matchers
2. **Extend patterns carefully**: Only add new pattern types if needed
3. **Test thoroughly**: Test with various inputs
4. **Document behavior**: Document custom behavior
5. **Consider performance**: Keep custom logic efficient

## Next Steps

- See [Lexers](../core-concepts/lexers.md) for basic usage
- Check [Custom Patterns](../advanced/custom-patterns.md) for examples

