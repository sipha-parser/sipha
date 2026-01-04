# Migration Guide: Sipha 0.5.x to 1.0.0

This guide covers the changes between Sipha 0.5.x and 1.0.0, and how to migrate your code.

## Overview of Changes

Version 1.0.0 introduces API stability and several improvements:

1. **Removed deprecated APIs** - Cleanup of deprecated methods
2. **Enhanced grammar macro** - Trivia annotation support
3. **Improved PEG incremental parsing** - Better node reuse support
4. **API stability** - Stable API with semantic versioning guarantees

## Breaking Changes

### Removed `parse_incremental_with_grammar` Method

The deprecated `parse_incremental_with_grammar` method has been removed. Use `parse_incremental` with `Some(grammar)` instead.

```rust
// Before (0.5.x)
let result = parser.parse_incremental_with_grammar(
    &tokens,
    old_tree,
    &edits,
    entry,
    &grammar,
);

// After (1.0.0)
let result = parser.parse_incremental(
    &tokens,
    old_tree,
    &edits,
    entry,
    Some(&grammar),  // Changed: grammar is now an Option parameter
);
```

The `parse_incremental` method signature is:
```rust
pub fn parse_incremental(
    &mut self,
    input: &[T],
    old_tree: Option<&GreenNode<T::Kind>>,
    edits: &[TextEdit],
    entry: N,
    grammar: Option<&Grammar<T, N>>,  // Now optional
) -> ParseResult<T, N>
```

If you don't have access to the grammar, you can pass `None`:
```rust
let result = parser.parse_incremental(
    &tokens,
    old_tree,
    &edits,
    entry,
    None,  // No cache population without grammar
);
```

## New Features

### Trivia Annotation in Grammar Macro

The `grammar!` macro now supports trivia annotations for tokens:

```rust
use sipha::grammar;

grammar! {
    #[entry]
    Expr = Term ((Plus | Minus) Term)*;
    
    Term = Factor ((Star | Slash) Factor)*;
    
    Factor = Number
           | Ident
           | LParen Expr RParen;
    
    // Token patterns with trivia annotation
    #[trivia]
    @Whitespace = r"\s+";
    
    #[trivia]
    @Comment = r"//.*";
    
    @Number = r"[0-9]+";
    @Ident = r"[a-zA-Z_][a-zA-Z0-9_]*";
    @Plus = "+";
    @Minus = "-";
    @Star = "*";
    @Slash = "/";
    @LParen = "(";
    @RParen = ")";
}
```

Tokens marked with `#[trivia]` will automatically have `is_trivia()` return `true` in the generated `SyntaxKind` implementation.

## Improvements

### PEG Incremental Parsing

PEG parser incremental parsing has been improved with better node reuse support. The test `test_peg_incremental_reuses_nodes` has been updated and re-enabled.

## Compatibility Notes

- **MSRV**: Rust 1.70+ (unchanged)
- **API Stability**: All public APIs are now stable and will follow semantic versioning
- **Breaking Changes**: Only the deprecated `parse_incremental_with_grammar` method was removed
- **Feature Flags**: No changes to feature flags

## Migration Checklist

- [ ] Replace all `parse_incremental_with_grammar` calls with `parse_incremental(..., Some(&grammar))`
- [ ] Update trivia token definitions in `grammar!` macros to use `#[trivia]` attribute (optional but recommended)
- [ ] Verify all tests pass after migration

## Getting Help

If you encounter issues during migration:

1. Check the API documentation: `cargo doc --open`
2. Review the examples in `crates/sipha/examples/`
3. File an issue on GitHub with the "migration" label
