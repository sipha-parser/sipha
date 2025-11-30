# Migration Guide: Grammar Transformation Architecture

This guide helps you migrate your code to the new grammar transformation architecture introduced in this version.

## Overview

The new architecture introduces:
- **Backend-specific grammar types**: Each parser backend now uses a specialized grammar representation (e.g., `LrGrammar`, `LlGrammar`)
- **Transformation pipeline**: Grammars are automatically transformed from the generic `Grammar` type to backend-specific formats
- **Optimization support**: Configurable grammar optimization during transformation
- **Unified transformation API**: Consistent transformation process across all backends

## What Changed

### Parser Creation (No Changes Required)

The public API for creating parsers remains **unchanged**:

```rust
// This still works exactly as before
let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)?;
```

The transformation happens internally, so existing code continues to work without modification.

### Configuration Options

All parser configs now include optimization options:

```rust
use sipha::grammar::hint::OptimizationLevel;

let config = LlConfig {
    optimize: true,  // NEW: Enable optimization
    optimization_level: OptimizationLevel::Basic,  // NEW: Set optimization level
    ..Default::default()
};
```

**Default behavior**: Optimization is disabled by default (`optimize: false`), so existing code continues to work without changes.

### Backend Grammar Types

Each backend now exposes its grammar type via the `ParserBackend` trait:

```rust
use sipha::backend::ParserBackend;

type BackendGrammar = <LlParser<T, N> as ParserBackend<T, N>>::BackendGrammar;
```

This is primarily for advanced use cases. Most users won't need to access this directly.

## Migration Steps

### Step 1: No Immediate Changes Required

If you're using the standard parser creation API, **no changes are needed**. Your code will continue to work as-is.

### Step 2: Enable Optimization (Optional)

If you want to enable grammar optimization for better performance:

```rust
// Before (still works)
let config = LlConfig::default();

// After (with optimization)
let config = LlConfig {
    optimize: true,
    optimization_level: OptimizationLevel::Basic,
    ..Default::default()
};
```

### Step 3: Update Tests (If Needed)

If your tests directly access parser internals, you may need to update them. Most tests should continue to work without changes.

## Breaking Changes

### None for Standard Usage

The public API remains backward compatible. The following continue to work:

- `Parser::new(&grammar, config)` - ✅ Works
- `parser.parse(&tokens, entry)` - ✅ Works
- `parser.parse_incremental(...)` - ✅ Works
- All configuration options (with new optional optimization fields) - ✅ Works

### Internal Changes

The following are internal implementation details and shouldn't affect most users:

- Parsers now store `backend_grammar` instead of raw `Grammar`
- Transformation happens automatically during parser creation
- Backend-specific grammar types are used internally

## Examples

### Example 1: Basic Parser (No Changes)

```rust
// This code works without any changes
let grammar = GrammarBuilder::new()
    .entry_point(MyNonTerminal::Expr)
    .rule(MyNonTerminal::Expr, Expr::token(...))
    .build()?;

let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)?;
let result = parser.parse(&tokens, MyNonTerminal::Expr);
```

### Example 2: With Optimization

```rust
// Enable optimization for better performance
let config = LlConfig {
    optimize: true,
    optimization_level: OptimizationLevel::Aggressive,
    ..Default::default()
};
let mut parser = LlParser::new(&grammar, config)?;
```

### Example 3: Accessing Backend Grammar (Advanced)

```rust
use sipha::backend::ParserBackend;

// Get the backend grammar type
type BackendGrammar = <LlParser<T, N> as ParserBackend<T, N>>::BackendGrammar;

// Note: Direct access to backend grammar is typically not needed
// The parser handles all grammar operations internally
```

## Troubleshooting

### Issue: "Parser creation fails with transformation error"

**Solution**: Check that your grammar is compatible with the chosen backend. The transformation pipeline validates grammars and reports specific errors.

### Issue: "Optimization fails"

**Solution**: Some optimizations may not be fully implemented for all backends. Try using `OptimizationLevel::Basic` instead of `Aggressive`, or disable optimization.

### Issue: "Tests fail after update"

**Solution**: If tests access parser internals directly, they may need updates. Most tests should continue to work. Check test output for specific error messages.

## Performance Considerations

- **Transformation overhead**: Grammar transformation happens once during parser creation. The overhead is minimal for most use cases.
- **Optimization cost**: Enabling optimization adds some overhead during parser creation but can improve parsing performance.
- **Caching**: Transformed grammars are cached internally when possible to avoid redundant transformations.

## Future Enhancements

The new architecture enables future enhancements:

- More sophisticated optimizations
- Backend-specific grammar features
- Better error messages during transformation
- Compile-time grammar analysis

## Questions?

If you encounter issues during migration, please:
1. Check this guide for common solutions
2. Review the error messages (they should be more specific now)
3. Open an issue with details about your use case

