# Grammar Transformation

This chapter explains how Sipha transforms grammars for different parser backends.

## Overview

Sipha uses a **grammar transformation pipeline** to automatically convert your grammar into a format optimized for each parser backend. This allows you to write your grammar once using a unified API, and Sipha handles the backend-specific details.

## The Transformation Pipeline

When you create a parser, Sipha automatically runs your grammar through this pipeline:

```mermaid
graph LR
    Grammar[Generic Grammar<br/>Grammar&lt;T, N&gt;] --> Validate[1. Validation]
    Validate --> Transform[2. Transformation]
    Transform --> Optimize{3. Optimization?}
    Optimize -->|Yes| Optimized[Optimized<br/>BackendGrammar]
    Optimize -->|No| BackendGrammar[BackendGrammar]
    
    style Grammar fill:#e1f5ff,color:#000000
    style BackendGrammar fill:#c8e6c9,color:#000000
    style Optimized fill:#fff9c4,color:#000000
```

### Step 1: Validation

The grammar is validated for compatibility with the chosen backend:

- **Undefined rules**: Check that all referenced non-terminals are defined
- **Backend compatibility**: Verify that the grammar uses features supported by the backend
- **Grammar errors**: Report any structural issues

### Step 2: Transformation

The grammar is transformed from the generic `Grammar<T, N>` format into a backend-specific representation:

- **LL**: `LlGrammar` with parsing tables and FIRST/FOLLOW sets
- **LR**: `LrGrammar` with LR parsing tables (action/goto tables)
- **GLR**: `GlrGrammar` with LR tables and ambiguity handling
- **PEG**: `PegGrammar` with memoization tables
- **Pratt**: `PrattGrammar` with operator precedence tables

During transformation:

- **Extended expressions** are converted to core expressions where possible
- **Unsupported features** are either transformed or rejected with clear error messages
- **Backend-specific optimizations** are applied (e.g., operator extraction for Pratt)

### Step 3: Optimization (Optional)

If optimization is enabled, the transformed grammar is optimized:

- **Table compression**: Reduce parsing table size (fully implemented)
- **Common prefix factoring**: Factor out common prefixes in choices (detection implemented, extraction planned)
- **Production inlining**: Inline small productions (fully implemented)
- **Operator table optimization**: Optimize operator precedence tables (Pratt, fully implemented)

## Backend-Specific Transformations

### LL Parser

**Transformation**:
- Converts `ExtendedExpr` to `CoreExpr` where needed
- Builds LL(k) parsing table
- Computes FIRST and FOLLOW sets
- Handles left-recursion elimination if needed

**Unsupported features**:
- `PrattOperator` (not applicable to LL)
- Some extended expressions may require transformation

### LR Parser

**Transformation**:
- Converts grammar to LR productions
- Builds LR(1) or LALR(1) parsing table
- Computes FIRST and FOLLOW sets
- Handles conflicts (shift/reduce, reduce/reduce)

**Unsupported features**:
- `PrattOperator` (not applicable to LR)
- Some extended expressions may require transformation

### GLR Parser

**Transformation**:
- Similar to LR, but handles conflicts by forking
- Builds parse forest representation
- Supports ambiguity tracking

**Unsupported features**:
- `PrattOperator` (not applicable to GLR)
- Some extended expressions may require transformation

### PEG Parser

**Transformation**:
- Minimal transformation (PEG supports most features natively)
- Sets up memoization tables
- Handles left-recursion detection

**Supported features**:
- Most `ExtendedExpr` features (lookahead, cut, etc.)
- `PrattOperator` is not supported (use PEG's ordered choice instead)

### Pratt Parser

**Transformation**:
- Extracts operator information from `PrattOperator` expressions
- Builds operator precedence table
- Converts expressions to core format where needed

**Supported features**:
- `PrattOperator` (native support)
- Core expressions
- Recovery points

**Unsupported features**:
- Lookahead
- Token classes
- Semantic predicates
- Backreferences

## Expression Transformation

### Core vs Extended Expressions

Sipha uses a two-level expression system:

1. **`CoreExpr`**: Universally supported by all backends
2. **`ExtendedExpr`**: Wraps `CoreExpr` and adds optional features

During transformation, backends:

1. **Check support**: Determine if an extended expression is supported
2. **Transform**: Convert to core expression if possible
3. **Reject**: Return an error if transformation is impossible

### Example: PrattOperator

```rust,ignore
// In your grammar
Expr::pratt_operator(
    Expr::rule(NonTerminal::Term),
    precedence: 10,
    associativity: Associativity::Left
)

// Pratt parser: Extracts operator info, builds precedence table
// Other parsers: Rejected or transformed to core expression
```

## Optimization Levels

### None (Default)

No optimization is performed. The grammar is transformed but not optimized.

### Basic

Basic optimizations are applied:

- **Table compression**: Remove redundant entries (fully implemented)
- **Operator table optimization**: Optimize operator precedence tables (Pratt, fully implemented)
- **Memoization cleanup**: Remove stale memo entries (PEG, fully implemented)

### Aggressive

Aggressive optimizations are applied:

- All basic optimizations, plus:
- **Common prefix factoring**: Factor out common prefixes (detection implemented, extraction planned)
- **Production inlining**: Inline small productions
- **Expression flattening**: Flatten deeply nested expressions

## Optimization Implementation Status

This section describes which optimizations are fully implemented, partially implemented, or planned for future releases.

### Fully Implemented Optimizations

These optimizations are complete and fully functional:

- **Dead rule elimination**: Removes unreferenced grammar rules (Universal optimizer)
- **Table compression**: Compresses parsing tables for LL, LR, GLR backends
- **Operator table optimization**: Optimizes operator precedence tables (Pratt parser)
- **Memoization cleanup**: Removes stale memo entries (PEG parser)
- **Production inlining**: Inlines small productions in LR parser

### Partially Implemented Optimizations

These optimizations detect patterns but cannot yet extract them due to architectural constraints:

- **Common subexpression elimination (CSE)**: 
  - ✅ **Detection**: Identifies common subexpressions that appear multiple times
  - ❌ **Extraction**: Cannot extract into helper rules (requires dynamic non-terminal creation)
  - The optimizer detects patterns and can identify optimization opportunities, but extraction requires the ability to create new non-terminals at runtime, which is not currently supported by the `NonTerminal` trait design.

- **Common prefix factoring (LL parser)**:
  - ✅ **Detection**: Identifies common prefixes in choice expressions
  - ❌ **Extraction**: Cannot factor prefixes into helper rules (requires dynamic non-terminal creation)
  - The optimizer can detect when choices share common prefixes that could be factored out, but extraction requires creating new non-terminals dynamically.

### Architectural Limitations

Some optimizations are limited by the current architecture:

- **Dynamic non-terminal creation**: The `NonTerminal` trait requires compile-time knowledge of all non-terminals. This prevents optimizations that would create new helper rules at runtime.
- **Expression structure comparison**: While improved, expression hashing for CSE detection is still simplified compared to full structural equality checking.

### Future Enhancements

Planned improvements include:

- Support for dynamic non-terminal creation to enable full CSE extraction
- Enhanced prefix factoring algorithms
- Better expression structure comparison for more accurate pattern detection
- Cost analysis to determine when optimizations are beneficial
- Metrics and profiling support for optimization effectiveness

## Enabling Optimization

Enable optimization via parser configuration:

```rust,ignore
use sipha::grammar::hint::OptimizationLevel;

let config = LlConfig {
    optimize: true,
    optimization_level: OptimizationLevel::Basic,
    ..Default::default()
};

let mut parser = LlParser::new(&grammar, config)?;
```

## Transformation Errors

If transformation fails, you'll get a clear error message:

```rust,ignore
match LlParser::new(&grammar, config) {
    Ok(parser) => { /* ... */ }
    Err(e) => {
        eprintln!("Transformation failed: {}", e);
        // Error will indicate which feature is unsupported
    }
}
```

Common transformation errors:

- **Unsupported feature**: A feature used in your grammar isn't supported by the backend
- **Invalid grammar**: The grammar structure is incompatible with the backend
- **Transformation failure**: An internal error during transformation

## Examples

### Example: Optimization Impact

Here's an example showing how optimization can improve parsing performance:

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::grammar::hint::OptimizationLevel;

let grammar = /* ... your grammar ... */;

// Without optimization
let config_no_opt = LlConfig {
    optimize: false,
    ..Default::default()
};
let parser_no_opt = LlParser::new(&grammar, config_no_opt)?;

// With aggressive optimization
let config_opt = LlConfig {
    optimize: true,
    optimization_level: OptimizationLevel::Aggressive,
    ..Default::default()
};
let parser_opt = LlParser::new(&grammar, config_opt)?;

// Both parsers produce the same results, but the optimized one
// may use less memory and parse faster
```

### Example: Pipeline Caching

When using backends that support caching (e.g., Pratt), you can benefit from cached transformations:

```rust,ignore
use sipha::backend::pipeline::GrammarTransformPipeline;
use sipha::backend::pratt::PrattTransformer;
use sipha::backend::traits::TransformConfig;

let config = TransformConfig {
    cache: true,
    ..Default::default()
};

// First transformation
let transformed1 = GrammarTransformPipeline::transform::<T, N, PrattTransformer>(
    &grammar,
    &config
)?;

// Store in cache
GrammarTransformPipeline::store_cached::<T, N, PrattTransformer>(
    &grammar,
    &config,
    &transformed1
);

// Later, retrieve from cache (avoids redundant transformation)
if let Some(cached) = GrammarTransformPipeline::get_cached::<T, N, PrattTransformer>(
    &grammar,
    &config
) {
    // Use cached grammar
}
```

### Example: Production Inlining (LR)

LR optimization can inline small productions to reduce table size:

```rust,ignore
// Before optimization:
// Expr -> Term
// Term -> Number

// After aggressive optimization with inlining:
// Expr -> Number  (Term is inlined)
```

This reduces the number of non-terminals and can improve parsing table efficiency.

## Best Practices

1. **Write backend-agnostic grammars**: Use core expressions when possible
2. **Use extended features judiciously**: Check backend support before using extended features
3. **Enable optimization for production**: Use `Basic` or `Aggressive` optimization for better performance
4. **Test with different backends**: Ensure your grammar works with your chosen backend
5. **Use caching when available**: Enable caching for backends that support it to avoid redundant transformations

## Next Steps

- Learn about [Parsing Basics](parsing-basics.md) to understand how parsing works
- Explore [Backend Overview](../backends/overview.md) to see backend-specific details
- Check [Architecture](../architecture/module-structure.md) for implementation details

