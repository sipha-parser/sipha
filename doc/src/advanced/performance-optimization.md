# Performance Optimization Guide

This guide covers advanced performance optimization techniques for Sipha parsers.

## Table Lookup Optimizations

### LL Parser Table Lookups

The LL parser uses optimized table lookups:

- **LL(1) Fast Path**: Single-token lookahead uses a direct hash lookup
- **Cached FIRST/FOLLOW Sets**: Pre-computed sets avoid repeated calculations
- **Optimized Key Construction**: Minimal allocations for table keys

### LR Parser Table Lookups

LR parsers use efficient state-based lookups:

- **Compact Table Representation**: Tables are stored in efficient formats
- **Fast State Transitions**: O(1) state lookup operations
- **Action/Reduce Caching**: Frequently accessed actions are cached

## Incremental Cache Optimization

### Cache Key Design

The incremental cache uses content-based keys for better hit rates:

```rust
use sipha::incremental::cache::ContentCacheKey;

// Optimized key creation from tokens
let key = ContentCacheKey::from_tokens(rule_spur, &tokens, context_hash);
```

### Cache Warming Strategies

Pre-populate the cache for better performance:

1. **Initial Parse with Grammar**: Always provide grammar for cache population
2. **Batch Parsing**: Parse multiple related inputs to warm the cache
3. **Selective Warming**: Warm cache for frequently accessed rules

### Cache Capacity Tuning

Adjust cache capacity based on your use case:

```rust
use sipha::incremental::cache::IncrementalCache;

// For large files: larger cache
let large_cache = IncrementalCache::new(50000);

// For small files: smaller cache (less memory)
let small_cache = IncrementalCache::new(1000);
```

## SIMD Optimizations

Sipha includes SIMD-accelerated lexing for common patterns:

- **Character Class Matching**: Uses `memchr` for SIMD-accelerated byte searching
- **Whitespace Skipping**: Optimized whitespace detection
- **Delimiter Finding**: Fast multi-byte delimiter search

SIMD optimizations are automatically enabled when available. No configuration needed.

## Memory Optimizations

### Compact String Usage

Tokens use `CompactString` which stores small strings inline:

- Strings ≤ 24 bytes: No heap allocation
- Larger strings: Heap allocation only when needed
- Automatic optimization for common token sizes

### Tree Sharing

Green trees are designed for sharing:

- **Arc-based**: Nodes use `Arc` for efficient sharing
- **Shared Subtrees**: Unchanged subtrees are automatically shared
- **Arena Allocation**: Optional arena allocation for batch parsing

### Token Interning

Rule names are interned to reduce memory:

- Shared string storage for rule names
- Fast string comparisons
- Reduced memory footprint for large grammars

## Profiling Tips

### Identify Hot Paths

Use profiling tools to identify bottlenecks:

1. **cargo flamegraph**: Generate flamegraphs to see hot paths
2. **perf**: Linux profiling tool for detailed analysis
3. **Benchmarks**: Use the benchmark suite to measure improvements

### Common Hot Paths

Typical hot paths in parsing:

1. **Tokenization**: Lexer DFA execution
2. **Table Lookups**: Parse table access
3. **Tree Construction**: Green node building
4. **Cache Lookups**: Incremental cache access

### Optimization Priorities

Focus optimization efforts on:

1. **Incremental Parsing Path**: Most important for interactive applications
2. **Table Lookups**: Critical for parsing performance
3. **Tokenization**: Important for overall throughput
4. **Tree Construction**: Significant for memory usage

## Backend-Specific Optimizations

### LL Parser

- Use minimal lookahead (k=1) when possible
- Factor common prefixes in grammar rules
- Avoid deep left recursion

### LR Parser

- Prefer LALR over canonical LR for smaller tables
- Use table compression when available
- Optimize state machine construction

### PEG Parser

- Tune memoization cache size
- Use cut operators to prevent backtracking
- Order alternatives by frequency (most common first)

## Benchmarking

Use the benchmark suite to measure performance:

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench --bench comparison_bench

# Compare incremental vs full parse
cargo bench --bench comparison_bench incremental_parsing
```

## Performance Checklist

- [ ] Use incremental parsing for interactive applications
- [ ] Provide grammar for cache population
- [ ] Tune cache capacity for your use case
- [ ] Use appropriate backend for your grammar
- [ ] Profile to identify actual bottlenecks
- [ ] Optimize hot paths based on profiling data
- [ ] Use compact strings (automatic)
- [ ] Enable arena allocation for batch parsing (if applicable)

