# Performance

This chapter covers performance optimization tips and benchmarking for Sipha.

## General Tips

### Use Incremental Parsing

For interactive applications, always use incremental parsing:

```rust,ignore
let mut incremental = IncrementalParser::new(parser);
// Much faster for small edits
```

### Choose the Right Backend

- **LL(k)**: Fast, good for most grammars
- **LR**: Fast, good for batch parsing
- **GLR**: Slower, only use for ambiguous grammars

### Optimize Grammar

- **Factor common patterns**: Extract common subexpressions
- **Avoid deep nesting**: Flatten deeply nested structures
- **Use appropriate lookahead**: Don't use more lookahead than needed

## Lexer Performance

### Pattern Ordering

Order patterns from most specific to least specific:

```rust,ignore
// Good: Specific patterns first
.token(MySyntaxKind::Keyword, Pattern::Literal("if".into()))
.token(MySyntaxKind::Ident, Pattern::CharClass(CharSet::letters()))

// Bad: General patterns first
.token(MySyntaxKind::Ident, Pattern::CharClass(CharSet::letters()))
.token(MySyntaxKind::Keyword, Pattern::Literal("if".into()))
```

### Use Character Classes

Prefer character classes over regex when possible:

```rust,ignore
// Good: Character class
.token(MySyntaxKind::Number, Pattern::CharClass(CharSet::digits()))

// Slower: Regex
.token(MySyntaxKind::Number, Pattern::Regex(r"\d+".into()))
```

### Keyword Trie

Keywords are matched using a trie for O(m) performance where m is keyword length.

## Parser Performance

### Cache Population

Always provide grammar for cache population:

```rust,ignore
// Good: Cache populated
parser.parse_incremental(&tokens, old_tree, &edits, entry, Some(&grammar))

// Bad: No cache
parser.parse_incremental(&tokens, old_tree, &edits, entry, None)
```

### Reuse Budget

Adjust reuse budget based on use case:

```rust,ignore
// For large files
let budget = ReuseBudget::Heuristic {
    max_depth: 30,
    max_nodes: 2000,
};

// For small files
let budget = ReuseBudget::Fixed(100);
```

## Memory Optimization

### Arena Allocation

Green nodes are arena-allocated for efficient memory usage.

### Node Sharing

Incremental parsing shares unchanged nodes, reducing memory usage.

### Cache Eviction

Cache automatically evicts old versions:

```rust,ignore
// Keep last 2 versions (default)
cache.evict_old_entries(2);
```

## Benchmarking

### Measure Parse Time

```rust,ignore
let start = std::time::Instant::now();
let result = parser.parse(&tokens, entry);
let duration = start.elapsed();

println!("Parse time: {:?}", duration);
println!("Metrics: {:?}", result.metrics);
```

### Compare Backends

```rust,ignore
// LL parser
let ll_start = Instant::now();
ll_parser.parse(&tokens, entry);
let ll_time = ll_start.elapsed();

// LR parser
let lr_start = Instant::now();
lr_parser.parse(&tokens, entry);
let lr_time = lr_start.elapsed();
```

## Profiling

### Use a Profiler

Profile your parser to find bottlenecks:

```bash
# With perf
perf record --call-graph=dwarf cargo run --release
perf report

# With valgrind
valgrind --tool=callgrind cargo run --release
```

### Common Bottlenecks

- **Lexer**: Pattern matching, especially regex
- **Parser**: Table lookups, especially for large grammars
- **Tree construction**: Node allocation and linking

## Best Practices

1. **Profile first**: Profile before optimizing
2. **Measure changes**: Benchmark before and after changes
3. **Use incremental parsing**: Always use for interactive apps
4. **Optimize grammar**: Factor and simplify grammar
5. **Choose right backend**: Use appropriate backend for use case

## Next Steps

- See [Grammar Analysis](grammar-analysis.md) for grammar optimization
- Check [Architecture](../architecture/performance-considerations.md) for design considerations

