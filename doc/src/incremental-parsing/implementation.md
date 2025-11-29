# Incremental Parsing Implementation Details

This chapter covers advanced implementation details for incremental parsing.

## Architecture

Incremental parsing consists of several components:

### IncrementalParser

The `IncrementalParser` wraps a backend parser and manages the parse cache:

```rust,ignore
pub struct IncrementalParser<P, T, N> {
    parser: P,
    cache: ParseCache<T::Kind>,
    _phantom: PhantomData<(T, N)>,
}
```

### IncrementalSession

The `IncrementalSession` provides context for incremental parsing:

```rust,ignore
pub struct IncrementalSession<'a, K: SyntaxKind> {
    old_tree: Option<&'a GreenNode<K>>,
    edits: &'a [TextEdit],
    affected: AffectedRegion,
    reusable: Vec<ReuseCandidate<K>>,
    reusable_by_pos: HashMap<TextSize, Vec<usize>>,
    cache: Option<&'a ParseCache<K>>,
}
```

### ParseCache

The `ParseCache` stores parse results for reuse:

```rust,ignore
pub struct ParseCache<K: SyntaxKind> {
    version: usize,
    nodes: HashMap<CacheKey, Arc<GreenNode<K>>>,
    interner: Rodeo,
}
```

## Node Reuse Algorithm

The node reuse algorithm works as follows:

1. **Compute affected region**: Determine minimal region to re-parse
2. **Expand region**: Add padding for context
3. **Find reusable nodes**: Traverse old tree, collecting nodes outside affected region
4. **Budget filtering**: Limit candidates based on budget
5. **Index by position**: Build position index for fast lookup

### Finding Reusable Nodes

```rust,ignore
fn find_reusable_nodes<K>(
    root: &GreenNode<K>,
    affected: TextRange,
    budget: usize,
) -> Vec<ReuseCandidate<K>> {
    let mut reusable = Vec::new();
    visit_green_spans(root, |span| {
        if reusable.len() >= budget {
            return;
        }
        
        if span.depth == 0 || span.range.intersect(affected).is_some() {
            return;
        }
        
        if let Some(node_arc) = span.node_arc.clone() {
            reusable.push(ReuseCandidate {
                node: node_arc,
                range: span.range,
                depth: span.depth,
            });
        }
    });
    
    reusable
}
```

## Cache Key Structure

Cache keys consist of:

- **Rule name**: Interned string for the non-terminal
- **Start position**: Text position where parsing started
- **Version**: Cache version for invalidation

```rust,ignore
struct CacheKey {
    rule: Spur,        // Interned rule name
    start: TextSize,   // Start position
    version: usize,     // Cache version
}
```

## Version Management

The cache uses versioning for invalidation:

1. **Increment version**: On each parse, increment cache version
2. **Evict old entries**: Remove entries from old versions
3. **Keep recent versions**: Maintain last N versions (default: 2)

```rust,ignore
fn evict_old_entries(&mut self, max_versions: usize) {
    let min_version = self.version.saturating_sub(max_versions);
    self.nodes.retain(|key, _| key.version >= min_version);
}
```

## Backend Integration

Backends implement incremental parsing via `parse_with_session`:

```rust,ignore
fn parse_with_session(
    &mut self,
    input: &[T],
    entry: N,
    session: &IncrementalSession<'_, T::Kind>,
) -> ParseResult<T, N> {
    // Use session to find reusable nodes
    // Re-parse only affected regions
    // Combine reused and new nodes
}
```

## Performance Optimizations

### Position Indexing

Reusable nodes are indexed by position for O(1) lookup:

```rust,ignore
let mut reusable_by_pos = HashMap::new();
for (idx, candidate) in reusable.iter().enumerate() {
    reusable_by_pos
        .entry(candidate.range.start())
        .or_insert_with(Vec::new)
        .push(idx);
}
```

### Budget Calculation

The heuristic budget considers multiple factors:

```rust,ignore
fn calculate(&self, tree_depth: usize, file_size: TextSize, affected_size: TextSize) -> usize {
    // Depth factor: deeper trees = more candidates
    let depth_factor = (tree_depth.min(max_depth) * 10).max(50);
    
    // File size factor: larger files = more candidates
    let size_factor = ((file_size / 100) as usize).min(max_nodes / 2);
    
    // Affected region factor: smaller regions = more candidates
    let affected_ratio = affected_size as f64 / file_size as f64;
    let affected_factor = ((1.0 - affected_ratio) * 200.0) as usize;
    
    (depth_factor + size_factor + affected_factor)
        .min(max_nodes)
        .max(50)
}
```

## Limitations and Trade-offs

### Context Sensitivity

Some parsing decisions depend on distant context, requiring larger re-parse regions:

- **Operator precedence**: May need parent context
- **Type inference**: May need sibling context
- **Scoping**: May need ancestor context

### Error Recovery

Error recovery may require re-parsing larger regions:

- **Synchronization**: May skip to distant recovery points
- **Best-effort**: May need full re-parse for complex errors

### Cache Overhead

Cache management has overhead:

- **Memory**: Cache uses memory for stored nodes
- **Lookup**: Cache lookups have some overhead
- **Eviction**: Periodic eviction has CPU cost

## Future Improvements

Potential improvements:

- **Incremental lexing**: Extend incremental capabilities to lexer
- **Smarter invalidation**: More precise cache invalidation
- **Parallel parsing**: Parse multiple regions in parallel
- **Predictive reuse**: Predict which nodes will be reusable

## Next Steps

- See [Usage](usage.md) for API examples
- Check [Examples](../examples/incremental-example.md) for complete examples

