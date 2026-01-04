# Feature Flags

Sipha uses feature flags to enable optional functionality.

## Backend Features

### `backend-ll`

Enable LL(k) parser backend (default).

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["backend-ll"] }
```

### `backend-lr`

Enable LR parser backend.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["backend-lr"] }
```

### `backend-glr`

Enable GLR parser backend (requires `backend-lr`).

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["backend-lr", "backend-glr"] }
```

## Optional Features

### `diagnostics`

Enable rich error diagnostics with miette.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["diagnostics"] }
```

### `unicode`

Enable full Unicode support for identifiers.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["unicode"] }
```

### `visitor`

Enable syntax tree visitor patterns.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["visitor"] }
```

### `query`

Enable XPath-like query API for syntax trees.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["query"] }
```

### `tree-utils`

Enable tree diffing and validation utilities.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["tree-utils"] }
```

### `serialize`

Enable serialization support for syntax trees using serde, serde_json, and rkyv.

This feature adds `Serialize` and `Deserialize` implementations to `GreenNode` and `GreenToken`, allowing you to serialize syntax trees to JSON or other formats.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["serialize"] }
```

Example usage:

```rust,ignore
use sipha::syntax::GreenNode;
use serde_json;

// Serialize a syntax tree to JSON
let json = serde_json::to_string(&green_node)?;

// Deserialize from JSON
let node: GreenNode<MySyntaxKind> = serde_json::from_str(&json)?;
```

### `arena`

Enable arena-based allocation for improved performance in batch parsing scenarios.

This feature uses `bumpalo` for efficient bump allocation, providing:
- Faster allocation (O(1) with no synchronization)
- Better cache locality (nodes stored contiguously)
- Batch deallocation (entire arena freed at once)

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["arena"] }
```

Example usage:

```rust,ignore
use sipha::arena::TreeArena;

// Create an arena for batch parsing
let arena = TreeArena::new();

// Allocate nodes in the arena
let node = arena.alloc_node(kind, children, text_len);

// All nodes are freed when arena is dropped
drop(arena);
```

### `parallel`

Enable parallel parsing support for batch file parsing (experimental).

This feature uses `rayon` to parse multiple files in parallel, significantly reducing total parsing time for large projects.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["parallel"] }
```

### `profiling`

Enable performance profiling with puffin (experimental).

This feature adds profiling instrumentation to help identify performance bottlenecks.

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["profiling"] }
```

## Feature Combinations

### Minimal

```toml
[dependencies]
sipha = "0.5.0"  # Only backend-ll
```

### Full

```toml
[dependencies]
sipha = { 
    version = "0.5.0", 
    features = [
        "backend-ll",
        "backend-lr",
        "backend-glr",
        "diagnostics",
        "unicode",
        "visitor",
        "query",
        "tree-utils",
        "serialize",
        "arena",
    ]
}
```

### Language Server

```toml
[dependencies]
sipha = { 
    version = "0.5.0", 
    features = [
        "backend-ll",
        "diagnostics",
        "visitor",
        "query",
    ]
}
```

## Default Features

By default, only `backend-ll` is enabled.

## Next Steps

- See [API Overview](api-overview.md) for API reference
- Check [FAQ](faq.md) for common questions

