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

