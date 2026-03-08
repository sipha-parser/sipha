# sipha

[![Release to crates.io](https://github.com/sipha-parser/sipha/actions/workflows/release.yml/badge.svg)](https://github.com/sipha-parser/sipha/actions/workflows/release.yml)

A PEG (Parsing Expression Grammar) parser with a stack-based VM, green/red syntax trees, and optional packrat memoisation.

This repository is a Cargo workspace containing:

| Crate | Description |
|-------|-------------|
| [**sipha**](sipha) | Core parser, grammar builder, syntax trees, and codegen |
| [**sipha-macros**](sipha-macros) | `sipha_grammar!` macro for PEG-style DSL |
| **sipha-analysis** | Analysis and semantic diagnostics |
| **sipha-display** | Tree/source display utilities |
| **sipha-sourcemap** | Source mapping support |
| **sipha-fmt** | Formatting and pretty-printing |
| **sipha-diff** | Diff utilities |

## Quick start

Add to your `Cargo.toml`:

```toml
[dependencies]
sipha = "2"
```

For the grammar macro:

```toml
[dependencies]
sipha = "2"
sipha-macros = "2"
```

See the [**sipha** crate README](sipha/README.md) for features, examples, and API overview.

## License

MIT
