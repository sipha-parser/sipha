# sipha

[![CI](https://github.com/sipha-parser/sipha/actions/workflows/ci.yml/badge.svg)](https://github.com/sipha-parser/sipha/actions/workflows/ci.yml)
[![Release to crates.io](https://github.com/sipha-parser/sipha/actions/workflows/release.yml/badge.svg)](https://github.com/sipha-parser/sipha/actions/workflows/release.yml)

A PEG (Parsing Expression Grammar) parser with a stack-based VM, green/red syntax trees, and optional packrat memoisation.

This repository is a Cargo workspace containing:

| Crate | Description |
|-------|-------------|
| [**sipha**](sipha) | Parser, grammar builder, syntax trees, codegen, and optional add-ons behind [Cargo features](sipha/README.md#optional-features) |
| [**sipha-macros**](sipha-macros) | `sipha_grammar!` macro for PEG-style DSL |

## Documentation

| Resource | What it covers |
|----------|----------------|
| [**API reference (docs.rs)**](https://docs.rs/sipha) | Generated docs for `sipha` (modules, types, traits). |
| [**sipha crate README**](sipha/README.md) | Features, quick examples, optional Cargo features, links to examples. |
| [**Cookbook**](sipha/docs/COOKBOOK.md) | Recipes: expression precedence, `recover_until`, `byte_dispatch`, trivia vs lexer rules. |
| [**Architecture**](ARCHITECTURE.md) | Data flow through builder → VM → trees, module map, where to change code. |
| [**sipha-macros README**](sipha-macros/README.md) | Grammar DSL directives, expression syntax, attributes. |

## Quick start

Add to your `Cargo.toml`:

```toml
[dependencies]
sipha = "3"
```

For the grammar macro:

```toml
[dependencies]
sipha = "3"
sipha-macros = "3"
```

See the [**sipha** crate README](sipha/README.md) for features, examples, and API overview.

## Repository

sipha may be developed in a standalone repo (e.g. `sipha-parser/sipha`) or as part of a larger **parsing** monorepo. When in a monorepo, run `cargo build` and `cargo test` from the **monorepo root** so the workspace uses the local sipha crates.

## Development

From the appropriate root (this repo or the monorepo root):

```bash
cargo fmt --all -- --check
cargo clippy --workspace --all-features -- -D warnings
cargo test --workspace --all-features
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for workflow and conventions, and [ARCHITECTURE.md](ARCHITECTURE.md) for crate layout and where to edit.

## License

MIT
