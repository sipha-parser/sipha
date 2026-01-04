# API Stability Policy

This document describes Sipha's API stability guarantees and versioning policy.

## Version 1.0.0 and Beyond

Sipha 1.0.0 marks the transition to a stable API. All public APIs are now considered stable and will follow semantic versioning principles.

## Semantic Versioning

Sipha follows [Semantic Versioning](https://semver.org/) (SemVer):

- **MAJOR version** (1.0.0, 2.0.0, etc.): Breaking changes that require code modifications
- **MINOR version** (1.1.0, 1.2.0, etc.): New features added in a backward-compatible manner
- **PATCH version** (1.0.1, 1.0.2, etc.): Backward-compatible bug fixes

### What Constitutes a Breaking Change?

A breaking change is any change that:
- Removes a public API (function, method, struct, enum variant, trait, etc.)
- Changes the signature of a public function or method
- Changes the public fields of a struct
- Removes or changes enum variants
- Changes trait requirements or implementations
- Changes default behavior in a way that breaks existing code
- Removes or changes feature flags

### What Does NOT Constitute a Breaking Change?

The following are considered non-breaking:
- Adding new public APIs (functions, methods, structs, etc.)
- Adding new enum variants (unless the enum is used in exhaustive matches)
- Adding new trait methods with default implementations
- Adding new optional parameters with default values
- Performance improvements
- Bug fixes that don't change behavior
- Documentation improvements
- Internal implementation changes that don't affect the public API

## Deprecation Policy

When an API is deprecated:

1. **Deprecation Notice**: The API is marked with `#[deprecated]` attribute and includes a message explaining the replacement
2. **Documentation**: The deprecation is documented in the API docs with migration guidance
3. **Removal Timeline**: Deprecated APIs are removed in the next major version (e.g., deprecated in 1.x removed in 2.0.0)
4. **Minimum Support**: Deprecated APIs remain functional for at least one major version cycle

## Feature Flag Stability

Feature flags are categorized as:

### Stable Features

These features are stable and their APIs will not change in breaking ways:

- `backend-ll` - LL(k) parser backend
- `backend-lr` - LR parser backend
- `backend-glr` - GLR parser backend
- `backend-peg` - PEG parser backend
- `diagnostics` - Miette integration for error messages
- `unicode` - Unicode support
- `query` - Syntax tree query APIs
- `visitor` - Visitor pattern support
- `traversal` - Tree traversal utilities
- `tree-utils` - Tree utility functions
- `grammar-docs` - Grammar documentation generation
- `serialize` - Serialization support (serde, serde_json, rkyv)
- `arena` - Arena allocation (bumpalo-based)

### Experimental Features

These features may have API changes:

- `parallel` - Parallel parsing support (may evolve)
- `profiling` - Performance profiling (may evolve)

## MSRV (Minimum Supported Rust Version)

- **Current MSRV**: Rust 1.70+
- **Policy**: MSRV increases are considered breaking changes and will only occur in major versions
- **Notice**: At least 6 months notice will be given before increasing MSRV

## Stability Guarantees by Component

### Core Parsing Infrastructure

- **Stable**: `Grammar`, `GrammarBuilder`, `Expr`, `Token`, `NonTerminal` traits
- **Stable**: Parser backend traits (`ParserBackend`, `ParserDriver`)
- **Stable**: Error types (`ParseError`, `ParseResult`, `ParseWarning`)

### Incremental Parsing

- **Stable**: `IncrementalParser`, `IncrementalSession`, `IncrementalCache`
- **Stable**: Node reuse APIs and cache management

### Syntax Trees

- **Stable**: `GreenNode`, `SyntaxNode`, `SyntaxKind` trait
- **Stable**: Tree traversal and query APIs
- **Stable**: Visitor pattern APIs

### Lexer

- **Stable**: `LexerBuilder`, `CompiledLexer`, `TokenStream` trait
- **Stable**: Pattern matching APIs

## Long-Term Support

Sipha 1.0.0 will receive:
- **Security fixes**: For at least 2 years
- **Critical bug fixes**: For at least 1 year
- **Documentation updates**: Ongoing

## Reporting Stability Issues

If you encounter an unexpected breaking change in a minor or patch release, please:

1. File an issue on GitHub
2. Include details about the breaking change
3. Tag the issue with "stability" label

We take API stability seriously and will address any unintended breaking changes promptly.

