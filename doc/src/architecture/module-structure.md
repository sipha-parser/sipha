# Module Structure

This chapter describes the organization of Sipha's codebase.

## Core Modules

### `syntax`

Syntax tree types and operations:

- `green.rs`: Green tree nodes
- `red.rs`: Red tree nodes
- `kind.rs`: Syntax kind trait
- `builder.rs`: Tree builder
- `visitor.rs`: Tree visitors
- `query.rs`: Tree queries

### `lexer`

Lexical analysis:

- `builder.rs`: Lexer builder
- `dfa.rs`: DFA construction and tokenization
- `token.rs`: Token types
- `mod.rs`: Module exports

### `grammar`

Grammar definition and validation:

- `builder.rs`: Grammar builder
- `expr.rs`: Grammar expressions
- `validate.rs`: Grammar validation
- `analysis.rs`: Grammar analysis
- `docs.rs`: Grammar documentation

### `backend`

Parser backends:

- `mod.rs`: Backend trait and common code
- `ll/`: LL(k) parser implementation
- `lr/`: LR parser implementation
- `glr/`: GLR parser implementation
- `common.rs`: Common backend utilities

### `incremental`

Incremental parsing support:

- `mod.rs`: Incremental parser and session

### `error`

Error types and diagnostics:

- `mod.rs`: Error types and diagnostics

### `parser`

Parser traits and interfaces:

- `mod.rs`: Parser traits

## Workspace Structure

```
sipha/
├── crates/
│   ├── sipha/          # Main library
│   ├── sipha_derive/   # Procedural macros
│   └── sipha_lsp/      # LSP support
├── fuzz/               # Fuzz testing
└── doc/                # Documentation (this book)
```

## Module Dependencies

```
syntax
  ↑
  ├── lexer
  │     ↓
  │   grammar
  │     ↓
  │   backend
  │     ↓
  │   incremental
  │
  └── error
```

## Next Steps

- See [Data Structures](data-structures.md) for key types
- Check [Design Principles](design-principles.md) for design rationale

