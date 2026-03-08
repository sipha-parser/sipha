# sipha architecture

This document describes the layering and crate boundaries of sipha, and where to add or change code.

## Data flow (core crate)

Dependencies flow **downward**: grammar definition → codegen → VM → parse output → tree.

```
┌─────────────────────────────────────────────────────────────────┐
│  GrammarBuilder (builder.rs) — PEG combinators, rules, trivia    │
├─────────────────────────────────────────────────────────────────┤
│  codegen — emit VM instructions from builder events              │
├─────────────────────────────────────────────────────────────────┤
│  Insn / ParseGraph (insn.rs) — instruction stream, literals       │
├─────────────────────────────────────────────────────────────────┤
│  Engine (engine.rs) — stack-based VM, memo, parse                │
├─────────────────────────────────────────────────────────────────┤
│  ParseOutput — consumed length, events, green builder state      │
├─────────────────────────────────────────────────────────────────┤
│  green / red (green.rs, red.rs) — immutable tree, syntax nodes   │
└─────────────────────────────────────────────────────────────────┘
```

- **builder** — `GrammarBuilder` API: `byte`, `literal`, `choice`, `node`, `token`, `parser_rule`, `lexer_rule`, `byte_dispatch`, `recover_until`, etc. Emits a linear instruction stream as you call combinators.
- **codegen** — Turns builder output into the final instruction sequence and literal table used by the VM.
- **insn** — `Insn` enum, `ParseGraph`, `LiteralTable`. The VM’s bytecode.
- **engine** — `Engine::parse()`, stack execution, backtracking, optional `MemoTable` for packrat.
- **green** / **red** — Green tree (immutable, no spans) and red layer (`SyntaxNode`, `SyntaxToken`, spans). Built from parse events via `GreenBuilder` / `build_green_tree`.

Supporting modules: **error** (diagnostics, `ParseError`, `SemanticDiagnostic`), **context** (parse flags), **trivia**, **expr** (precedence helpers), **parsed_doc** / **line_index** (source + tree + line map), **walk** (visitor), **source_map**, **tree_display**, **memo**, **simd**, **incremental**.

## Crate layout

| Crate | Role |
|-------|------|
| **sipha** | Core: builder, codegen, VM, green/red trees, diagnostics, walk, parsed_doc. |
| **sipha-macros** | `sipha_grammar!` DSL and `SyntaxKinds` derive. Parses macro input, lowers to `GrammarBuilder` calls. |
| **sipha-analysis** | Analysis utilities built on the red tree. |
| **sipha-display** | Tree and grammar display / debugging. |
| **sipha-sourcemap** | Source mapping (spans, offsets). |
| **sipha-fmt** | Formatting and pretty-printing. |
| **sipha-diff** | Diff utilities for trees/source. |

## Where to change what

| Goal | Where to work |
|------|----------------|
| New combinator or builder API | `sipha/src/builder.rs`, then codegen if new insn needed. |
| New VM instruction or instruction shape | `sipha/src/insn.rs`, then `engine.rs` to execute it, and codegen if builder must emit it. |
| Parse behaviour (backtracking, memo, recovery) | `sipha/src/engine.rs`. |
| Green/red tree representation | `sipha/src/green.rs`, `green_builder.rs`, `red.rs`. |
| Diagnostics (parse or semantic) | `sipha/src/error.rs`. |
| Macro DSL syntax or directives | `sipha-macros`: `parse.rs`, `ir.rs`, `lower.rs`. |
| Expression precedence helpers | `sipha/src/expr.rs`. |
| Trivia / parser vs lexer rules | `sipha/src/builder.rs` (trivia injection), docs in COOKBOOK. |
| Display or debugging | `sipha-display`, or `sipha/src/tree_display.rs`. |
| Source maps | `sipha_sourcemap`, or `sipha/src/source_map.rs`. |

## Safety note

`BuiltGraph::as_graph()` returns a `ParseGraph` that borrows from the `BuiltGraph` via a `'static` cast. Keep the `BuiltGraph` alive and do not mutate it while any `ParseGraph` (or references derived from it) is in use.
