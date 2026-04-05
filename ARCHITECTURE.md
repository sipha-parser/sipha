# sipha architecture

This document describes the layering and crate boundaries of sipha, and where to add or change code.

## Data flow (core crate)

Dependencies flow **downward**: grammar definition → codegen → VM → parse output → tree.

```
┌─────────────────────────────────────────────────────────────────┐
│  GrammarBuilder (parse/builder.rs) — PEG combinators, trivia     │
├─────────────────────────────────────────────────────────────────┤
│  codegen — emit VM instructions from builder events              │
├─────────────────────────────────────────────────────────────────┤
│  Insn / ParseGraph (parse/insn.rs) — instruction stream, tables  │
├─────────────────────────────────────────────────────────────────┤
│  Engine (parse/engine/) — stack-based VM, memo, parse           │
├─────────────────────────────────────────────────────────────────┤
│  ParseOutput — consumed length, events, green builder state      │
├─────────────────────────────────────────────────────────────────┤
│  green / red (tree/) — immutable tree, syntax nodes              │
└─────────────────────────────────────────────────────────────────┘
```

- **builder** — `GrammarBuilder` API: `byte`, `literal`, `choice`, `node`, `token`, `parser_rule`, `lexer_rule`, `byte_dispatch`, `recover_until`, etc. Emits a linear instruction stream as you call combinators.
- **codegen** — Turns builder output into the final instruction sequence and literal table used by the VM.
- **insn** — `Insn` enum, `ParseGraph<'a>`, `LiteralTable`. The VM’s bytecode.
- **engine** — `Engine::parse()`, stack execution, backtracking, optional `MemoTable` for packrat.
- **green** / **red** — Green tree (immutable, no spans) and red layer (`SyntaxNode`, `SyntaxToken`, spans). Built from parse events via `GreenBuilder` / `build_green_tree`.

Supporting areas (under `sipha/src/`): **diagnostics/** (`ParseError` / `BadGraphKind`, `ParsedDoc`, line index, optional miette / UTF-16), **parse/** (**context**, **expr**, **memo**, **simd**, **incremental**, **string_table**), **tree/** (**trivia**, **walk**, **emit**, **transform**, **tree_display**).

Optional tooling lives in **extras/** and is compiled only when the matching Cargo feature is set (`analysis`, `display`, `sourcemap`, `fmt`, `diff`); see `sipha/Cargo.toml`. **Incremental** reparse (`parse/incremental.rs`) is gated by the **`incremental`** feature (reuse tokens and nodes after an edit).

## Crate layout

| Crate | Role |
|-------|------|
| **sipha** | Core: `parse/`, `tree/`, `diagnostics/`, `types/`, `extras/` (feature-gated). |
| **sipha-macros** | `sipha_grammar!` DSL and `LexKinds` / `RuleKinds` derives. Parses macro input, lowers to `GrammarBuilder` calls. |

## Where to change what

| Goal | Where to work |
|------|----------------|
| New combinator or builder API | `sipha/src/parse/builder.rs`, then codegen if new insn needed. |
| New VM instruction or instruction shape | `sipha/src/parse/insn.rs`, then `parse/engine` VM to execute it, and codegen if builder must emit it. |
| Parse behaviour (backtracking, memo, recovery) | `sipha/src/parse/engine/`. |
| Green/red tree representation | `sipha/src/tree/green.rs`, `green_builder.rs`, `red.rs`. |
| Diagnostics (parse or semantic) | `sipha/src/diagnostics/error.rs`. |
| Macro DSL syntax or directives | `sipha-macros`: `parse.rs`, `ir.rs`, `lower.rs`. |
| Expression precedence helpers | `sipha/src/parse/expr.rs`. |
| Trivia / parser vs lexer rules | `sipha/src/parse/builder.rs` (trivia injection), docs in COOKBOOK. |
| Grammar PEG / DOT export | `sipha` with the `display` feature (`sipha::extras::display`). |
| Red-tree display (tests) | `sipha/src/tree/tree_display.rs`. |
| Span maps (transformed ↔ original) | `sipha/src/diagnostics/source_map.rs`; `transform_with_mapping` with the `sourcemap` feature. |
| Formatter helpers | `sipha` with the `fmt` feature (`sipha::extras::fmt`). |
| Tree diff / S-expression test helpers | `sipha` with the `diff` feature (`sipha::extras::diff`, `sipha::assert_parse!`). |
| Incremental reparse (green reuse) | `sipha` with the `incremental` feature (`sipha::parse::incremental`). |

## Safety note

`BuiltGraph::as_graph()` returns a `ParseGraph<'_>` that borrows all tables from the `BuiltGraph`. The graph must not outlive the `BuiltGraph`. Generated static grammars use `ParseGraph<'static>` with real `&'static` slices (see codegen).
