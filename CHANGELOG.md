# Changelog

All notable changes to sipha will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.0.0] — 2026-03-30

### Documentation

- Workspace and crate READMEs: documentation index, expanded Cargo feature table (including `walk`, `emit`, `transform`, `miette`, `utf16`), examples table, and links to docs.rs.
- [COOKBOOK.md](sipha/docs/COOKBOOK.md): docs.rs links for GitHub readers; full-grammar reference points to in-repo examples instead of an external crate.
- [CONTRIBUTING.md](CONTRIBUTING.md): clearer links to the sipha crate README; `cargo doc` uses `--all-features` to match optional modules.
- [ARCHITECTURE.md](ARCHITECTURE.md): supporting modules and crate table aligned with `sipha/src` layout.
- [sipha-macros/README.md](sipha-macros/README.md): crates.io-friendly sipha link and cross-links.
- Crate docs on docs.rs: `[package.metadata.docs.rs] all-features = true` so feature-gated modules (e.g. `extras`) are documented.
- Rustdoc: fixed broken or private intra-doc links across `diagnostics`, `parse`, `tree`, and `types` (`cargo doc -p sipha --all-features --no-deps` is warning-free).

### Changed

- **Breaking:** `build_green_tree_with_reuse` (with the `incremental` feature) now takes `old_root: &Arc<GreenNode>` instead of `&GreenNode`, matching [`SyntaxNode::green`](https://docs.rs/sipha/latest/sipha/tree/red/struct.SyntaxNode.html#method.green).
- **Breaking:** `reparse`, `reparse_with_output`, and `build_green_tree_with_reuse` take `edits: &[TextEdit]` (use [`std::slice::from_ref`](https://doc.rust-lang.org/stable/std/slice/fn.from_ref.html) for a single edit) instead of `&TextEdit` / `&TextEdit`; span reuse matches [`TextEdit::apply_edits`](https://docs.rs/sipha/latest/sipha/parse/incremental/struct.TextEdit.html#method.apply_edits) for multiple non-overlapping edits.
- **Removed:** `GrammarBuilder::choice7` and `GrammarBuilder::choice8` (use [`choices`](https://docs.rs/sipha/latest/sipha/parse/builder/struct.GrammarBuilder.html#method.choices) or [`sipha::choices!`](https://docs.rs/sipha/latest/sipha/macro.choices.html) for seven or more alternatives).
- **Breaking:** `ParseGraph` is now `ParseGraph<'a>` with lifetime-safe borrows from `BuiltGraph::as_graph()` (no fabricated `'static` pointers). `LiteralTable` / `FlagMaskTable` are similarly generic over the borrow. Generated `GRAMMAR: ParseGraph` from `codegen` remains `ParseGraph<'static>`.
- **Breaking:** `ParseError::BadGraph` is now `ParseError::BadGraph(BadGraphKind)` with a structured `BadGraphKind` (`sipha::parse::engine::BadGraphKind`, re-exported in `sipha::prelude`).
- **Breaking:** `sipha::prelude` is split into nested modules (`prelude::parse`, `prelude::tree`, `prelude::diagnostics`, `prelude::extras`); the flat `prelude::*` re-exports are preserved for common use. Crate-root aliases: `sipha::engine`, `sipha::insn`, `sipha::context`, `sipha::codegen`, and (with `display`) `sipha::display`.
- **Breaking:** Satellite crates `sipha-analysis`, `sipha-display`, `sipha-sourcemap`, `sipha-fmt`, and `sipha-diff` are merged into the **sipha** crate behind Cargo features (`analysis`, `display`, `sourcemap`, `fmt`, `diff`). Replace `sipha_analysis::…` with `sipha::extras::analysis::…` (or `sipha::prelude::*` with the feature enabled), and similarly for the other modules. `sipha-macros` remains a separate crate.

### Added

- **`incremental` Cargo feature** — `sipha::parse::incremental`: full reparse after one or more [`TextEdit`](https://docs.rs/sipha/latest/sipha/parse/incremental/struct.TextEdit.html) values with reuse of unchanged [`Arc`](https://doc.rust-lang.org/stable/std/sync/struct.Arc.html) green tokens and nodes ([`reparse`](https://docs.rs/sipha/latest/sipha/parse/incremental/fn.reparse.html), [`reparse_with_output`](https://docs.rs/sipha/latest/sipha/parse/incremental/fn.reparse_with_output.html), [`build_green_tree_with_reuse`](https://docs.rs/sipha/latest/sipha/parse/incremental/fn.build_green_tree_with_reuse.html)); [`TextEdit::apply_edits`](https://docs.rs/sipha/latest/sipha/parse/incremental/struct.TextEdit.html#method.apply_edits) builds the new buffer; same non-overlapping invariants for reuse. Prelude re-exports when the feature is enabled. Example: `incremental_reparse`.

- **Contributing**
  - CONTRIBUTING.md: setup, pre-PR checklist, scope, conventions.
  - ARCHITECTURE.md: data flow, crate layout, “where to change what” table.
  - CHANGELOG.md: this file.
  - README: Development section and repository note.
  - GitHub issue and pull request templates.

## [2.0.0] — (existing release)

### Added

- PEG parser with stack-based VM, green/red trees, packrat memoisation.
- Grammar builder and sipha-macros DSL for defining grammars.
- sipha-analysis: semantic analysis and error reporting.
- sipha-display: tree and rule formatting for debugging.
- sipha-sourcemap: mapping between source positions and parse nodes.
- sipha-fmt: pretty-printing and formatting of parse trees.
- sipha-diff: structural diff for trees.
- Optional `utf16` feature for UTF-16 line/column offsets (e.g. for LSP).

[Unreleased]: https://github.com/sipha-parser/sipha/compare/v3.0.0...HEAD
[3.0.0]: https://github.com/sipha-parser/sipha/releases/tag/v3.0.0
[2.0.0]: https://github.com/sipha-parser/sipha/releases/tag/v2.0.0

