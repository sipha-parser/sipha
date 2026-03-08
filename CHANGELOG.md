# Changelog

All notable changes to sipha will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

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

[Unreleased]: https://github.com/sipha-parser/sipha/compare/v2.0.0...HEAD
[2.0.0]: https://github.com/sipha-parser/sipha/releases/tag/v2.0.0
