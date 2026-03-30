# Contributing to sipha

Thanks for your interest in contributing. This guide explains how to get set up, what to run before opening a PR, and where to put changes.

## Repository and scope

sipha is developed as part of the **parsing** monorepo. When working in that workspace, use the **repository root** for `cargo` commands so the workspace `[patch.crates-io]` uses the local sipha crates. If you are contributing to a standalone sipha repository (e.g. `sipha-parser/sipha`), work from the sipha root and run the same commands from there.

**Scope:** sipha is a generic PEG parser library. Keep changes language-agnostic. LeekScript-specific logic belongs in **leekscript-rs**, not in sipha.

## Getting started

From the appropriate root (monorepo root or sipha root):

```bash
cargo build
cargo test --workspace --all-features
```

Run the examples to sanity-check:

```bash
cargo run -p sipha --example json_grammar
cargo run -p sipha --example macro_grammar
```

See the [sipha crate README](sipha/README.md) (user-facing API and features), the workspace [README](README.md), and [ARCHITECTURE.md](ARCHITECTURE.md) for internals and where to edit.

## Before opening a PR

1. **Format**
   ```bash
   cargo fmt --all -- --check
   ```
   Fix with `cargo fmt --all` if needed.

2. **Clippy**
   ```bash
   cargo clippy --workspace --all-features -- -D warnings
   ```

3. **Tests**
   ```bash
   cargo test --workspace --all-features
   ```

4. **Docs** (optional but recommended)
   ```bash
   cargo doc --workspace --all-features --no-deps
   ```
   Add `--open` to view in a browser. This builds [rustdoc](https://doc.rust-lang.org/rustdoc/) for the workspace. Prefer doc comments on public items so [docs.rs](https://docs.rs/sipha) stays accurate.

CI runs format, clippy, and tests. Keep `#[allow(...)]` to a minimum; prefer fixing the underlying cause. If you must add an allow, keep it narrow and add a short comment.

## Conventions

- **Commits:** Prefer [Conventional Commits](https://www.conventionalcommits.org/), e.g. `feat(engine): ...`, `fix(builder): ...`, `docs: ...`, `test(memo): ...`.
- **Branches:** Short, descriptive names, e.g. `feature/byte-dispatch`, `fix/parse-error-span`.
- **Changelog:** Add an entry to [CHANGELOG.md](CHANGELOG.md) for user-visible changes (see [Keep a Changelog](https://keepachangelog.com/)).

## Where to ask

Open a [GitHub Issue](https://github.com/sipha-parser/sipha/issues) (or the monorepo’s issue tracker if that’s where you’re contributing) for questions, design discussion, or bug reports.
