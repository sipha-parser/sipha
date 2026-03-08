# sipha-analysis

Grammar-agnostic analysis helpers for [sipha](https://docs.rs/sipha) syntax trees.

Provides scope extents (offset → scope) and definition collection for LSP (go-to-def, references, document symbols). Grammar-specific logic is passed via callbacks.

See the main [sipha](https://github.com/sipha-parser/sipha) crate for the parser and tree APIs.
