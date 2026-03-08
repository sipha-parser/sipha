# sipha-diff

Tree diff and comparison for [sipha](https://docs.rs/sipha) syntax trees.

Compare two syntax trees by their emitted text (round-trip). Useful for tests (assert formatted output) or refactors (compare before/after). Includes grammar test helpers with S-expressions (`assert_parse_eq`, `assert_parse!`).

See the main [sipha](https://github.com/sipha-parser/sipha) crate for the parser and tree APIs.
