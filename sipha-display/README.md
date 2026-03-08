# sipha-display

Utilities to display [sipha](https://docs.rs/sipha) grammars as PEG notation or as DOT graphs.

- **PEG text** — `to_peg(&graph)` for human-readable rule definitions.
- **DOT** — `to_rule_dep_dot`, `to_cfg_dot` for dependency and CFG visualisation.

See the main [sipha](https://github.com/sipha-parser/sipha) crate for the parser and grammar builder.
