# Fuzzing sipha

This directory contains a `cargo-fuzz` harness for the `sipha` crate.

## Setup

Install `cargo-fuzz`:

```bash
cargo install cargo-fuzz
```

## Run

From the repository root:

```bash
cargo fuzz run engine_parse
```

Additional targets:

```bash
cargo fuzz run json_parse
cargo fuzz run jsonc_parse
cargo fuzz run unicode_parse
cargo fuzz run macro_parse
```

