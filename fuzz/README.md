# sipha fuzzing

This directory is compatible with `cargo-fuzz`.

## Setup

```bash
cargo install cargo-fuzz
```

## Run

From the repository root:

```bash
cargo fuzz run engine_parse
```

## Tips

- **Keep inputs small**: try `-max_len=4096` while iterating, then lift limits later.
- **Make it reproducible**: add `-runs=1 <artifact_path>` to replay a crash.
- **Let it run longer**: use `-timeout=10` (or higher) if targets do heavier work.

## Targets

- `engine_parse`: VM/backtracking/error-path stress on a small expression grammar
- `memo_equivalence`: compare parse results with and without packrat memoisation
- `incremental_reparse`: incremental reparse after random edits vs full reparse

