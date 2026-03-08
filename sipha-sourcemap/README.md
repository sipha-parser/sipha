# sipha-sourcemap

Build span mappings from [sipha](https://docs.rs/sipha) transforms.

Run a transform and get back both the new tree and a `SpanMap` that maps spans in the new text back to the original source. Use for diagnostics or IDE features after rewriting.

See the main [sipha](https://github.com/sipha-parser/sipha) crate for the parser and transform API.
