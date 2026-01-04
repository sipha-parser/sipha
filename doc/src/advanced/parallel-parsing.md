# Parallel Parsing

Sipha supports parsing multiple files in parallel to improve performance on large projects.

## Overview

The parallel parsing infrastructure allows you to:

- Parse multiple files concurrently
- Aggregate results across files
- Report progress during batch parsing
- Handle errors gracefully

## Thread Safety

All parser backends are designed to be thread-safe:

- **ParserBackend trait**: Requires `Send` bound
- **State types**: All implement `Send + Sync`
- **Grammar**: Uses `Arc` for shared ownership across threads
- **Parse results**: Use `Arc` for efficient sharing

## Usage

### Basic Parallel Parsing

```rust
use sipha::parser::parallel::{ParallelParser, ParseBatch, ParallelConfig};
use sipha::grammar::Grammar;
use std::sync::Arc;

// Create a shared grammar
let grammar = Arc::new(my_grammar);

// Create parallel parser
let parser = ParallelParser::new(grammar);

// Create a batch of files to parse
let mut batch = ParseBatch::new();
batch.add("file1.txt", content1, entry_point);
batch.add("file2.txt", content2, entry_point);

// Parse in parallel
let results = parser.parse_batch(
    &batch,
    |content| lexer.tokenize(content),
    |grammar, tokens, entry| {
        let mut parser = create_parser(grammar);
        let result = parser.parse(tokens, entry);
        (Some(result.root), result.errors)
    },
);
```

### With Progress Reporting

```rust
let progress = Box::new(|completed, total| {
    println!("Progress: {}/{}", completed, total);
});

let results = parser.parse_batch_with_progress(
    &batch,
    lexer_fn,
    parse_fn,
    progress,
);
```

### Configuration

```rust
let config = ParallelConfig {
    num_threads: 4,      // Use 4 threads (0 = auto-detect)
    fail_fast: false,    // Continue on errors
    chunk_size: 8,       // Files per thread chunk
};

let parser = ParallelParser::with_config(grammar, config);
```

## Performance Considerations

### When to Use Parallel Parsing

- **Large projects**: Projects with many files benefit most
- **Independent files**: Files that don't depend on each other
- **CPU-bound parsing**: When parsing is CPU-intensive

### Thread Pool Configuration

The parallel parser uses Rayon's thread pool:

- Default: Uses all available CPU cores
- Custom: Set `num_threads` in `ParallelConfig`
- Auto: Set `num_threads` to 0 for automatic detection

### Load Balancing

Files are distributed across threads using chunking:

- Small chunks: Better load balancing, more overhead
- Large chunks: Less overhead, potential load imbalance
- Default: 4 files per chunk (good balance)

## Error Handling

Parallel parsing aggregates errors from all files:

```rust
let summary = aggregate_results(&results);

println!("Total files: {}", summary.total_files);
println!("Successful: {}", summary.successful);
println!("Failed: {}", summary.failed);
println!("Success rate: {:.2}%", summary.success_rate());
```

## Limitations

- **Incremental parsing**: Not yet supported in parallel mode
- **Shared state**: Each thread uses its own parser state
- **Memory**: All files are loaded into memory simultaneously

## Best Practices

1. **Batch size**: Parse in batches of 50-100 files for optimal performance
2. **Error handling**: Use `fail_fast: false` to get complete error reports
3. **Progress reporting**: Use progress callbacks for user feedback
4. **Memory management**: Consider streaming for very large batches

## Example: Project-Wide Parsing

```rust
use std::path::Path;
use walkdir::WalkDir;

let mut batch = ParseBatch::new();

// Collect all files
for entry in WalkDir::new("src") {
    let entry = entry?;
    if entry.file_type().is_file() {
        let content = std::fs::read_to_string(entry.path())?;
        batch.add(entry.path().to_string_lossy(), content, entry_point);
    }
}

// Parse all files in parallel
let results = parser.parse_batch(&batch, lexer_fn, parse_fn);

// Aggregate results
let summary = aggregate_results(&results);
println!("Parsed {} files with {:.2}% success rate", 
    summary.total_files, summary.success_rate());
```

