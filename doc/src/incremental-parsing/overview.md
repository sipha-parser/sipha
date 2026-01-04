# Incremental Parsing Overview

Incremental parsing is Sipha's core strength. It allows you to efficiently update your parse tree when code changes, rather than re-parsing everything from scratch.

## Why Incremental Parsing?

In interactive applications like IDEs, users edit code frequently. Traditional parsers re-parse the entire file on every change, which can be slow for large files. Incremental parsing:

- **Reuses unchanged subtrees** from previous parses
- **Only re-parses affected regions** instead of the entire file
- **Maintains parse caches** for fast updates
- **Scales to large codebases** efficiently

This makes Sipha perfect for building language servers, IDEs, and other tools that need to parse code as users type.

## Benefits

### Performance

Incremental parsing provides significant performance improvements:

- **Fast updates**: Only re-parse changed regions
- **Memory efficient**: Shared tree representation
- **Scalable**: Handles large files efficiently
- **IDE-ready**: Designed for interactive editing scenarios

### User Experience

For interactive applications, incremental parsing means:

- **Responsive editing**: No lag when typing
- **Real-time feedback**: Errors and diagnostics update instantly
- **Smooth experience**: Large files don't slow down the editor

## How It Works (High Level)

1. **Initial parse**: Parse the entire file and cache results
2. **Edit detection**: Identify which regions changed
3. **Node reuse**: Find unchanged subtrees from previous parse
4. **Incremental re-parse**: Only re-parse affected regions
5. **Tree reconstruction**: Combine reused nodes with new parse results

## Current Status

Incremental parsing is fully implemented with complete node reuse and cache management:

- [X] **Node reuse**: Unchanged subtrees are automatically identified and reused
- [X] **Cache population**: Parse results are cached for future reuse
- [X] **Affected range computation**: Only affected regions are re-parsed
- [X] **Smart invalidation**: Cache entries are invalidated based on edit locations
- [X] **Incremental lexing**: Token reuse for efficient re-tokenization

The parser automatically integrates reusable nodes from previous parses, providing significant performance improvements for interactive editing scenarios.

## Incremental Lexing

Sipha also supports **incremental lexing**, which efficiently re-tokenizes source code when edits are made:

- **Token reuse**: Unchanged tokens are preserved (only positions updated)
- **Delta-based updates**: Only affected token regions are re-tokenized
- **Line tracking**: Efficient line/column lookups for error reporting
- **Version tracking**: Cache invalidation based on edit versions

Incremental lexing works seamlessly with incremental parsing, providing end-to-end efficiency from tokenization to parse tree construction.

## When to Use Incremental Parsing

Use incremental parsing when:

- Building language servers or IDEs
- Creating interactive code editors
- Building tools that need real-time syntax analysis
- Parsing large files that change frequently
- Providing live error checking and diagnostics

For batch parsing (one-time parsing of files), regular parsing is sufficient and may be faster due to less overhead.

## Next Steps

- Learn [How It Works](how-it-works.md) for detailed implementation
- See [Usage](usage.md) for API examples
- Read [Implementation Details](implementation.md) for advanced topics

