# FAQ

Frequently asked questions about Sipha.

## General

### What is Sipha?

Sipha is a flexible, incremental parsing library for Rust with support for multiple parsing algorithms.

### Why use Sipha?

Sipha is designed for interactive applications like IDEs and language servers that need fast, incremental parsing.

### Is Sipha production-ready?

Sipha 1.0.0 provides a stable API suitable for production use. The core parsing infrastructure, incremental parsing, and all major backends (LL, LR, GLR, PEG) are stable and well-tested. We follow semantic versioning, so breaking changes will only occur in major version releases (2.0.0, 3.0.0, etc.). New features and improvements continue to be added in minor and patch releases.

## Parsing

### Which backend should I use?

See [Choosing a Backend](../backends/choosing.md) for guidance.

### Does Sipha support left recursion?

- **LL parser**: With left-recursion elimination
- **LR parser**: Yes, naturally
- **GLR parser**: Yes, naturally

### How do I handle ambiguous grammars?

Use the GLR backend. See [GLR Parser](../backends/glr-parser.md) for details.

## Incremental Parsing

### When should I use incremental parsing?

Use incremental parsing for interactive applications like IDEs and language servers.

### How much faster is incremental parsing?

- **Small edits**: 10-100x faster
- **Medium edits**: 5-20x faster
- **Large edits**: 2-5x faster

### Do I need to provide the grammar for incremental parsing?

Yes, providing the grammar enables cache population, which improves performance.

## Performance

### How do I optimize performance?

See [Performance](../advanced/performance.md) for optimization tips.

### Is Sipha fast?

Sipha is competitive with other Rust parsing libraries for batch parsing, and significantly faster for incremental parsing.

## Grammar

### How do I define a grammar?

Use `GrammarBuilder`. See [Grammars](../core-concepts/grammars.md) for details.

### Can I use regex in patterns?

Yes, use `Pattern::Regex`. See [Lexers](../core-concepts/lexers.md) for details.

## Error Handling

### How do I handle errors?

Check `result.errors` after parsing. See [Error Handling](../error-handling/overview.md) for details.

### Does Sipha recover from errors?

Yes, Sipha supports configurable error recovery strategies.

## Contributing

### How do I contribute?

See [Contributing](../extending/contributing.md) for guidelines.

### Where do I report bugs?

Open an issue on GitHub.

## Next Steps

- See [Getting Started](../getting-started.md) to get started
- Check [Examples](../examples/) for examples

