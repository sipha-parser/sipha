# Sipha

**A flexible, incremental parsing library for Rust**

[![Crates.io](https://img.shields.io/crates/v/sipha.svg)](https://crates.io/crates/sipha)
[![docs.rs](https://docs.rs/sipha/badge.svg)](https://docs.rs/sipha)
[![CI](https://github.com/sipha-parser/sipha/actions/workflows/ci.yml/badge.svg)](https://github.com/sipha-parser/sipha/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![MSRV](https://img.shields.io/badge/rustc-1.70+-orange.svg)
[![rustdoc coverage](https://img.shields.io/badge/docs-85%25-brightgreen)](https://github.com/sipha-parser/sipha/actions)

A flexible, incremental parsing library for Rust with support for multiple parsing algorithms. Sipha is designed from the ground up to support **incremental parsing**—the ability to efficiently re-parse only the changed portions of your code, making it ideal for interactive applications like IDEs, editors, and language servers.

📚 **[Read the Book](https://sipha-parser.github.io/sipha/)** - Comprehensive guide and documentation

## Status: 0.5.0 Release

Sipha 0.5.0 provides a stable foundation for incremental parsing with multiple backends. The core API is stable, though some advanced features may continue to evolve. We welcome feedback and contributions!

## Key Features

### Incremental Parsing (Primary Focus)

Sipha's standout feature is its **incremental parsing** capability. When you edit code, Sipha can:

- **Reuse unchanged subtrees** from previous parses
- **Reparse only affected regions** instead of the entire file
- **Maintain parse caches** for efficient updates
- **Dramatically improve performance** in interactive editing scenarios

This makes Sipha perfect for building language servers, IDEs, and other tools that need to parse code as users type.

### Additional Features

- **Multiple parsing backends**: Choose from LL(k), LR, and more (via feature flags)
- **Immutable syntax trees**: Green/red tree representation for efficient manipulation
- **Error recovery**: Configurable error recovery strategies for robust parsing
- **Flexible grammar definition**: Builder API for defining your grammar
- **Unicode support**: Full Unicode support for identifiers and text (optional)
- **Rich diagnostics**: Beautiful error messages with miette integration (optional)
- **Tree traversal**: Visitor patterns and query APIs for working with syntax trees

## Installation

Add Sipha to your `Cargo.toml`:

```toml
[dependencies]
sipha = "0.5.0"
```

Or with specific features:

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["diagnostics", "unicode", "backend-ll"] }
```

## Quick Start

Let's build a simple arithmetic expression parser step by step. This example will help you understand the core concepts.

### Step 1: Define Your Syntax Kinds

First, define the tokens and non-terminals your parser will use. Sipha uses a unified `SyntaxKind` trait for both terminals (tokens) and non-terminals (grammar rules):

```rust
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ArithSyntaxKind {
    // Terminals (produced by lexer)
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals (produced by parser)
    Expr,
    Term,
    Factor,
}

impl SyntaxKind for ArithSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, ArithSyntaxKind::Expr | ArithSyntaxKind::Term | ArithSyntaxKind::Factor)
    }
    
    fn is_trivia(self) -> bool {
        matches!(self, ArithSyntaxKind::Whitespace)
    }
}
```

### Step 2: Build a Lexer

Create a lexer to tokenize your input text:

```rust
use sipha::lexer::{LexerBuilder, Pattern, CharSet};

let lexer = LexerBuilder::new()
    .token(ArithSyntaxKind::Number, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::digits())),
        min: 1,
        max: None,
    })
    .token(ArithSyntaxKind::Plus, Pattern::Literal("+".into()))
    .token(ArithSyntaxKind::Minus, Pattern::Literal("-".into()))
    .token(ArithSyntaxKind::Multiply, Pattern::Literal("*".into()))
    .token(ArithSyntaxKind::Divide, Pattern::Literal("/".into()))
    .token(ArithSyntaxKind::LParen, Pattern::Literal("(".into()))
    .token(ArithSyntaxKind::RParen, Pattern::Literal(")".into()))
    .token(ArithSyntaxKind::Whitespace, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
        min: 1,
        max: None,
    })
    .trivia(ArithSyntaxKind::Whitespace)
    .build(ArithSyntaxKind::Eof, ArithSyntaxKind::Number)
    .expect("Failed to build lexer");
```

### Step 3: Tokenize Input

```rust
let input = "42 + 10";
let tokens = lexer.tokenize(input)
    .expect("Failed to tokenize input");
```

### Step 4: Define Non-Terminals and Build Grammar

```rust
use sipha::grammar::{GrammarBuilder, Token, NonTerminal, Expr};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ArithNonTerminal {
    Expr,
    Term,
    Factor,
}

impl NonTerminal for ArithNonTerminal {
    fn name(&self) -> &str {
        match self {
            ArithNonTerminal::Expr => "Expr",
            ArithNonTerminal::Term => "Term",
            ArithNonTerminal::Factor => "Factor",
        }
    }
}

// Build your grammar rules
let grammar = GrammarBuilder::new()
    .entry_point(ArithNonTerminal::Expr)
    // Add your grammar rules here
    .build()
    .expect("Failed to build grammar");
```

### Step 5: Parse!

```rust
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;

let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");

let result = parser.parse(&tokens, ArithNonTerminal::Expr);
```

For a complete working example, see [`examples/basic_arithmetic.rs`](crates/sipha/examples/basic_arithmetic.rs).

## Incremental Parsing

Incremental parsing is Sipha's core strength. It allows you to efficiently update your parse tree when code changes, rather than re-parsing everything from scratch.

### Why Incremental Parsing?

In interactive applications like IDEs, users edit code frequently. Traditional parsers re-parse the entire file on every change, which can be slow for large files. Incremental parsing:

- **Reuses unchanged nodes** from previous parses
- **Only re-parses affected regions** 
- **Maintains parse caches** for fast updates
- **Scales to large codebases** efficiently

### How It Works

```rust
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::syntax::{TextRange, TextSize};

// Initial parse
let result1 = parser.parse_incremental(&tokens, None, &[], entry_point);

// After an edit (e.g., user changed "42" to "100")
let edits = vec![TextEdit {
    range: TextRange::new(
        TextSize::from(0),
        TextSize::from(2)
    ),
    new_text: "100".into(),
}];

// Incremental re-parse - only affected regions are re-parsed
let result2 = parser.parse_incremental(
    &new_tokens,
    Some(&result1.root),
    &edits,
    entry_point
);
```

The parser automatically:
1. Identifies which parts of the tree are affected by the edit
2. Reuses unchanged subtrees
3. Only re-parses the minimal affected region

### Current Status

Incremental parsing is fully implemented in version 0.5.0 with complete node reuse and cache management:

- [X] **Node reuse**: Unchanged subtrees are automatically identified and reused from previous parses
- [X] **Cache population**: Parse results are cached and can be reused for future parses
- [X] **Affected range computation**: Only affected regions are re-parsed
- [X] **Smart cache invalidation**: Cache entries are invalidated based on edit locations
- [X] **Cross-session caching**: Persistent parse cache enables node reuse across multiple parse sessions

The parser automatically integrates reusable nodes from previous parses and queries the persistent cache during parsing, providing significant performance improvements for interactive editing scenarios.

## GLR Parsing

Sipha includes a GLR (Generalized LR) parser backend for handling ambiguous grammars. GLR parsing extends LR parsing to handle non-deterministic grammars by maintaining multiple parser stacks and forking on conflicts.

### When to Use GLR

Use the GLR backend when:
- Your grammar has inherent ambiguities (e.g., C++ template syntax)
- You need to handle multiple valid parse trees
- You want to disambiguate at parse time using precedence/associativity rules

### Basic Usage

```rust
use sipha::backend::glr::{GlrParser, GlrConfig};
use sipha::backend::ParserBackend;

// Create GLR parser
let config = GlrConfig::default();
let parser = GlrParser::new(&grammar, config)
    .expect("Failed to create GLR parser");

// Parse with GLR - returns a parse forest for ambiguous results
let result = parser.parse(&tokens, entry_point);

// Handle ambiguity if present
if let Some(forest) = result.forest {
    // Multiple parse trees exist - disambiguate
    let disambiguated = forest.disambiguate(|alternatives| {
        // Custom disambiguation logic
        alternatives.first().cloned()
    });
}
```

### Disambiguation

GLR parsers can produce parse forests when multiple valid parse trees exist. Sipha provides several disambiguation strategies:

- **Precedence-based**: Resolve conflicts using operator precedence
- **Associativity-based**: Resolve conflicts using operator associativity
- **Custom strategies**: Implement your own disambiguation logic

For more details, see the [`backend::glr`](crates/sipha/src/backend/glr/mod.rs) module documentation.

## Architecture Overview

### Parsing Backends

Sipha supports multiple parsing algorithms via feature flags:

- **LL(k) Parser** (`backend-ll`): Top-down predictive parsing with configurable lookahead
  - Supports left-recursion elimination
  - Configurable error recovery
  - Incremental parsing support

- **LR Parser** (`backend-lr`): Bottom-up shift-reduce parsing
  - Efficient for many grammar types
  - Good error recovery

- **GLR Parser** (`backend-glr`): Generalized LR parsing for ambiguous grammars
  - Handles non-deterministic and ambiguous grammars
  - Parse forest representation for ambiguity tracking
  - Configurable disambiguation strategies (precedence, associativity)
  - Incremental parsing support
  - Ideal for complex languages like C++ with inherent ambiguities

- **Planned**: PEG, Packrat, Earley parsers

### Syntax Trees

Sipha uses an immutable **green/red tree** representation:

- **Green trees**: Compact, shared representation stored in an arena
- **Red trees**: Convenient API for traversing and querying the tree
- **Efficient memory usage**: Shared subtrees reduce memory footprint
- **Fast traversal**: Optimized for common tree operations

### Error Recovery

Sipha provides configurable error recovery strategies:

- **Synchronization tokens**: Skip to known recovery points
- **Delimited recovery**: Skip to matching delimiters
- **Best-effort parsing**: Continue parsing despite errors

## Examples

The repository includes several examples to help you get started:

- **[Basic Arithmetic](crates/sipha/examples/basic_arithmetic.rs)**: A step-by-step arithmetic expression parser
- **[Simple Calculator](crates/sipha/examples/simple_calculator.rs)**: A more complete calculator with error handling

Run examples with:

```bash
cargo run --example basic_arithmetic
cargo run --example simple_calculator
```

## API Overview

### Core Modules

- **`syntax`**: Syntax tree types (green/red trees, nodes, tokens)
- **`grammar`**: Grammar definition and validation
- **`lexer`**: Tokenization and lexing
- **`parser`**: Parser traits and interfaces
- **`backend`**: Parser backend implementations (LL, LR, etc.)
- **`error`**: Error types and diagnostics
- **`incremental`**: Incremental parsing support

### Key Types

- `SyntaxKind`: Trait for syntax node kinds
- `Token`: Trait for tokens
- `NonTerminal`: Trait for grammar non-terminals
- `GrammarBuilder`: Build grammars declaratively
- `LexerBuilder`: Build lexers with pattern matching
- `ParserBackend`: Unified interface for all parser backends
- `SyntaxNode`: Red tree node for traversal
- `GreenNode`: Green tree node for storage

## Performance

Incremental parsing provides significant performance benefits:

- **Fast updates**: Only re-parse changed regions
- **Memory efficient**: Shared tree representation
- **Scalable**: Handles large files efficiently
- **IDE-ready**: Designed for interactive editing scenarios

For batch parsing (non-interactive), Sipha is competitive with other Rust parsing libraries. The incremental parsing capabilities make it particularly well-suited for language servers and editors.

## Comparison with Alternatives

| Feature | Sipha | pest | nom | lalrpop |
|---------|-------|------|-----|--------|
| Incremental parsing | ✅ | ❌ | ❌ | ❌ |
| Multiple backends | ✅ | ❌ | ❌ | ❌ |
| Syntax trees | ✅ | ✅ | ❌ | ✅ |
| Error recovery | ✅ | ✅ | ✅ | ✅ |
| Grammar DSL | ❌ | ✅ | ❌ | ✅ |
| Zero-copy | Partial | ✅ | ✅ | ❌ |

**When to use Sipha:**
- Building language servers or IDEs
- Need incremental parsing for interactive editing
- Want flexibility in choosing parsing algorithms
- Need rich syntax tree manipulation
- Parsing ambiguous grammars (use GLR backend)

**When to consider alternatives:**
- Simple one-off parsers (pest or nom might be simpler)
- Maximum performance for batch parsing (nom might be faster)
- Prefer declarative grammar DSLs (pest or lalrpop)

## Future Features

Sipha continues to evolve. Planned features for future releases include:

### Short Term (Post-0.5.0)
- **Enhanced error recovery**: More sophisticated recovery strategies
- **Better diagnostics**: Improved error messages and suggestions
- **Performance optimizations**: Further speed improvements for large files

### Medium Term
- **Additional backends**: PEG, Packrat, and Earley parsers
- **Grammar visualization**: Interactive tools for visualizing and debugging grammars
- **Incremental lexing**: Extend incremental capabilities to the lexer for even better performance

### Long Term
- **Language server framework**: Higher-level abstractions for building language servers
- **Parallel parsing**: Parse multiple files in parallel
- **Advanced grammar analysis**: Real-time grammar optimization suggestions

## Contributing

Contributions are welcome! Sipha 0.5.0 provides a solid foundation, and there are many opportunities to help:

- **Bug reports**: Found a bug? Please open an issue!
- **Feature requests**: Have an idea? We'd love to hear it!
- **Code contributions**: Pull requests are welcome
- **Documentation**: Help improve our docs and examples
- **Testing**: Help expand our test coverage

### Development Setup

```bash
# Clone the repository
git clone https://github.com/yourusername/sipha.git
cd sipha

# Run tests
cargo test

# Run examples
cargo run --example basic_arithmetic

# Build documentation
cargo doc --open
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Documentation

- **[The Sipha Book](https://sipha-parser.github.io/sipha/)**: Comprehensive guide and documentation
- **[API Documentation](https://docs.rs/sipha)**: Full API reference on docs.rs
- **[Examples](crates/sipha/examples/)**: Working examples in the repository
- **[Source Code](https://github.com/yourusername/sipha)**: Browse the source on GitHub

## Acknowledgments

Sipha is inspired by:
- [rust-analyzer](https://github.com/rust-lang/rust-analyzer) for incremental parsing ideas
- [rowan](https://github.com/rust-analyzer/rowan) for green/red tree design
- Various parsing libraries in the Rust ecosystem

---

**Note**: Sipha 0.5.0 provides a complete incremental parsing solution. The core API is stable, and we continue to add features and improvements based on user feedback.

