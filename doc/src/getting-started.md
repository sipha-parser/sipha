# Getting Started

This guide will help you get up and running with Sipha. We'll cover installation, a quick start example, and an overview of the core concepts.

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

### Available Features

- `backend-ll`: Enable LL(k) parser backend (default)
- `backend-lr`: Enable LR parser backend
- `backend-glr`: Enable GLR parser backend (requires `backend-lr`)
- `diagnostics`: Enable rich error diagnostics with miette
- `unicode`: Enable full Unicode support for identifiers
- `visitor`: Enable syntax tree visitor patterns
- `query`: Enable XPath-like query API for syntax trees
- `tree-utils`: Enable tree diffing and validation utilities

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
use sipha::grammar::{GrammarBuilder, NonTerminal, Expr};
use sipha::lexer::Token as LexerToken;
use sipha::syntax::{TextRange, TextSize};

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

// Helper function to create tokens with proper ranges
fn create_token(kind: ArithSyntaxKind, text: &str, offset: u32) -> LexerToken<ArithSyntaxKind> {
    let len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
    LexerToken::new(kind, text, TextRange::at(TextSize::from(offset), len))
}

// Build your grammar rules
let grammar = GrammarBuilder::new()
    .entry_point(ArithNonTerminal::Expr)
    // Simple grammar: Expr -> Number
    .rule(ArithNonTerminal::Expr, Expr::token(create_token(
        ArithSyntaxKind::Number,
        "42",
        0
    )))
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

For a complete working example, see the [Basic Arithmetic Example](examples/basic-arithmetic.md) or check out [`examples/basic_arithmetic.rs`](../../crates/sipha/examples/basic_arithmetic.rs) in the repository.

## Core Concepts Overview

Before diving deeper, here's a quick overview of Sipha's core concepts:

### Syntax Kinds

Sipha uses a unified `SyntaxKind` trait for both terminals (tokens) and non-terminals (grammar rules). This design simplifies the API and allows for flexible grammar definitions.

See [Syntax Kinds](core-concepts/syntax-kinds.md) for more details.

### Lexers

Lexers convert raw text into tokens. Sipha provides a flexible lexer builder API that supports:

- Pattern matching (literals, character classes, repetitions)
- Trivia handling (whitespace, comments)
- DFA-based tokenization for performance

See [Lexers](core-concepts/lexers.md) for more details.

### Grammars

Grammars define the structure of your language. Sipha uses a builder API to define grammar rules declaratively.

See [Grammars](core-concepts/grammars.md) for more details.

### Parsing Backends

Sipha supports multiple parsing algorithms:

- **LL(k)**: Top-down predictive parsing
- **LR**: Bottom-up shift-reduce parsing
- **GLR**: Generalized LR for ambiguous grammars

See [Parsing Backends](backends/overview.md) for more details.

### Syntax Trees

Sipha uses an immutable green/red tree representation:

- **Green trees**: Compact, shared representation
- **Red trees**: Convenient API for traversal

See [Syntax Trees](syntax-trees/green-red-trees.md) for more details.

### Incremental Parsing

Sipha's key feature is incremental parsing—the ability to efficiently re-parse only changed regions. This is essential for interactive applications.

See [Incremental Parsing](incremental-parsing/overview.md) for more details.

## Next Steps

Now that you have Sipha installed and understand the basics, you can:

1. Read about [Core Concepts](core-concepts/syntax-kinds.md) in detail
2. Explore [Incremental Parsing](incremental-parsing/overview.md) to understand Sipha's key feature
3. Check out [Examples](examples/basic-arithmetic.md) to see real-world usage
4. Learn about [Parsing Backends](backends/overview.md) to choose the right algorithm

