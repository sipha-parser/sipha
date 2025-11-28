# Syntax Kinds

Sipha uses a unified `SyntaxKind` trait for both terminals (tokens) and non-terminals (grammar rules). This design simplifies the API and allows for flexible grammar definitions.

## Overview

In traditional parser generators, terminals and non-terminals are separate types. Sipha unifies them into a single `SyntaxKind` enum, which simplifies the API and makes it easier to work with syntax trees.

## The SyntaxKind Trait

The `SyntaxKind` trait is the foundation of Sipha's type system:

```rust
pub trait SyntaxKind: Copy + Clone + PartialEq + Eq + Hash + Debug {
    fn is_terminal(self) -> bool;
    fn is_trivia(self) -> bool;
}
```

### Required Methods

- `is_terminal(self) -> bool`: Returns `true` if this kind represents a terminal (token), `false` if it's a non-terminal (grammar rule).
- `is_trivia(self) -> bool`: Returns `true` if this kind represents trivia (whitespace, comments) that should be ignored during parsing.

## Defining Your Syntax Kinds

You define syntax kinds as an enum that implements the `SyntaxKind` trait:

```rust
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MySyntaxKind {
    // Terminals (produced by lexer)
    Number,
    Plus,
    Minus,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals (produced by parser)
    Expr,
    Term,
    Factor,
}

impl SyntaxKind for MySyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, 
            MySyntaxKind::Expr | 
            MySyntaxKind::Term | 
            MySyntaxKind::Factor
        )
    }
    
    fn is_trivia(self) -> bool {
        matches!(self, MySyntaxKind::Whitespace)
    }
}
```

## Terminals vs Non-Terminals

### Terminals

Terminals are produced by the lexer and represent actual tokens in the input text:

- Keywords (e.g., `if`, `while`, `return`)
- Operators (e.g., `+`, `-`, `*`, `/`)
- Identifiers (e.g., variable names)
- Literals (e.g., numbers, strings)
- Punctuation (e.g., `(`, `)`, `{`, `}`)
- Trivia (e.g., whitespace, comments)

### Non-Terminals

Non-terminals are produced by the parser and represent grammar rules:

- Expression nodes (e.g., `Expr`, `Term`, `Factor`)
- Statement nodes (e.g., `Stmt`, `IfStmt`, `WhileStmt`)
- Declaration nodes (e.g., `Decl`, `FnDecl`, `VarDecl`)

## Trivia

Trivia are tokens that don't affect the parse tree structure but are preserved for formatting and error messages:

- Whitespace
- Comments
- Line breaks

Mark trivia kinds with `is_trivia()` returning `true`. The parser will automatically skip trivia during parsing, but they'll still be available in the syntax tree.

## Best Practices

1. **Use descriptive names**: Choose names that clearly indicate what the kind represents.
2. **Group related kinds**: Keep terminals and non-terminals organized logically.
3. **Mark trivia correctly**: Ensure `is_trivia()` returns `true` for all trivia kinds.
4. **Use exhaustive matching**: When implementing `is_terminal()`, use exhaustive patterns to catch all cases.

## Example: Complete Syntax Kind Definition

```rust
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum CalculatorSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals
    Expr,
    Term,
    Factor,
}

impl SyntaxKind for CalculatorSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(
            self,
            CalculatorSyntaxKind::Expr
                | CalculatorSyntaxKind::Term
                | CalculatorSyntaxKind::Factor
        )
    }
    
    fn is_trivia(self) -> bool {
        matches!(self, CalculatorSyntaxKind::Whitespace)
    }
}
```

## Next Steps

Now that you understand syntax kinds, you can:

- Learn about [Lexers](lexers.md) to see how terminals are produced
- Explore [Grammars](grammars.md) to see how non-terminals are used
- Check out [Syntax Trees](syntax-trees/green-red-trees.md) to see how kinds are used in trees

