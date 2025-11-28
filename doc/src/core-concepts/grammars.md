# Grammars

Grammars define the structure of your language. Sipha uses a builder API to define grammar rules declaratively.

## Overview

A grammar in Sipha consists of:

- **Non-terminals**: Grammar rules (e.g., `Expr`, `Stmt`, `Decl`)
- **Production rules**: How non-terminals are expanded (e.g., `Expr -> Term + Expr`)
- **Entry point**: The starting non-terminal for parsing

## GrammarBuilder API

Use `GrammarBuilder` to construct grammars:

```rust
use sipha::grammar::{GrammarBuilder, Expr, NonTerminal, Token};

let grammar = GrammarBuilder::new()
    .entry_point(MyNonTerminal::Expr)
    .rule(MyNonTerminal::Expr, Expr::token(some_token))
    .build()
    .expect("Failed to build grammar");
```

## Grammar Expressions

Grammar rules use `Expr` to define right-hand sides:

### Token Expressions

Match a specific token:

```rust
use sipha::grammar::Expr;
use sipha::lexer::Token;
use sipha::syntax::{TextRange, TextSize};

let token = Token::new(
    MySyntaxKind::Number,
    "42",
    TextRange::at(TextSize::zero(), TextSize::from(2))
);
.rule(MyNonTerminal::Factor, Expr::token(token))
```

### Sequence Expressions

Match a sequence of expressions:

```rust
.rule(MyNonTerminal::Expr, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::Term),
    Expr::token(plus_token),
    Expr::non_terminal(MyNonTerminal::Expr),
]))
```

### Choice Expressions

Match one of several alternatives:

```rust
.rule(MyNonTerminal::Expr, Expr::choice(vec![
    Expr::non_terminal(MyNonTerminal::Term),
    Expr::non_terminal(MyNonTerminal::Expr),
]))
```

### Optional Expressions

Match an optional expression:

```rust
.rule(MyNonTerminal::Stmt, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::If),
    Expr::optional(Box::new(Expr::non_terminal(MyNonTerminal::Else))),
]))
```

### Repeat Expressions

Match zero or more repetitions:

```rust
// Zero or more
.rule(MyNonTerminal::Args, Expr::repeat(
    Box::new(Expr::non_terminal(MyNonTerminal::Arg)),
    0,
    None,
))

// One or more
.rule(MyNonTerminal::Stmts, Expr::repeat(
    Box::new(Expr::non_terminal(MyNonTerminal::Stmt)),
    1,
    None,
))
```

### Empty Expression

Match nothing (epsilon):

```rust
.rule(MyNonTerminal::Optional, Expr::Empty)
```

## Complete Example: Arithmetic Grammar

```rust
use sipha::grammar::{GrammarBuilder, Expr, NonTerminal};
use sipha::lexer::Token as LexerToken;
use sipha::syntax::{TextRange, TextSize};

// Helper function to create tokens with proper ranges
fn create_token(kind: ArithSyntaxKind, text: &str, offset: u32) -> LexerToken<ArithSyntaxKind> {
    let len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
    LexerToken::new(kind, text, TextRange::at(TextSize::from(offset), len))
}

let grammar = GrammarBuilder::new()
    .entry_point(ArithNonTerminal::Expr)
    
    // Expr -> Term | Expr + Term | Expr - Term
    .rule(ArithNonTerminal::Expr, Expr::choice(vec![
        Expr::seq(vec![
            Expr::non_terminal(ArithNonTerminal::Expr),
            Expr::token(create_token(ArithSyntaxKind::Plus, "+", 0)),
            Expr::non_terminal(ArithNonTerminal::Term),
        ]),
        Expr::seq(vec![
            Expr::non_terminal(ArithNonTerminal::Expr),
            Expr::token(create_token(ArithSyntaxKind::Minus, "-", 0)),
            Expr::non_terminal(ArithNonTerminal::Term),
        ]),
        Expr::non_terminal(ArithNonTerminal::Term),
    ]))
    
    // Term -> Factor | Term * Factor | Term / Factor
    .rule(ArithNonTerminal::Term, Expr::choice(vec![
        Expr::seq(vec![
            Expr::non_terminal(ArithNonTerminal::Term),
            Expr::token(create_token(ArithSyntaxKind::Multiply, "*", 0)),
            Expr::non_terminal(ArithNonTerminal::Factor),
        ]),
        Expr::seq(vec![
            Expr::non_terminal(ArithNonTerminal::Term),
            Expr::token(create_token(ArithSyntaxKind::Divide, "/", 0)),
            Expr::non_terminal(ArithNonTerminal::Factor),
        ]),
        Expr::non_terminal(ArithNonTerminal::Factor),
    ]))
    
    // Factor -> Number | ( Expr )
    .rule(ArithNonTerminal::Factor, Expr::choice(vec![
        Expr::token(create_token(ArithSyntaxKind::Number, "42", 0)),
        Expr::seq(vec![
            Expr::token(create_token(ArithSyntaxKind::LParen, "(", 0)),
            Expr::non_terminal(ArithNonTerminal::Expr),
            Expr::token(create_token(ArithSyntaxKind::RParen, ")", 0)),
        ]),
    ]))
    
    .build()
    .expect("Failed to build grammar");
```

## Grammar Validation

Sipha validates grammars before use:

```rust
let grammar = GrammarBuilder::new()
    .entry_point(MyNonTerminal::Expr)
    .rule(MyNonTerminal::Expr, Expr::token(some_token))
    .build()
    .expect("Failed to build grammar");

// Validate grammar for a specific backend
let errors = LlParser::validate(&grammar);
if !errors.is_empty() {
    for error in errors {
        eprintln!("Grammar error: {:?}", error);
    }
}
```

Common validation errors:

- **Left recursion**: Some backends don't support left recursion
- **Ambiguity**: Some backends require unambiguous grammars
- **Missing rules**: Referenced non-terminals must be defined
- **Unreachable rules**: Rules that can't be reached from the entry point

## Backend Hints

Provide hints to backends about how to handle rules:

```rust
use sipha::grammar::hint::PrecedenceHint;

.rule(MyNonTerminal::Expr, Expr::choice(vec![
    // Higher precedence
    Expr::seq(vec![
        Expr::non_terminal(MyNonTerminal::Term),
        Expr::token(multiply_token),
        Expr::non_terminal(MyNonTerminal::Term),
    ]).with_hint(PrecedenceHint::new(2)),
    // Lower precedence
    Expr::seq(vec![
        Expr::non_terminal(MyNonTerminal::Expr),
        Expr::token(plus_token),
        Expr::non_terminal(MyNonTerminal::Term),
    ]).with_hint(PrecedenceHint::new(1)),
]))
```

## Grammar Analysis

Sipha provides tools for analyzing grammars:

- **Left recursion detection**: Find rules with left recursion
- **Reachability analysis**: Find unreachable rules
- **First/Follow sets**: Compute FIRST and FOLLOW sets for LL parsing
- **Conflict detection**: Find shift/reduce and reduce/reduce conflicts

See [Grammar Analysis](advanced/grammar-analysis.md) for more details.

## Best Practices

1. **Start simple**: Begin with a simple grammar and add complexity gradually
2. **Use meaningful names**: Choose clear names for non-terminals
3. **Factor common patterns**: Extract common subexpressions into separate rules
4. **Handle precedence explicitly**: Use precedence hints or grammar structure
5. **Test thoroughly**: Test with various inputs, including edge cases
6. **Validate early**: Validate grammars before using them in parsers

## Next Steps

- Learn about [Parsing Basics](parsing-basics.md) to see how grammars are used
- Explore [Parsing Backends](../backends/overview.md) to understand backend-specific requirements
- Check out [Grammar Analysis](../advanced/grammar-analysis.md) for advanced grammar tools

## See Also

- [Cheat Sheet](../reference/cheat-sheet.md) - Quick reference for grammar expressions
- [Examples](../examples/basic-arithmetic.md) - Complete grammar examples
- [Glossary](../reference/glossary.md) - Definitions of grammar-related terms

