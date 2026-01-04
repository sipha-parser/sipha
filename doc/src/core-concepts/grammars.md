# Grammars

Grammars define the structure of your language. Sipha provides two ways to define grammars: a **Grammar DSL** macro for declarative definitions, and a **Builder API** for programmatic construction.

## Overview

A grammar in Sipha consists of:

- **Non-terminals**: Grammar rules (e.g., `Expr`, `Stmt`, `Decl`)
- **Production rules**: How non-terminals are expanded (e.g., `Expr -> Term + Expr`)
- **Entry point**: The starting non-terminal for parsing

## Grammar DSL

The **Grammar DSL** is a declarative macro that lets you define grammars using an EBNF-like syntax. It's the recommended approach for most use cases as it's more concise and readable.

### Basic Usage

```rust,ignore
use sipha::grammar;

grammar! {
    #[entry]
    Expr = Term ((Plus | Minus) Term)*;
    
    Term = Factor ((Star | Slash) Factor)*;
    
    Factor = Number
           | Ident
           | LParen Expr RParen;
    
    // Token patterns
    #[trivia]
    @Whitespace = r"\s+";
    
    @Number = r"[0-9]+";
    @Ident = r"[a-zA-Z_][a-zA-Z0-9_]*";
    @Plus = "+";
    @Minus = "-";
    @Star = "*";
    @Slash = "/";
    @LParen = "(";
    @RParen = ")";
}
```

### Grammar DSL Syntax

- **Rules**: `RuleName = Expression;`
- **Entry point**: Mark a rule with `#[entry]` to make it the starting point
- **Alternatives**: Use `|` to separate alternatives: `A | B | C`
- **Sequences**: Space-separated expressions form sequences: `A B C`
- **Repetition**: 
  - `*` for zero or more: `A*`
  - `+` for one or more: `A+`
  - `?` for optional: `A?`
- **Grouping**: Use parentheses: `(A | B) C`
- **Token definitions**: Use `@TokenName = pattern;` to define tokens
- **Trivia**: Mark tokens as trivia with `#[trivia]` attribute

### Token Patterns

Tokens can be defined with:
- **Literal strings**: `@Plus = "+";`
- **Raw string literals**: `@Number = r"[0-9]+";` (regex patterns)
- **Trivia tokens**: `#[trivia] @Whitespace = r"\s+";`

### When to Use Grammar DSL

Use the Grammar DSL when:
- You want a concise, readable grammar definition
- You prefer declarative syntax similar to EBNF
- Your grammar is relatively static
- You want compile-time grammar validation

The Grammar DSL generates the same grammar structure as the Builder API and is fully compatible with all parser backends.

## GrammarBuilder API

The **Builder API** provides programmatic control over grammar construction. Use it when you need dynamic grammar building or runtime grammar generation.

Use `GrammarBuilder` to construct grammars:

```rust,ignore
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

```rust,ignore
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

```rust,ignore
.rule(MyNonTerminal::Expr, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::Term),
    Expr::token(plus_token),
    Expr::non_terminal(MyNonTerminal::Expr),
]))
```

### Choice Expressions

Match one of several alternatives:

```rust,ignore
.rule(MyNonTerminal::Expr, Expr::choice(vec![
    Expr::non_terminal(MyNonTerminal::Term),
    Expr::non_terminal(MyNonTerminal::Expr),
]))
```

### Optional Expressions

Match an optional expression:

```rust,ignore
.rule(MyNonTerminal::Stmt, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::If),
    Expr::optional(Box::new(Expr::non_terminal(MyNonTerminal::Else))),
]))
```

### Repeat Expressions

Match zero or more repetitions:

```rust,ignore
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

```rust,ignore
.rule(MyNonTerminal::Optional, Expr::Empty)
```

## Complete Example: Arithmetic Grammar

```rust,ignore
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

```rust,ignore
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

```rust,ignore
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

## Choosing Between Grammar DSL and Builder API

### Use Grammar DSL when:
- You want a concise, readable grammar definition
- You prefer declarative syntax similar to EBNF
- Your grammar is relatively static
- You want compile-time grammar validation

### Use Builder API when:
- You need dynamic grammar construction
- You want programmatic control over rule creation
- You're building grammars at runtime
- You need to conditionally add rules based on configuration

Both approaches generate the same grammar structure and are fully compatible with all parser backends. You can even mix them if needed.

## Best Practices

1. **Start simple**: Begin with a simple grammar and add complexity gradually
2. **Use meaningful names**: Choose clear names for non-terminals
3. **Factor common patterns**: Extract common subexpressions into separate rules
4. **Handle precedence explicitly**: Use precedence hints or grammar structure
5. **Test thoroughly**: Test with various inputs, including edge cases
6. **Validate early**: Validate grammars before using them in parsers
7. **Prefer Grammar DSL**: For most use cases, the Grammar DSL is more readable and maintainable

## Next Steps

- Learn about [Parsing Basics](parsing-basics.md) to see how grammars are used
- Explore [Parsing Backends](../backends/overview.md) to understand backend-specific requirements
- Check out [Grammar Analysis](../advanced/grammar-analysis.md) for advanced grammar tools

## See Also

- [Cheat Sheet](../reference/cheat-sheet.md) - Quick reference for grammar expressions
- [Examples](../examples/basic-arithmetic.md) - Complete grammar examples
- [Glossary](../reference/glossary.md) - Definitions of grammar-related terms

