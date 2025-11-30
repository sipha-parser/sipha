# Pratt Parser

The Pratt parser is a recursive descent parser that uses operator precedence parsing, making it ideal for parsing expressions with operators.

## Overview

Pratt parsers are particularly well-suited for:

- **Expression parsing**: Natural handling of operator precedence and associativity
- **Operator-heavy languages**: Languages with many infix, prefix, and postfix operators
- **Precedence-based parsing**: Direct support for precedence levels without complex grammar transformations

## How It Works

Pratt parsing uses a recursive descent algorithm with precedence levels:

1. **Precedence-based parsing**: Each operator has a precedence level (higher = binds tighter)
2. **Associativity handling**: Left-associative, right-associative, or non-associative operators
3. **Recursive descent**: Parses expressions by recursively descending through precedence levels

### Example: Expression Parsing

```rust,ignore
// Grammar with PrattOperator expressions
use sipha::grammar::{GrammarBuilder, Expr};
use sipha::grammar::hint::Associativity;

let grammar = GrammarBuilder::new()
    .entry_point(MyNonTerminal::Expr)
    .rule(MyNonTerminal::Expr, 
        Expr::pratt_operator(
            Expr::rule(MyNonTerminal::Term),
            precedence: 10,  // Lower precedence
            associativity: Associativity::Left
        )
    )
    .rule(MyNonTerminal::Term,
        Expr::pratt_operator(
            Expr::rule(MyNonTerminal::Factor),
            precedence: 20,  // Higher precedence
            associativity: Associativity::Left
        )
    )
    .build()?;
```

## Configuration

```rust,ignore
use sipha::backend::pratt::{PrattParser, PrattConfig};
use sipha::backend::ParserBackend;

let config = PrattConfig {
    error_recovery: true,
    max_errors: 100,
    optimize: false,
    optimization_level: OptimizationLevel::None,
};

let mut parser = PrattParser::new(&grammar, config)?;
```

### Configuration Options

- **`error_recovery`**: Enable error recovery during parsing
- **`max_errors`**: Maximum number of errors before giving up
- **`optimize`**: Enable grammar optimization
- **`optimization_level`**: Level of optimization (`None`, `Basic`, or `Aggressive`)

## Grammar Features

The Pratt parser supports:

- ✅ **Core expressions**: All core grammar expressions
- ✅ **PrattOperator**: Native support for operator precedence parsing
- ✅ **Recovery points**: Error recovery markers
- ❌ **Lookahead**: Not supported
- ❌ **Token classes**: Not supported
- ❌ **Semantic predicates**: Not supported
- ❌ **Backreferences**: Not supported

## Operator Precedence

Pratt parsers handle operator precedence naturally through the `PrattOperator` expression:

```rust,ignore
// Define operators with precedence
Expr::pratt_operator(
    inner_expr,
    precedence: 10,           // Precedence level
    associativity: Associativity::Left  // How operators associate
)
```

### Precedence Rules

- **Higher precedence**: Operators with higher precedence values bind tighter
- **Associativity**: 
  - `Left`: `a + b + c` parses as `(a + b) + c`
  - `Right`: `a ^ b ^ c` parses as `a ^ (b ^ c)`
  - `None`: Non-associative operators

## Example: Arithmetic Expressions

```rust,ignore
use sipha::backend::pratt::{PrattParser, PrattConfig};
use sipha::backend::ParserBackend;
use sipha::grammar::{GrammarBuilder, Expr};
use sipha::grammar::hint::Associativity;

// Define tokens
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Token {
    Number(i32),
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
}

// Define non-terminals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NonTerminal {
    Expr,
    Term,
    Factor,
}

// Build grammar with Pratt operators
let grammar = GrammarBuilder::new()
    .entry_point(NonTerminal::Expr)
    .rule(NonTerminal::Expr,
        Expr::pratt_operator(
            Expr::rule(NonTerminal::Term),
            precedence: 10,  // + and - have lower precedence
            associativity: Associativity::Left
        )
    )
    .rule(NonTerminal::Term,
        Expr::pratt_operator(
            Expr::rule(NonTerminal::Factor),
            precedence: 20,  // * and / have higher precedence
            associativity: Associativity::Left
        )
    )
    .rule(NonTerminal::Factor,
        Expr::choice(vec![
            Expr::token(Token::Number(0)),  // Placeholder
            Expr::delimited(
                Expr::token(Token::LParen),
                Expr::rule(NonTerminal::Expr),
                Expr::token(Token::RParen)
            )
        ])
    )
    .build()?;

// Create parser
let config = PrattConfig::default();
let mut parser = PrattParser::new(&grammar, config)?;

// Parse expression
let tokens = vec![
    Token::Number(1),
    Token::Plus,
    Token::Number(2),
    Token::Multiply,
    Token::Number(3),
];

let result = parser.parse(&tokens, NonTerminal::Expr);
// Parses as: 1 + (2 * 3) due to precedence
```

## Error Recovery

Pratt parsers support configurable error recovery:

- **Token skipping**: Skip unexpected tokens and continue
- **Synchronization points**: Skip to known recovery tokens
- **Best-effort parsing**: Continue parsing despite errors

## Performance

- **Parsing time**: O(n) for unambiguous grammars
- **Memory usage**: Small - recursive descent with minimal state
- **Good for**: Expression parsing, operator-heavy languages

## When to Use Pratt Parser

Use the Pratt parser when:

- ✅ You need to parse expressions with operators
- ✅ Operator precedence is important
- ✅ You want natural precedence handling without complex grammar transformations
- ✅ Your grammar is expression-heavy

Consider other backends when:

- ❌ You need lookahead or semantic predicates
- ❌ Your grammar is not expression-focused
- ❌ You need ambiguity handling (use GLR instead)

## Comparison with Other Backends

| Feature | Pratt | LL | LR | PEG |
|---------|-------|----|----|-----|
| Operator precedence | Native | Via grammar | Via grammar | Via ordering |
| Expression parsing | Excellent | Good | Good | Good |
| Complexity | Low | Medium | Medium | Medium |
| Error recovery | Good | Good | Excellent | Good |

## Next Steps

- Learn about [Grammar Transformation](../architecture/module-structure.md) to understand how grammars are transformed
- Check [Choosing a Backend](choosing.md) for guidance on selecting the right parser
- See [Examples](../examples/) for real-world usage patterns

