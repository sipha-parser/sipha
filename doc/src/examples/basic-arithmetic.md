# Basic Arithmetic Example

This example walks through building a simple arithmetic expression parser step by step.

## Overview

We'll build a parser for arithmetic expressions like `42 + 10 * 3` with proper operator precedence.

## Step 1: Define Syntax Kinds

```rust,ignore
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ArithSyntaxKind {
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

impl SyntaxKind for ArithSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, 
            ArithSyntaxKind::Expr | 
            ArithSyntaxKind::Term | 
            ArithSyntaxKind::Factor
        )
    }
    
    fn is_trivia(self) -> bool {
        matches!(self, ArithSyntaxKind::Whitespace)
    }
}
```

## Step 2: Build Lexer

```rust,ignore
# use sipha::syntax::SyntaxKind;
# 
# #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
# enum ArithSyntaxKind {
#     Number, Plus, Minus, Multiply, Divide, LParen, RParen, Whitespace, Eof,
#     Expr, Term, Factor,
# }
# impl SyntaxKind for ArithSyntaxKind {
#     fn is_terminal(self) -> bool { !matches!(self, ArithSyntaxKind::Expr | ArithSyntaxKind::Term | ArithSyntaxKind::Factor) }
#     fn is_trivia(self) -> bool { matches!(self, ArithSyntaxKind::Whitespace) }
# }
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

## Step 3: Define Grammar

```rust,ignore
# use sipha::syntax::SyntaxKind;
# #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
# enum ArithSyntaxKind {
#     Number, Plus, Minus, Multiply, Divide, LParen, RParen, Whitespace, Eof,
#     Expr, Term, Factor,
# }
# impl SyntaxKind for ArithSyntaxKind {
#     fn is_terminal(self) -> bool { !matches!(self, ArithSyntaxKind::Expr | ArithSyntaxKind::Term | ArithSyntaxKind::Factor) }
#     fn is_trivia(self) -> bool { matches!(self, ArithSyntaxKind::Whitespace) }
# }
use sipha::grammar::{GrammarBuilder, NonTerminal, Expr};
use sipha::lexer::Token as LexerToken;
use sipha::syntax::{TextRange, TextSize};
use std::convert::TryFrom;

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

## Step 4: Parse

```rust,ignore
# use sipha::syntax::SyntaxKind;
# #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
# enum ArithSyntaxKind {
#     Number, Plus, Minus, Multiply, Divide, LParen, RParen, Whitespace, Eof,
#     Expr, Term, Factor,
# }
# impl SyntaxKind for ArithSyntaxKind {
#     fn is_terminal(self) -> bool { !matches!(self, ArithSyntaxKind::Expr | ArithSyntaxKind::Term | ArithSyntaxKind::Factor) }
#     fn is_trivia(self) -> bool { matches!(self, ArithSyntaxKind::Whitespace) }
# }
# use sipha::lexer::{LexerBuilder, Pattern, CharSet};
# let lexer = LexerBuilder::new()
#     .token(ArithSyntaxKind::Number, Pattern::Repeat {
#         pattern: Box::new(Pattern::CharClass(CharSet::digits())),
#         min: 1, max: None,
#     })
#     .token(ArithSyntaxKind::Plus, Pattern::Literal("+".into()))
#     .token(ArithSyntaxKind::Minus, Pattern::Literal("-".into()))
#     .token(ArithSyntaxKind::Multiply, Pattern::Literal("*".into()))
#     .token(ArithSyntaxKind::Divide, Pattern::Literal("/".into()))
#     .token(ArithSyntaxKind::LParen, Pattern::Literal("(".into()))
#     .token(ArithSyntaxKind::RParen, Pattern::Literal(")".into()))
#     .token(ArithSyntaxKind::Whitespace, Pattern::Repeat {
#         pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
#         min: 1, max: None,
#     })
#     .trivia(ArithSyntaxKind::Whitespace)
#     .build(ArithSyntaxKind::Eof, ArithSyntaxKind::Number)
#     .expect("Failed to build lexer");
# use sipha::grammar::{GrammarBuilder, NonTerminal, Expr};
# use sipha::lexer::Token as LexerToken;
# use sipha::syntax::{TextRange, TextSize};
# use std::convert::TryFrom;
# #[derive(Debug, Clone, PartialEq, Eq, Hash)]
# enum ArithNonTerminal { Expr, Term, Factor, }
# impl NonTerminal for ArithNonTerminal {
#     fn name(&self) -> &str { match self { ArithNonTerminal::Expr => "Expr", ArithNonTerminal::Term => "Term", ArithNonTerminal::Factor => "Factor", } }
# }
# fn create_token(kind: ArithSyntaxKind, text: &str, offset: u32) -> LexerToken<ArithSyntaxKind> {
#     let len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
#     LexerToken::new(kind, text, TextRange::at(TextSize::from(offset), len))
# }
# let grammar = GrammarBuilder::new()
#     .entry_point(ArithNonTerminal::Expr)
#     .rule(ArithNonTerminal::Expr, Expr::choice(vec![
#         Expr::seq(vec![Expr::non_terminal(ArithNonTerminal::Expr), Expr::token(create_token(ArithSyntaxKind::Plus, "+", 0)), Expr::non_terminal(ArithNonTerminal::Term)]),
#         Expr::seq(vec![Expr::non_terminal(ArithNonTerminal::Expr), Expr::token(create_token(ArithSyntaxKind::Minus, "-", 0)), Expr::non_terminal(ArithNonTerminal::Term)]),
#         Expr::non_terminal(ArithNonTerminal::Term),
#     ]))
#     .rule(ArithNonTerminal::Term, Expr::choice(vec![
#         Expr::seq(vec![Expr::non_terminal(ArithNonTerminal::Term), Expr::token(create_token(ArithSyntaxKind::Multiply, "*", 0)), Expr::non_terminal(ArithNonTerminal::Factor)]),
#         Expr::seq(vec![Expr::non_terminal(ArithNonTerminal::Term), Expr::token(create_token(ArithSyntaxKind::Divide, "/", 0)), Expr::non_terminal(ArithNonTerminal::Factor)]),
#         Expr::non_terminal(ArithNonTerminal::Factor),
#     ]))
#     .rule(ArithNonTerminal::Factor, Expr::choice(vec![
#         Expr::token(create_token(ArithSyntaxKind::Number, "42", 0)),
#         Expr::seq(vec![Expr::token(create_token(ArithSyntaxKind::LParen, "(", 0)), Expr::non_terminal(ArithNonTerminal::Expr), Expr::token(create_token(ArithSyntaxKind::RParen, ")", 0))]),
#     ]))
#     .build().expect("Failed to build grammar");
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;
use sipha::syntax::SyntaxNode;

let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)
    .expect("Failed to create parser");

let input = "42 + 10 * 3";
let tokens = lexer.tokenize(input)
    .expect("Failed to tokenize");

let result = parser.parse(&tokens, ArithNonTerminal::Expr);

if !result.errors.is_empty() {
    eprintln!("Errors: {:?}", result.errors);
} else {
    let root = SyntaxNode::new_root(result.root.clone());
    println!("Parse successful!");
    println!("Root: {:?}", root.kind());
}
```

## Complete Example

See [`examples/basic_arithmetic.rs`](../../crates/sipha/examples/basic_arithmetic.rs) for the complete working example.

## Next Steps

- Try [Incremental Parsing Example](incremental-example.md)
- Check [GLR Example](glr-example.md) for ambiguous grammars

