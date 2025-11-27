//! Basic arithmetic expression parser example
//!
//! This example demonstrates how to:
//! 1. Define syntax kinds for tokens and non-terminals
//! 2. Build a lexer to tokenize input
//! 3. Build a grammar for arithmetic expressions
//! 4. Parse input and handle errors

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
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
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ArithToken {
    kind: ArithSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for ArithToken {
    type Kind = ArithSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(u32::MAX))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum ArithNonTerminal {
    Expr,
    Term,
    Factor,
}

impl NonTerminal for ArithNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }
}

fn create_token(kind: ArithSyntaxKind, text: &str, _offset: u32) -> ArithToken {
    ArithToken {
        kind,
        text: text.into(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Basic Arithmetic Parser Example ===\n");

    // Step 1: Build the lexer
    println!("1. Building lexer...");
    let lexer = LexerBuilder::new()
        .token(
            ArithSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(ArithSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(ArithSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(ArithSyntaxKind::Multiply, Pattern::Literal("*".into()))
        .token(ArithSyntaxKind::Divide, Pattern::Literal("/".into()))
        .token(ArithSyntaxKind::LParen, Pattern::Literal("(".into()))
        .token(ArithSyntaxKind::RParen, Pattern::Literal(")".into()))
        .token(
            ArithSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(ArithSyntaxKind::Whitespace)
        .build(ArithSyntaxKind::Eof, ArithSyntaxKind::Number)?;

    println!("   ✓ Lexer built successfully\n");

    // Step 2: Tokenize input
    println!("2. Tokenizing input: \"42 + 10\"");
    let input = "42 + 10";
    let tokens = lexer
        .tokenize(input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;

    println!("   Tokens found: {}", tokens.len());
    for (i, token) in tokens.iter().enumerate() {
        println!("     [{}] {:?}: \"{}\"", i, token.kind, token.text);
    }
    println!();

    // Step 3: Build grammar
    println!("3. Building grammar...");
    let grammar = GrammarBuilder::new()
        .entry_point(ArithNonTerminal::Expr)
        .rule(
            ArithNonTerminal::Expr,
            Expr::token(create_token(ArithSyntaxKind::Number, "42", 0)),
        )
        .build()?;

    println!("   ✓ Grammar built successfully");
    println!("   Entry point: {:?}\n", grammar.entry_point());

    // Step 4: Display grammar structure
    println!("4. Grammar structure:");
    if let Some(rule) = grammar.get_rule(&ArithNonTerminal::Expr) {
        println!("   Expr -> {:?}", rule.rhs);
    }
    println!();

    println!("=== Example completed successfully! ===");

    Ok(())
}
