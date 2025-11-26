//! Simple calculator parser example
//!
//! This example shows a more complete calculator with error handling

use sipha::grammar::{GrammarBuilder, Token, NonTerminal, Expr};
use sipha::lexer::{LexerBuilder, Pattern, CharSet};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum CalcSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    Whitespace,
    Eof,
    // Non-terminals
    Expr,
}

impl SyntaxKind for CalcSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }
    
    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CalcToken {
    kind: CalcSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for CalcToken {
    type Kind = CalcSyntaxKind;
    
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
enum CalcNonTerminal {
    Expr,
}

impl NonTerminal for CalcNonTerminal {
    fn name(&self) -> &'static str {
        "Expr"
    }
}

fn create_token(kind: CalcSyntaxKind, text: &str, _offset: u32) -> CalcToken {
    CalcToken {
        kind,
        text: text.into(),
    }
}

fn main() {
    println!("=== Simple Calculator Parser ===\n");
    
    // Build lexer
    let lexer = match LexerBuilder::new()
        .token(CalcSyntaxKind::Number, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::digits())),
            min: 1,
            max: None,
        })
        .token(CalcSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(CalcSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(CalcSyntaxKind::Multiply, Pattern::Literal("*".into()))
        .token(CalcSyntaxKind::Divide, Pattern::Literal("/".into()))
        .token(CalcSyntaxKind::Whitespace, Pattern::Repeat {
            pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
            min: 1,
            max: None,
        })
        .trivia(CalcSyntaxKind::Whitespace)
        .build(CalcSyntaxKind::Eof, CalcSyntaxKind::Number)
    {
        Ok(lexer) => lexer,
        Err(e) => {
            eprintln!("Failed to build lexer: {e}");
            return;
        }
    };
    
    // Test cases
    let test_cases = vec![
        "42",
        "10 + 20",
        "5 * 3",
        "100 / 2",
        "1 + 2 + 3",
    ];
    
    for input in test_cases {
        println!("Input: \"{input}\"");
        
        match lexer.tokenize(input) {
            Ok(tokens) => {
                println!("  Tokens: {}", tokens.len());
                for token in &tokens {
                    if token.kind != CalcSyntaxKind::Eof {
                        print!("    {:?} ", token.kind);
                    }
                }
                println!();
            }
            Err(e) => {
                println!("  Error: {e:?}");
            }
        }
        println!();
    }
    
    // Build grammar
    let grammar = match GrammarBuilder::new()
        .entry_point(CalcNonTerminal::Expr)
        .rule(CalcNonTerminal::Expr, Expr::token(create_token(
            CalcSyntaxKind::Number,
            "42",
            0,
        )))
        .build()
    {
        Ok(grammar) => grammar,
        Err(e) => {
            eprintln!("Failed to build grammar: {e:?}");
            return;
        }
    };
    
    println!("Grammar entry point: {:?}", grammar.entry_point());
    println!("\n=== Example completed ===");
}
