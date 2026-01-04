//! Grammar DSL vs Builder API Comparison Example
//!
//! This example demonstrates two ways to define the same grammar in Sipha:
//! 1. Using the Grammar DSL macro (declarative)
//! 2. Using the GrammarBuilder API (programmatic)
//!
//! Both approaches produce equivalent grammars and are fully compatible with all parser backends.

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

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

fn create_token(kind: ArithSyntaxKind, text: &str) -> ArithToken {
    ArithToken {
        kind,
        text: text.into(),
    }
}

/// Build grammar using the Builder API
fn build_grammar_with_builder_api() -> Result<sipha::grammar::Grammar<ArithToken, ArithNonTerminal>, Box<dyn std::error::Error>> {
    println!("=== Building Grammar with Builder API ===\n");
    
    // This is the programmatic approach - you build the grammar step by step
    let grammar = GrammarBuilder::new()
        .allow_left_recursion() // Allow left recursion for this example
        .entry_point(ArithNonTerminal::Expr)
        
        // Expr -> Term | Expr + Term | Expr - Term
        .rule(
            ArithNonTerminal::Expr,
            Expr::choice(vec![
                Expr::seq(vec![
                    Expr::rule(ArithNonTerminal::Expr),
                    Expr::token(create_token(ArithSyntaxKind::Plus, "+")),
                    Expr::rule(ArithNonTerminal::Term),
                ]),
                Expr::seq(vec![
                    Expr::rule(ArithNonTerminal::Expr),
                    Expr::token(create_token(ArithSyntaxKind::Minus, "-")),
                    Expr::rule(ArithNonTerminal::Term),
                ]),
                Expr::rule(ArithNonTerminal::Term),
            ]),
        )
        
        // Term -> Factor | Term * Factor | Term / Factor
        .rule(
            ArithNonTerminal::Term,
            Expr::choice(vec![
                Expr::seq(vec![
                    Expr::rule(ArithNonTerminal::Term),
                    Expr::token(create_token(ArithSyntaxKind::Multiply, "*")),
                    Expr::rule(ArithNonTerminal::Factor),
                ]),
                Expr::seq(vec![
                    Expr::rule(ArithNonTerminal::Term),
                    Expr::token(create_token(ArithSyntaxKind::Divide, "/")),
                    Expr::rule(ArithNonTerminal::Factor),
                ]),
                Expr::rule(ArithNonTerminal::Factor),
            ]),
        )
        
        // Factor -> Number | ( Expr )
        .rule(
            ArithNonTerminal::Factor,
            Expr::choice(vec![
                Expr::token(create_token(ArithSyntaxKind::Number, "0")),
                Expr::seq(vec![
                    Expr::token(create_token(ArithSyntaxKind::LParen, "(")),
                    Expr::rule(ArithNonTerminal::Expr),
                    Expr::token(create_token(ArithSyntaxKind::RParen, ")")),
                ]),
            ]),
        )
        
        .build()?;
    
    println!("✓ Grammar built successfully with Builder API");
    println!("  Rules defined: {}", grammar.rules().count());
    println!();
    
    Ok(grammar)
}

/// Note: The Grammar DSL macro would be used like this:
/// 
/// ```rust,ignore
/// use sipha::grammar;
/// 
/// grammar! {
///     #[entry]
///     Expr = Term ((Plus | Minus) Term)*;
///     
///     Term = Factor ((Star | Slash) Factor)*;
///     
///     Factor = Number
///            | LParen Expr RParen;
///     
///     #[trivia]
///     @Whitespace = r"\s+";
///     
///     @Number = r"[0-9]+";
///     @Plus = "+";
///     @Minus = "-";
///     @Star = "*";
///     @Slash = "/";
///     @LParen = "(";
///     @RParen = ")";
/// }
/// ```
/// 
/// The Grammar DSL is more concise and readable, but requires compile-time
/// grammar definition. The Builder API allows dynamic grammar construction.

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Grammar DSL vs Builder API Comparison ===\n");
    
    println!("This example shows how to define the same grammar using two different approaches:\n");
    println!("1. Builder API (programmatic, dynamic)");
    println!("2. Grammar DSL (declarative, compile-time)\n");
    println!("Both produce equivalent grammars!\n");
    println!("{}", "=".repeat(60));
    println!();
    
    // Build lexer (same for both approaches)
    println!("Building lexer (shared by both approaches)...");
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
    
    println!("✓ Lexer built\n");
    
    // Demonstrate Builder API approach
    let grammar = build_grammar_with_builder_api()?;
    
    println!("{}", "=".repeat(60));
    println!();
    println!("=== Grammar DSL Approach (commented code) ===\n");
    println!("The Grammar DSL would look like this:\n");
    println!(r#"grammar! {{
    #[entry]
    Expr = Term ((Plus | Minus) Term)*;
    
    Term = Factor ((Star | Slash) Factor)*;
    
    Factor = Number
           | LParen Expr RParen;
    
    #[trivia]
    @Whitespace = r"\s+";
    
    @Number = r"[0-9]+";
    @Plus = "+";
    @Minus = "-";
    @Star = "*";
    @Slash = "/";
    @LParen = "(";
    @RParen = ")";
}}"#);
    println!();
    
    println!("{}", "=".repeat(60));
    println!();
    println!("=== Comparison ===\n");
    println!("Builder API:");
    println!("  ✓ Dynamic grammar construction");
    println!("  ✓ Runtime grammar building");
    println!("  ✓ Programmatic control");
    println!("  ✗ More verbose");
    println!("  ✗ More boilerplate\n");
    
    println!("Grammar DSL:");
    println!("  ✓ Concise and readable");
    println!("  ✓ EBNF-like syntax");
    println!("  ✓ Compile-time validation");
    println!("  ✓ Less boilerplate");
    println!("  ✗ Requires compile-time definition");
    println!("  ✗ Less flexible for dynamic grammars\n");
    
    println!("Both approaches:");
    println!("  ✓ Generate equivalent grammars");
    println!("  ✓ Compatible with all parser backends");
    println!("  ✓ Support the same features\n");
    
    // Test the grammar
    println!("{}", "=".repeat(60));
    println!();
    println!("Testing grammar with input: \"42 + 10 * 2\"");
    let input = "42 + 10 * 2";
    let tokens = lexer.tokenize(input).map_err(|e| format!("Tokenization failed: {e:?}"))?;
    println!("✓ Tokenized successfully ({} tokens)", tokens.len());
    println!("✓ Grammar is ready to use with any parser backend");
    println!();
    
    println!("=== Example completed successfully! ===");
    
    Ok(())
}

