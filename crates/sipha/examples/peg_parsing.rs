//! PEG (Parsing Expression Grammar) parsing example
//!
//! This example demonstrates how to:
//! 1. Use PEG parser with ordered choice semantics
//! 2. Enable memoization (packrat parsing) for performance
//! 3. Use incremental parsing with PEG
//! 4. Compare PEG vs other backends for specific use cases
//!
//! This example requires the `backend-peg` feature to be enabled.
//! Run with: `cargo run --example peg_parsing --features backend-peg`

#[cfg(not(feature = "backend-peg"))]
fn main() {
    eprintln!("This example requires the 'backend-peg' feature to be enabled.");
    eprintln!("Run with: cargo run --example peg_parsing --features backend-peg");
    std::process::exit(1);
}

#[cfg(feature = "backend-peg")]
use sipha::backend::ParserBackend;
#[cfg(feature = "backend-peg")]
use sipha::backend::peg::{PegConfig, PegParser};
#[cfg(feature = "backend-peg")]
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal};
#[cfg(feature = "backend-peg")]
use sipha::incremental::{IncrementalParser, TextEdit};
#[cfg(feature = "backend-peg")]
use sipha::lexer::{CharSet, LexerBuilder, Pattern, Token};
#[cfg(feature = "backend-peg")]
use sipha::syntax::{SyntaxKind, TextRange, TextSize};
#[cfg(feature = "backend-peg")]
use std::time::Instant;

#[cfg(feature = "backend-peg")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum PegSyntaxKind {
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
    #[allow(dead_code)]
    Expr,
    #[allow(dead_code)]
    Term,
    #[allow(dead_code)]
    Factor,
}

#[cfg(feature = "backend-peg")]
impl SyntaxKind for PegSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[cfg(feature = "backend-peg")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PegNonTerminal {
    Expr,
    Term,
    Factor,
}

#[cfg(feature = "backend-peg")]
impl NonTerminal for PegNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }
}

#[cfg(feature = "backend-peg")]
fn create_token(kind: PegSyntaxKind, text: &str) -> Token<PegSyntaxKind> {
    Token::new(
        kind,
        text,
        TextRange::at(
            TextSize::zero(),
            TextSize::from(u32::try_from(text.len()).unwrap_or(0)),
        ),
    )
}

#[cfg(feature = "backend-peg")]
#[allow(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== PEG Parsing Example ===\n");

    // Step 1: Build lexer
    println!("1. Building lexer...");
    let lexer = LexerBuilder::new()
        .token(
            PegSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(PegSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(PegSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(PegSyntaxKind::Multiply, Pattern::Literal("*".into()))
        .token(PegSyntaxKind::Divide, Pattern::Literal("/".into()))
        .token(PegSyntaxKind::LParen, Pattern::Literal("(".into()))
        .token(PegSyntaxKind::RParen, Pattern::Literal(")".into()))
        .token(
            PegSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(PegSyntaxKind::Whitespace)
        .build(PegSyntaxKind::Eof, PegSyntaxKind::Number)?;

    println!("   ✓ Lexer built successfully\n");

    // Step 2: Build grammar with ordered choice
    // PEG uses ordered choice: first match wins
    // This grammar demonstrates precedence through ordering
    println!("2. Building grammar with ordered choice (PEG semantics)...");
    let grammar = GrammarBuilder::<Token<PegSyntaxKind>, PegNonTerminal>::new()
        .entry_point(PegNonTerminal::Expr)
        .rule(
            PegNonTerminal::Expr,
            Expr::Choice(vec![
                // Expr -> Expr + Term (lower precedence, tried first)
                Expr::Seq(vec![
                    Expr::Rule(PegNonTerminal::Expr),
                    Expr::token(create_token(PegSyntaxKind::Plus, "+")),
                    Expr::Rule(PegNonTerminal::Term),
                ]),
                // Expr -> Expr - Term
                Expr::Seq(vec![
                    Expr::Rule(PegNonTerminal::Expr),
                    Expr::token(create_token(PegSyntaxKind::Minus, "-")),
                    Expr::Rule(PegNonTerminal::Term),
                ]),
                // Expr -> Term (base case, tried last)
                Expr::Rule(PegNonTerminal::Term),
            ]),
        )
        .rule(
            PegNonTerminal::Term,
            Expr::Choice(vec![
                // Term -> Term * Factor (higher precedence)
                Expr::Seq(vec![
                    Expr::Rule(PegNonTerminal::Term),
                    Expr::token(create_token(PegSyntaxKind::Multiply, "*")),
                    Expr::Rule(PegNonTerminal::Factor),
                ]),
                // Term -> Term / Factor
                Expr::Seq(vec![
                    Expr::Rule(PegNonTerminal::Term),
                    Expr::token(create_token(PegSyntaxKind::Divide, "/")),
                    Expr::Rule(PegNonTerminal::Factor),
                ]),
                // Term -> Factor (base case)
                Expr::Rule(PegNonTerminal::Factor),
            ]),
        )
        .rule(
            PegNonTerminal::Factor,
            Expr::Choice(vec![
                // Factor -> ( Expr )
                Expr::Seq(vec![
                    Expr::token(create_token(PegSyntaxKind::LParen, "(")),
                    Expr::Rule(PegNonTerminal::Expr),
                    Expr::token(create_token(PegSyntaxKind::RParen, ")")),
                ]),
                // Factor -> Number
                Expr::token(create_token(PegSyntaxKind::Number, "n")),
            ]),
        )
        .build()?;

    println!("   ✓ Grammar built (ordered choice for precedence)\n");

    // Step 3: Tokenize input
    println!("3. Tokenizing input: \"1 + 2 * 3\"");
    let input = "1 + 2 * 3";
    let tokens = lexer
        .tokenize(input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;

    println!(
        "   Tokens: {:?}\n",
        tokens.iter().map(|t| t.text.as_str()).collect::<Vec<_>>()
    );

    // Step 4: Create PEG parser with memoization
    println!("4. Creating PEG parser with memoization (packrat parsing)...");
    let config = PegConfig {
        enable_memoization: true,
        max_memo_size: 10000,
        error_recovery: true,
        max_errors: 100,
        max_backtrack_depth: 1000,
    };
    let parser = PegParser::new(&grammar, config)?;
    let mut incremental = IncrementalParser::new(parser);
    println!("   ✓ PEG parser created with memoization enabled\n");

    // Step 5: Parse with PEG
    println!("5. Parsing with PEG...");
    let start = Instant::now();
    let result = incremental.parse_incremental(
        tokens.as_slice(),
        None,
        &[],
        PegNonTerminal::Expr,
        Some(&grammar),
    );
    let parse_time = start.elapsed();

    if result.errors.is_empty() {
        println!("   ✓ Parse successful!");
        println!("   - Parse time: {parse_time:?}");
        println!("   - Tokens consumed: {}", result.metrics.tokens_consumed);
        println!("   - Nodes created: {}\n", result.metrics.nodes_created);
    } else {
        println!("   Parse errors:");
        for error in &result.errors {
            println!("   - {error:?}");
        }
        println!();
    }

    // Step 6: Demonstrate ordered choice
    println!("6. Demonstrating ordered choice (first match wins):");
    println!("   In PEG, alternatives are tried in order.");
    println!("   The first matching alternative is used, even if later");
    println!("   alternatives would also match.\n");

    // Step 7: Demonstrate incremental parsing
    println!("7. Demonstrating incremental parsing with memoization...");
    let edits = vec![TextEdit::replace(
        TextRange::new(TextSize::from(4), TextSize::from(5)),
        "4",
    )];
    let start = Instant::now();
    let result2 = incremental.parse_incremental(
        tokens.as_slice(),
        Some(&result.root),
        &edits,
        PegNonTerminal::Expr,
        Some(&grammar),
    );
    let incremental_time = start.elapsed();
    println!("   - Incremental parse time: {incremental_time:?}");
    println!("   - Cache hits: {}\n", result2.metrics.cache_hits);

    // Step 8: Compare with and without memoization
    println!("8. Comparing performance with/without memoization:");
    let config_with = PegConfig {
        enable_memoization: true,
        ..Default::default()
    };
    let config_without = PegConfig {
        enable_memoization: false,
        ..Default::default()
    };

    let mut parser_with = PegParser::new(&grammar, config_with)?;
    let mut parser_without = PegParser::new(&grammar, config_without)?;

    let start = Instant::now();
    let _result_with = parser_with.parse(tokens.as_slice(), PegNonTerminal::Expr);
    let time_with = start.elapsed();

    let start = Instant::now();
    let _result_without = parser_without.parse(tokens.as_slice(), PegNonTerminal::Expr);
    let time_without = start.elapsed();

    println!("   - With memoization: {time_with:?}");
    println!("   - Without memoization: {time_without:?}");
    println!(
        "   - Speedup: {:.2}x\n",
        time_without.as_secs_f64() / time_with.as_secs_f64().max(0.000_001)
    );

    println!("=== Example Complete ===");
    Ok(())
}
