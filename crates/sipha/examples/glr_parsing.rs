//! GLR (Generalized LR) parsing example
//!
//! This example demonstrates how to:
//! 1. Define an ambiguous grammar
//! 2. Use the GLR parser to handle ambiguity
//! 3. Work with parse forests
//! 4. Apply disambiguation strategies
//!
//! This example requires the `backend-glr` feature to be enabled.
//! Run with: `cargo run --example glr_parsing --features backend-glr`

#[cfg(not(feature = "backend-glr"))]
fn main() {
    eprintln!("This example requires the 'backend-glr' feature to be enabled.");
    eprintln!("Run with: cargo run --example glr_parsing --features backend-glr");
    std::process::exit(1);
}

#[cfg(feature = "backend-glr")]
use sipha::backend::ParserBackend;
#[cfg(feature = "backend-glr")]
use sipha::backend::glr::{DisambiguationStrategy, GlrConfig, GlrParser};
#[cfg(feature = "backend-glr")]
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal};
#[cfg(feature = "backend-glr")]
use sipha::lexer::{CharSet, LexerBuilder, Pattern, Token};
#[cfg(feature = "backend-glr")]
use sipha::syntax::{SyntaxKind, TextRange, TextSize};

#[cfg(feature = "backend-glr")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AmbiguousSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    Eof,
    // Non-terminals
    #[allow(dead_code)]
    Expr,
}

#[cfg(feature = "backend-glr")]
impl SyntaxKind for AmbiguousSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[cfg(feature = "backend-glr")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AmbiguousNonTerminal {
    Expr,
}

#[cfg(feature = "backend-glr")]
impl NonTerminal for AmbiguousNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
        }
    }
}

#[cfg(feature = "backend-glr")]
fn create_token(kind: AmbiguousSyntaxKind, text: &str) -> Token<AmbiguousSyntaxKind> {
    Token::new(
        kind,
        text,
        TextRange::at(
            TextSize::zero(),
            TextSize::from(u32::try_from(text.len()).unwrap_or(0)),
        ),
    )
}

#[cfg(feature = "backend-glr")]
#[allow(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== GLR Parsing Example ===\n");

    // Step 1: Build lexer
    println!("1. Building lexer...");
    let lexer = LexerBuilder::new()
        .token(
            AmbiguousSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(AmbiguousSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(AmbiguousSyntaxKind::Minus, Pattern::Literal("-".into()))
        .build(AmbiguousSyntaxKind::Eof, AmbiguousSyntaxKind::Number)?;

    println!("   ✓ Lexer built successfully\n");

    // Step 2: Build ambiguous grammar
    // This grammar is ambiguous: "1 + 2 - 3" can be parsed as either
    // (1 + 2) - 3 or 1 + (2 - 3)
    println!("2. Building ambiguous grammar...");
    let grammar = GrammarBuilder::<Token<AmbiguousSyntaxKind>, AmbiguousNonTerminal>::new()
        .entry_point(AmbiguousNonTerminal::Expr)
        .rule(
            AmbiguousNonTerminal::Expr,
            Expr::Choice(vec![
                // Expr -> Number
                Expr::token(create_token(AmbiguousSyntaxKind::Number, "1")),
                // Expr -> Expr + Expr (ambiguous!)
                Expr::Seq(vec![
                    Expr::Rule(AmbiguousNonTerminal::Expr),
                    Expr::token(create_token(AmbiguousSyntaxKind::Plus, "+")),
                    Expr::Rule(AmbiguousNonTerminal::Expr),
                ]),
                // Expr -> Expr - Expr (ambiguous!)
                Expr::Seq(vec![
                    Expr::Rule(AmbiguousNonTerminal::Expr),
                    Expr::token(create_token(AmbiguousSyntaxKind::Minus, "-")),
                    Expr::Rule(AmbiguousNonTerminal::Expr),
                ]),
            ]),
        )
        .build()?;

    println!("   ✓ Grammar built (contains ambiguity)\n");

    // Step 3: Tokenize input
    println!("3. Tokenizing input: \"1 + 2 - 3\"");
    let input = "1 + 2 - 3";
    let tokens = lexer
        .tokenize(input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;

    println!(
        "   Tokens: {:?}\n",
        tokens.iter().map(|t| t.text.as_str()).collect::<Vec<_>>()
    );

    // Step 4: Create GLR parser with disambiguation
    println!("4. Creating GLR parser...");
    let config = GlrConfig {
        disambiguation: DisambiguationStrategy::Associativity, // Left-associative
        max_stacks: 100,
        return_forest: true, // Return parse forest to see ambiguity
        ..Default::default()
    };
    let mut parser: GlrParser<Token<AmbiguousSyntaxKind>, AmbiguousNonTerminal> =
        GlrParser::new(&grammar, config)?;
    println!("   ✓ GLR parser created\n");

    // Step 5: Parse with GLR
    println!("5. Parsing with GLR...");
    let result = parser.parse(tokens.as_slice(), AmbiguousNonTerminal::Expr);

    if let Some(forest) = &result.forest {
        println!("   Parse forest generated:");
        println!("   - Root count: {}", forest.root_count());
        println!("   - Is ambiguous: {}", forest.is_ambiguous());
        println!("   - Total alternatives: {}\n", forest.count_alternatives());

        if forest.is_ambiguous() {
            println!("   ⚠️  Ambiguity detected! Multiple parse trees exist.");
            println!("   The parser has applied disambiguation strategy.\n");
        }
    }

    // Step 6: Check for errors
    if result.errors.is_empty() {
        println!("6. ✓ Parse successful!\n");
    } else {
        println!("6. Parse errors:");
        for error in &result.errors {
            println!("   - {error:?}");
        }
        println!();
    }

    // Step 7: Demonstrate different disambiguation strategies
    println!("7. Demonstrating different disambiguation strategies:");

    let strategies = vec![
        (DisambiguationStrategy::None, "None"),
        (DisambiguationStrategy::Precedence, "Precedence"),
        (DisambiguationStrategy::Associativity, "Associativity"),
    ];

    for (strategy, name) in strategies {
        let config = GlrConfig {
            disambiguation: strategy,
            max_stacks: 100,
            return_forest: true,
            ..Default::default()
        };
        let mut parser: GlrParser<Token<AmbiguousSyntaxKind>, AmbiguousNonTerminal> =
            GlrParser::new(&grammar, config)?;
        let result = parser.parse(tokens.as_slice(), AmbiguousNonTerminal::Expr);

        let alt_count = result
            .forest
            .as_ref()
            .map_or(0, sipha::backend::glr::ParseForest::count_alternatives);
        println!("   - {name}: {alt_count} alternatives");
    }

    println!("\n=== Example Complete ===");
    Ok(())
}
