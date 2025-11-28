//! Incremental parsing example
//!
//! This example demonstrates how to:
//! 1. Perform an initial parse
//! 2. Apply text edits incrementally
//! 3. Compare incremental vs full reparse performance
//! 4. Understand cache management

use sipha::backend::ParserBackend;
use sipha::backend::ll::{LlConfig, LlParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal};
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::lexer::{CharSet, LexerBuilder, Pattern, Token};
use sipha::syntax::{SyntaxKind, SyntaxNode, TextRange, TextSize};
use std::time::Instant;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum IncrementalSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    Eof,
    // Non-terminals
    #[allow(dead_code)]
    Expr,
}

impl SyntaxKind for IncrementalSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum IncrementalNonTerminal {
    Expr,
}

impl NonTerminal for IncrementalNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
        }
    }
}

fn create_token(kind: IncrementalSyntaxKind, text: &str) -> Token<IncrementalSyntaxKind> {
    Token::new(
        kind,
        text,
        TextRange::at(
            TextSize::zero(),
            TextSize::from(u32::try_from(text.len()).unwrap_or(0)),
        ),
    )
}

#[allow(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Incremental Parsing Example ===\n");

    // Step 1: Build lexer
    println!("1. Building lexer...");
    let lexer = LexerBuilder::new()
        .token(
            IncrementalSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(IncrementalSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(IncrementalSyntaxKind::Minus, Pattern::Literal("-".into()))
        .build(IncrementalSyntaxKind::Eof, IncrementalSyntaxKind::Number)?;

    println!("   ✓ Lexer built successfully\n");

    // Step 2: Build grammar
    println!("2. Building grammar...");
    let grammar = GrammarBuilder::<Token<IncrementalSyntaxKind>, IncrementalNonTerminal>::new()
        .entry_point(IncrementalNonTerminal::Expr)
        .rule(
            IncrementalNonTerminal::Expr,
            Expr::Choice(vec![
                Expr::token(create_token(IncrementalSyntaxKind::Number, "1")),
                Expr::Seq(vec![
                    Expr::Rule(IncrementalNonTerminal::Expr),
                    Expr::token(create_token(IncrementalSyntaxKind::Plus, "+")),
                    Expr::Rule(IncrementalNonTerminal::Expr),
                ]),
                Expr::Seq(vec![
                    Expr::Rule(IncrementalNonTerminal::Expr),
                    Expr::token(create_token(IncrementalSyntaxKind::Minus, "-")),
                    Expr::Rule(IncrementalNonTerminal::Expr),
                ]),
            ]),
        )
        .build()?;

    println!("   ✓ Grammar built successfully\n");

    // Step 3: Initial parse
    println!("3. Performing initial parse...");
    let initial_input = "1 + 2 + 3";
    let initial_tokens = lexer
        .tokenize(initial_input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;

    let parser: LlParser<Token<IncrementalSyntaxKind>, IncrementalNonTerminal> =
        LlParser::new(&grammar, LlConfig::default())?;
    let mut incremental_parser = IncrementalParser::new(parser);

    let start = Instant::now();
    let initial_result = incremental_parser.parse_incremental(
        initial_tokens.as_slice(),
        None,
        &[],
        IncrementalNonTerminal::Expr,
        Some(&grammar),
    );
    let initial_time = start.elapsed();

    println!("   Input: \"{initial_input}\"");
    println!("   Parse time: {initial_time:?}");
    println!("   ✓ Initial parse complete\n");

    // Step 4: Apply incremental edit
    println!("4. Applying incremental edit (changing '2' to '20')...");
    let edited_input = "1 + 20 + 3";
    let edited_tokens = lexer
        .tokenize(edited_input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;

    // Create text edit
    let edit = TextEdit::replace(
        TextRange::new(TextSize::from(4), TextSize::from(5)), // Replace "2" with "20"
        "20",
    );
    let edits = vec![edit];

    let start = Instant::now();
    let incremental_result = incremental_parser.parse_incremental(
        edited_tokens.as_slice(),
        Some(&initial_result.root),
        &edits,
        IncrementalNonTerminal::Expr,
        Some(&grammar),
    );
    let incremental_time = start.elapsed();

    println!("   Input: \"{edited_input}\"");
    println!("   Incremental parse time: {incremental_time:?}");
    println!("   ✓ Incremental parse complete\n");

    // Step 5: Compare with full reparse
    println!("5. Comparing with full reparse...");
    let mut full_parser: LlParser<Token<IncrementalSyntaxKind>, IncrementalNonTerminal> =
        LlParser::new(&grammar, LlConfig::default())?;

    let start = Instant::now();
    let full_result = full_parser.parse(edited_tokens.as_slice(), IncrementalNonTerminal::Expr);
    let full_time = start.elapsed();

    println!("   Full reparse time: {full_time:?}");
    println!(
        "   Speedup: {:.2}x faster",
        full_time.as_secs_f64() / incremental_time.as_secs_f64().max(0.0001)
    );
    println!("   ✓ Full reparse complete\n");

    // Step 6: Verify results match
    println!("6. Verifying results...");
    let incremental_node = SyntaxNode::new_root(incremental_result.root);
    let full_node = SyntaxNode::new_root(full_result.root);
    let incremental_text = incremental_node.text();
    let full_text = full_node.text();

    if incremental_text == full_text {
        println!("   ✓ Results match! Incremental parsing preserved correctness.");
    } else {
        println!("   ⚠️  Results differ (this may be expected in some cases)");
        println!("   Incremental: \"{incremental_text}\"");
        println!("   Full: \"{full_text}\"");
    }
    println!();

    // Step 7: Demonstrate multiple edits
    println!("7. Applying multiple incremental edits...");
    let mut current_root = initial_result.root.clone();

    let edits_sequence = vec![
        ("1 + 2 + 3", "1 + 20 + 3", 4, 5, "20"),
        ("1 + 20 + 3", "1 + 20 + 30", 8, 9, "30"),
        ("1 + 20 + 30", "10 + 20 + 30", 0, 1, "10"),
    ];

    for (old_text, new_text, start_byte, end_byte, replacement) in edits_sequence {
        let edit = TextEdit::replace(
            TextRange::new(TextSize::from(start_byte), TextSize::from(end_byte)),
            replacement,
        );
        let current_tokens = lexer
            .tokenize(new_text)
            .map_err(|e| format!("Tokenization failed: {e:?}"))?;

        let start = Instant::now();
        let result = incremental_parser.parse_incremental(
            current_tokens.as_slice(),
            Some(&current_root),
            &[edit],
            IncrementalNonTerminal::Expr,
            Some(&grammar),
        );
        let edit_time = start.elapsed();

        current_root = result.root.clone();
        println!("   Edit: \"{old_text}\" -> \"{new_text}\" ({edit_time:?})");
    }

    println!("\n=== Example Complete ===");
    Ok(())
}
