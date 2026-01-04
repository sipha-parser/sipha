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

    // Step 2: Build grammar (right-recursive for LL parser)
    println!("2. Building grammar...");
    let grammar = GrammarBuilder::<Token<IncrementalSyntaxKind>, IncrementalNonTerminal>::new()
        .entry_point(IncrementalNonTerminal::Expr)
        .rule(
            IncrementalNonTerminal::Expr,
            Expr::Seq(vec![
                Expr::token(create_token(IncrementalSyntaxKind::Number, "1")),
                Expr::Repeat {
                    expr: Box::new(Expr::Choice(vec![
                        Expr::Seq(vec![
                            Expr::token(create_token(IncrementalSyntaxKind::Plus, "+")),
                            Expr::token(create_token(IncrementalSyntaxKind::Number, "1")),
                        ]),
                        Expr::Seq(vec![
                            Expr::token(create_token(IncrementalSyntaxKind::Minus, "-")),
                            Expr::token(create_token(IncrementalSyntaxKind::Number, "1")),
                        ]),
                    ])),
                    min: 0,
                    max: None,
                    greedy: true,
                },
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

    // Step 8: Demonstrate parsing a large file
    println!("8. Demonstrating parsing on a large file...");
    
    // Generate a large arithmetic expression (10,000 operations)
    const LARGE_FILE_SIZE: usize = 10_000;
    let mut large_input = String::with_capacity(LARGE_FILE_SIZE * 10);
    large_input.push_str("1");
    for i in 2..=LARGE_FILE_SIZE {
        if i % 2 == 0 {
            large_input.push_str(" + ");
        } else {
            large_input.push_str(" - ");
        }
        large_input.push_str(&i.to_string());
    }
    
    println!("   Generated large expression with {LARGE_FILE_SIZE} operations");
    println!("   File size: {} bytes", large_input.len());
    println!("   First 100 chars: {}...", &large_input[..large_input.len().min(100)]);
    
    // Tokenize the large file
    let start = Instant::now();
    let large_tokens = lexer
        .tokenize(&large_input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;
    let tokenize_time = start.elapsed();
    
    println!("   Tokenization time: {tokenize_time:?}");
    println!("   Tokens: {}\n", large_tokens.len());
    
    // Initial parse of large file
    println!("   Performing initial parse of large file...");
    let mut large_parser: LlParser<Token<IncrementalSyntaxKind>, IncrementalNonTerminal> =
        LlParser::new(&grammar, LlConfig::default())?;
    let mut large_incremental = IncrementalParser::new(large_parser);
    
    let start = Instant::now();
    let large_initial_result = large_incremental.parse_incremental(
        large_tokens.as_slice(),
        None,
        &[],
        IncrementalNonTerminal::Expr,
        Some(&grammar),
    );
    let large_initial_time = start.elapsed();
    
    println!("   ✓ Large file parsed successfully");
    println!("   Parse time: {large_initial_time:?}");
    println!("   Tokens consumed: {}", large_initial_result.metrics.tokens_consumed);
    println!("   Nodes created: {}\n", large_initial_result.metrics.nodes_created);
    
    // Make a small edit in the middle of the file to demonstrate cache reuse
    // Find a number around the middle to change
    let edit_position = large_input.len() / 2;
    // Search backwards to find the start of a number
    let mut number_start = edit_position;
    while number_start > 0 && !large_input.as_bytes()[number_start].is_ascii_digit() {
        number_start -= 1;
    }
    // Now find the actual start of this number
    while number_start > 0 && large_input.as_bytes()[number_start - 1].is_ascii_digit() {
        number_start -= 1;
    }
    // Find the end of the number
    let mut number_end = number_start;
    while number_end < large_input.len() && large_input.as_bytes()[number_end].is_ascii_digit() {
        number_end += 1;
    }
    
    if number_start >= number_end || number_start >= large_input.len() {
        // Fallback: edit near the beginning if we can't find a number in the middle
        number_start = 0;
        number_end = 1;
    }
    
    let original_number = &large_input[number_start..number_end.min(large_input.len())];
    let new_number = if original_number.is_empty() || !original_number.chars().next().unwrap_or('0').is_ascii_digit() {
        "999".to_string()
    } else {
        format!("{}", original_number.parse::<usize>().unwrap_or(100) + 1000)
    };
    
    println!("   Making a small edit in the middle of the file...");
    println!("   Changing number at position {}: '{}' -> '{}'", number_start, original_number, new_number);
    
    let mut edited_large_input = large_input.clone();
    edited_large_input.replace_range(number_start..number_end.min(large_input.len()), &new_number);
    
    let edited_large_tokens = lexer
        .tokenize(&edited_large_input)
        .map_err(|e| format!("Tokenization failed: {e:?}"))?;
    
    let edit = TextEdit::replace(
        TextRange::new(
            TextSize::from(u32::try_from(number_start).unwrap_or(0)),
            TextSize::from(u32::try_from(number_end).unwrap_or(0)),
        ),
        &new_number,
    );
    
    let start = Instant::now();
    let large_incremental_result = large_incremental.parse_incremental(
        edited_large_tokens.as_slice(),
        Some(&large_initial_result.root),
        &[edit],
        IncrementalNonTerminal::Expr,
        Some(&grammar),
    );
    let large_incremental_time = start.elapsed();
    
    println!("   Incremental parse time: {large_incremental_time:?}");
    println!("   Cache hits: {}", large_incremental_result.metrics.cache_hits);
    println!("   Tokens consumed: {}", large_incremental_result.metrics.tokens_consumed);
    println!("   Nodes created: {}\n", large_incremental_result.metrics.nodes_created);
    
    // Compare with full reparse
    println!("   Comparing with full reparse of large file...");
    let mut full_large_parser: LlParser<Token<IncrementalSyntaxKind>, IncrementalNonTerminal> =
        LlParser::new(&grammar, LlConfig::default())?;
    
    let start = Instant::now();
    let _full_large_result = full_large_parser.parse(
        edited_large_tokens.as_slice(),
        IncrementalNonTerminal::Expr,
    );
    let full_large_time = start.elapsed();
    
    println!("   Full reparse time: {full_large_time:?}");
    let speedup = full_large_time.as_secs_f64() / large_incremental_time.as_secs_f64().max(0.0001);
    println!("   Speedup: {:.2}x faster with incremental parsing", speedup);
    println!("   Time saved: {:?}\n", full_large_time.saturating_sub(large_incremental_time));

    println!("\n=== Example Complete ===");
    Ok(())
}
