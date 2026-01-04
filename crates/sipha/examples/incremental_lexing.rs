//! Incremental Lexing Example
//!
//! This example demonstrates how to use incremental lexing to efficiently
//! re-tokenize source code when edits are made, reusing unchanged tokens.

use sipha::lexer::incremental::{IncrementalLexer, TextEdit};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ArithSyntaxKind {
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
}

impl SyntaxKind for ArithSyntaxKind {
    fn is_terminal(self) -> bool {
        true
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

fn build_arithmetic_lexer() -> sipha::lexer::CompiledLexer<ArithSyntaxKind> {
    LexerBuilder::new()
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
        .build(ArithSyntaxKind::Eof, ArithSyntaxKind::Number)
        .expect("Failed to build lexer")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Incremental Lexing Example ===\n");

    // Build lexer
    println!("1. Building arithmetic lexer...");
    let lexer = build_arithmetic_lexer();
    println!("   ✓ Lexer built\n");

    // Create incremental lexer with initial input
    println!("2. Creating incremental lexer with input: \"42 + 10\"");
    let mut incr = IncrementalLexer::new(lexer, "42 + 10".into());
    println!("   Initial tokens: {}", incr.tokens().len());
    println!("   Version: {}\n", incr.version());

    // Demonstrate token reuse with a small edit
    println!("3. Making a small edit: change \"42\" to \"100\"");
    println!("   This should reuse tokens after the edit point\n");

    let edit = TextEdit::replace(0..2, "100");
    let delta = incr.update(&edit, "100 + 10");

    println!("   Edit applied:");
    println!("     - Version: {} (incremented)", incr.version());
    println!("     - Changed token range: {:?}", delta.changed_range);
    println!("     - Tokens removed: {}", delta.removed_count);
    println!("     - Tokens added: {}", delta.added_count);
    println!("     - Net change: {}", delta.token_count_delta());
    println!();

    // Show that tokens after the edit are preserved
    println!("4. Token reuse demonstration:");
    println!("   Input: \"100 + 10\"");
    println!("   Total tokens: {}", incr.tokens().len());
    println!("   Tokens after edit point should be reused (positions updated)\n");

    // Demonstrate multiple edits
    println!("5. Making another edit: change \"10\" to \"20\"");
    let edit2 = TextEdit::replace(6..8, "20");
    let delta2 = incr.update(&edit2, "100 + 20");

    println!("   Edit applied:");
    println!("     - Version: {} (incremented)", incr.version());
    println!("     - Changed token range: {:?}", delta2.changed_range);
    println!("     - Tokens removed: {}", delta2.removed_count);
    println!("     - Tokens added: {}", delta2.added_count);
    println!();

    // Show line tracking
    println!("6. Line tracking:");
    let multiline_input = "42 + 10\n20 * 5\n100 / 2";
    let mut incr2 = IncrementalLexer::new(build_arithmetic_lexer(), multiline_input.into());

    println!(
        "   Input with {} lines",
        multiline_input.matches('\n').count() + 1
    );
    println!(
        "   Line at offset 0: {}",
        incr2.line_at_offset(TextSize::from(0))
    );
    println!(
        "   Line at offset 10: {}",
        incr2.line_at_offset(TextSize::from(10))
    );
    println!(
        "   Line at offset 20: {}",
        incr2.line_at_offset(TextSize::from(20))
    );
    println!();

    println!("=== Example completed! ===");
    println!();
    println!("Key benefits of incremental lexing:");
    println!("  ✓ Only re-tokenizes affected regions");
    println!("  ✓ Reuses unchanged tokens (just updates positions)");
    println!("  ✓ Tracks line numbers for efficient lookups");
    println!("  ✓ Maintains version for cache invalidation");
    println!("  ✓ Works seamlessly with incremental parsing");

    Ok(())
}
