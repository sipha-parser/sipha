//! Performance Tuning Guide Example
//!
//! This example demonstrates various performance optimization techniques
//! for Sipha parsers, including:
//! 1. Choosing the right backend
//! 2. Optimizing grammar structure
//! 3. Using incremental parsing effectively
//! 4. Memory optimization strategies
//!
//! Run with: cargo run --example performance_tuning --release

use sipha::backend::ParserBackend;
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextRange, TextSize};
use std::time::Instant;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum PerfSyntaxKind {
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
    Expr,
    Term,
    Factor,
}

impl SyntaxKind for PerfSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PerfNonTerminal {
    Expr,
    Term,
    Factor,
}

impl NonTerminal for PerfNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }
}

fn create_token(kind: PerfSyntaxKind, text: &str) -> Token<PerfSyntaxKind> {
    use sipha::lexer::Token;
    Token::new(
        kind,
        text,
        TextRange::at(TextSize::zero(), TextSize::from(u32::try_from(text.len()).unwrap_or(0))),
    )
}

fn build_lexer() -> sipha::lexer::CompiledLexer<PerfSyntaxKind> {
    LexerBuilder::new()
        .token(
            PerfSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(PerfSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(PerfSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(PerfSyntaxKind::Multiply, Pattern::Literal("*".into()))
        .token(PerfSyntaxKind::Divide, Pattern::Literal("/".into()))
        .token(PerfSyntaxKind::LParen, Pattern::Literal("(".into()))
        .token(PerfSyntaxKind::RParen, Pattern::Literal(")".into()))
        .token(
            PerfSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(PerfSyntaxKind::Whitespace)
        .build(PerfSyntaxKind::Eof, PerfSyntaxKind::Number)
        .expect("Failed to build lexer")
}

fn build_grammar() -> sipha::grammar::Grammar<Token<PerfSyntaxKind>, PerfNonTerminal> {
    GrammarBuilder::new()
        .entry_point(PerfNonTerminal::Expr)
        .rule(
            PerfNonTerminal::Expr,
            Expr::choice([
                Expr::seq([
                    Expr::rule(PerfNonTerminal::Term),
                    Expr::token(create_token(PerfSyntaxKind::Plus, "+")),
                    Expr::rule(PerfNonTerminal::Expr),
                ]),
                Expr::rule(PerfNonTerminal::Term),
            ]),
        )
        .rule(
            PerfNonTerminal::Term,
            Expr::choice([
                Expr::seq([
                    Expr::rule(PerfNonTerminal::Factor),
                    Expr::token(create_token(PerfSyntaxKind::Multiply, "*")),
                    Expr::rule(PerfNonTerminal::Term),
                ]),
                Expr::rule(PerfNonTerminal::Factor),
            ]),
        )
        .rule(
            PerfNonTerminal::Factor,
            Expr::choice([
                Expr::token(create_token(PerfSyntaxKind::Number, "1")),
                Expr::seq([
                    Expr::token(create_token(PerfSyntaxKind::LParen, "(")),
                    Expr::rule(PerfNonTerminal::Expr),
                    Expr::token(create_token(PerfSyntaxKind::RParen, ")")),
                ]),
            ]),
        )
        .build()
        .expect("Failed to build grammar")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Sipha Performance Tuning Guide ===\n");

    let lexer = build_lexer();
    let grammar = build_grammar();

    // Generate a large expression for benchmarking
    let large_expr: String = (1..=100)
        .map(|i| format!("{i} + "))
        .collect::<String>()
        + "0";
    
    println!("1. Backend Selection");
    println!("   Testing different backends on expression with 100 terms...\n");

    let tokens = lexer.tokenize(&large_expr)?;

    #[cfg(feature = "backend-ll")]
    {
        use sipha::backend::ll::{LlConfig, LlParser};
        let config = LlConfig::default();
        let mut parser = LlParser::new(&grammar, config)?;
        
        let start = Instant::now();
        let _result = parser.parse(&tokens, PerfNonTerminal::Expr);
        let elapsed = start.elapsed();
        println!("   LL Parser: {:?}", elapsed);
    }

    #[cfg(feature = "backend-lr")]
    {
        use sipha::backend::lr::{LrConfig, LrParser};
        let config = LrConfig::default();
        let mut parser = LrParser::new(&grammar, config)?;
        
        let start = Instant::now();
        let _result = parser.parse(&tokens, PerfNonTerminal::Expr);
        let elapsed = start.elapsed();
        println!("   LR Parser: {:?}", elapsed);
    }

    #[cfg(feature = "backend-peg")]
    {
        use sipha::backend::peg::{PegConfig, PegParser};
        let config = PegConfig::default();
        let mut parser = PegParser::new(&grammar, config)?;
        
        let start = Instant::now();
        let _result = parser.parse(&tokens, PerfNonTerminal::Expr);
        let elapsed = start.elapsed();
        println!("   PEG Parser: {:?}", elapsed);
    }

    println!("\n2. Incremental Parsing Benefits");
    println!("   Comparing incremental vs full re-parse for small edits...\n");

    #[cfg(feature = "backend-ll")]
    {
        use sipha::backend::ll::{LlConfig, LlParser};
        let config = LlConfig::default();
        let parser = LlParser::new(&grammar, config)?;
        let mut incremental_parser = IncrementalParser::new(parser);

        // Initial parse
        let initial_expr = "1 + 2 + 3 + 4 + 5";
        let initial_tokens = lexer.tokenize(initial_expr)?;
        let start = Instant::now();
        let initial_result = incremental_parser.parse_incremental(
            &initial_tokens,
            None,
            &[],
            PerfNonTerminal::Expr,
            Some(&grammar),
        );
        let initial_time = start.elapsed();

        // Incremental edit (change "2" to "20")
        let edited_expr = "1 + 20 + 3 + 4 + 5";
        let edited_tokens = lexer.tokenize(edited_expr)?;
        let edit = TextEdit {
            range: TextRange::new(TextSize::from(4), TextSize::from(5)),
            new_text: "20".into(),
        };
        let start = Instant::now();
        let _incremental_result = incremental_parser.parse_incremental(
            &edited_tokens,
            Some(&initial_result.root),
            &[edit],
            PerfNonTerminal::Expr,
            None,
        );
        let incremental_time = start.elapsed();

        // Full re-parse
        let start = Instant::now();
        let mut parser = LlParser::new(&grammar, LlConfig::default())?;
        let _full_result = parser.parse(&edited_tokens, PerfNonTerminal::Expr);
        let full_time = start.elapsed();

        println!("   Initial parse: {:?}", initial_time);
        println!("   Incremental edit: {:?}", incremental_time);
        println!("   Full re-parse: {:?}", full_time);
        println!(
            "   Speedup: {:.2}x",
            full_time.as_secs_f64() / incremental_time.as_secs_f64()
        );
    }

    println!("\n3. Memory Optimization Tips");
    println!("   - Use CompactString for token text (automatic)");
    println!("   - Enable arena allocation for batch parsing (feature: arena)");
    println!("   - Reuse parse caches across sessions");
    println!("   - Use incremental parsing to avoid full re-parses");

    println!("\n4. Grammar Optimization Tips");
    println!("   - Factor common patterns");
    println!("   - Minimize nesting depth");
    println!("   - Use appropriate lookahead (LL parser)");
    println!("   - Avoid deep left recursion");

    println!("\n=== Performance Tuning Guide Complete ===");
    println!("\nFor detailed benchmarks, run: cargo bench");

    Ok(())
}

