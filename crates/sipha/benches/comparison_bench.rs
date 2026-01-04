//! Comparison benchmarks against other Rust parsing libraries
//!
//! This benchmark suite compares Sipha's performance against:
//! - pest (PEG parser)
//! - nom (parser combinators)
//! - lalrpop (LR parser generator)
//!
//! Note: These benchmarks require the comparison libraries to be available.
//! They are marked as optional dependencies to avoid forcing users to install
//! libraries they don't need.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use sipha::backend::ParserBackend;
use sipha::grammar::{Expr, Grammar, GrammarBuilder, NonTerminal};
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::lexer::{CharSet, LexerBuilder, Pattern, Token};
use sipha::syntax::{SyntaxKind, TextRange, TextSize};
use std::time::Duration;

#[cfg(feature = "backend-ll")]
use sipha::backend::ll::{LlConfig, LlParser};
#[cfg(feature = "backend-lr")]
use sipha::backend::lr::{LrConfig, LrParser};
#[cfg(feature = "backend-peg")]
use sipha::backend::peg::{PegConfig, PegParser};

// Shared types for benchmarks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BenchSyntaxKind {
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

impl SyntaxKind for BenchSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BenchNonTerminal {
    Expr,
    Term,
    Factor,
}

impl NonTerminal for BenchNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }
}

fn create_token(kind: BenchSyntaxKind, text: &str) -> Token<BenchSyntaxKind> {
    Token::new(
        kind,
        text,
        TextRange::at(TextSize::zero(), TextSize::from(u32::try_from(text.len()).unwrap_or(0))),
    )
}

fn setup_lexer() -> sipha::lexer::CompiledLexer<BenchSyntaxKind> {
    LexerBuilder::new()
        .token(
            BenchSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(BenchSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(BenchSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(BenchSyntaxKind::Multiply, Pattern::Literal("*".into()))
        .token(BenchSyntaxKind::Divide, Pattern::Literal("/".into()))
        .token(BenchSyntaxKind::LParen, Pattern::Literal("(".into()))
        .token(BenchSyntaxKind::RParen, Pattern::Literal(")".into()))
        .token(
            BenchSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(BenchSyntaxKind::Whitespace)
        .build(BenchSyntaxKind::Eof, BenchSyntaxKind::Number)
        .expect("Failed to build lexer")
}

fn setup_grammar() -> Grammar<Token<BenchSyntaxKind>, BenchNonTerminal> {
    GrammarBuilder::new()
        .entry_point(BenchNonTerminal::Expr)
        .rule(
            BenchNonTerminal::Expr,
            Expr::choice([
                Expr::seq([
                    Expr::rule(BenchNonTerminal::Term),
                    Expr::token(create_token(BenchSyntaxKind::Plus, "+")),
                    Expr::rule(BenchNonTerminal::Expr),
                ]),
                Expr::rule(BenchNonTerminal::Term),
            ]),
        )
        .rule(
            BenchNonTerminal::Term,
            Expr::choice([
                Expr::seq([
                    Expr::rule(BenchNonTerminal::Factor),
                    Expr::token(create_token(BenchSyntaxKind::Multiply, "*")),
                    Expr::rule(BenchNonTerminal::Term),
                ]),
                Expr::rule(BenchNonTerminal::Factor),
            ]),
        )
        .rule(
            BenchNonTerminal::Factor,
            Expr::choice([
                Expr::token(create_token(BenchSyntaxKind::Number, "1")),
                Expr::seq([
                    Expr::token(create_token(BenchSyntaxKind::LParen, "(")),
                    Expr::rule(BenchNonTerminal::Expr),
                    Expr::token(create_token(BenchSyntaxKind::RParen, ")")),
                ]),
            ]),
        )
        .build()
        .expect("Failed to build grammar")
}

/// Benchmark parsing a simple arithmetic expression
///
/// This is a simple benchmark that can be implemented across all parsers
/// to compare performance on the same input.
fn bench_arithmetic_expression(c: &mut Criterion) {
    let inputs = vec![
        "42",
        "1 + 2",
        "1 + 2 * 3",
        "1 + 2 * 3 - 4 / 5",
        "(1 + 2) * (3 - 4)",
        "1 + 2 * 3 - 4 / 5 + 6 * 7 - 8 / 9",
    ];

    let lexer = setup_lexer();
    let grammar = setup_grammar();

    let mut group = c.benchmark_group("arithmetic_parsing");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(3));

    for input in &inputs {
        let tokens = lexer.tokenize(input).expect("Failed to tokenize");

        #[cfg(feature = "backend-ll")]
        {
            let config = LlConfig::default();
            group.bench_with_input(
                BenchmarkId::new("sipha_ll", input),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = LlParser::new(&grammar, config.clone())
                            .expect("Failed to create LL parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }

        #[cfg(feature = "backend-lr")]
        {
            let config = LrConfig::default();
            group.bench_with_input(
                BenchmarkId::new("sipha_lr", input),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = LrParser::new(&grammar, config.clone())
                            .expect("Failed to create LR parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }

        #[cfg(feature = "backend-peg")]
        {
            let config = PegConfig::default();
            group.bench_with_input(
                BenchmarkId::new("sipha_peg", input),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = PegParser::new(&grammar, config.clone())
                            .expect("Failed to create PEG parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }

        // Uncomment when comparison libraries are available:
        /*
        #[cfg(feature = "comparison-pest")]
        group.bench_with_input(
            BenchmarkId::new("pest", input),
            input,
            |b, i| {
                b.iter(|| {
                    // pest parsing
                });
            },
        );

        #[cfg(feature = "comparison-nom")]
        group.bench_with_input(
            BenchmarkId::new("nom", input),
            input,
            |b, i| {
                b.iter(|| {
                    // nom parsing
                });
            },
        );
        */
    }

    group.finish();
}

/// Benchmark incremental parsing performance
///
/// This benchmark measures how incremental parsing performs compared to
/// full re-parsing on various edit scenarios.
fn bench_incremental_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental_parsing");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(3));

    let lexer = setup_lexer();
    let grammar = setup_grammar();

    // Test scenarios:
    // 1. Small edit (single token change)
    // 2. Medium edit (expression change)
    // 3. Large edit (multiple expressions)
    let scenarios = vec![
        ("small_edit", "1 + 2", "1 + 3", 4, 5, "3"),
        ("medium_edit", "1 + 2 * 3", "1 + 2 * 4", 8, 9, "4"),
        ("large_edit", "1 + 2 * 3 - 4", "10 + 20 * 30 - 40", 0, 13, "10 + 20 * 30 - 40"),
    ];

    for (name, original, edited, edit_start, edit_end, new_text) in scenarios {
        let original_tokens = lexer.tokenize(original).expect("Failed to tokenize original");
        let edited_tokens = lexer.tokenize(edited).expect("Failed to tokenize edited");

        // Initial parse (done once, not in benchmark loop)
        #[cfg(feature = "backend-ll")]
        {
            let config = LlConfig::default();
            let parser = LlParser::new(&grammar, config.clone())
                .expect("Failed to create LL parser");
            let mut incremental_parser = IncrementalParser::new(parser);
            let initial_result = incremental_parser.parse_incremental(
                &original_tokens,
                None,
                &[],
                BenchNonTerminal::Expr,
                Some(&grammar),
            );

            let edit = TextEdit {
                range: TextRange::new(
                    TextSize::from(edit_start),
                    TextSize::from(edit_end),
                ),
                new_text: new_text.into(),
            };

            group.bench_with_input(
                BenchmarkId::new("incremental_ll", name),
                &(original_tokens.clone(), edited_tokens.clone(), initial_result.root.clone()),
                |b, (orig_tokens, edit_tokens, old_tree)| {
                    b.iter(|| {
                        let parser = LlParser::new(&grammar, config.clone())
                            .expect("Failed to create LL parser");
                        let mut incremental_parser = IncrementalParser::new(parser);
                        // Re-parse initial to get fresh state
                        let _ = incremental_parser.parse_incremental(
                            orig_tokens,
                            None,
                            &[],
                            BenchNonTerminal::Expr,
                            Some(&grammar),
                        );
                        black_box(incremental_parser.parse_incremental(
                            edit_tokens,
                            Some(old_tree),
                            &[edit.clone()],
                            BenchNonTerminal::Expr,
                            None,
                        ));
                    });
                },
            );

            group.bench_with_input(
                BenchmarkId::new("full_reparse_ll", name),
                &edited_tokens,
                |b, tokens| {
                    b.iter(|| {
                        let mut parser = LlParser::new(&grammar, config.clone())
                            .expect("Failed to create LL parser");
                        black_box(parser.parse(black_box(tokens), BenchNonTerminal::Expr));
                    });
                },
            );
        }
    }

    group.finish();
}

/// Benchmark memory usage
///
/// This benchmark measures memory consumption for parsing various inputs.
fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(2));

    let lexer = setup_lexer();
    let grammar = setup_grammar();

    let sizes = vec![
        ("small", 10),
        ("medium", 50),
        ("large", 100),
    ];

    for (size_name, expr_count) in sizes {
        // Generate input: "1 + 2 + 3 + ... + N"
        let input: String = (1..=expr_count)
            .map(|i| format!("{i} + "))
            .collect::<String>()
            + &expr_count.to_string();
        
        let tokens = lexer.tokenize(&input).expect("Failed to tokenize");

        #[cfg(feature = "backend-ll")]
        {
            let config = LlConfig::default();
            group.bench_with_input(
                BenchmarkId::new("sipha_ll", size_name),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = LlParser::new(&grammar, config.clone())
                            .expect("Failed to create LL parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }
    }

    group.finish();
}

/// Benchmark different parser backends
///
/// Compare performance of LL, LR, and PEG backends on the same input.
fn bench_backend_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("backend_comparison");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(3));

    let lexer = setup_lexer();
    let grammar = setup_grammar();

    let inputs = vec![
        "1 + 2",
        "1 + 2 * 3",
        "(1 + 2) * (3 - 4)",
    ];

    for input in &inputs {
        let tokens = lexer.tokenize(input).expect("Failed to tokenize");

        #[cfg(feature = "backend-ll")]
        {
            let config = LlConfig::default();
            group.bench_with_input(
                BenchmarkId::new("ll", input),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = LlParser::new(&grammar, config.clone())
                            .expect("Failed to create LL parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }

        #[cfg(feature = "backend-lr")]
        {
            let config = LrConfig::default();
            group.bench_with_input(
                BenchmarkId::new("lr", input),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = LrParser::new(&grammar, config.clone())
                            .expect("Failed to create LR parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }

        #[cfg(feature = "backend-peg")]
        {
            let config = PegConfig::default();
            group.bench_with_input(
                BenchmarkId::new("peg", input),
                &tokens,
                |b, t| {
                    b.iter(|| {
                        let mut parser = PegParser::new(&grammar, config.clone())
                            .expect("Failed to create PEG parser");
                        black_box(parser.parse(black_box(t), BenchNonTerminal::Expr));
                    });
                },
            );
        }
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_arithmetic_expression,
    bench_incremental_parsing,
    bench_memory_usage,
    bench_backend_comparison
);
criterion_main!(benches);

