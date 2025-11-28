use criterion::{Criterion, criterion_group, criterion_main};
use sipha::backend::ParserBackend;
#[cfg(feature = "backend-glr")]
use sipha::backend::glr::GlrStack;
use sipha::backend::ll::{LlConfig, LlParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::syntax::SyntaxKind;
use std::hint::black_box;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
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
struct BenchToken {
    kind: BenchSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for BenchToken {
    type Kind = BenchSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> sipha::syntax::TextSize {
        sipha::syntax::TextSize::from(u32::try_from(self.text.len()).unwrap_or(0))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
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

fn create_number_token(value: &str) -> BenchToken {
    BenchToken {
        kind: BenchSyntaxKind::Number,
        text: value.into(),
    }
}

fn create_plus_token() -> BenchToken {
    BenchToken {
        kind: BenchSyntaxKind::Plus,
        text: "+".into(),
    }
}

fn create_multiply_token() -> BenchToken {
    BenchToken {
        kind: BenchSyntaxKind::Multiply,
        text: "*".into(),
    }
}

#[cfg(not(feature = "backend-glr"))]
fn glr_leaf_node() -> std::sync::Arc<sipha::syntax::GreenNode<BenchSyntaxKind>> {
    use sipha::syntax::{GreenNode, TextSize};
    use std::sync::Arc;
    Arc::new(GreenNode::new(
        BenchSyntaxKind::Number,
        vec![],
        TextSize::from(1),
    ))
}

fn setup_grammar() -> sipha::grammar::Grammar<BenchToken, BenchNonTerminal> {
    GrammarBuilder::new()
        .entry_point(BenchNonTerminal::Expr)
        .rule(
            BenchNonTerminal::Expr,
            Expr::choice([
                Expr::seq([
                    Expr::rule(BenchNonTerminal::Term),
                    Expr::token(create_plus_token()),
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
                    Expr::token(create_multiply_token()),
                    Expr::rule(BenchNonTerminal::Term),
                ]),
                Expr::rule(BenchNonTerminal::Factor),
            ]),
        )
        .rule(
            BenchNonTerminal::Factor,
            Expr::choice([
                Expr::token(create_number_token("1")),
                Expr::seq([
                    Expr::token(BenchToken {
                        kind: BenchSyntaxKind::LParen,
                        text: "(".into(),
                    }),
                    Expr::rule(BenchNonTerminal::Expr),
                    Expr::token(BenchToken {
                        kind: BenchSyntaxKind::RParen,
                        text: ")".into(),
                    }),
                ]),
            ]),
        )
        .build()
        .unwrap()
}

fn create_tokens(expr: &str) -> Vec<BenchToken> {
    let mut tokens = Vec::new();
    for ch in expr.chars() {
        match ch {
            '+' => tokens.push(create_plus_token()),
            '*' => tokens.push(create_multiply_token()),
            '(' => tokens.push(BenchToken {
                kind: BenchSyntaxKind::LParen,
                text: "(".into(),
            }),
            ')' => tokens.push(BenchToken {
                kind: BenchSyntaxKind::RParen,
                text: ")".into(),
            }),
            '0'..='9' => {
                if let Some(last) = tokens.last_mut()
                    && last.kind == BenchSyntaxKind::Number
                {
                    last.text.push_str(&ch.to_string());
                    continue;
                }
                tokens.push(create_number_token(&ch.to_string()));
            }
            _ => {} // Skip whitespace
        }
    }
    tokens
}

fn bench_full_parse(c: &mut Criterion) {
    let grammar = setup_grammar();
    let config = LlConfig::default();
    let tokens = create_tokens("1+2*3+4*5");

    c.bench_function("full_parse_small", |b| {
        b.iter(|| {
            let mut parser = LlParser::new(&grammar, config.clone()).unwrap();
            black_box(parser.parse(black_box(&tokens), BenchNonTerminal::Expr));
        });
    });
}

fn bench_incremental_parse(c: &mut Criterion) {
    use sipha::incremental::{IncrementalParser, TextEdit};
    use sipha::syntax::{TextRange, TextSize};

    let grammar = setup_grammar();
    let config = LlConfig::default();
    let tokens1 = create_tokens("1+2*3");
    let tokens2 = create_tokens("1+2*4");

    // Initial parse
    let parser1 = LlParser::new(&grammar, config.clone()).unwrap();
    let mut incremental_parser1 = IncrementalParser::new(parser1);
    let result1 = incremental_parser1.parse_incremental_with_grammar(
        &tokens1,
        None,
        &[],
        BenchNonTerminal::Expr,
        &grammar,
    );

    c.bench_function("incremental_parse_small_edit", |b| {
        b.iter(|| {
            let parser = LlParser::new(&grammar, config.clone()).unwrap();
            let mut incremental_parser = IncrementalParser::new(parser);
            // Re-parse initial to get fresh state
            let _ = incremental_parser.parse_incremental_with_grammar(
                &tokens1,
                None,
                &[],
                BenchNonTerminal::Expr,
                &grammar,
            );
            let edits = vec![TextEdit {
                range: TextRange::new(TextSize::from(4), TextSize::from(5)),
                new_text: "4".into(),
            }];
            black_box(incremental_parser.parse_incremental(
                black_box(&tokens2),
                Some(&result1.root),
                black_box(&edits),
                BenchNonTerminal::Expr,
                None, // No grammar for cache population in benchmark
            ));
        });
    });
}

fn bench_grammar_analysis(c: &mut Criterion) {
    use sipha::grammar::analysis::GrammarMetrics;

    let grammar = setup_grammar();

    c.bench_function("grammar_metrics", |b| {
        b.iter(|| {
            black_box(GrammarMetrics::compute(black_box(&grammar)));
        });
    });
}

#[cfg(feature = "backend-glr")]
#[allow(dead_code)]
fn glr_leaf_node() -> std::sync::Arc<sipha::syntax::GreenNode<BenchSyntaxKind>> {
    use sipha::syntax::{GreenNode, TextSize};
    // Create a simple leaf node for benchmarking
    // GreenNode::new already returns Arc
    GreenNode::new(BenchSyntaxKind::Number, vec![], TextSize::from(1))
}

#[cfg(feature = "backend-glr")]
fn bench_glr_stack_sharing(c: &mut Criterion) {
    let leaf = glr_leaf_node();

    c.bench_function("glr_stack_fork_merge", |b| {
        b.iter(|| {
            let mut base = GlrStack::with_initial_state(0);
            for state in 1..64 {
                base.push(state, vec![leaf.clone()]);
            }
            let mut fork = base.fork();
            fork.push(128, vec![leaf.clone()]);
            let mut merged = base.clone();
            merged.merge(&fork);
            black_box(merged.len());
        });
    });

    c.bench_function("glr_stack_shared_prefix_len", |b| {
        b.iter(|| {
            let mut left = GlrStack::with_initial_state(0);
            let mut right = GlrStack::with_initial_state(0);
            for state in 1..128 {
                let nodes = vec![glr_leaf_node()];
                left.push(state, nodes.clone());
                right.push(state, nodes);
            }
            right.push(256, vec![glr_leaf_node()]);
            black_box(left.shared_prefix_len(&right));
        });
    });
}

#[cfg(not(feature = "backend-glr"))]
criterion_group!(
    benches,
    bench_full_parse,
    bench_incremental_parse,
    bench_grammar_analysis
);

#[cfg(feature = "backend-glr")]
criterion_group!(
    benches,
    bench_full_parse,
    bench_incremental_parse,
    bench_grammar_analysis,
    bench_glr_stack_sharing
);
criterion_main!(benches);
