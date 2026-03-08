//! Benchmarks for the sipha parse engine.
//!
//! - `parse_success`: long digit list; exercises the success path and the
//!   optimization that moves `events`/`tree_events` out of the engine instead
//!   of cloning (saves two large allocations per parse).
//! - `parse_memo`: choice-heavy grammar; compares `memo_on` (uses
//!   `query_replay` to extend the event buffer in place on cache hit) vs
//!   `memo_off`.

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use sipha::prelude::*;
use sipha::types::classes;

// ─── Digit-list grammar (many events per parse) ───────────────────────────────

fn build_digit_list_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    // start = digit+ ; digit = [0-9]
    g.rule("start", |g| {
        g.repeat(1.., |g| {
            g.call("digit");
        });
        g.end_of_input();
        g.accept();
    });
    g.rule("digit", |g| {
        g.class(classes::DIGIT);
    });
    g.finish().expect("digit list grammar valid")
}

// ─── Grammar that benefits from memo (backtracking) ───────────────────────────

fn build_memo_heavy_grammar() -> BuiltGraph {
    // S = (A | B | C)+ ; A = "aaa" ; B = "bbb" ; C = "ccc"
    // Repeated choice causes same (rule, pos) to be re-entered.
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.repeat(1.., |g| {
            g.choices(vec![
                Box::new(|g: &mut GrammarBuilder| {
                    g.call("a");
                }),
                Box::new(|g: &mut GrammarBuilder| {
                    g.call("b");
                }),
                Box::new(|g: &mut GrammarBuilder| {
                    g.call("c");
                }),
            ]);
        });
        g.end_of_input();
        g.accept();
    });
    g.rule("a", |g| {
        g.literal(b"aaa");
    });
    g.rule("b", |g| {
        g.literal(b"bbb");
    });
    g.rule("c", |g| {
        g.literal(b"ccc");
    });
    g.finish().expect("memo grammar valid")
}

fn bench_parse_success(c: &mut Criterion) {
    let built = build_digit_list_grammar();
    let graph = built.as_graph();

    let mut group = c.benchmark_group("parse_success");
    for size in [10_000, 50_000, 100_000] {
        let input: Vec<u8> = (0..size).map(|i| b'0' + (i % 10) as u8).collect();
        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(
            criterion::BenchmarkId::new("digit_list", size),
            &input,
            |b, input| {
                let mut engine = Engine::new();
                b.iter(|| {
                    let out = engine.parse(black_box(&graph), black_box(input.as_slice()));
                    black_box(out)
                });
            },
        );
    }
    group.finish();
}

fn bench_parse_with_memo(c: &mut Criterion) {
    let built = build_memo_heavy_grammar();
    let graph = built.as_graph();
    // Input: "aaabbbcccaaabbbccc..." so we get many rule calls and memo hits on repeat.
    let input: Vec<u8> = (0..30_000)
        .flat_map(|i| {
            let s = match i % 3 {
                0 => b"aaa",
                1 => b"bbb",
                _ => b"ccc",
            };
            s.to_vec()
        })
        .collect();

    let mut group = c.benchmark_group("parse_memo");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("memo_on", |b| {
        let mut engine = Engine::new().with_memo();
        b.iter(|| {
            let out = engine.parse(black_box(&graph), black_box(&input));
            black_box(out)
        });
    });
    group.bench_function("memo_off", |b| {
        let mut engine = Engine::new();
        b.iter(|| {
            let out = engine.parse(black_box(&graph), black_box(&input));
            black_box(out)
        });
    });
    group.finish();
}

criterion_group!(benches, bench_parse_success, bench_parse_with_memo);
criterion_main!(benches);
