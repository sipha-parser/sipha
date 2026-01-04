//! Memory usage benchmarks
//!
//! This benchmark suite measures memory consumption for various parsing scenarios.
//! It helps identify memory hotspots and optimize allocation patterns.

use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use sipha::syntax::{GreenNode, GreenNodeBuilder, SyntaxKind, SyntaxNode, TextSize};
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BenchSyntaxKind {
    Number,
    Plus,
    Expr,
}

impl SyntaxKind for BenchSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

/// Benchmark memory usage for syntax tree construction
fn bench_tree_construction_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("tree_construction_memory");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(2));

    let tree_sizes = vec![
        ("small", 10),
        ("medium", 100),
        ("large", 1000),
        ("xlarge", 10000),
    ];

    for (size_name, node_count) in tree_sizes {
        group.bench_with_input(
            BenchmarkId::new("green_tree", size_name),
            &node_count,
            |b, count| {
                b.iter(|| {
                    let mut builder = GreenNodeBuilder::<BenchSyntaxKind>::new();
                    // Build a balanced tree structure
                    build_tree_recursive(&mut builder, BenchSyntaxKind::Expr, *count);
                    let green = builder.finish();
                    black_box(green);
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("red_tree", size_name),
            &node_count,
            |b, count| {
                let mut builder = GreenNodeBuilder::<BenchSyntaxKind>::new();
                build_tree_recursive(&mut builder, BenchSyntaxKind::Expr, *count);
                let green = builder.finish();
                b.iter(|| {
                    let red = SyntaxNode::new_root(green.clone());
                    black_box(red);
                });
            },
        );
    }

    group.finish();
}

fn build_tree_recursive(
    builder: &mut GreenNodeBuilder<BenchSyntaxKind>,
    kind: BenchSyntaxKind,
    remaining: usize,
) {
    if remaining == 0 {
        builder.token(BenchSyntaxKind::Number, "1");
        return;
    }

    builder.start_node(kind);
    if remaining > 1 {
        // Build two children
        build_tree_recursive(builder, BenchSyntaxKind::Expr, remaining / 2);
        builder.token(BenchSyntaxKind::Plus, "+");
        build_tree_recursive(
            builder,
            BenchSyntaxKind::Expr,
            remaining - remaining / 2 - 1,
        );
    } else {
        builder.token(BenchSyntaxKind::Number, "1");
    }
    builder.finish_node();
}

/// Benchmark memory usage for incremental parsing cache
fn bench_cache_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_memory");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(2));

    use lasso::Spur;
    use sipha::incremental::cache::{CacheEntry, ContentCacheKey, IncrementalCache};
    use std::sync::Arc;

    let cache_sizes = vec![("small", 10), ("medium", 100), ("large", 1000)];

    for (size_name, entry_count) in cache_sizes {
        group.bench_with_input(
            BenchmarkId::new("parse_cache", size_name),
            &entry_count,
            |b, count| {
                b.iter(|| {
                    let mut cache: IncrementalCache<BenchSyntaxKind> =
                        IncrementalCache::new(*count);

                    // Create cache entries
                    for i in 0..*count {
                        let mut builder = GreenNodeBuilder::<BenchSyntaxKind>::new();
                        builder.start_node(BenchSyntaxKind::Expr);
                        builder.token(BenchSyntaxKind::Number, &i.to_string());
                        builder.finish_node();
                        let node = Arc::new(builder.finish());

                        let key = ContentCacheKey::new(
                            Spur::default(), // Using default spur for benchmark
                            i as u64,
                            0,
                        );
                        let entry = CacheEntry::new(node, 1, TextSize::from(1), 0);
                        cache.insert(key, entry);
                    }

                    black_box(cache);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark memory usage for different allocation strategies
fn bench_allocation_strategies(c: &mut Criterion) {
    let mut group = c.benchmark_group("allocation_strategies");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(2));

    let sizes = vec![("small", 100), ("medium", 1000), ("large", 10000)];

    for (size_name, node_count) in sizes {
        group.bench_with_input(
            BenchmarkId::new("standard", size_name),
            &node_count,
            |b, count| {
                b.iter(|| {
                    let mut nodes = Vec::new();
                    for i in 0..*count {
                        let mut builder = GreenNodeBuilder::<BenchSyntaxKind>::new();
                        builder.start_node(BenchSyntaxKind::Expr);
                        builder.token(BenchSyntaxKind::Number, &i.to_string());
                        builder.finish_node();
                        let node = builder.finish();
                        nodes.push(node);
                    }
                    black_box(nodes);
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_tree_construction_memory,
    bench_cache_memory,
    bench_allocation_strategies
);
criterion_main!(benches);
