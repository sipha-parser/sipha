#![cfg(feature = "incremental")]

use proptest::prelude::*;

use sipha::prelude::*;
use sipha::tree::sexp::{syntax_node_to_sexp, SexpOptions};

fn ab_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.node(1, |g| {
            g.zero_or_more(|g| {
                g.token(10, |g| {
                    g.choice(|g| g.byte(b'a'), |g| g.byte(b'b'));
                });
            });
        });
        g.end_of_input();
        g.accept();
    });
    g.finish().expect("grammar should be valid")
}

fn apply_single_edit(old: &[u8], start: usize, end: usize, replacement: &[u8]) -> (TextEdit, Vec<u8>) {
    let edit = TextEdit {
        start: start as u32,
        end: end as u32,
        new_text: replacement.to_vec(),
    };
    let new_source = TextEdit::apply_edits(old, std::slice::from_ref(&edit));
    (edit, new_source)
}

proptest! {
    // Keep this small so it runs quickly in CI while still exercising reuse logic.
    #![proptest_config(ProptestConfig {
        cases: 256,
        max_shrink_iters: 0,
        .. ProptestConfig::default()
    })]

    #[test]
    fn incremental_reparse_matches_full_parse(
        old_len in 0usize..64,
        // a single edit expressed as (start,end,new_len); replacement bytes are generated separately
        start in 0usize..64,
        end in 0usize..64,
        new_len in 0usize..32,
        seed in any::<u64>(),
    ) {
        let built = ab_grammar();
        let graph = built.as_graph();

        // Deterministic pseudo-random source derived from seed.
        let mut s = seed;
        let mut old_source = Vec::with_capacity(old_len);
        for _ in 0..old_len {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1);
            old_source.push(if (s & 1) == 0 { b'a' } else { b'b' });
        }

        let mut engine = Engine::new();
        let old_out = engine.parse(&graph, &old_source).expect("old_source should parse");
        let old_root = old_out.syntax_root(&old_source).expect("root should exist");

        let start = start.min(old_source.len());
        let end = end.min(old_source.len());
        let (start, end) = if start <= end { (start, end) } else { (end, start) };

        let mut repl = Vec::with_capacity(new_len);
        for _ in 0..new_len {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1);
            repl.push(if (s & 1) == 0 { b'a' } else { b'b' });
        }

        let (edit, new_source) = apply_single_edit(&old_source, start, end, &repl);

        // Incremental reparse.
        let inc_root = sipha::parse::incremental::reparse(
            &mut engine,
            &graph,
            &old_source,
            &old_root,
            std::slice::from_ref(&edit),
        ).expect("incremental reparse should succeed").expect("root should exist");

        // Full parse of new source.
        let full_out = engine.parse(&graph, &new_source).expect("new_source should parse");
        let full_root = full_out.syntax_root(&new_source).expect("root should exist");

        let opts = SexpOptions::full();
        let inc_sexp = syntax_node_to_sexp(&inc_root, &opts);
        let full_sexp = syntax_node_to_sexp(&full_root, &opts);
        prop_assert_eq!(inc_sexp, full_sexp);
    }
}

