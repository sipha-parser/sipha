#![cfg(feature = "std")]

mod common;

use common::{expr_grammar, sexp};
use proptest::prelude::*;
use sipha::prelude::*;

fn grammarish_bytes() -> impl Strategy<Value = Vec<u8>> {
    // Keep inputs in a subset that our `expr_grammar` can usually handle.
    // We still `prop_assume!` successful parses to keep the property focused on equivalence.
    const ALLOWED: &[u8] = b"0123456789+*() ";
    prop::collection::vec(prop::sample::select(ALLOWED), 1..128)
}

proptest! {
    #[test]
    fn memoised_and_non_memoised_are_equivalent(src in grammarish_bytes()) {
        let built = expr_grammar();
        let graph = built.as_graph();

        let mut e1 = Engine::new();
        let out1 = match e1.parse(&graph, &src) {
            Ok(o) => o,
            Err(_) => return Ok(()),
        };
        prop_assume!(out1.consumed as usize == src.len());
        let Some(root1) = out1.syntax_root(&src) else {
            return Ok(());
        };

        let mut e2 = Engine::new().with_memo();
        let out2 = match e2.parse(&graph, &src) {
            Ok(o) => o,
            Err(_) => return Err(TestCaseError::fail("memoised parse failed")),
        };
        prop_assert_eq!(out1.consumed, out2.consumed);
        prop_assert_eq!(out2.consumed as usize, src.len());

        let Some(root2) = out2.syntax_root(&src) else {
            return Err(TestCaseError::fail("memoised syntax_root failed"));
        };
        prop_assert_eq!(sexp(&root1), sexp(&root2));
    }
}

#[cfg(feature = "incremental")]
proptest! {
    #[test]
    fn incremental_reparse_matches_full_parse(
        old_src in grammarish_bytes(),
        start in 0u8..=127,
        end_delta in 0u8..=127,
        insert in grammarish_bytes(),
    ) {
        let built = expr_grammar();
        let graph = built.as_graph();

        let mut engine = Engine::new();
        let old_out = match engine.parse(&graph, &old_src) {
            Ok(o) => o,
            Err(_) => return Ok(()),
        };
        prop_assume!(old_out.consumed as usize == old_src.len());
        let Some(old_root) = old_out.syntax_root(&old_src) else {
            return Ok(());
        };

        let len = old_src.len();
        prop_assume!(len > 0);
        let s = (start as usize) % len;
        let e = s + ((end_delta as usize) % (len - s));

        let edit = sipha::parse::incremental::TextEdit {
            start: s as u32,
            end: e as u32,
            new_text: insert,
        };
        let edits = [edit];
        let new_src = sipha::parse::incremental::TextEdit::apply_edits(&old_src, &edits);

        // Full parse and root.
        let full_out = match engine.parse(&graph, &new_src) {
            Ok(o) => o,
            Err(_) => return Ok(()),
        };
        prop_assume!(full_out.consumed as usize == new_src.len());
        let Some(full_root) = full_out.syntax_root(&new_src) else {
            return Ok(());
        };

        // Incremental reparse.
        let mut engine2 = Engine::new();
        let inc_root = match sipha::parse::incremental::reparse(&mut engine2, &graph, &old_src, &old_root, &edits) {
            Ok(Some(r)) => r,
            _ => return Ok(()),
        };

        prop_assert_eq!(sexp(&full_root), sexp(&inc_root));
    }
}
