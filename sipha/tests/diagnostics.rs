#![cfg(feature = "std")]

#[path = "common/grammar.rs"]
mod grammar;

use grammar::expr_grammar;
use sipha::prelude::*;

#[test]
fn diagnostic_formats_with_line_and_snippet() {
    let built = expr_grammar();
    let graph = built.as_graph();
    let mut engine = Engine::new();

    let src = b"1 + * 2\n3";
    let err = engine.parse(&graph, src).unwrap_err();
    let ParseError::NoMatch(diag) = err else {
        panic!("expected NoMatch parse error, got: {err:?}");
    };

    // Format via Diagnostic directly (no need for ParsedDoc when parse fails).
    let li = LineIndex::new(src);
    let s = diag.format_with_source(src, &li, Some(&graph.literals), Some(&graph));
    assert!(s.contains("line") || s.contains(":"), "{s}");
    assert!(s.contains('^'), "expected caret snippet, got: {s}");
}
