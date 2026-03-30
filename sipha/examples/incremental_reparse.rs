//! Incremental reparse with green-tree reuse (`incremental` feature).
//!
//! Run: `cargo run -p sipha --example incremental_reparse --features incremental`

use sipha::parse::builder::GrammarBuilder;
use sipha::parse::incremental::{reparse_with_output, TextEdit};
use sipha::parse::insn::ParseGraph;
use sipha::prelude::*;
use std::sync::Arc;

fn graph() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.rule("start", |g| {
        g.node(1, |g| {
            g.token(10, |g| {
                g.literal(b"hello");
            });
        });
        g.end_of_input();
        g.accept();
    });
    g.finish().expect("grammar")
}

fn main() -> Result<(), sipha::parse::engine::ParseError> {
    let built = graph();
    let graph: ParseGraph<'_> = built.as_graph();
    let source = b"hello";
    let mut engine = Engine::new();
    let out = engine.parse(&graph, source)?;
    let root = out.syntax_root(source).expect("root");

    // No edits: same bytes as `apply_edits` identity, full tree reuse.
    let edits: &[TextEdit] = &[];
    let mut engine = Engine::new();
    let result = reparse_with_output(&mut engine, &graph, source, &root, edits)?;
    assert_eq!(result.parse_output.consumed, source.len() as u32);
    let new_root = result.root.expect("root");
    assert!(
        Arc::ptr_eq(root.green(), new_root.green()),
        "no-op edit should reuse the green root"
    );
    println!("ok: incremental reparse after no-op edit");
    Ok(())
}
