#![no_main]

use libfuzzer_sys::fuzz_target;
use sipha::prelude::*;
use sipha_macros::sipha_grammar;
use std::sync::OnceLock;

fn macro_grammar() -> BuiltGraph {
    sipha_grammar! {
        @trivia ws;
        @start start;

        #[lexer] ws = (" " | "\t" | "\n")*;
        #[parser] start = "a" "b"+;
    }
}

fuzz_target!(|data: &[u8]| {
    // Keep the grammar stable; fuzz only the input.
    static BUILT: OnceLock<BuiltGraph> = OnceLock::new();
    let built = BUILT.get_or_init(macro_grammar);
    let graph = built.as_graph();

    let mut engine = Engine::new();
    let _ = engine.parse(&graph, data);
});

