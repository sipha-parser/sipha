#![no_main]

use libfuzzer_sys::fuzz_target;
use sipha::prelude::*;
use std::sync::OnceLock;

fuzz_target!(|data: &[u8]| {
    // Keep the grammar stable; fuzz only the input.
    static BUILT: OnceLock<BuiltGraph> = OnceLock::new();
    let built = BUILT.get_or_init(sipha_fuzz::grammars::jsonc_trivia_skip);
    let graph = built.as_graph();

    let mut engine = Engine::new().with_memo();
    let _ = engine.parse(&graph, data);
});

