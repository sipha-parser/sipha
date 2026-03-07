//! Example: build a small grammar using `sipha_grammar`! macro.

use sipha::prelude::*;
use sipha_macros::sipha_grammar;

fn main() -> Result<(), sipha::engine::ParseError> {
    let built = sipha_grammar! {
        @trivia ws;
        @start start;

        #[lexer] ws = (" " | "\t" | "\n")*;
        #[parser] start = "a" "b"+;
    };

    let graph = built.as_graph();
    let mut engine = Engine::new();

    let out = engine.parse(&graph, b"abbb")?;
    assert_eq!(out.consumed, 4);

    // "a" alone fails (need at least one "b")
    assert!(engine.parse(&graph, b"a").is_err());

    println!("macro_grammar example OK");
    Ok(())
}
