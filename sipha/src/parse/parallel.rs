//! Parallel parsing helpers (requires `std` + `parallel`).
//!
//! This module is intentionally conservative: it parallelizes **across independent
//! inputs** by running one [`Engine`](crate::parse::engine::Engine) per worker thread.
//! A single PEG parse remains sequential.

use rayon::prelude::*;

use crate::parse::engine::{Engine, ParseError, ParseOutput};
use crate::parse::insn::ParseGraph;

/// Parse many inputs concurrently using a shared grammar graph.
///
/// The output order matches the input order.
///
/// # Errors
///
/// Each input yields its own `Result`. A failure parsing one input does not stop others.
#[must_use]
pub fn parse_many<'g>(
    graph: &ParseGraph<'g>,
    inputs: &[&[u8]],
) -> Vec<Result<ParseOutput, ParseError>> {
    parse_many_with(graph, inputs, Engine::new)
}

/// Like [`parse_many`], but constructs each [`Engine`] using `make_engine`.
///
/// Useful for enabling memoisation via `Engine::with_memo()` or tuning capacities.
#[must_use]
pub fn parse_many_with<'g, F>(
    graph: &ParseGraph<'g>,
    inputs: &[&[u8]],
    make_engine: F,
) -> Vec<Result<ParseOutput, ParseError>>
where
    F: Fn() -> Engine + Sync,
{
    inputs
        .par_iter()
        .map(|input| {
            let mut engine = make_engine();
            engine.parse(graph, input)
        })
        .collect()
}

/// Like [`parse_many`], but constructs syntax roots (green+red) for each successful parse.
#[cfg(feature = "std")]
#[must_use]
pub fn parse_many_syntax_roots<'g>(
    graph: &ParseGraph<'g>,
    inputs: &[&[u8]],
) -> Vec<Result<Option<crate::tree::red::SyntaxNode>, ParseError>> {
    parse_many_with(graph, inputs, Engine::new)
        .into_par_iter()
        .zip(inputs.par_iter().copied())
        .map(|(res, input)| res.map(|out| out.syntax_root(input)))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::builder::GrammarBuilder;

    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}

    #[test]
    fn graph_is_sync_engine_is_send() {
        assert_send::<Engine>();
        assert_sync::<ParseGraph<'_>>();
    }

    #[test]
    fn parse_many_matches_serial() {
        let mut g = GrammarBuilder::new();
        g.rule("start", |g| {
            g.byte(b'a');
            g.end_of_input();
            g.accept();
        });
        let built = g.finish().unwrap();
        let graph = built.as_graph();

        let inputs: Vec<&[u8]> = vec![b"a", b"a", b"a", b"a"];

        let serial: Vec<_> = inputs
            .iter()
            .map(|i| {
                let mut e = Engine::new();
                e.parse(&graph, i)
            })
            .collect();
        let parallel = parse_many(&graph, &inputs);

        assert_eq!(serial.len(), parallel.len());
        for (a, b) in serial.iter().zip(parallel.iter()) {
            assert_eq!(a.is_ok(), b.is_ok());
            if let (Ok(ao), Ok(bo)) = (a, b) {
                assert_eq!(ao.consumed, bo.consumed);
                assert_eq!(ao.tree_events.len(), bo.tree_events.len());
                assert_eq!(ao.events.len(), bo.events.len());
            }
        }
    }
}
