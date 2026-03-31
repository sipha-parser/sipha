//! Convenience helpers for common parse flows.

use crate::diagnostics::parsed_doc::ParsedDoc;
use crate::parse::engine::{Engine, ParseError};
use crate::parse::insn::ParseGraph;

/// Parse the given source and build a [`ParsedDoc`] on success.
///
/// This is a convenience wrapper around `Engine::parse` + `ParsedDoc::from_slice`.
pub fn parse_to_doc(
    engine: &mut Engine,
    graph: &ParseGraph<'_>,
    source: impl AsRef<[u8]>,
) -> Result<ParsedDoc, ParseError> {
    let src = source.as_ref();
    let out = engine.parse(graph, src)?;
    ParsedDoc::from_slice(src, &out)
        .ok_or_else(|| ParseError::BadGraph(crate::parse::engine::BadGraphKind::InvalidTreeEvents))
}

