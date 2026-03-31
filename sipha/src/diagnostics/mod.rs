//! Diagnostics, spans, line indexing, and error formatting.

pub mod error;
pub mod grammar_names;

pub use grammar_names::{GrammarNames, SliceGrammarNames};
#[cfg(feature = "std")]
pub mod line_index;
#[cfg(feature = "std")]
pub mod parsed_doc;
#[cfg(feature = "std")]
pub mod source_map;

#[cfg(feature = "utf16")]
pub mod utf16;

#[cfg(feature = "lsp")]
pub mod lsp;

#[cfg(feature = "miette")]
pub mod miette_support;
