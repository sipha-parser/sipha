//! Diagnostics, spans, line indexing, and error formatting.

pub mod error;
pub mod grammar_names;

pub use grammar_names::{GrammarNames, SliceGrammarNames};
pub mod line_index;
pub mod parsed_doc;
pub mod source_map;

#[cfg(feature = "utf16")]
pub mod utf16;

#[cfg(feature = "miette")]
pub mod miette_support;
