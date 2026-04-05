//! Parsing-related APIs (grammar builder, engine, VM IR, memoisation).
//!
//! Note: incremental reparsing APIs live in [`incremental`] and are feature-gated.

#[cfg(feature = "std")]
pub mod builder;
#[cfg(feature = "std")]
pub mod lexer_batch;
pub mod capture;
#[cfg(feature = "std")]
pub mod codegen;
pub mod context;
#[cfg(feature = "std")]
pub mod expr;
#[cfg(feature = "std")]
pub(crate) mod graph_optimize;
#[cfg_attr(docsrs, doc(cfg(feature = "incremental")))]
#[cfg(feature = "incremental")]
pub mod incremental;
pub mod insn;
#[cfg(feature = "std")]
pub mod memo;
#[cfg(feature = "parallel")]
pub mod parallel;
#[cfg(feature = "experimental_parallel_units")]
pub mod parallel_units;
#[cfg(feature = "partial-reparse")]
pub mod partial_reparse;
pub mod simd;
pub mod string_table;

pub mod engine;
#[cfg(feature = "std")]
pub mod parse_to_doc;

pub mod sublanguage;
