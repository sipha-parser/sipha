//! Parsing-related APIs (grammar builder, engine, VM IR, memoisation).
//!
//! Note: incremental reparsing APIs live in [`incremental`] and are feature-gated.

#[cfg(feature = "std")]
pub mod builder;
pub mod capture;
#[cfg(feature = "std")]
pub mod codegen;
pub mod context;
#[cfg(feature = "std")]
pub mod expr;
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

pub mod sublanguage;
