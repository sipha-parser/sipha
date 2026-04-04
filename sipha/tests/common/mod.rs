#![cfg(feature = "std")]

//! Shared integration-test fixtures.
//!
//! [`grammar`] is the minimal dependency for parse-error / diagnostics tests.
//! [`helpers`] provides [`sexp`] for tree / property tests. Successful-parse setup lives next to the
//! tests that need it (see `expr_tree.rs`).

pub mod grammar;
pub mod helpers;

pub use grammar::expr_grammar;
pub use helpers::sexp;
