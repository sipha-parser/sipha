//! Common grammar fragments (feature-gated).
//!
//! These are intentionally small, composable helpers for building lexer/parser rules with
//! [`GrammarBuilder`](crate::parse::builder::GrammarBuilder).
//!
//! Note: sipha parses **bytes**. Some helpers here use Unicode terminals like
//! `char(...)` / `char_range(...)` (UTF-8 decoding in the VM) and therefore match Unicode
//! *codepoints*.

pub mod ascii;
pub mod comments;
pub mod strings;
pub mod unicode;

pub use comments::*;

