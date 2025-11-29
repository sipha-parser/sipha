//! Semantic predicates for context-sensitive parsing
//!
//! This module provides the `SemanticPredicate` trait for implementing
//! semantic checks during parsing. Semantic predicates allow parsing decisions
//! to be based on semantic information (e.g., symbol tables, type information)
//! rather than just syntax.
//!
//! # Example
//!
//! ```rust,no_run
//! use sipha::grammar::predicate::SemanticPredicate;
//! use sipha::grammar::{Grammar, Token, NonTerminal};
//!
//! #[derive(Debug)]
//! struct IsTypeDefined;
//!
//! impl<T: Token, N: NonTerminal> SemanticPredicate<T, N> for IsTypeDefined {
//!     fn check(&self, grammar: &Grammar<T, N>, input: &[T], pos: usize) -> bool {
//!         // Check if a type is defined in the symbol table
//!         // This is a simplified example
//!         pos < input.len()
//!     }
//! }
//! ```

use crate::grammar::{Grammar, NonTerminal, Token};

/// Trait for semantic predicates that can be evaluated during parsing.
///
/// Semantic predicates allow parsing decisions to be based on semantic
/// information (e.g., symbol tables, type information) rather than just
/// syntax. This enables parsing of context-sensitive languages.
///
/// # Thread Safety
///
/// Implementations must be `Send + Sync` to be used in multi-threaded
/// parsing contexts.
pub trait SemanticPredicate<T, N>: std::fmt::Debug + Send + Sync
where
    T: Token,
    N: NonTerminal,
{
    /// Check if the semantic predicate is satisfied at the current position.
    ///
    /// # Arguments
    ///
    /// * `grammar` - The grammar being used for parsing
    /// * `input` - The input token stream
    /// * `pos` - The current position in the input
    ///
    /// # Returns
    ///
    /// `true` if the predicate is satisfied, `false` otherwise.
    fn check(&self, grammar: &Grammar<T, N>, input: &[T], pos: usize) -> bool;
}
