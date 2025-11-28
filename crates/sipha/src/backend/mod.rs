//! # Parser Backends
//!
//! This module provides parser backend implementations for different parsing algorithms.
//!
//! ## Available Backends
//!
//! - **LL(k)**: Top-down predictive parsing with configurable lookahead
//!   - Supports left-recursion elimination
//!   - Configurable error recovery
//!   - Incremental parsing support
//!
//! ## Usage
//!
//! Backends implement the [`ParserBackend`] trait, which provides a unified interface
//! for parsing regardless of the underlying algorithm.
//!
//! ```rust,no_run
//! use sipha::backend::ll::LlParser;
//! use sipha::backend::ParserBackend;
//!
//! // Example usage (types would need to be properly defined with Token and NonTerminal traits)
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! #     // This is a conceptual example - actual usage requires implementing
//! #     // Token and NonTerminal traits for your types
//! #     Ok(())
//! # }
//!
//! // let mut parser = LlParser::new(&grammar, config)?;
//! // let result = parser.parse(&tokens, entry_point);
//! ```
//!
//! ## Error Recovery
//!
//! Backends support configurable error recovery strategies:
//!
//! - **Synchronization tokens**: Skip to known recovery points
//! - **Delimited recovery**: Skip to matching delimiters
//! - **Best-effort parsing**: Continue parsing despite errors
//!
//! See [`ParserBackend::parse_incremental`] for incremental parsing support.

use crate::error::ParseResult;
use crate::grammar::{Grammar, NonTerminal, Token};
use crate::incremental::IncrementalSession;

#[cfg(feature = "backend-ll")]
pub mod ll;

#[cfg(feature = "backend-lr")]
pub mod lr;

#[cfg(feature = "backend-glr")]
pub mod glr;

pub mod common;

/// Main parser backend trait
pub trait ParserBackend<T, N>: Sized + Send
where
    T: Token,
    N: NonTerminal,
{
    type Config: Default + Clone;
    type Error: std::error::Error + Send + Sync + 'static;
    type State: Send + Sync;

    /// Create parser from grammar
    ///
    /// # Errors
    ///
    /// Returns an error if the parser cannot be created from the given grammar and config.
    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error>;

    /// Parse input tokens
    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N>;

    /// Parse with incremental support (default: full reparse)
    fn parse_incremental(
        &mut self,
        input: &[T],
        old_tree: Option<&crate::syntax::GreenNode<T::Kind>>,
        edits: &[crate::incremental::TextEdit],
        entry: N,
    ) -> ParseResult<T, N>
    where
        T: Token,
    {
        if edits.is_empty() {
            return self.parse(input, entry);
        }

        let session = IncrementalSession::new(old_tree, edits);
        self.parse_with_session(input, entry, &session)
    }

    /// Hook that receives pre-computed incremental context.
    fn parse_with_session(
        &mut self,
        input: &[T],
        entry: N,
        session: &IncrementalSession<'_, T::Kind>,
    ) -> ParseResult<T, N>
    where
        T: Token,
    {
        let _ = session;
        self.parse(input, entry)
    }

    /// Validate grammar compatibility
    fn validate(grammar: &Grammar<T, N>) -> Vec<crate::grammar::GrammarError<T, N>>;

    /// Get backend capabilities
    fn capabilities() -> BackendCapabilities;

    /// Access internal state for debugging
    fn state(&self) -> &Self::State;
}

/// Capabilities of a parsing backend
///
/// Note: This struct uses multiple boolean fields for clarity in the public API.
/// While this triggers `clippy::struct_excessive_bools`, the self-documenting
/// nature of named boolean fields is preferred over bitflags for API clarity.
#[derive(Debug, Clone)]
#[allow(clippy::struct_excessive_bools)]
pub struct BackendCapabilities {
    pub name: &'static str,
    pub algorithm: Algorithm,
    pub supports_left_recursion: bool,
    pub supports_ambiguity: bool,
    pub supports_incremental: bool,
    pub supports_error_recovery: bool,
    pub max_lookahead: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Algorithm {
    LL,
    LR,
    GLR,
    PEG,
    Packrat,
    Earley,
}
