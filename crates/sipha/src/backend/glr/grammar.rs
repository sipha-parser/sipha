//! GLR-specific grammar representation
//!
//! This module defines `GlrGrammar`, which wraps the LR parsing table
//! and adds GLR-specific structures for handling ambiguity.

#[cfg(feature = "backend-lr")]
use crate::backend::lr::LrParsingTable;
use crate::grammar::{Grammar, NonTerminal, Token};

/// GLR-specific grammar representation
///
/// GLR extends LR parsing to handle ambiguous grammars by maintaining
/// multiple parser stacks. This wraps the LR table and adds GLR-specific
/// configuration.
pub struct GlrGrammar<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Original grammar (needed for accessing rule expressions)
    pub original_grammar: Grammar<T, N>,
    /// LR parsing table (reused from LR backend)
    #[cfg(feature = "backend-lr")]
    pub lr_table: LrParsingTable<T, N>,
    /// Original grammar entry point
    pub entry_point: N,
}

impl<T, N> GlrGrammar<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new GLR grammar from an LR table
    #[must_use]
    #[cfg(feature = "backend-lr")]
    pub fn new(
        original_grammar: Grammar<T, N>,
        lr_table: LrParsingTable<T, N>,
        entry_point: N,
    ) -> Self {
        Self {
            original_grammar,
            lr_table,
            entry_point,
        }
    }

    /// Get the LR table
    #[must_use]
    #[cfg(feature = "backend-lr")]
    pub fn lr_table(&self) -> &LrParsingTable<T, N> {
        &self.lr_table
    }
}
