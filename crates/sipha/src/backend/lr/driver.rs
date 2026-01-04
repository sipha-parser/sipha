//! LR Parser Driver Adapter
//!
//! This module provides a `ParserDriver` implementation that wraps the LR parser.

use crate::backend::lr::{config::LrConfig, state::LrParserState, table::LrParsingTable};
use crate::error::ParseResult;
use crate::grammar::{Grammar, NonTerminal, Token};
use std::sync::Arc;

/// The state for the LR parser driver
#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub struct LrDriverState {
    /// Stack of (`state_id`, nodes)
    stack: Vec<(usize, usize)>,
    /// Current position in the token stream
    position: usize,
}

/// Decision made by the LR driver
#[derive(Debug, Clone)]
pub enum LrDecision {
    /// Shift token and go to state
    Shift { next_state: usize },
    /// Reduce by rule
    Reduce { rule_index: usize, pop_count: usize },
    /// Accept the parse
    Accept,
    /// Error - no valid parse action
    Error,
}

/// LR Parser Driver implementation
pub struct LrDriver<T: Token, N: NonTerminal> {
    grammar: Arc<Grammar<T, N>>,
    table: LrParsingTable<T, N>,
    config: LrConfig,
}

impl<T: Token, N: NonTerminal> LrDriver<T, N> {
    /// Create a new LR driver
    ///
    /// # Errors
    /// Returns an error if the parsing table cannot be built.
    pub fn new(grammar: Arc<Grammar<T, N>>, config: LrConfig) -> Result<Self, String> {
        let table = LrParsingTable::new(&grammar, config.use_lalr)?;

        Ok(Self {
            grammar,
            table,
            config,
        })
    }

    /// Parse using the LR algorithm
    ///
    /// This method wraps the existing LR parser functionality
    pub fn parse(&self, input: &[T], entry: &N) -> ParseResult<T, N> {
        let mut state = LrParserState::new();
        super::parser::parse(
            &self.grammar,
            &self.table,
            input,
            entry,
            &self.config,
            &mut state,
        )
    }

    /// Get the underlying grammar
    #[must_use]
    pub fn grammar(&self) -> &Grammar<T, N> {
        &self.grammar
    }

    /// Get the parsing table
    #[must_use]
    pub fn table(&self) -> &LrParsingTable<T, N> {
        &self.table
    }

    /// Get the configuration
    #[must_use]
    pub const fn config(&self) -> &LrConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    // Tests would require full grammar and token setup
}
