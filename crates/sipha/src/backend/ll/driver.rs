//! LL Parser Driver Adapter
//!
//! This module provides a `ParserDriver` implementation that wraps the LL parser.

use crate::backend::ll::{config::LlConfig, table::ParsingTable};
use crate::error::ParseResult;
use crate::grammar::{Grammar, NonTerminal, Token};
use std::sync::Arc;

/// The state for the LL parser driver
#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub struct LlDriverState {
    /// Current position in the token stream
    position: usize,
    /// Current parse stack (non-terminals to parse)
    stack: Vec<LlStackItem>,
}

/// Item on the LL parse stack
#[derive(Debug, Clone)]
pub enum LlStackItem {
    /// Non-terminal to expand
    NonTerminal(String),
    /// Terminal to match
    Terminal,
    /// Node start marker
    NodeStart { kind_idx: usize },
    /// Node end marker
    NodeEnd,
}

/// Decision made by the LL driver
#[derive(Debug, Clone)]
pub enum LlDecision {
    /// Shift (consume a token)
    Shift,
    /// Expand a non-terminal
    Expand { rule_index: usize },
    /// Accept the parse
    Accept,
    /// Error - no valid parse action
    Error,
}

/// LL Parser Driver implementation
pub struct LlDriver<T: Token, N: NonTerminal> {
    grammar: Arc<Grammar<T, N>>,
    table: ParsingTable<T, N>,
    config: LlConfig,
}

impl<T: Token, N: NonTerminal> LlDriver<T, N> {
    /// Create a new LL driver
    ///
    /// # Errors
    /// Returns an error if the parsing table cannot be built.
    pub fn new(grammar: Arc<Grammar<T, N>>, config: LlConfig) -> Result<Self, String> {
        let table = ParsingTable::new(&grammar, config.lookahead)?;

        Ok(Self {
            grammar,
            table,
            config,
        })
    }

    /// Parse using the LL algorithm
    ///
    /// This method wraps the existing LL parser functionality
    pub fn parse(&self, input: &[T], entry: &N) -> ParseResult<T, N> {
        use crate::backend::ll::LlParserState;

        let mut state = LlParserState::new();
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
    pub fn table(&self) -> &ParsingTable<T, N> {
        &self.table
    }

    /// Get the configuration
    #[must_use]
    pub const fn config(&self) -> &LlConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    // Tests would require full grammar and token setup
}
