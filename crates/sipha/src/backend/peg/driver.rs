//! PEG Parser Driver Adapter
//!
//! This module provides a `ParserDriver` implementation that wraps the PEG parser.

use crate::backend::peg::{config::PegConfig, state::PegParserState};
use crate::error::ParseResult;
use crate::grammar::{Grammar, NonTerminal, Token};
use std::sync::Arc;

/// The state for the PEG parser driver
#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub struct PegDriverState {
    /// Current position in the token stream
    position: usize,
    /// Memoization table index
    memo_index: usize,
    /// Left recursion detection stack
    lr_stack: Vec<String>,
}

/// Decision made by the PEG driver
#[derive(Debug, Clone)]
pub enum PegDecision {
    /// Try to match a rule
    TryRule { rule_name: String },
    /// Match succeeded, consume tokens
    Match { consumed: usize },
    /// Match failed, backtrack
    Backtrack { to_position: usize },
    /// Left recursion detected
    LeftRecursion { rule_name: String },
    /// Accept the parse
    Accept,
    /// Error - no valid parse
    Error,
}

/// PEG Parser Driver implementation
pub struct PegDriver<T: Token, N: NonTerminal> {
    grammar: Arc<Grammar<T, N>>,
    config: PegConfig,
}

impl<T: Token, N: NonTerminal> PegDriver<T, N> {
    /// Create a new PEG driver
    pub fn new(grammar: Arc<Grammar<T, N>>, config: PegConfig) -> Self {
        Self { grammar, config }
    }

    /// Parse using the PEG algorithm
    ///
    /// This method wraps the existing PEG parser functionality
    pub fn parse(&self, input: &[T], entry: &N) -> ParseResult<T, N> {
        let mut state = PegParserState::new();
        super::parser::parse(&self.grammar, input, entry, &self.config, &mut state)
    }

    /// Get the underlying grammar
    #[must_use]
    pub fn grammar(&self) -> &Grammar<T, N> {
        &self.grammar
    }

    /// Get the configuration
    #[must_use]
    pub const fn config(&self) -> &PegConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    // Tests would require full grammar and token setup
}
