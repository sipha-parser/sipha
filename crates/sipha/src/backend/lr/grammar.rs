//! LR-specific grammar representation
//!
//! This module defines `LrGrammar`, which contains the LR parsing table
//! and cached analysis results needed for LR parsing.

use crate::backend::lr::table::{Action, LrParsingTable, Production};
use crate::grammar::{NonTerminal, Token};
use hashbrown::{HashMap, HashSet};

/// LR-specific grammar representation
///
/// This contains the parsing table and cached analysis results
/// needed for efficient LR parsing.
pub struct LrGrammar<T, N>
where
    T: Clone,
    N: Clone,
{
    /// LR parsing table with action and goto tables
    pub table: LrParsingTable<T, N>,
    /// Cached FIRST sets for all non-terminals
    pub first_sets: HashMap<N, HashSet<T>>,
    /// Cached FOLLOW sets for all non-terminals
    pub follow_sets: HashMap<N, HashSet<T>>,
    /// Original grammar entry point
    pub entry_point: N,
}

impl<T, N> LrGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    /// Create a new LR grammar from a parsing table and cached analysis
    #[must_use]
    pub fn new(
        table: LrParsingTable<T, N>,
        first_sets: HashMap<N, HashSet<T>>,
        follow_sets: HashMap<N, HashSet<T>>,
        entry_point: N,
    ) -> Self {
        Self {
            table,
            first_sets,
            follow_sets,
            entry_point,
        }
    }

    /// Get the action for a state and token
    #[must_use]
    pub fn get_action(&self, state: usize, token: Option<&T>) -> Action {
        self.table.get_action(state, token)
    }

    /// Get the goto state for a state and non-terminal
    #[must_use]
    pub fn get_goto(&self, state: usize, nt: &N) -> Option<usize> {
        self.table.get_goto(state, nt)
    }

    /// Get the number of states
    #[must_use]
    pub fn num_states(&self) -> usize {
        self.table.num_states()
    }

    /// Get a production by index
    #[must_use]
    pub fn get_production(&self, idx: usize) -> Option<&Production<T, N>> {
        self.table.get_production(idx)
    }

    /// Get expected tokens for a state (for error messages)
    #[must_use]
    pub fn get_expected_tokens(&self, state: usize) -> Vec<T> {
        self.table.get_expected_tokens(state)
    }
}
