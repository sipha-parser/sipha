//! LL-specific grammar representation
//!
//! This module defines `LlGrammar`, which contains the LL parsing table
//! and cached analysis results needed for LL parsing.

use crate::backend::ll::table::ParsingTable;
use crate::grammar::{Grammar, NonTerminal, Token};
use ahash::RandomState;
use hashbrown::{HashMap, HashSet};

/// LL-specific grammar representation
///
/// This contains the parsing table and cached analysis results
/// needed for efficient LL parsing.
pub struct LlGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    /// Original grammar (needed for accessing rule expressions)
    pub original_grammar: Grammar<T, N>,
    /// LL parsing table
    pub table: ParsingTable<T, N>,
    /// Cached FIRST sets for all non-terminals
    pub first_sets: HashMap<N, HashSet<T, RandomState>, RandomState>,
    /// Cached FOLLOW sets for all non-terminals
    pub follow_sets: HashMap<N, HashSet<T, RandomState>, RandomState>,
    /// Original grammar entry point
    pub entry_point: N,
    /// Lookahead depth (k in LL(k))
    pub k: usize,
}

impl<T, N> LlGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    /// Create a new LL grammar from a parsing table and cached analysis
    #[must_use]
    pub fn new(
        original_grammar: Grammar<T, N>,
        table: ParsingTable<T, N>,
        first_sets: HashMap<N, HashSet<T, RandomState>, RandomState>,
        follow_sets: HashMap<N, HashSet<T, RandomState>, RandomState>,
        entry_point: N,
        k: usize,
    ) -> Self {
        Self {
            original_grammar,
            table,
            first_sets,
            follow_sets,
            entry_point,
            k,
        }
    }

    /// Get the production index for a non-terminal and lookahead token
    #[must_use]
    pub fn get(&self, nt: &N, lookahead: &[T]) -> Option<usize> {
        self.table.get(nt, lookahead)
    }

    /// Get the FIRST set for a non-terminal
    #[must_use]
    pub fn first_set(&self, nt: &N) -> Option<&HashSet<T, RandomState>> {
        self.first_sets.get(nt)
    }

    /// Get the FOLLOW set for a non-terminal
    #[must_use]
    pub fn follow_set(&self, nt: &N) -> Option<&HashSet<T, RandomState>> {
        self.follow_sets.get(nt)
    }
}
