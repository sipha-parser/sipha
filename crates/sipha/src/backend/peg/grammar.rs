//! PEG-specific grammar representation
//!
//! This module defines `PegGrammar`, which contains memoization tables
//! and cached analysis results needed for PEG parsing.

use crate::grammar::{Grammar, NonTerminal, Token};
use hashbrown::HashMap;

/// PEG-specific grammar representation
///
/// This contains memoization tables and cached analysis results
/// needed for efficient PEG parsing.
pub struct PegGrammar<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Original grammar (needed for accessing rule expressions)
    pub original_grammar: Grammar<T, N>,
    /// Memoization table: (rule, position) -> parse result
    /// This is built during parsing, not at grammar creation time
    pub memo_table: HashMap<(N, usize), MemoEntry>,
    /// Original grammar entry point
    pub entry_point: N,
}

/// Memoization entry for packrat parsing
#[derive(Debug, Clone)]
pub enum MemoEntry {
    /// Successful parse: end position and text length
    Success {
        end_pos: usize,
        text_len: crate::syntax::TextSize,
    },
    /// Failed parse
    Failure,
    /// Currently being computed (for left recursion detection)
    Computing,
}

impl<T, N> PegGrammar<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new PEG grammar
    #[must_use]
    pub fn new(original_grammar: Grammar<T, N>) -> Self {
        let entry_point = original_grammar.entry_point().clone();
        Self {
            original_grammar,
            memo_table: HashMap::new(),
            entry_point,
        }
    }

    /// Get a memoized result
    #[must_use]
    pub fn get_memo(&self, rule: &N, pos: usize) -> Option<&MemoEntry> {
        self.memo_table.get(&(rule.clone(), pos))
    }

    /// Set a memoized result
    pub fn set_memo(&mut self, rule: N, pos: usize, entry: MemoEntry) {
        self.memo_table.insert((rule, pos), entry);
    }

    /// Clear the memoization table
    pub fn clear_memo(&mut self) {
        self.memo_table.clear();
    }
}
