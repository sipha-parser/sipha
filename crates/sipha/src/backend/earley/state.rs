//! Parser state for Earley parser

use crate::grammar::{Grammar, NonTerminal, Token};
use std::collections::HashMap;

/// Parser state for Earley algorithm
pub struct EarleyParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Cache for parse results (for incremental parsing)
    parse_cache: HashMap<(N, usize), Option<usize>>,
}

impl<T, N> EarleyParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new parser state
    pub fn new() -> Self {
        Self {
            parse_cache: HashMap::new(),
        }
    }
    
    /// Clear the parse cache
    pub fn clear_cache(&mut self) {
        self.parse_cache.clear();
    }
}

impl<T, N> Default for EarleyParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}

