//! Parser state for Earley parser

use crate::grammar::{NonTerminal, Token};
use std::collections::HashMap;

/// Parser state for Earley algorithm
pub struct EarleyParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Cache for parse results (for incremental parsing)
    parse_cache: HashMap<(N, usize), Option<usize>>,
    #[allow(dead_code)] // Reserved for future use
    _phantom: std::marker::PhantomData<T>,
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
            _phantom: std::marker::PhantomData,
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
