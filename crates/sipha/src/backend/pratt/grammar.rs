//! Pratt-specific grammar representation
//!
//! This module defines `PrattGrammar`, which contains operator precedence
//! information and cached analysis results needed for Pratt parsing.

use crate::grammar::hint::Associativity;
use crate::grammar::{Grammar, NonTerminal, Token};
use hashbrown::HashMap;

/// Operator information for Pratt parsing
#[derive(Debug, Clone)]
pub struct OperatorInfo {
    /// Precedence level (higher = binds tighter)
    pub precedence: u32,
    /// Associativity of the operator
    pub associativity: Associativity,
    /// Whether this is a prefix operator
    pub is_prefix: bool,
    /// Whether this is an infix operator
    pub is_infix: bool,
    /// Whether this is a postfix operator
    pub is_postfix: bool,
}

/// Pratt-specific grammar representation
///
/// This contains operator precedence information and cached analysis results
/// needed for efficient Pratt parsing.
pub struct PrattGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    /// Original grammar (needed for accessing rule expressions)
    pub original_grammar: Grammar<T, N>,
    /// Operator precedence table: token -> operator info
    pub operators: HashMap<T, OperatorInfo>,
    /// Original grammar entry point
    pub entry_point: N,
}

impl<T, N> PrattGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    /// Clone the grammar
    #[must_use]
    pub fn clone_grammar(&self) -> Self {
        Self {
            original_grammar: self.original_grammar.clone(),
            operators: self.operators.clone(),
            entry_point: self.entry_point.clone(),
        }
    }
}

impl<T, N> Clone for PrattGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    fn clone(&self) -> Self {
        self.clone_grammar()
    }
}

impl<T, N> PrattGrammar<T, N>
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    /// Create a new Pratt grammar
    #[must_use]
    pub fn new(original_grammar: Grammar<T, N>) -> Self {
        let entry_point = original_grammar.entry_point().clone();
        Self {
            original_grammar,
            operators: HashMap::default(),
            entry_point,
        }
    }

    /// Add an operator to the precedence table
    pub fn add_operator(&mut self, token: T, info: OperatorInfo) {
        self.operators.insert(token, info);
    }

    /// Get operator information for a token
    #[must_use]
    pub fn get_operator(&self, token: &T) -> Option<&OperatorInfo> {
        self.operators.get(token)
    }

    /// Check if a token is an operator
    #[must_use]
    pub fn is_operator(&self, token: &T) -> bool {
        self.operators.contains_key(token)
    }
}
