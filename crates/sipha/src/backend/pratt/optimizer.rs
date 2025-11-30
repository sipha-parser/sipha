//! Pratt grammar optimizer
//!
//! Provides optimizations specific to Pratt parsing, such as:
//! - Operator precedence table optimization
//! - Expression flattening
//! - Common subexpression elimination

use crate::backend::pratt::grammar::PrattGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{NonTerminal, Token};

/// Pratt grammar optimizer
pub struct PrattOptimizer;

impl<T, N> GrammarOptimizer<T, N> for PrattOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = PrattGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is
                Ok(grammar.clone_grammar())
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: operator table optimization
                Self::optimize_operators(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: operator optimization + expression flattening
                let optimized = Self::optimize_operators(grammar)?;
                Self::flatten_expressions(&optimized)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: false, // Pratt doesn't benefit from factoring
            can_eliminate_left_recursion: false, // Pratt handles left recursion natively via precedence
            can_compress_tables: true,           // Can optimize operator table
            can_merge_states: false,             // Pratt doesn't use states
        }
    }
}

impl PrattOptimizer {
    /// Optimize the operator precedence table
    fn optimize_operators<T, N>(
        grammar: &PrattGrammar<T, N>,
    ) -> Result<PrattGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // For now, just return a clone
        // In a full implementation, this would:
        // - Remove duplicate operator entries
        // - Optimize precedence ordering
        // - Compress the table representation
        Ok(grammar.clone_grammar())
    }

    /// Flatten nested expressions
    fn flatten_expressions<T, N>(
        grammar: &PrattGrammar<T, N>,
    ) -> Result<PrattGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // For now, just return a clone
        // In a full implementation, this would:
        // - Flatten deeply nested expressions
        // - Optimize operator chains
        Ok(grammar.clone_grammar())
    }
}
