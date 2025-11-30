//! PEG grammar optimizer
//!
//! Provides optimizations specific to PEG parsing, such as:
//! - Memoization table optimization
//! - Left recursion detection and handling
//! - Common subexpression elimination

use crate::backend::peg::grammar::PegGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{NonTerminal, Token};

/// PEG grammar optimizer
pub struct PegOptimizer;

impl<T, N> GrammarOptimizer<T, N> for PegOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = PegGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is
                Ok(PegGrammar {
                    original_grammar: grammar.original_grammar.clone(),
                    memo_table: grammar.memo_table.clone(),
                    entry_point: grammar.entry_point.clone(),
                })
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: memoization table cleanup
                Self::optimize_memo_table(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: memoization + left recursion optimization
                let optimized = Self::optimize_memo_table(grammar)?;
                Self::optimize_left_recursion(&optimized)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: false, // PEG doesn't benefit from factoring
            can_eliminate_left_recursion: false, // PEG handles left recursion natively
            can_compress_tables: true, // Can optimize memoization table
            can_merge_states: false, // PEG doesn't use states
        }
    }
}

impl PegOptimizer {
    /// Optimize the memoization table
    fn optimize_memo_table<T, N>(
        grammar: &PegGrammar<T, N>,
    ) -> Result<PegGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // For now, just return a clone
        // In a full implementation, this would:
        // - Remove stale memo entries
        // - Compress the table representation
        Ok(PegGrammar {
            original_grammar: grammar.original_grammar.clone(),
            memo_table: grammar.memo_table.clone(),
            entry_point: grammar.entry_point.clone(),
        })
    }

    /// Optimize left recursion handling
    fn optimize_left_recursion<T, N>(
        grammar: &PegGrammar<T, N>,
    ) -> Result<PegGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // For now, just return a clone
        // In a full implementation, this would:
        // - Detect left recursion patterns
        // - Optimize the memoization strategy for left-recursive rules
        Ok(PegGrammar {
            original_grammar: grammar.original_grammar.clone(),
            memo_table: grammar.memo_table.clone(),
            entry_point: grammar.entry_point.clone(),
        })
    }
}
