//! LR grammar optimizer
//!
//! Provides optimizations specific to LR parsing, such as:
//! - State merging (LALR vs canonical LR)
//! - Table compression
//! - Production inlining

use crate::backend::lr::grammar::LrGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{NonTerminal, Token};

/// LR grammar optimizer
pub struct LrOptimizer;

impl<T, N> GrammarOptimizer<T, N> for LrOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = LrGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is
                Ok(LrGrammar::new(
                    grammar.table.clone(),
                    grammar.first_sets.clone(),
                    grammar.follow_sets.clone(),
                    grammar.entry_point.clone(),
                ))
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: table compression
                Self::compress_table(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: table compression + production inlining
                let compressed = Self::compress_table(grammar)?;
                Self::inline_productions(&compressed)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: false, // LR doesn't benefit from factoring
            can_eliminate_left_recursion: false, // LR handles left recursion natively
            can_compress_tables: true,
            can_merge_states: true, // LALR is state merging
        }
    }
}

impl LrOptimizer {
    /// Compress the parsing table by removing redundant entries
    fn compress_table<T, N>(_grammar: &LrGrammar<T, N>) -> Result<LrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // In a full implementation, this would:
        // - Remove duplicate states
        // - Compress action/goto tables
        // - Optimize production storage
        // - Rebuild the parsing table
        Err(OptimizeError::OptimizationFailed(
            "Table compression not yet implemented. Requires rebuilding LrParsingTable."
                .to_string(),
        ))
    }

    /// Inline small productions to reduce table size
    fn inline_productions<T, N>(
        _grammar: &LrGrammar<T, N>,
    ) -> Result<LrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // In a full implementation, this would:
        // - Identify small productions (1-2 items)
        // - Inline them into parent productions
        // - Rebuild the parsing table
        Err(OptimizeError::OptimizationFailed(
            "Production inlining not yet implemented. Requires rebuilding LrParsingTable."
                .to_string(),
        ))
    }
}
