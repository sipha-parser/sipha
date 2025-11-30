//! LL grammar optimizer
//!
//! Provides optimizations specific to LL parsing, such as:
//! - Left recursion elimination
//! - Common prefix factoring
//! - Table compression

use crate::backend::ll::grammar::LlGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{NonTerminal, Token};

/// LL grammar optimizer
pub struct LlOptimizer;

impl<T, N> GrammarOptimizer<T, N> for LlOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = LlGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is by rebuilding it
                // Note: This requires rebuilding the parsing table, which is expensive
                // In practice, optimization should be skipped when level is None
                Ok(LlGrammar::new(
                    grammar.original_grammar.clone(),
                    grammar.table.clone(),
                    grammar.first_sets.clone(),
                    grammar.follow_sets.clone(),
                    grammar.entry_point.clone(),
                    grammar.k,
                ))
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: table compression
                Self::compress_table(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: table compression + common prefix factoring
                let compressed = Self::compress_table(grammar)?;
                Self::factor_common_prefixes(&compressed)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: true,                   // LL benefits from factoring
            can_eliminate_left_recursion: true, // LL requires left recursion elimination
            can_compress_tables: true,
            can_merge_states: false, // LL doesn't use states
        }
    }
}

impl LlOptimizer {
    /// Compress the parsing table by removing redundant entries
    fn compress_table<T, N>(_grammar: &LlGrammar<T, N>) -> Result<LlGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Optimization not yet implemented - requires rebuilding the grammar
        // In a full implementation, this would:
        // - Remove duplicate table entries
        // - Compress the table representation
        // - Rebuild the LlGrammar with the optimized table
        Err(OptimizeError::OptimizationFailed(
            "Table compression not yet implemented - requires grammar rebuilding".to_string(),
        ))
    }

    /// Factor common prefixes to reduce table size
    fn factor_common_prefixes<T, N>(
        _grammar: &LlGrammar<T, N>,
    ) -> Result<LlGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Optimization not yet implemented - requires rebuilding the grammar
        // In a full implementation, this would:
        // - Identify common prefixes in choices
        // - Factor them out into helper non-terminals
        // - Rebuild the parsing table and LlGrammar
        Err(OptimizeError::OptimizationFailed(
            "Common prefix factoring not yet implemented - requires grammar rebuilding".to_string(),
        ))
    }
}
