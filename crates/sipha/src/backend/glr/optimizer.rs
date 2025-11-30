//! GLR grammar optimizer
//!
//! Provides optimizations specific to GLR parsing, such as:
//! - Stack merging optimization
//! - Ambiguity reduction
//! - Table compression (reuses LR optimizations)

use crate::backend::glr::grammar::GlrGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{NonTerminal, Token};

/// GLR grammar optimizer
pub struct GlrOptimizer;

impl<T, N> GrammarOptimizer<T, N> for GlrOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = GlrGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is
                Ok(GlrGrammar {
                    original_grammar: grammar.original_grammar.clone(),
                    lr_table: grammar.lr_table.clone(),
                    entry_point: grammar.entry_point.clone(),
                })
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: table compression (reuse LR optimizer)
                Self::optimize_table(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: table compression + ambiguity reduction
                let optimized = Self::optimize_table(grammar)?;
                Self::reduce_ambiguity(&optimized)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: false, // GLR doesn't benefit from factoring
            can_eliminate_left_recursion: false, // GLR handles left recursion natively
            can_compress_tables: true, // Can optimize LR table
            can_merge_states: true, // GLR benefits from state merging
        }
    }
}

impl GlrOptimizer {
    /// Optimize the LR table (reuses LR optimizer logic)
    fn optimize_table<T, N>(_grammar: &GlrGrammar<T, N>) -> Result<GlrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // In a full implementation, this would:
        // - Use LR optimizer to compress the table
        // - Optimize for GLR-specific patterns
        // - Rebuild the table
        Err(OptimizeError::OptimizationFailed(
            "Table optimization not yet implemented. Requires rebuilding LrParsingTable."
                .to_string(),
        ))
    }

    /// Reduce ambiguity in the grammar
    fn reduce_ambiguity<T, N>(
        _grammar: &GlrGrammar<T, N>,
    ) -> Result<GlrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // In a full implementation, this would:
        // - Identify ambiguous productions
        // - Apply disambiguation hints
        // - Optimize the table for common disambiguation patterns
        // - Rebuild the table
        Err(OptimizeError::OptimizationFailed(
            "Ambiguity reduction not yet implemented. Requires rebuilding LrParsingTable."
                .to_string(),
        ))
    }
}
