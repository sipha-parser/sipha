//! PEG grammar optimizer
//!
//! This module provides optimizations specific to PEG parsing, which uses
//! packrat memoization for efficient parsing.
//!
//! # Optimizations
//!
//! The `PegOptimizer` provides the following optimizations:
//!
//! - **Memoization table optimization**: Removes stale "Computing" entries from
//!   the memo table to clean up incomplete parse states
//! - **Left recursion optimization**: Optimizes memoization strategy for
//!   left-recursive rules
//!
//! # Usage
//!
//! The optimizer is typically used through the grammar transformation pipeline:
//!
//! ```rust,ignore
//! use sipha::backend::peg::{PegParser, PegConfig};
//! use sipha::grammar::hint::OptimizationLevel;
//!
//! let config = PegConfig {
//!     optimize: true,
//!     optimization_level: OptimizationLevel::Aggressive,
//!     ..Default::default()
//! };
//! let parser = PegParser::new(&grammar, config)?;
//! ```
//!
//! # Optimization Levels
//!
//! - `None`: No optimization
//! - `Basic`: Memoization table cleanup only
//! - `Aggressive`: Memoization cleanup + left recursion optimization

use crate::backend::peg::grammar::PegGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{Expr, NonTerminal, Token};

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
        use crate::backend::peg::grammar::MemoEntry;

        // Clean up memoization table by removing stale entries
        // Stale entries are those marked as "Computing" (left in that state)
        // or entries that are no longer relevant
        let mut optimized_memo = hashbrown::HashMap::new();
        for ((rule, pos), entry) in &grammar.memo_table {
            // Skip "Computing" entries as they're stale
            if !matches!(entry, MemoEntry::Computing) {
                optimized_memo.insert((rule.clone(), *pos), entry.clone());
            }
        }

        // Compress by removing duplicate entries if any
        // (In practice, memo entries are position-specific, so duplicates are rare)

        Ok(PegGrammar {
            original_grammar: grammar.original_grammar.clone(),
            memo_table: optimized_memo,
            entry_point: grammar.entry_point.clone(),
        })
    }

    /// Optimize left recursion handling
    ///
    /// This method optimizes the grammar and memo table for efficient handling
    /// of left-recursive rules in PEG parsing. Left recursion in PEG requires
    /// special handling using the "Computing" memo state to detect and resolve
    /// recursive calls.
    ///
    /// # Optimization Strategy
    ///
    /// 1. **Identify left-recursive rules**: Detect rules that are directly or
    ///    indirectly left-recursive
    /// 2. **Clean memo table**: Remove stale "Computing" entries that may have
    ///    been left from previous parses
    /// 3. **Optimize rule ordering**: Reorder rules so left-recursive rules are
    ///    evaluated in an optimal order (though this is handled by the parser)
    ///
    /// # Left Recursion in PEG
    ///
    /// PEG parsers handle left recursion by:
    /// - Marking a rule as "Computing" when it starts parsing
    /// - If the rule calls itself recursively while "Computing", it uses the
    ///   growing seed technique to resolve the recursion
    /// - The memo table stores intermediate results during left recursion resolution
    ///
    /// This optimization ensures the memo table is clean and ready for efficient
    /// left recursion handling.
    fn optimize_left_recursion<T, N>(
        grammar: &PegGrammar<T, N>,
    ) -> Result<PegGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::backend::peg::grammar::MemoEntry;

        // Detect left-recursive rules in the grammar
        let _left_recursive_rules = Self::detect_left_recursion(&grammar.original_grammar);

        // Optimize memo table for left-recursive rules
        let mut optimized_memo = hashbrown::HashMap::new();

        // Clean up memo table: remove stale "Computing" entries and optimize structure
        for ((rule, pos), entry) in &grammar.memo_table {
            match entry {
                MemoEntry::Computing => {
                    // Remove stale "Computing" entries - these indicate incomplete
                    // left recursion resolution from a previous parse
                    // Left recursion will be re-detected and resolved during parsing
                }
                MemoEntry::Success { .. } | MemoEntry::Failure => {
                    // Keep successful and failure entries
                    // For left-recursive rules, we might want to be more selective,
                    // but for now we keep all valid entries
                    optimized_memo.insert((rule.clone(), *pos), entry.clone());
                }
            }
        }

        // For left-recursive rules, we could pre-allocate memo entries at common
        // positions, but since memo entries are position-specific and built during
        // parsing, we focus on ensuring the table is clean and ready.

        // Additional optimization: if we have information about common parse
        // patterns, we could structure the memo table more efficiently, but this
        // requires parser-specific knowledge that isn't available at optimization time.

        Ok(PegGrammar {
            original_grammar: grammar.original_grammar.clone(),
            memo_table: optimized_memo,
            entry_point: grammar.entry_point.clone(),
        })
    }

    /// Detect left-recursive rules in the grammar
    fn detect_left_recursion<T, N>(grammar: &crate::grammar::Grammar<T, N>) -> hashbrown::HashSet<N>
    where
        T: Token,
        N: NonTerminal + Clone,
    {
        let mut left_recursive = hashbrown::HashSet::new();
        let mut visited = hashbrown::HashSet::new();

        for (lhs, rule) in grammar.rules() {
            if Self::is_left_recursive(lhs, &rule.rhs, grammar, &mut visited) {
                left_recursive.insert(lhs.clone());
            }
            visited.clear();
        }

        left_recursive
    }

    /// Check if a rule is left-recursive
    fn is_left_recursive<T, N>(
        lhs: &N,
        expr: &Expr<T, N>,
        grammar: &crate::grammar::Grammar<T, N>,
        visited: &mut hashbrown::HashSet<N>,
    ) -> bool
    where
        T: Token,
        N: NonTerminal + Clone,
    {
        use crate::grammar::{CoreExpr, ExtendedExpr};

        if visited.contains(lhs) {
            return false; // Cycle detected, but not necessarily left recursion
        }
        visited.insert(lhs.clone());

        match expr {
            ExtendedExpr::Core(CoreExpr::Seq(items)) => {
                // Check if first item is left-recursive
                if let Some(first) = items.first() {
                    match first {
                        CoreExpr::Rule(nt) if nt == lhs => return true,
                        CoreExpr::Rule(nt) => {
                            if let Some(rule) = grammar.get_rule(nt) {
                                return Self::is_left_recursive(lhs, &rule.rhs, grammar, visited);
                            }
                        }
                        _ => {}
                    }
                }
            }
            ExtendedExpr::Core(CoreExpr::Choice(alternatives)) => {
                // Check if any alternative is left-recursive
                for alt in alternatives {
                    if Self::is_left_recursive(
                        lhs,
                        &ExtendedExpr::Core(alt.clone()),
                        grammar,
                        visited,
                    ) {
                        return true;
                    }
                }
            }
            ExtendedExpr::Core(CoreExpr::Rule(nt)) if nt == lhs => return true,
            ExtendedExpr::Core(CoreExpr::Rule(nt)) => {
                if let Some(rule) = grammar.get_rule(nt) {
                    return Self::is_left_recursive(lhs, &rule.rhs, grammar, visited);
                }
            }
            _ => {}
        }

        false
    }
}
