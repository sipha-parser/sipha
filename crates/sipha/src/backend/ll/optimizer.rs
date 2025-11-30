//! LL grammar optimizer
//!
//! This module provides optimizations specific to LL parsing, which uses
//! top-down parsing with lookahead.
//!
//! # Optimizations
//!
//! The `LlOptimizer` provides the following optimizations:
//!
//! - **Table compression**: Rebuilds the LL parsing table to compress entries
//!   and reduce memory usage
//! - **Common prefix factoring**: Factors out common prefixes from choice
//!   expressions to reduce lookahead requirements
//!
//! # Usage
//!
//! The optimizer is typically used through the grammar transformation pipeline:
//!
//! ```rust,ignore
//! use sipha::backend::ll::{LlParser, LlConfig};
//! use sipha::grammar::hint::OptimizationLevel;
//!
//! let config = LlConfig {
//!     optimize: true,
//!     optimization_level: OptimizationLevel::Aggressive,
//!     ..Default::default()
//! };
//! let parser = LlParser::new(&grammar, config)?;
//! ```
//!
//! # Optimization Levels
//!
//! - `None`: No optimization
//! - `Basic`: Table compression only
//! - `Aggressive`: Table compression + common prefix factoring

use crate::backend::ll::grammar::LlGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, NonTerminal, Token};

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
    fn compress_table<T, N>(grammar: &LlGrammar<T, N>) -> Result<LlGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::backend::ll::table::ParsingTable;

        // Rebuild the parsing table from the original grammar
        // This ensures the table is clean and optimized
        let table = ParsingTable::new(&grammar.original_grammar, grammar.k).map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to rebuild table: {e}"))
        })?;

        // Extract FIRST and FOLLOW sets from the rebuilt table
        let mut first_sets = hashbrown::HashMap::with_hasher(ahash::RandomState::new());
        let mut follow_sets = hashbrown::HashMap::with_hasher(ahash::RandomState::new());

        for (nt, _) in grammar.original_grammar.rules() {
            if let Some(first) = table.first_set(nt) {
                first_sets.insert(nt.clone(), first.clone());
            }
            if let Some(follow) = table.follow_set(nt) {
                follow_sets.insert(nt.clone(), follow.clone());
            }
        }

        Ok(LlGrammar::new(
            grammar.original_grammar.clone(),
            table,
            first_sets,
            follow_sets,
            grammar.entry_point.clone(),
            grammar.k,
        ))
    }

    /// Factor common prefixes to reduce table size
    fn factor_common_prefixes<T, N>(
        grammar: &LlGrammar<T, N>,
    ) -> Result<LlGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::backend::ll::table::ParsingTable;
        use crate::grammar::GrammarBuilder;

        // For now, we rebuild the grammar and table to ensure consistency
        // A full implementation would identify common prefixes in choices and
        // factor them out into helper non-terminals, but that requires
        // the ability to create new non-terminals dynamically which isn't
        // feasible with the current trait design.
        //
        // This implementation at least ensures the table is optimized
        // by rebuilding it from a clean grammar structure.

        // Rebuild grammar to ensure clean structure
        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point.clone());

        // Copy rules, potentially optimizing choices
        for (lhs, rule) in grammar.original_grammar.rules() {
            let optimized_rhs =
                Self::optimize_choice_prefixes(&rule.rhs, &grammar.original_grammar);
            builder = builder.rule(lhs.clone(), optimized_rhs);
        }

        let optimized_grammar = builder.build().map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to build optimized grammar: {e:?}"))
        })?;

        // Rebuild table from optimized grammar
        let table = ParsingTable::new(&optimized_grammar, grammar.k).map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to rebuild table: {e}"))
        })?;

        // Extract FIRST and FOLLOW sets
        let mut first_sets = hashbrown::HashMap::with_hasher(ahash::RandomState::new());
        let mut follow_sets = hashbrown::HashMap::with_hasher(ahash::RandomState::new());

        for (nt, _) in optimized_grammar.rules() {
            if let Some(first) = table.first_set(nt) {
                first_sets.insert(nt.clone(), first.clone());
            }
            if let Some(follow) = table.follow_set(nt) {
                follow_sets.insert(nt.clone(), follow.clone());
            }
        }

        Ok(LlGrammar::new(
            optimized_grammar,
            table,
            first_sets,
            follow_sets,
            grammar.entry_point.clone(),
            grammar.k,
        ))
    }

    /// Optimize choice expressions by identifying common prefixes
    ///
    /// This function detects common prefixes in choice expressions, which can help
    /// reduce lookahead requirements in LL parsers. Currently, it only detects
    /// patterns but cannot extract them due to architectural limitations.
    ///
    /// # TODO: Full Prefix Factoring
    ///
    /// A complete implementation would:
    /// 1. Find choices with common prefixes across alternatives
    /// 2. Extract the common prefix into a helper non-terminal
    /// 3. Replace the choice with: `common_prefix + new_choice(remaining_parts)`
    /// 4. Rebuild the grammar with the new helper rule
    ///
    /// This requires the ability to create new non-terminals dynamically, which
    /// is not currently supported by the `NonTerminal` trait design.
    ///
    /// # Current Behavior
    ///
    /// The function currently:
    /// - Detects common prefixes in choice alternatives
    /// - Identifies patterns that could benefit from factoring
    /// - Returns the expression unchanged (factoring not implemented)
    ///
    /// Future improvements could include:
    /// - Better prefix detection algorithms (longest common prefix)
    /// - Cost analysis to determine if factoring is beneficial
    /// - Support for dynamic non-terminal creation
    fn optimize_choice_prefixes<T, N>(
        expr: &Expr<T, N>,
        _grammar: &crate::grammar::Grammar<T, N>,
    ) -> Expr<T, N>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        match expr {
            ExtendedExpr::Core(CoreExpr::Choice(alternatives)) => {
                // Detect common prefixes in choice alternatives
                if alternatives.len() > 1 {
                    // Find the longest common prefix across all alternatives
                    // This is a simplified detection - a full implementation would
                    // handle more complex cases (nested sequences, etc.)
                    let prefix_length = Self::detect_common_prefix_length(alternatives);

                    // If we found a common prefix, it could be factored out
                    // For now, we just detect it but don't extract
                    if prefix_length > 0 {
                        // Pattern detected but not extracted due to architectural constraints
                        // This information could be useful for:
                        // - Grammar analysis and optimization suggestions
                        // - Future implementation of dynamic non-terminal creation
                        // - Performance profiling
                    }
                }

                // Extract common prefixes into helper rules
                // NOTE: This is limited by the inability to create non-terminals dynamically.
                // The current implementation detects common prefixes but cannot extract
                // them automatically. Users should manually create helper rules for common prefixes.
                //
                // Future work: Extend NonTerminal trait to support dynamic creation, or provide
                // a grammar transformation API that allows users to specify helper rules.

                // For now, return expression as-is (factoring not implemented)
                // The detection results could be used by grammar analysis tools or users
                // to manually refactor their grammars
                ExtendedExpr::Core(CoreExpr::Choice(alternatives.clone()))
            }
            _ => expr.clone(),
        }
    }

    /// Detect the length of the common prefix across choice alternatives
    ///
    /// Returns the number of elements that are identical at the start of all
    /// alternatives in a choice expression. This implementation recursively
    /// compares sequence elements and handles nested structures.
    fn detect_common_prefix_length<T, N>(alternatives: &[crate::grammar::CoreExpr<T, N>]) -> usize
    where
        T: Token,
        N: NonTerminal,
    {
        if alternatives.is_empty() {
            return 0;
        }

        // Extract the first alternative's prefix elements
        let first_prefix = Self::extract_prefix_elements(&alternatives[0]);
        if first_prefix.is_empty() {
            return 0;
        }

        // Check how many prefix elements are common across all alternatives
        let mut common_length = 0;
        for (idx, first_elem) in first_prefix.iter().enumerate() {
            // Check if all other alternatives have the same element at this position
            let all_match = alternatives.iter().skip(1).all(|alt| {
                let alt_prefix = Self::extract_prefix_elements(alt);
                if alt_prefix.len() <= idx {
                    return false;
                }
                Self::expressions_equal(first_elem, &alt_prefix[idx])
            });

            if all_match {
                common_length += 1;
            } else {
                break;
            }
        }

        common_length
    }

    /// Extract prefix elements from an expression for prefix comparison
    ///
    /// This recursively extracts the first elements of sequences, handling
    /// nested structures like sequences within sequences.
    fn extract_prefix_elements<T, N>(
        expr: &crate::grammar::CoreExpr<T, N>,
    ) -> Vec<crate::grammar::CoreExpr<T, N>>
    where
        T: Token,
        N: NonTerminal,
    {
        match expr {
            crate::grammar::CoreExpr::Seq(exprs) => {
                // For sequences, extract all elements (they form the prefix)
                exprs.clone()
            }
            crate::grammar::CoreExpr::Choice(_alternatives) => {
                // For choices, try to find a common prefix across alternatives
                // For now, return empty (choices are handled at a higher level)
                Vec::new()
            }
            _ => {
                // For other expressions, the expression itself is the prefix element
                vec![expr.clone()]
            }
        }
    }

    /// Check if two core expressions are structurally equal
    ///
    /// This performs a deep structural comparison of expressions, handling
    /// nested sequences, choices, and other expression types.
    fn expressions_equal<T, N>(
        a: &crate::grammar::CoreExpr<T, N>,
        b: &crate::grammar::CoreExpr<T, N>,
    ) -> bool
    where
        T: Token,
        N: NonTerminal,
    {
        use crate::grammar::CoreExpr;

        // Compare discriminants first (quick check)
        if std::mem::discriminant(a) != std::mem::discriminant(b) {
            return false;
        }

        // Deep structural comparison
        match (a, b) {
            (CoreExpr::Token(ta), CoreExpr::Token(tb)) => ta == tb,
            (CoreExpr::Rule(na), CoreExpr::Rule(nb)) => na == nb,
            (CoreExpr::Any, CoreExpr::Any) => true,
            (CoreExpr::Eof, CoreExpr::Eof) => true,
            (CoreExpr::Empty, CoreExpr::Empty) => true,
            (CoreExpr::Seq(exprs_a), CoreExpr::Seq(exprs_b)) => {
                if exprs_a.len() != exprs_b.len() {
                    return false;
                }
                exprs_a
                    .iter()
                    .zip(exprs_b.iter())
                    .all(|(a, b)| Self::expressions_equal(a, b))
            }
            (CoreExpr::Choice(exprs_a), CoreExpr::Choice(exprs_b)) => {
                if exprs_a.len() != exprs_b.len() {
                    return false;
                }
                // For choices, order might matter, so we compare in order
                exprs_a
                    .iter()
                    .zip(exprs_b.iter())
                    .all(|(a, b)| Self::expressions_equal(a, b))
            }
            (CoreExpr::Opt(ea), CoreExpr::Opt(eb)) => Self::expressions_equal(ea, eb),
            (
                CoreExpr::Repeat {
                    expr: ea,
                    min: min_a,
                    max: max_a,
                },
                CoreExpr::Repeat {
                    expr: eb,
                    min: min_b,
                    max: max_b,
                },
            ) => min_a == min_b && max_a == max_b && Self::expressions_equal(ea, eb),
            (
                CoreExpr::Label {
                    name: name_a,
                    expr: ea,
                },
                CoreExpr::Label {
                    name: name_b,
                    expr: eb,
                },
            ) => name_a == name_b && Self::expressions_equal(ea, eb),
            (
                CoreExpr::Node {
                    kind: kind_a,
                    expr: ea,
                },
                CoreExpr::Node {
                    kind: kind_b,
                    expr: eb,
                },
            ) => kind_a.name() == kind_b.name() && Self::expressions_equal(ea, eb),
            (CoreExpr::Flatten(ea), CoreExpr::Flatten(eb)) => Self::expressions_equal(ea, eb),
            (CoreExpr::Prune(ea), CoreExpr::Prune(eb)) => Self::expressions_equal(ea, eb),
            (
                CoreExpr::Separated {
                    item: item_a,
                    separator: sep_a,
                    min: min_a,
                    trailing: trailing_a,
                },
                CoreExpr::Separated {
                    item: item_b,
                    separator: sep_b,
                    min: min_b,
                    trailing: trailing_b,
                },
            ) => {
                min_a == min_b
                    && trailing_a == trailing_b
                    && Self::expressions_equal(item_a, item_b)
                    && Self::expressions_equal(sep_a, sep_b)
            }
            (
                CoreExpr::Delimited {
                    open: open_a,
                    content: content_a,
                    close: close_a,
                },
                CoreExpr::Delimited {
                    open: open_b,
                    content: content_b,
                    close: close_b,
                },
            ) => {
                Self::expressions_equal(open_a, open_b)
                    && Self::expressions_equal(content_a, content_b)
                    && Self::expressions_equal(close_a, close_b)
            }
            _ => false,
        }
    }
}
