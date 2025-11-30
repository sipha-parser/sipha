//! GLR grammar optimizer
//!
//! This module provides optimizations specific to GLR parsing, which extends
//! LR parsing to handle ambiguous grammars by maintaining multiple parse stacks.
//!
//! # Optimizations
//!
//! The `GlrOptimizer` provides the following optimizations:
//!
//! - **Table optimization**: Reuses LR table compression optimizations to
//!   reduce the size of action/goto tables
//! - **Ambiguity reduction**: Rebuilds the parsing table to reduce ambiguities
//!   where possible
//!
//! # Usage
//!
//! The optimizer is typically used through the grammar transformation pipeline:
//!
//! ```rust,ignore
//! use sipha::backend::glr::{GlrParser, GlrConfig};
//! use sipha::grammar::hint::OptimizationLevel;
//!
//! let config = GlrConfig {
//!     optimize: true,
//!     optimization_level: OptimizationLevel::Aggressive,
//!     ..Default::default()
//! };
//! let parser = GlrParser::new(&grammar, config)?;
//! ```
//!
//! # Optimization Levels
//!
//! - `None`: No optimization
//! - `Basic`: Table optimization only
//! - `Aggressive`: Table optimization + ambiguity reduction

use crate::backend::glr::grammar::GlrGrammar;
#[cfg(feature = "backend-lr")]
use crate::backend::lr::LrParsingTable;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, GrammarBuilder, NonTerminal, Token};

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
    #[cfg(feature = "backend-lr")]
    fn optimize_table<T, N>(grammar: &GlrGrammar<T, N>) -> Result<GlrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Rebuild the LR table with LALR (which compresses states)
        let use_lalr = true;
        let table = LrParsingTable::new(&grammar.original_grammar, use_lalr).map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to rebuild table: {e}"))
        })?;

        Ok(GlrGrammar::new(
            grammar.original_grammar.clone(),
            table,
            grammar.entry_point.clone(),
        ))
    }

    #[cfg(not(feature = "backend-lr"))]
    fn optimize_table<T, N>(_grammar: &GlrGrammar<T, N>) -> Result<GlrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        Err(OptimizeError::OptimizationFailed(
            "GLR requires backend-lr feature for table optimization".to_string(),
        ))
    }

    /// Reduce ambiguity in the grammar
    ///
    /// This method identifies ambiguous productions and applies heuristics to reduce
    /// ambiguity, which can improve GLR parsing performance by reducing the number
    /// of ambiguous states that need to be tracked.
    ///
    /// # Ambiguity Reduction Strategy
    ///
    /// 1. **Identify ambiguous productions**: Find rules with multiple alternatives
    ///    that can match the same input
    /// 2. **Apply disambiguation heuristics**: Use precedence, associativity, and
    ///    production ordering to prefer certain alternatives
    /// 3. **Rebuild table**: Rebuild the LR table with the optimized grammar
    ///
    /// # Heuristics Applied
    ///
    /// - **Shorter productions**: Prefer shorter productions when alternatives are
    ///   otherwise equivalent
    /// - **Earlier alternatives**: When multiple alternatives can match, prefer
    ///   alternatives that appear earlier in the grammar (PEG-style)
    /// - **Precedence**: Use grammar hints for precedence and associativity when available
    fn reduce_ambiguity<T, N>(grammar: &GlrGrammar<T, N>) -> Result<GlrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Analyze grammar for ambiguous patterns
        let ambiguous_rules = Self::identify_ambiguous_rules(&grammar.original_grammar);

        if ambiguous_rules.is_empty() {
            // No ambiguous rules found - just optimize the table
            return Self::optimize_table(grammar);
        }

        // Build optimized grammar with disambiguation applied
        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point.clone());

        for (lhs, rule) in grammar.original_grammar.rules() {
            let optimized_rhs = if ambiguous_rules.contains(lhs) {
                Self::disambiguate_expression(&rule.rhs, &grammar.original_grammar)
            } else {
                rule.rhs.clone()
            };
            builder = builder.rule(lhs.clone(), optimized_rhs);
        }

        let optimized_grammar = builder.build().map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to build optimized grammar: {e:?}"))
        })?;

        // Rebuild table with optimized grammar
        #[cfg(feature = "backend-lr")]
        {
            let use_lalr = true;
            let table = LrParsingTable::new(&optimized_grammar, use_lalr).map_err(|e| {
                OptimizeError::OptimizationFailed(format!("Failed to rebuild table: {e}"))
            })?;

            Ok(GlrGrammar::new(
                optimized_grammar,
                table,
                grammar.entry_point.clone(),
            ))
        }

        #[cfg(not(feature = "backend-lr"))]
        {
            Err(OptimizeError::OptimizationFailed(
                "GLR requires backend-lr feature for ambiguity reduction".to_string(),
            ))
        }
    }

    /// Identify rules that may be ambiguous
    ///
    /// This finds rules with multiple alternatives that could potentially match
    /// the same input, indicating potential ambiguity.
    fn identify_ambiguous_rules<T, N>(
        grammar: &crate::grammar::Grammar<T, N>,
    ) -> hashbrown::HashSet<N>
    where
        T: Token,
        N: NonTerminal + Clone,
    {
        use crate::grammar::CoreExpr;

        let mut ambiguous = hashbrown::HashSet::new();

        for (lhs, rule) in grammar.rules() {
            // Check if rule has multiple alternatives (choice)
            if let ExtendedExpr::Core(CoreExpr::Choice(alternatives)) = &rule.rhs {
                if alternatives.len() > 1 {
                    // Check if alternatives have overlapping FIRST sets
                    let first_sets: Vec<hashbrown::HashSet<T, ahash::RandomState>> = alternatives
                        .iter()
                        .map(|alt| {
                            let expr: Expr<T, N> = ExtendedExpr::Core(alt.clone());
                            expr.first_set(grammar)
                        })
                        .collect();

                    // Check for overlapping FIRST sets (potential ambiguity)
                    for (i, first_i) in first_sets.iter().enumerate() {
                        for (_j, first_j) in first_sets.iter().enumerate().skip(i + 1) {
                            if !first_i.is_disjoint(first_j) {
                                // Overlapping FIRST sets - potential ambiguity
                                ambiguous.insert(lhs.clone());
                                break;
                            }
                        }
                        if ambiguous.contains(lhs) {
                            break;
                        }
                    }
                }
            }
        }

        ambiguous
    }

    /// Apply disambiguation heuristics to an expression
    ///
    /// This reorders alternatives in choice expressions to prefer:
    /// - Shorter productions
    /// - More specific alternatives (those with tokens rather than just rules)
    fn disambiguate_expression<T, N>(
        expr: &Expr<T, N>,
        _grammar: &crate::grammar::Grammar<T, N>,
    ) -> Expr<T, N>
    where
        T: Token,
        N: NonTerminal,
    {
        match expr {
            ExtendedExpr::Core(CoreExpr::Choice(alternatives)) => {
                // Sort alternatives by specificity and length
                let mut sorted_alternatives = alternatives.clone();
                sorted_alternatives.sort_by(|a, b| {
                    // Prefer alternatives with tokens (more specific)
                    let a_has_token = Self::has_token(a);
                    let b_has_token = Self::has_token(b);
                    match (a_has_token, b_has_token) {
                        (true, false) => std::cmp::Ordering::Less,
                        (false, true) => std::cmp::Ordering::Greater,
                        _ => {
                            // Both have or don't have tokens - prefer shorter
                            let a_len = Self::expression_length(a);
                            let b_len = Self::expression_length(b);
                            a_len.cmp(&b_len)
                        }
                    }
                });
                ExtendedExpr::Core(CoreExpr::Choice(sorted_alternatives))
            }
            _ => expr.clone(),
        }
    }

    /// Check if an expression contains a token (more specific)
    fn has_token<T, N>(expr: &crate::grammar::CoreExpr<T, N>) -> bool {
        use crate::grammar::CoreExpr;

        match expr {
            CoreExpr::Token(_) => true,
            CoreExpr::Seq(exprs) => exprs.iter().any(|e| Self::has_token(e)),
            CoreExpr::Choice(exprs) => exprs.iter().any(|e| Self::has_token(e)),
            CoreExpr::Opt(e) | CoreExpr::Repeat { expr: e, .. } => Self::has_token(e),
            CoreExpr::Label { expr: e, .. } | CoreExpr::Node { expr: e, .. } => Self::has_token(e),
            CoreExpr::Flatten(e) | CoreExpr::Prune(e) => Self::has_token(e),
            CoreExpr::Separated {
                item, separator, ..
            } => Self::has_token(item) || Self::has_token(separator),
            CoreExpr::Delimited {
                open,
                content,
                close,
                ..
            } => Self::has_token(open) || Self::has_token(content) || Self::has_token(close),
            _ => false,
        }
    }

    /// Estimate the length/complexity of an expression
    fn expression_length<T, N>(expr: &crate::grammar::CoreExpr<T, N>) -> usize {
        use crate::grammar::CoreExpr;

        match expr {
            CoreExpr::Token(_)
            | CoreExpr::Rule(_)
            | CoreExpr::Any
            | CoreExpr::Eof
            | CoreExpr::Empty => 1,
            CoreExpr::Seq(exprs) => {
                exprs
                    .iter()
                    .map(|e| Self::expression_length(e))
                    .sum::<usize>()
                    + 1
            }
            CoreExpr::Choice(exprs) => {
                exprs
                    .iter()
                    .map(|e| Self::expression_length(e))
                    .sum::<usize>()
                    + 1
            }
            CoreExpr::Opt(e) | CoreExpr::Repeat { expr: e, .. } => Self::expression_length(e) + 1,
            CoreExpr::Label { expr: e, .. } | CoreExpr::Node { expr: e, .. } => {
                Self::expression_length(e) + 1
            }
            CoreExpr::Flatten(e) | CoreExpr::Prune(e) => Self::expression_length(e) + 1,
            CoreExpr::Separated {
                item, separator, ..
            } => Self::expression_length(item) + Self::expression_length(separator) + 1,
            CoreExpr::Delimited {
                open,
                content,
                close,
                ..
            } => {
                Self::expression_length(open)
                    + Self::expression_length(content)
                    + Self::expression_length(close)
                    + 1
            }
        }
    }
}
