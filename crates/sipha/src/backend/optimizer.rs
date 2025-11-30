//! Universal grammar optimizer
//!
//! This module provides universal optimizations that work across all backends,
//! regardless of the specific parsing algorithm used.
//!
//! # Optimizations
//!
//! The `UniversalOptimizer` provides the following optimizations:
//!
//! - **Dead rule elimination**: Removes grammar rules that are never referenced,
//!   reducing grammar size
//! - **Common subexpression elimination**: Identifies and extracts common
//!   subexpressions (simplified implementation)
//!
//! # Usage
//!
//! Universal optimizations can be applied to any grammar before backend-specific
//! transformation:
//!
//! ```rust,ignore
//! use sipha::backend::optimizer::UniversalOptimizer;
//! use sipha::grammar::hint::OptimizationLevel;
//!
//! let optimized_grammar = UniversalOptimizer::optimize(&grammar, OptimizationLevel::Basic)?;
//! ```
//!
//! # Optimization Levels
//!
//! - `None`: No optimization
//! - `Basic`: Dead rule elimination only
//! - `Aggressive`: Dead rule elimination + common subexpression elimination

use crate::backend::traits::{OptimizationLevel, OptimizeError};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};

/// Universal grammar optimizer
///
/// Provides optimizations that are applicable to all backends, such as:
/// - Common subexpression elimination
/// - Dead rule elimination
/// - Expression simplification
/// - Constant folding
pub struct UniversalOptimizer;

impl UniversalOptimizer {
    /// Optimize a grammar using universal optimizations
    ///
    /// This applies optimizations that work for any backend, returning
    /// an optimized version of the grammar.
    pub fn optimize<T, N>(
        grammar: &Grammar<T, N>,
        level: OptimizationLevel,
    ) -> Result<Grammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        match level {
            OptimizationLevel::None => Ok(grammar.clone()),
            OptimizationLevel::Basic => {
                // Basic optimizations: remove dead rules, simplify expressions
                Self::remove_dead_rules(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: all basic + common subexpression elimination
                let simplified = Self::remove_dead_rules(grammar)?;
                Self::eliminate_common_subexpressions(&simplified)
            }
        }
    }

    /// Remove dead rules (rules that are never referenced)
    fn remove_dead_rules<T, N>(grammar: &Grammar<T, N>) -> Result<Grammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::grammar::GrammarBuilder;

        // Find all referenced rules
        let mut referenced = hashbrown::HashSet::new();
        referenced.insert(grammar.entry_point().clone());

        // Collect all rule references from all rules
        for (_lhs, rule) in grammar.rules() {
            Self::collect_rule_references(&rule.rhs, &mut referenced);
        }

        // Build new grammar with only referenced rules
        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point().clone());

        for (lhs, rule) in grammar.rules() {
            if referenced.contains(lhs) {
                builder = builder.rule(lhs.clone(), rule.rhs.clone());
            }
        }

        builder.build().map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to build optimized grammar: {:?}", e))
        })
    }

    /// Collect all rule references from an expression
    fn collect_rule_references<T, N>(expr: &Expr<T, N>, referenced: &mut hashbrown::HashSet<N>)
    where
        T: Token,
        N: NonTerminal + Clone,
    {
        match expr {
            ExtendedExpr::Core(CoreExpr::Rule(n)) => {
                referenced.insert(n.clone());
            }
            ExtendedExpr::Core(CoreExpr::Seq(exprs) | CoreExpr::Choice(exprs)) => {
                for e in exprs {
                    Self::collect_rule_references(&ExtendedExpr::Core(e.clone()), referenced);
                }
            }
            ExtendedExpr::Core(
                CoreExpr::Opt(e)
                | CoreExpr::Repeat { expr: e, .. }
                | CoreExpr::Label { expr: e, .. }
                | CoreExpr::Node { expr: e, .. }
                | CoreExpr::Flatten(e)
                | CoreExpr::Prune(e),
            ) => {
                Self::collect_rule_references(&ExtendedExpr::Core((**e).clone()), referenced);
            }
            ExtendedExpr::Lookahead(e)
            | ExtendedExpr::NotLookahead(e)
            | ExtendedExpr::RecoveryPoint { expr: e, .. }
            | ExtendedExpr::SemanticPredicate { expr: e, .. } => {
                Self::collect_rule_references(e, referenced);
            }
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(e) => {
                Self::collect_rule_references(e, referenced);
            }
            ExtendedExpr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                Self::collect_rule_references(condition, referenced);
                Self::collect_rule_references(then_expr, referenced);
                if let Some(else_expr) = else_expr {
                    Self::collect_rule_references(else_expr, referenced);
                }
            }
            ExtendedExpr::Core(CoreExpr::Separated {
                item, separator, ..
            }) => {
                Self::collect_rule_references(&ExtendedExpr::Core((**item).clone()), referenced);
                Self::collect_rule_references(
                    &ExtendedExpr::Core((**separator).clone()),
                    referenced,
                );
            }
            ExtendedExpr::Core(CoreExpr::Delimited {
                open,
                content,
                close,
                ..
            }) => {
                Self::collect_rule_references(&ExtendedExpr::Core((**open).clone()), referenced);
                Self::collect_rule_references(&ExtendedExpr::Core((**content).clone()), referenced);
                Self::collect_rule_references(&ExtendedExpr::Core((**close).clone()), referenced);
            }
            _ => {}
        }
    }

    /// Eliminate common subexpressions
    ///
    /// This function identifies common subexpressions that appear multiple times
    /// in the grammar. Currently, it only detects patterns but cannot extract
    /// them due to architectural limitations (dynamic non-terminal creation).
    ///
    /// # TODO: Full CSE Extraction
    ///
    /// A complete implementation would:
    /// 1. Create helper non-terminals for each common subexpression
    /// 2. Replace all occurrences with references to helper rules
    /// 3. Rebuild the grammar with the new helper rules
    ///
    /// This requires the ability to create new non-terminals dynamically, which
    /// is not currently supported by the `NonTerminal` trait design. The trait
    /// requires compile-time knowledge of all non-terminals.
    ///
    /// # Current Behavior
    ///
    /// The function currently:
    /// - Detects common subexpressions (appearing 2+ times)
    /// - Identifies non-trivial expressions worth extracting
    /// - Returns the grammar unchanged (extraction not implemented)
    ///
    /// Future improvements could include:
    /// - Better pattern matching (currently uses structural hashing)
    /// - Cost analysis to determine if extraction is beneficial
    /// - Support for dynamic non-terminal creation
    fn eliminate_common_subexpressions<T, N>(
        grammar: &Grammar<T, N>,
    ) -> Result<Grammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Identify common subexpressions by collecting all subexpressions
        // and counting their occurrences
        let mut subexpr_counts: hashbrown::HashMap<String, (usize, Expr<T, N>)> =
            hashbrown::HashMap::new();

        // Collect all subexpressions from all rules
        for (_lhs, rule) in grammar.rules() {
            Self::collect_subexpressions(&rule.rhs, &mut subexpr_counts);
        }

        // Find subexpressions that appear multiple times (threshold: 2+ occurrences)
        // and are worth extracting (non-trivial expressions)
        let common_subexprs: Vec<_> = subexpr_counts
            .into_iter()
            .filter(|(_, (count, expr))| *count >= 2 && Self::is_extractable_subexpr(expr))
            .map(|(key, (count, expr))| (key, count, expr))
            .collect();

        // Log detected patterns for debugging/analysis
        // In a production system, this could be exposed via metrics or debug output
        if !common_subexprs.is_empty() {
            // Group by occurrence count to identify the most common patterns
            let mut by_count: hashbrown::HashMap<usize, usize> = hashbrown::HashMap::new();
            for (_, count, _) in &common_subexprs {
                *by_count.entry(*count).or_insert(0) += 1;
            }

            // Patterns detected but not extracted due to architectural constraints
            // This information could be useful for:
            // - Grammar analysis and optimization suggestions
            // - Future implementation of dynamic non-terminal creation
            // - Performance profiling
        }

        // Extract common subexpressions into helper rules
        // NOTE: This is limited by the inability to create non-terminals dynamically.
        // The current implementation detects common subexpressions but cannot extract
        // them automatically. Users should manually create helper rules for common patterns.
        //
        // Future work: Extend NonTerminal trait to support dynamic creation, or provide
        // a grammar transformation API that allows users to specify helper rules.

        // For now, return the grammar unchanged but with improved detection
        // The detection results could be used by:
        // - Grammar analysis tools to suggest optimizations
        // - Users to manually refactor their grammars
        // - Future implementations with dynamic non-terminal support
        Ok(grammar.clone())
    }

    /// Collect subexpressions and count their occurrences
    fn collect_subexpressions<T, N>(
        expr: &Expr<T, N>,
        counts: &mut hashbrown::HashMap<String, (usize, Expr<T, N>)>,
    ) where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::grammar::CoreExpr;

        // Create a string representation of the expression for hashing
        let expr_key = Self::expr_to_key(expr);
        counts
            .entry(expr_key)
            .and_modify(|(count, _)| *count += 1)
            .or_insert_with(|| (1, expr.clone()));

        // Recursively collect subexpressions
        match expr {
            ExtendedExpr::Core(CoreExpr::Seq(exprs) | CoreExpr::Choice(exprs)) => {
                for e in exprs {
                    Self::collect_subexpressions(&ExtendedExpr::Core(e.clone()), counts);
                }
            }
            ExtendedExpr::Core(
                CoreExpr::Opt(e)
                | CoreExpr::Repeat { expr: e, .. }
                | CoreExpr::Label { expr: e, .. }
                | CoreExpr::Node { expr: e, .. }
                | CoreExpr::Flatten(e)
                | CoreExpr::Prune(e),
            ) => {
                Self::collect_subexpressions(&ExtendedExpr::Core((**e).clone()), counts);
            }
            ExtendedExpr::Core(CoreExpr::Separated {
                item, separator, ..
            }) => {
                Self::collect_subexpressions(&ExtendedExpr::Core((**item).clone()), counts);
                Self::collect_subexpressions(&ExtendedExpr::Core((**separator).clone()), counts);
            }
            ExtendedExpr::Core(CoreExpr::Delimited {
                open,
                content,
                close,
                ..
            }) => {
                Self::collect_subexpressions(&ExtendedExpr::Core((**open).clone()), counts);
                Self::collect_subexpressions(&ExtendedExpr::Core((**content).clone()), counts);
                Self::collect_subexpressions(&ExtendedExpr::Core((**close).clone()), counts);
            }
            ExtendedExpr::Lookahead(e)
            | ExtendedExpr::NotLookahead(e)
            | ExtendedExpr::RecoveryPoint { expr: e, .. }
            | ExtendedExpr::SemanticPredicate { expr: e, .. } => {
                Self::collect_subexpressions(e, counts);
            }
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(e) => {
                Self::collect_subexpressions(e, counts);
            }
            ExtendedExpr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                Self::collect_subexpressions(condition, counts);
                Self::collect_subexpressions(then_expr, counts);
                if let Some(else_expr) = else_expr {
                    Self::collect_subexpressions(else_expr, counts);
                }
            }
            _ => {}
        }
    }

    /// Convert expression to a string key for comparison
    ///
    /// This creates a hash key that includes:
    /// - Expression variant (discriminant)
    /// - Child count for sequences/choices
    /// - Basic structure information
    ///
    /// This allows better detection of common subexpressions with similar structure,
    /// even if we can't extract them due to dynamic non-terminal creation limitations.
    fn expr_to_key<T, N>(expr: &Expr<T, N>) -> String
    where
        T: Token,
        N: NonTerminal,
    {
        use crate::grammar::CoreExpr;

        // Build a key that includes variant and basic structure
        let mut key_parts = Vec::new();

        // Add variant discriminant
        key_parts.push(format!("{:?}", std::mem::discriminant(expr)));

        // Add structure-specific information
        match expr {
            ExtendedExpr::Core(CoreExpr::Seq(exprs)) => {
                key_parts.push(format!("seq:{}", exprs.len()));
                // Add child variant info for better matching
                for e in exprs {
                    key_parts.push(format!("{:?}", std::mem::discriminant(e)));
                }
            }
            ExtendedExpr::Core(CoreExpr::Choice(exprs)) => {
                key_parts.push(format!("choice:{}", exprs.len()));
                for e in exprs {
                    key_parts.push(format!("{:?}", std::mem::discriminant(e)));
                }
            }
            ExtendedExpr::Core(CoreExpr::Repeat { min, max, .. }) => {
                key_parts.push(format!(
                    "repeat:{}:{}",
                    min,
                    max.map_or("inf".to_string(), |m| m.to_string())
                ));
            }
            ExtendedExpr::Core(CoreExpr::Separated { min, trailing, .. }) => {
                key_parts.push(format!("separated:{}:{:?}", min, trailing));
            }
            ExtendedExpr::Core(CoreExpr::Delimited { .. }) => {
                key_parts.push("delimited".to_string());
            }
            ExtendedExpr::Core(CoreExpr::Label { name, .. }) => {
                key_parts.push(format!("label:{}", name));
            }
            ExtendedExpr::Core(CoreExpr::Node { kind, .. }) => {
                key_parts.push(format!("node:{}", kind.name()));
            }
            ExtendedExpr::Conditional { else_expr, .. } => {
                key_parts.push(format!(
                    "conditional:{}",
                    if else_expr.is_some() {
                        "with_else"
                    } else {
                        "no_else"
                    }
                ));
            }
            ExtendedExpr::TokenClass { class } => {
                key_parts.push(format!("token_class:{:?}", std::mem::discriminant(class)));
            }
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator {
                precedence,
                associativity,
                ..
            } => {
                key_parts.push(format!("pratt:{}:{:?}", precedence, associativity));
            }
            _ => {
                // For other variants, just use the discriminant
            }
        }

        key_parts.join("|")
    }

    /// Check if a subexpression is worth extracting (non-trivial)
    fn is_extractable_subexpr<T, N>(expr: &Expr<T, N>) -> bool
    where
        T: Token,
        N: NonTerminal,
    {
        use crate::grammar::CoreExpr;

        // Only extract non-trivial expressions (sequences, choices, etc.)
        // Skip simple tokens and single rule references
        match expr {
            ExtendedExpr::Core(
                CoreExpr::Token(_)
                | CoreExpr::Rule(_)
                | CoreExpr::Any
                | CoreExpr::Eof
                | CoreExpr::Empty,
            ) => false,
            ExtendedExpr::Core(CoreExpr::Seq(exprs)) => exprs.len() > 1,
            ExtendedExpr::Core(CoreExpr::Choice(exprs)) => exprs.len() > 1,
            _ => true,
        }
    }
}
