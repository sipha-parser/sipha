//! Universal grammar optimizer
//!
//! This module provides universal optimizations that work across all backends,
//! such as common subexpression elimination, dead rule elimination, and
//! expression simplification.

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
    #[allow(clippy::unnecessary_wraps)] // May return errors in future implementation
    fn eliminate_common_subexpressions<T, N>(
        grammar: &Grammar<T, N>,
    ) -> Result<Grammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // For now, just return the grammar as-is
        // In a full implementation, this would:
        // - Identify common subexpressions across rules
        // - Extract them into helper rules
        // - Replace occurrences with references to helper rules
        Ok(grammar.clone())
    }
}
