//! # Grammar Analysis
//!
//! Tools for analyzing and optimizing context-free grammars.
//!
//! This module provides utilities for:
//! - Computing grammar complexity metrics
//! - Analyzing FIRST/FOLLOW sets
//! - Detecting potential issues and optimization opportunities
//! - Providing suggestions for grammar improvements

use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};
use hashbrown::HashSet;

/// Metrics about a grammar's complexity
#[derive(Debug, Clone)]
pub struct GrammarMetrics {
    /// Total number of rules in the grammar
    pub rule_count: usize,
    /// Number of non-terminals
    pub non_terminal_count: usize,
    /// Maximum depth of rule nesting
    pub max_depth: usize,
    /// Average number of alternatives per choice
    pub avg_alternatives: f64,
    /// Number of nullable rules
    pub nullable_count: usize,
    /// Number of rules with left recursion
    pub left_recursive_count: usize,
}

impl GrammarMetrics {
    /// Compute metrics for a grammar
    #[must_use]
    pub fn compute<T, N>(grammar: &Grammar<T, N>) -> Self
    where
        T: Token,
        N: NonTerminal,
    {
        let rule_count = grammar.rules().count();
        let mut non_terminals = HashSet::new();
        let mut total_alternatives = 0;
        let mut choice_count = 0;
        let mut nullable_count = 0;
        let mut left_recursive_count = 0;
        let mut max_depth = 0;

        for (nt, rule) in grammar.rules() {
            non_terminals.insert(nt);

            if rule.rhs.is_nullable(grammar) {
                nullable_count += 1;
            }

            // Check for left recursion
            if Self::is_left_recursive(grammar, nt, &rule.rhs, &mut HashSet::new()) {
                left_recursive_count += 1;
            }

            // Compute depth and count alternatives
            let (depth, alternatives) = Self::analyze_expr(&rule.rhs, grammar, 0);
            max_depth = max_depth.max(depth);
            if alternatives > 0 {
                total_alternatives += alternatives;
                choice_count += 1;
            }
        }

        // Calculate average alternatives
        // Precision loss from usize to f64 is acceptable for metrics calculation
        let avg_alternatives = if choice_count > 0 {
            f64::from(u32::try_from(total_alternatives).unwrap_or(u32::MAX))
                / f64::from(choice_count)
        } else {
            0.0
        };

        Self {
            rule_count,
            non_terminal_count: non_terminals.len(),
            max_depth,
            avg_alternatives,
            nullable_count,
            left_recursive_count,
        }
    }

    fn analyze_expr<T, N>(
        expr: &Expr<T, N>,
        grammar: &Grammar<T, N>,
        depth: usize,
    ) -> (usize, usize)
    where
        T: Token,
        N: NonTerminal,
    {
        match expr {
            ExtendedExpr::Core(CoreExpr::Choice(alternatives)) => {
                let mut max_depth = depth;
                let alt_count = alternatives.len();
                for alt in alternatives {
                    let (d, _) =
                        Self::analyze_expr(&ExtendedExpr::Core(alt.clone()), grammar, depth + 1);
                    max_depth = max_depth.max(d);
                }
                (max_depth, alt_count)
            }
            ExtendedExpr::Core(CoreExpr::Seq(exprs)) => {
                let mut max_depth = depth;
                for e in exprs {
                    let (d, _) =
                        Self::analyze_expr(&ExtendedExpr::Core(e.clone()), grammar, depth + 1);
                    max_depth = max_depth.max(d);
                }
                (max_depth, 0)
            }
            ExtendedExpr::Core(CoreExpr::Rule(n)) => {
                grammar.get_rule(n).map_or((depth, 0), |rule| {
                    Self::analyze_expr(&rule.rhs, grammar, depth + 1)
                })
            }
            ExtendedExpr::Core(
                CoreExpr::Opt(inner)
                | CoreExpr::Repeat { expr: inner, .. }
                | CoreExpr::Label { expr: inner, .. }
                | CoreExpr::Node { expr: inner, .. }
                | CoreExpr::Flatten(inner)
                | CoreExpr::Prune(inner),
            ) => Self::analyze_expr(&ExtendedExpr::Core(*inner.clone()), grammar, depth + 1),
            ExtendedExpr::Lookahead(inner)
            | ExtendedExpr::NotLookahead(inner)
            | ExtendedExpr::RecoveryPoint { expr: inner, .. } => {
                Self::analyze_expr(inner, grammar, depth + 1)
            }
            ExtendedExpr::Core(CoreExpr::Delimited {
                open,
                content,
                close,
                ..
            }) => {
                let (d1, _) =
                    Self::analyze_expr(&ExtendedExpr::Core(*open.clone()), grammar, depth + 1);
                let (d2, _) =
                    Self::analyze_expr(&ExtendedExpr::Core(*content.clone()), grammar, depth + 1);
                let (d3, _) =
                    Self::analyze_expr(&ExtendedExpr::Core(*close.clone()), grammar, depth + 1);
                (d1.max(d2).max(d3), 0)
            }
            ExtendedExpr::Core(CoreExpr::Separated {
                item, separator, ..
            }) => {
                let (d1, _) =
                    Self::analyze_expr(&ExtendedExpr::Core(*item.clone()), grammar, depth + 1);
                let (d2, _) =
                    Self::analyze_expr(&ExtendedExpr::Core(*separator.clone()), grammar, depth + 1);
                (d1.max(d2), 0)
            }
            _ => (depth, 0),
        }
    }

    fn is_left_recursive<T, N>(
        grammar: &Grammar<T, N>,
        start: &N,
        expr: &Expr<T, N>,
        visited: &mut HashSet<N>,
    ) -> bool
    where
        T: Token,
        N: NonTerminal,
    {
        if !visited.insert(start.clone()) {
            return true; // Cycle detected
        }

        match expr {
            ExtendedExpr::Core(CoreExpr::Rule(n)) => {
                if n == start {
                    return true; // Direct left recursion
                }
                grammar
                    .get_rule(n)
                    .is_some_and(|rule| Self::is_left_recursive(grammar, start, &rule.rhs, visited))
            }
            ExtendedExpr::Core(CoreExpr::Seq(exprs)) => {
                // Check if first element can start with the non-terminal
                exprs.first().is_some_and(|first| {
                    Self::is_left_recursive(
                        grammar,
                        start,
                        &ExtendedExpr::Core(first.clone()),
                        visited,
                    )
                })
            }
            ExtendedExpr::Core(CoreExpr::Choice(alternatives)) => {
                // Left recursive if any alternative is left recursive
                alternatives.iter().any(|alt| {
                    Self::is_left_recursive(
                        grammar,
                        start,
                        &ExtendedExpr::Core(alt.clone()),
                        visited,
                    )
                })
            }
            ExtendedExpr::Core(CoreExpr::Opt(inner) | CoreExpr::Repeat { expr: inner, .. }) => {
                // Optional/repeat can be empty, so check if inner is left recursive
                Self::is_left_recursive(
                    grammar,
                    start,
                    &ExtendedExpr::Core(*inner.clone()),
                    visited,
                )
            }
            _ => false,
        }
    }
}

/// Suggestions for optimizing a grammar
#[derive(Debug, Clone)]
pub struct OptimizationSuggestion {
    /// Type of suggestion
    pub kind: SuggestionKind,
    /// Rule or non-terminal this applies to
    pub target: String,
    /// Description of the suggestion
    pub message: String,
}

/// Types of optimization suggestions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuggestionKind {
    /// Consider factoring common prefixes
    FactorCommonPrefix,
    /// Consider removing left recursion
    RemoveLeftRecursion,
    /// Consider simplifying nullable rules
    SimplifyNullable,
    /// Consider reducing choice alternatives
    ReduceAlternatives,
    /// Consider using helper non-terminals
    UseHelperNonTerminal,
}

/// Analyze a grammar and provide optimization suggestions
pub fn analyze_grammar<T, N>(grammar: &Grammar<T, N>) -> Vec<OptimizationSuggestion>
where
    T: Token,
    N: NonTerminal,
{
    let mut suggestions = Vec::new();
    let metrics = GrammarMetrics::compute(grammar);

    // Suggest removing left recursion if present
    if metrics.left_recursive_count > 0 {
        suggestions.push(OptimizationSuggestion {
            kind: SuggestionKind::RemoveLeftRecursion,
            target: "grammar".to_string(),
            message: format!(
                "Grammar contains {} left-recursive rule(s). Consider using left-recursion elimination.",
                metrics.left_recursive_count
            ),
        });
    }

    // Suggest reducing alternatives if average is high
    if metrics.avg_alternatives > 5.0 {
        suggestions.push(OptimizationSuggestion {
            kind: SuggestionKind::ReduceAlternatives,
            target: "grammar".to_string(),
            message: format!(
                "Average of {:.1} alternatives per choice is high. Consider factoring common prefixes.",
                metrics.avg_alternatives
            ),
        });
    }

    // Check for nullable rules that might benefit from simplification
    if metrics.nullable_count > metrics.rule_count / 2 {
        suggestions.push(OptimizationSuggestion {
            kind: SuggestionKind::SimplifyNullable,
            target: "grammar".to_string(),
            message: format!(
                "Many nullable rules ({}). Consider simplifying nullable expressions.",
                metrics.nullable_count
            ),
        });
    }

    // Check for rules with many alternatives
    for (nt, rule) in grammar.rules() {
        if let ExtendedExpr::Core(CoreExpr::Choice(alternatives)) = &rule.rhs
            && alternatives.len() > 8
        {
            suggestions.push(OptimizationSuggestion {
                kind: SuggestionKind::FactorCommonPrefix,
                target: nt.name().to_string(),
                message: format!(
                    "Rule '{}' has {} alternatives. Consider factoring common prefixes.",
                    nt.name(),
                    alternatives.len()
                ),
            });
        }
    }

    suggestions
}

/// Get a summary of grammar characteristics
#[must_use]
pub fn grammar_summary<T, N>(grammar: &Grammar<T, N>) -> String
where
    T: Token,
    N: NonTerminal,
{
    let metrics = GrammarMetrics::compute(grammar);
    format!(
        "Grammar summary: {} rules, {} non-terminals, max depth {}, {:.1} avg alternatives, {} nullable, {} left-recursive",
        metrics.rule_count,
        metrics.non_terminal_count,
        metrics.max_depth,
        metrics.avg_alternatives,
        metrics.nullable_count,
        metrics.left_recursive_count
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::{Expr, GrammarBuilder};
    use crate::syntax::SyntaxKind;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestSyntaxKind {
        Number,
        Plus,
        Eof,
        Expr,
    }

    impl SyntaxKind for TestSyntaxKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Expr)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    struct TestToken {
        kind: TestSyntaxKind,
    }

    impl Token for TestToken {
        type Kind = TestSyntaxKind;
        fn kind(&self) -> Self::Kind {
            self.kind
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum TestNonTerminal {
        Expr,
    }

    impl NonTerminal for TestNonTerminal {
        fn name(&self) -> &'static str {
            "Expr"
        }
    }

    #[test]
    fn test_grammar_metrics() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(
                TestNonTerminal::Expr,
                Expr::token(TestToken {
                    kind: TestSyntaxKind::Number,
                }),
            )
            .build()
            .unwrap();

        let metrics = GrammarMetrics::compute(&grammar);
        assert_eq!(metrics.rule_count, 1);
        assert_eq!(metrics.non_terminal_count, 1);
    }

    #[test]
    fn test_grammar_summary() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(
                TestNonTerminal::Expr,
                Expr::token(TestToken {
                    kind: TestSyntaxKind::Number,
                }),
            )
            .build()
            .unwrap();

        let summary = grammar_summary(&grammar);
        assert!(summary.contains("1 rules"));
    }
}
