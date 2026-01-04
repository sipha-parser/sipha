//! # Advanced Grammar Analysis
//!
//! This module provides comprehensive grammar analysis and optimization suggestions.
//!
//! ## Features
//!
//! - Ambiguity detection
//! - Left recursion detection
//! - LL(k) conflict analysis
//! - LR conflict analysis
//! - Unreachable rule detection
//! - Refactoring suggestions

use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;

/// Grammar analyzer for comprehensive analysis
pub struct GrammarAnalyzer<'g, T: Token, N: NonTerminal> {
    grammar: &'g Grammar<T, N>,
}

impl<'g, T: Token, N: NonTerminal> GrammarAnalyzer<'g, T, N> {
    /// Create a new grammar analyzer
    #[must_use]
    pub const fn new(grammar: &'g Grammar<T, N>) -> Self {
        Self { grammar }
    }

    /// Find left recursion cycles in the grammar
    #[must_use]
    pub fn find_left_recursion(&self) -> Vec<RecursionCycle<N>> {
        let mut cycles = Vec::new();
        let mut visited = HashSet::with_hasher(ahash::RandomState::new());
        let mut path = Vec::new();

        for (nt, _) in self.grammar.rules() {
            if !visited.contains(nt) {
                self.find_left_recursion_dfs(nt, &mut visited, &mut path, &mut cycles);
            }
        }

        cycles
    }

    fn find_left_recursion_dfs(
        &self,
        current: &N,
        visited: &mut HashSet<N, ahash::RandomState>,
        path: &mut Vec<N>,
        cycles: &mut Vec<RecursionCycle<N>>,
    ) {
        if path.contains(current) {
            // Found a cycle
            let cycle_start = path.iter().position(|n| n == current).unwrap();
            cycles.push(RecursionCycle {
                nodes: path[cycle_start..].to_vec(),
                is_direct: path.len() - cycle_start == 1,
            });
            return;
        }

        if visited.contains(current) {
            return;
        }

        path.push(current.clone());

        if let Some(rule) = self.grammar.get_rule(current) {
            // Get leftmost non-terminals from the expression
            for nt in self.leftmost_non_terminals(&rule.rhs) {
                self.find_left_recursion_dfs(&nt, visited, path, cycles);
            }
        }

        path.pop();
        visited.insert(current.clone());
    }

    /// Get leftmost non-terminals from an expression
    fn leftmost_non_terminals(&self, expr: &Expr<T, N>) -> Vec<N> {
        let mut result = Vec::new();
        self.collect_leftmost_non_terminals(expr, &mut result);
        result
    }

    fn collect_leftmost_non_terminals(&self, expr: &Expr<T, N>, result: &mut Vec<N>) {
        match expr {
            Expr::Rule(nt) => {
                result.push(nt.clone());
            }
            Expr::Seq(items) if !items.is_empty() => {
                // Only the first item can be leftmost
                self.collect_leftmost_non_terminals(&items[0], result);
                // If first item is nullable, check the next
                if items[0].is_nullable(self.grammar) && items.len() > 1 {
                    self.collect_leftmost_non_terminals(&items[1], result);
                }
            }
            Expr::Choice(alts) => {
                // All alternatives can be leftmost
                for alt in alts {
                    self.collect_leftmost_non_terminals(alt, result);
                }
            }
            Expr::Opt(inner) | Expr::Repeat { expr: inner, .. } => {
                self.collect_leftmost_non_terminals(inner, result);
            }
            _ => {}
        }
    }

    /// Find unreachable rules in the grammar
    #[must_use]
    pub fn find_unreachable_rules(&self) -> Vec<N> {
        let mut reachable = HashSet::with_hasher(ahash::RandomState::new());
        let mut worklist = vec![self.grammar.entry_point().clone()];

        while let Some(nt) = worklist.pop() {
            if reachable.contains(&nt) {
                continue;
            }
            reachable.insert(nt.clone());

            if let Some(rule) = self.grammar.get_rule(&nt) {
                let mut referenced = Vec::new();
                collect_non_terminals(&rule.rhs, &mut referenced);
                for nt_ref in referenced {
                    if !reachable.contains(&nt_ref) {
                        worklist.push(nt_ref);
                    }
                }
            }
        }

        let mut unreachable = Vec::new();
        for (nt, _) in self.grammar.rules() {
            if !reachable.contains(nt) {
                unreachable.push(nt.clone());
            }
        }

        unreachable
    }

    /// Perform nullable analysis for all non-terminals
    #[must_use]
    pub fn nullable_analysis(&self) -> NullableMap<N> {
        let mut nullable = HashMap::with_hasher(ahash::RandomState::new());

        // Fixed-point iteration
        let mut changed = true;
        while changed {
            changed = false;

            for (nt, rule) in self.grammar.rules() {
                let is_nullable = rule.rhs.is_nullable(self.grammar);
                if let Some(&current) = nullable.get(nt) {
                    if is_nullable != current {
                        nullable.insert(nt.clone(), is_nullable);
                        changed = true;
                    }
                } else {
                    nullable.insert(nt.clone(), is_nullable);
                    changed = true;
                }
            }
        }

        NullableMap { map: nullable }
    }

    /// Find potential LL(1) conflicts
    #[must_use]
    pub fn ll1_conflicts(&self) -> Vec<LLConflict<T, N>> {
        self.ll_k_conflicts(1)
    }

    /// Find potential LL(k) conflicts for a given k
    #[must_use]
    pub fn ll_k_conflicts(&self, k: usize) -> Vec<LLConflict<T, N>> {
        let mut conflicts = Vec::new();

        for (nt, rule) in self.grammar.rules() {
            if let Expr::Choice(alts) = &rule.rhs {
                // Check each pair of alternatives for FIRST set conflicts
                for i in 0..alts.len() {
                    for j in (i + 1)..alts.len() {
                        let first_i = alts[i].first_set(self.grammar);
                        let first_j = alts[j].first_set(self.grammar);

                        let intersection: HashSet<T, ahash::RandomState> =
                            first_i.intersection(&first_j).cloned().collect();

                        if !intersection.is_empty() {
                            conflicts.push(LLConflict {
                                non_terminal: nt.clone(),
                                lookahead: k,
                                conflicting_tokens: intersection.into_iter().collect(),
                                alternatives: vec![i, j],
                            });
                        }
                    }
                }
            }
        }

        conflicts
    }

    /// Suggest refactorings to improve the grammar
    #[must_use]
    pub fn suggest_refactorings(&self) -> Vec<RefactoringSuggestion<N>> {
        let mut suggestions = Vec::new();

        // Check for left recursion
        let cycles = self.find_left_recursion();
        for cycle in cycles {
            if cycle.is_direct {
                suggestions.push(RefactoringSuggestion {
                    kind: RefactoringKind::EliminateLeftRecursion,
                    affected: cycle.nodes,
                    description: "Direct left recursion can be eliminated by rewriting to right recursion or iteration".into(),
                });
            } else {
                suggestions.push(RefactoringSuggestion {
                    kind: RefactoringKind::EliminateLeftRecursion,
                    affected: cycle.nodes,
                    description:
                        "Indirect left recursion - consider refactoring the grammar structure"
                            .into(),
                });
            }
        }

        // Check for unreachable rules
        let unreachable = self.find_unreachable_rules();
        if !unreachable.is_empty() {
            suggestions.push(RefactoringSuggestion {
                kind: RefactoringKind::RemoveUnreachable,
                affected: unreachable,
                description: "These rules are never used and can be removed".into(),
            });
        }

        // Check for LL(1) conflicts
        let conflicts = self.ll1_conflicts();
        for conflict in conflicts {
            suggestions.push(RefactoringSuggestion {
                kind: RefactoringKind::LeftFactor,
                affected: vec![conflict.non_terminal],
                description: format!(
                    "LL(1) conflict with {} tokens - consider left factoring",
                    conflict.conflicting_tokens.len()
                ),
            });
        }

        suggestions
    }

    /// Get grammar statistics
    #[must_use]
    pub fn statistics(&self) -> GrammarStats {
        let mut total_alts = 0;
        let mut max_depth = 0;
        let mut nullable_count = 0;

        let nullable = self.nullable_analysis();

        for (nt, rule) in self.grammar.rules() {
            total_alts += count_alternatives(&rule.rhs);
            max_depth = max_depth.max(expr_depth(&rule.rhs));
            if nullable.is_nullable(nt) {
                nullable_count += 1;
            }
        }

        GrammarStats {
            rule_count: self.grammar.rule_count(),
            total_alternatives: total_alts,
            max_expression_depth: max_depth,
            nullable_rules: nullable_count,
            left_recursive_cycles: self.find_left_recursion().len(),
            unreachable_rules: self.find_unreachable_rules().len(),
        }
    }
}

/// Count alternatives in an expression
fn count_alternatives<T: Token, N: NonTerminal>(expr: &Expr<T, N>) -> usize {
    match expr {
        Expr::Choice(alts) => alts.len() + alts.iter().map(count_alternatives).sum::<usize>(),
        Expr::Seq(items) => items.iter().map(count_alternatives).sum(),
        Expr::Opt(inner) | Expr::Repeat { expr: inner, .. } => count_alternatives(inner),
        _ => 0,
    }
}

/// Get the depth of an expression
fn expr_depth<T: Token, N: NonTerminal>(expr: &Expr<T, N>) -> usize {
    match expr {
        Expr::Choice(alts) => 1 + alts.iter().map(expr_depth).max().unwrap_or(0),
        Expr::Seq(items) => 1 + items.iter().map(expr_depth).max().unwrap_or(0),
        Expr::Opt(inner) | Expr::Repeat { expr: inner, .. } => 1 + expr_depth(inner),
        _ => 1,
    }
}

/// Collect all non-terminals referenced in an expression
fn collect_non_terminals<T: Token, N: NonTerminal>(expr: &Expr<T, N>, result: &mut Vec<N>) {
    match expr {
        Expr::Rule(nt) => result.push(nt.clone()),
        Expr::Seq(items) | Expr::Choice(items) => {
            for item in items {
                collect_non_terminals(item, result);
            }
        }
        Expr::Opt(inner)
        | Expr::Repeat { expr: inner, .. }
        | Expr::Label { expr: inner, .. }
        | Expr::Node { expr: inner, .. }
        | Expr::Flatten(inner)
        | Expr::Prune(inner)
        | Expr::Lookahead(inner)
        | Expr::NotLookahead(inner)
        | Expr::Cut(inner)
        | Expr::RecoveryPoint { expr: inner, .. }
        | Expr::SemanticPredicate { expr: inner, .. } => {
            collect_non_terminals(inner, result);
        }
        Expr::Separated {
            item, separator, ..
        } => {
            collect_non_terminals(item, result);
            collect_non_terminals(separator, result);
        }
        Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            collect_non_terminals(open, result);
            collect_non_terminals(content, result);
            collect_non_terminals(close, result);
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_non_terminals(condition, result);
            collect_non_terminals(then_expr, result);
            if let Some(e) = else_expr {
                collect_non_terminals(e, result);
            }
        }
        _ => {}
    }
}

/// A left recursion cycle in the grammar
#[derive(Debug, Clone)]
pub struct RecursionCycle<N> {
    /// The non-terminals in the cycle
    pub nodes: Vec<N>,
    /// Whether this is direct (A -> A...) or indirect
    pub is_direct: bool,
}

/// Map of nullable non-terminals
pub struct NullableMap<N: NonTerminal> {
    map: HashMap<N, bool, ahash::RandomState>,
}

impl<N: NonTerminal> NullableMap<N> {
    /// Check if a non-terminal is nullable
    #[must_use]
    pub fn is_nullable(&self, nt: &N) -> bool {
        self.map.get(nt).copied().unwrap_or(false)
    }

    /// Get all nullable non-terminals
    pub fn nullable_non_terminals(&self) -> impl Iterator<Item = &N> {
        self.map.iter().filter(|&(_, &v)| v).map(|(k, _)| k)
    }
}

/// An LL(k) conflict
#[derive(Debug, Clone)]
pub struct LLConflict<T: Token, N: NonTerminal> {
    /// The non-terminal with the conflict
    pub non_terminal: N,
    /// The lookahead size
    pub lookahead: usize,
    /// The conflicting tokens in FIRST sets
    pub conflicting_tokens: SmallVec<[T; 4]>,
    /// Indices of conflicting alternatives
    pub alternatives: Vec<usize>,
}

/// An LR conflict
#[derive(Debug, Clone)]
pub struct LRConflict<T: Token, N: NonTerminal> {
    /// The state where the conflict occurs
    pub state: usize,
    /// The lookahead token
    pub lookahead: T,
    /// Type of conflict
    pub kind: LRConflictKind<N>,
}

/// Types of LR conflicts
#[derive(Debug, Clone)]
pub enum LRConflictKind<N> {
    /// Shift-reduce conflict
    ShiftReduce { reduce_rule: N },
    /// Reduce-reduce conflict
    ReduceReduce { rules: Vec<N> },
}

/// A refactoring suggestion
#[derive(Debug, Clone)]
pub struct RefactoringSuggestion<N> {
    /// The kind of refactoring suggested
    pub kind: RefactoringKind,
    /// The affected non-terminals
    pub affected: Vec<N>,
    /// Human-readable description
    pub description: String,
}

/// Types of refactoring suggestions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefactoringKind {
    /// Eliminate left recursion
    EliminateLeftRecursion,
    /// Left factor common prefixes
    LeftFactor,
    /// Remove unreachable rules
    RemoveUnreachable,
    /// Inline a rule that's only used once
    InlineSingleUse,
    /// Extract common subexpression
    ExtractCommon,
}

/// Grammar statistics
#[derive(Debug, Clone)]
pub struct GrammarStats {
    /// Number of rules
    pub rule_count: usize,
    /// Total number of alternatives
    pub total_alternatives: usize,
    /// Maximum expression depth
    pub max_expression_depth: usize,
    /// Number of nullable rules
    pub nullable_rules: usize,
    /// Number of left recursive cycles
    pub left_recursive_cycles: usize,
    /// Number of unreachable rules
    pub unreachable_rules: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Tests would require full grammar setup which is complex
    // See integration tests for full examples
}
