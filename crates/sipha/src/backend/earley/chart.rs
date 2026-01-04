//! Chart data structure for Earley parser

use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use std::collections::{HashMap, HashSet};

/// An Earley item (state) in the chart
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EarleyItem<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// The non-terminal being parsed
    pub lhs: N,
    /// The production rule index
    pub rule_index: usize,
    /// Current position in the rule (dot position)
    pub dot: usize,
    /// Start position in input
    pub start: usize,
}

impl<T, N> EarleyItem<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new Earley item
    pub fn new(lhs: N, rule_index: usize, dot: usize, start: usize) -> Self {
        Self {
            lhs,
            rule_index,
            dot,
            start,
        }
    }
    
    /// Check if this item is complete (dot is at the end)
    pub fn is_complete(&self, rule_rhs_len: usize) -> bool {
        self.dot >= rule_rhs_len
    }
    
    /// Get the symbol after the dot
    pub fn next_symbol(&self, rule_rhs: &Expr<T, N>) -> Option<EarleySymbol<T, N>> {
        match rule_rhs {
            Expr::Seq(items) => {
                if self.dot < items.len() {
                    Some(expr_to_symbol(&items[self.dot]))
                } else {
                    None
                }
            }
            _ => {
                if self.dot == 0 {
                    Some(expr_to_symbol(rule_rhs))
                } else {
                    None
                }
            }
        }
    }
}

/// Symbol in an Earley item (either terminal or non-terminal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EarleySymbol<T, N>
where
    T: Token,
    N: NonTerminal,
{
    Terminal(T),
    NonTerminal(N),
}

/// Convert a grammar expression to an Earley symbol
/// 
/// For complex expressions, we extract the first symbol that can be matched.
/// This is used during Earley parsing to determine what to predict/scan next.
fn expr_to_symbol<T, N>(expr: &Expr<T, N>) -> EarleySymbol<T, N>
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(t) => EarleySymbol::Terminal(t.clone()),
        Expr::Rule(n) => EarleySymbol::NonTerminal(n.clone()),
        Expr::Seq(items) => {
            // For sequences, return the first item's symbol
            if let Some(first) = items.first() {
                expr_to_symbol(first)
            } else {
                // Empty sequence - this shouldn't happen in valid grammars
                panic!("Empty sequence in grammar");
            }
        }
        Expr::Choice(items) => {
            // For choices, we need to predict all alternatives
            // Return the first one for now - the predictor will handle all alternatives
            if let Some(first) = items.first() {
                expr_to_symbol(first)
            } else {
                panic!("Empty choice in grammar");
            }
        }
        Expr::Opt(inner) => expr_to_symbol(inner),
        Expr::Repeat { expr: inner, .. } => expr_to_symbol(inner),
        Expr::Separated { item, .. } => expr_to_symbol(item),
        Expr::Delimited { open, .. } => expr_to_symbol(open),
        Expr::Node { expr: inner, .. } => expr_to_symbol(inner),
        Expr::Label { expr: inner, .. } => expr_to_symbol(inner),
        Expr::Flatten(inner) => expr_to_symbol(inner),
        Expr::Prune(inner) => expr_to_symbol(inner),
        Expr::Lookahead(inner) => expr_to_symbol(inner),
        Expr::NotLookahead(inner) => expr_to_symbol(inner),
        Expr::Cut(inner) => expr_to_symbol(inner),
        Expr::SemanticPredicate { expr: inner, .. } => expr_to_symbol(inner),
        Expr::RecoveryPoint { expr: inner, .. } => expr_to_symbol(inner),
        Expr::Any => {
            // Any matches any token - treat as a special terminal
            // This is a limitation: we can't represent "any" properly in EarleySymbol
            // For now, we'll need to handle this specially in the parser
            panic!("Expr::Any not directly supported in Earley parser - use Expr::TokenClass instead");
        }
        Expr::Eof => {
            panic!("Expr::Eof should be handled specially, not as a symbol");
        }
        Expr::Empty => {
            panic!("Expr::Empty should be handled specially, not as a symbol");
        }
        Expr::TokenClass { .. } => {
            // Token classes need special handling - for now, treat as Any
            panic!("Expr::TokenClass not fully supported in Earley parser yet");
        }
        Expr::Conditional { condition, .. } => expr_to_symbol(condition),
        Expr::Backreference { .. } => {
            panic!("Expr::Backreference not supported in Earley parser");
        }
    }
}

/// Chart for Earley parsing (set of states for each position)
pub struct EarleyChart<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Sets of items for each position
    /// chart[i] contains items that have been recognized up to position i
    chart: Vec<HashSet<EarleyItem<T, N>>>,
}

impl<T, N> EarleyChart<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new chart for parsing input of given length
    pub fn new(input_len: usize) -> Self {
        let mut chart = Vec::with_capacity(input_len + 1);
        for _ in 0..=input_len {
            chart.push(HashSet::new());
        }
        Self { chart }
    }
    
    /// Get the set of items at a given position
    pub fn get(&self, position: usize) -> &HashSet<EarleyItem<T, N>> {
        &self.chart[position]
    }
    
    /// Get the set of items at a given position (mutable)
    pub fn get_mut(&mut self, position: usize) -> &mut HashSet<EarleyItem<T, N>> {
        &mut self.chart[position]
    }
    
    /// Add an item to the chart at the given position
    /// Returns true if the item was newly added
    pub fn add(&mut self, position: usize, item: EarleyItem<T, N>) -> bool {
        self.chart[position].insert(item)
    }
    
    /// Check if an item exists at the given position
    pub fn contains(&self, position: usize, item: &EarleyItem<T, N>) -> bool {
        self.chart[position].contains(item)
    }
    
    /// Get the number of positions (input length + 1)
    pub fn len(&self) -> usize {
        self.chart.len()
    }
}

