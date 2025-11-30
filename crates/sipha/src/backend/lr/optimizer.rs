//! LR grammar optimizer
//!
//! This module provides optimizations specific to LR parsing, which uses
//! bottom-up parsing with action/goto tables.
//!
//! # Optimizations
//!
//! The `LrOptimizer` provides the following optimizations:
//!
//! - **Table compression**: Rebuilds the LR parsing table to compress action/goto
//!   tables and reduce memory usage
//! - **Production inlining**: Inlines small productions (1-2 items) that are
//!   referenced only once, reducing the number of non-terminals
//!
//! # Usage
//!
//! The optimizer is typically used through the grammar transformation pipeline:
//!
//! ```rust,ignore
//! use sipha::backend::lr::{LrParser, LrConfig};
//! use sipha::grammar::hint::OptimizationLevel;
//!
//! let config = LrConfig {
//!     optimize: true,
//!     optimization_level: OptimizationLevel::Aggressive,
//!     ..Default::default()
//! };
//! let parser = LrParser::new(&grammar, config)?;
//! ```
//!
//! # Optimization Levels
//!
//! - `None`: No optimization
//! - `Basic`: Table compression only
//! - `Aggressive`: Table compression + production inlining

use crate::backend::lr::grammar::LrGrammar;
use crate::backend::lr::table::LrParsingTable;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{CoreExpr, ExtendedExpr, Grammar, GrammarBuilder, NonTerminal, Token};

/// LR grammar optimizer
pub struct LrOptimizer;

impl<T, N> GrammarOptimizer<T, N> for LrOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = LrGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is
                Ok(LrGrammar::new(
                    grammar.table.clone(),
                    grammar.first_sets.clone(),
                    grammar.follow_sets.clone(),
                    grammar.entry_point.clone(),
                ))
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: table compression
                Self::compress_table(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: table compression + production inlining
                let compressed = Self::compress_table(grammar)?;
                Self::inline_productions(&compressed)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: false, // LR doesn't benefit from factoring
            can_eliminate_left_recursion: false, // LR handles left recursion natively
            can_compress_tables: true,
            can_merge_states: true, // LALR is state merging
        }
    }
}

impl LrOptimizer {
    /// Compress the parsing table by removing redundant entries
    fn compress_table<T, N>(grammar: &LrGrammar<T, N>) -> Result<LrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Reconstruct grammar from table productions for rebuilding
        let reconstructed_grammar = Self::reconstruct_grammar_from_table(grammar)?;

        // Rebuild the table with LALR (which is a form of compression via state merging)
        let use_lalr = true; // Use LALR for compression
        let table = LrParsingTable::new(&reconstructed_grammar, use_lalr).map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to rebuild table: {e}"))
        })?;

        // Recompute FIRST and FOLLOW sets
        let follow_sets_raw = reconstructed_grammar.compute_follow_sets();
        let follow_sets: hashbrown::HashMap<N, hashbrown::HashSet<T>> = follow_sets_raw
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();

        let mut first_sets_map: hashbrown::HashMap<N, hashbrown::HashSet<T>> =
            hashbrown::HashMap::new();
        for (nt, _rule) in reconstructed_grammar.rules() {
            let first_set = reconstructed_grammar
                .get_rule(nt)
                .map(|r| r.rhs.first_set(&reconstructed_grammar))
                .unwrap_or_default();
            first_sets_map.insert(nt.clone(), first_set.into_iter().collect());
        }

        Ok(LrGrammar::new(
            table,
            first_sets_map,
            follow_sets,
            grammar.entry_point.clone(),
        ))
    }

    /// Reconstruct grammar from table productions
    fn reconstruct_grammar_from_table<T, N>(
        grammar: &LrGrammar<T, N>,
    ) -> Result<Grammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::backend::lr::table::ProductionItem;

        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point.clone());

        // Group productions by LHS
        let mut rules: hashbrown::HashMap<N, Vec<Vec<ProductionItem<T, N>>>> =
            hashbrown::HashMap::new();

        // Iterate through productions (skip index 0 which is the augmented start)
        // We'll try indices until we get None
        let mut prod_idx = 1;
        while let Some(prod) = grammar.table.get_production(prod_idx) {
            let lhs = prod.lhs.clone();
            rules
                .entry(lhs)
                .or_insert_with(Vec::new)
                .push(prod.rhs.clone());
            prod_idx += 1;
        }

        // Convert productions back to grammar rules
        for (lhs, productions) in rules {
            let alternatives: Vec<CoreExpr<T, N>> = productions
                .into_iter()
                .map(|items| {
                    let expr_items: Vec<CoreExpr<T, N>> = items
                        .into_iter()
                        .map(|item| match item {
                            ProductionItem::Token(t) => CoreExpr::Token(t),
                            ProductionItem::NonTerminal(nt) => CoreExpr::Rule(nt),
                        })
                        .collect();
                    if expr_items.len() == 1 {
                        expr_items.into_iter().next().unwrap()
                    } else {
                        CoreExpr::Seq(expr_items)
                    }
                })
                .collect();

            let rhs = if alternatives.len() == 1 {
                ExtendedExpr::Core(alternatives.into_iter().next().unwrap())
            } else {
                ExtendedExpr::Core(CoreExpr::Choice(alternatives))
            };

            builder = builder.rule(lhs, rhs);
        }

        builder.build().map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to reconstruct grammar: {e:?}"))
        })
    }

    /// Inline small productions to reduce table size
    fn inline_productions<T, N>(grammar: &LrGrammar<T, N>) -> Result<LrGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Reconstruct grammar with inlined productions
        let reconstructed_grammar = Self::reconstruct_grammar_with_inlining(grammar)?;

        // Rebuild the table
        let use_lalr = true;
        let table = LrParsingTable::new(&reconstructed_grammar, use_lalr).map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to rebuild table: {e}"))
        })?;

        // Recompute FIRST and FOLLOW sets
        let follow_sets_raw = reconstructed_grammar.compute_follow_sets();
        let follow_sets: hashbrown::HashMap<N, hashbrown::HashSet<T>> = follow_sets_raw
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();

        let mut first_sets_map: hashbrown::HashMap<N, hashbrown::HashSet<T>> =
            hashbrown::HashMap::new();
        for (nt, _rule) in reconstructed_grammar.rules() {
            let first_set = reconstructed_grammar
                .get_rule(nt)
                .map(|r| r.rhs.first_set(&reconstructed_grammar))
                .unwrap_or_default();
            first_sets_map.insert(nt.clone(), first_set.into_iter().collect());
        }

        Ok(LrGrammar::new(
            table,
            first_sets_map,
            follow_sets,
            grammar.entry_point.clone(),
        ))
    }

    /// Reconstruct grammar with small productions inlined.
    ///
    /// This method identifies small productions (productions with 1-2 items) that are referenced
    /// only once and inlines them directly into their parent productions. This reduces the
    /// number of non-terminals and can improve parsing table size and performance.
    ///
    /// # Inlining Strategy
    ///
    /// A production is eligible for inlining if:
    ///
    /// 1. **Size**: All productions for the non-terminal have 1-2 items (tokens or non-terminals)
    /// 2. **Reference count**: The non-terminal is referenced exactly once in the entire grammar
    /// 3. **Not entry point**: The non-terminal is not the grammar's entry point
    ///
    /// # Handling Multiple Productions
    ///
    /// When a non-terminal has multiple productions (choice alternatives):
    ///
    /// - **Single production**: The production items are directly inlined into the parent
    /// - **Multiple productions**: The first alternative is inlined (a full implementation would
    ///   create a choice, but this is simplified to avoid complexity)
    ///
    /// # Recursive Inlining
    ///
    /// The method handles nested inlining by checking if an inlined non-terminal itself contains
    /// other inlinable non-terminals. To avoid infinite loops, recursive inlining is limited:
    /// if a non-terminal marked for inlining contains another inlinable non-terminal, the inner
    /// one is kept as a reference rather than being inlined.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// // Before inlining:
    /// // Expr: Term
    /// // Term: Number
    ///
    /// // After inlining (if Term is referenced only once):
    /// // Expr: Number
    /// ```
    ///
    /// # Returns
    ///
    /// Returns a new `Grammar` with inlined productions. The grammar structure is preserved,
    /// but small helper productions are removed and their content is inlined into parent rules.
    ///
    /// # Errors
    ///
    /// Returns an error if the grammar cannot be rebuilt after inlining (e.g., due to
    /// structural issues).
    fn reconstruct_grammar_with_inlining<T, N>(
        grammar: &LrGrammar<T, N>,
    ) -> Result<Grammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::backend::lr::table::ProductionItem;

        // First, reconstruct the grammar normally
        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point.clone());

        // Group productions by LHS
        let mut rules: hashbrown::HashMap<N, Vec<Vec<ProductionItem<T, N>>>> =
            hashbrown::HashMap::new();
        let mut all_non_terminals = hashbrown::HashSet::new();

        let mut prod_idx = 1;
        while let Some(prod) = grammar.table.get_production(prod_idx) {
            let lhs = prod.lhs.clone();
            all_non_terminals.insert(lhs.clone());
            // Track non-terminals used in RHS
            for item in &prod.rhs {
                if let ProductionItem::NonTerminal(nt) = item {
                    all_non_terminals.insert(nt.clone());
                }
            }
            rules
                .entry(lhs)
                .or_insert_with(Vec::new)
                .push(prod.rhs.clone());
            prod_idx += 1;
        }

        // Identify small productions to inline (1-2 items, not entry point, referenced only once)
        let mut to_inline: hashbrown::HashSet<N> = hashbrown::HashSet::new();
        let mut reference_count: hashbrown::HashMap<N, usize> = hashbrown::HashMap::new();

        // Count references to each non-terminal
        for productions in rules.values() {
            for prod_items in productions {
                for item in prod_items {
                    if let ProductionItem::NonTerminal(nt) = item {
                        *reference_count.entry(nt.clone()).or_insert(0) += 1;
                    }
                }
            }
        }

        // Mark small productions for inlining (1-2 items, referenced once, not entry point)
        for (lhs, productions) in &rules {
            if lhs == &grammar.entry_point {
                continue; // Don't inline entry point
            }
            if let Some(count) = reference_count.get(lhs) {
                if *count == 1 {
                    // Check if all productions for this LHS are small (1-2 items)
                    let all_small = productions.iter().all(|prod| prod.len() <= 2);
                    if all_small && !productions.is_empty() {
                        to_inline.insert(lhs.clone());
                    }
                }
            }
        }

        // Build grammar with inlined productions
        for (lhs, productions) in &rules {
            if to_inline.contains(lhs) {
                // Skip this rule - it will be inlined
                continue;
            }

            // Process each production and expand alternatives when inlining
            let mut alternatives: Vec<CoreExpr<T, N>> = Vec::new();

            for items in productions {
                // Helper function to inline a non-terminal and return its alternatives
                fn inline_non_terminal<T: Token + Clone, N: NonTerminal + Clone>(
                    nt: &N,
                    rules: &hashbrown::HashMap<N, Vec<Vec<ProductionItem<T, N>>>>,
                    to_inline: &hashbrown::HashSet<N>,
                ) -> Vec<Vec<CoreExpr<T, N>>> {
                    if let Some(inlined_productions) = rules.get(nt) {
                        inlined_productions
                            .iter()
                            .map(|inlined_items| {
                                inlined_items
                                    .iter()
                                    .map(|inlined_item| match inlined_item {
                                        ProductionItem::Token(t) => CoreExpr::Token(t.clone()),
                                        ProductionItem::NonTerminal(nt2) => {
                                            // Recursively inline if needed, but avoid infinite loops
                                            if to_inline.contains(nt2) {
                                                // For recursive inlining, keep as reference to avoid loops
                                                CoreExpr::Rule(nt2.clone())
                                            } else {
                                                CoreExpr::Rule(nt2.clone())
                                            }
                                        }
                                    })
                                    .collect()
                            })
                            .collect()
                    } else {
                        // Production not found - return as single rule reference
                        vec![vec![CoreExpr::Rule(nt.clone())]]
                    }
                }

                // Build alternatives by processing items and expanding inlined non-terminals
                let mut current_alternatives: Vec<Vec<CoreExpr<T, N>>> = vec![Vec::new()];

                for item in items {
                    match item {
                        ProductionItem::Token(t) => {
                            // Add token to all current alternatives
                            for alt in &mut current_alternatives {
                                alt.push(CoreExpr::Token(t.clone()));
                            }
                        }
                        ProductionItem::NonTerminal(nt) => {
                            if to_inline.contains(nt) {
                                // Inline this non-terminal: expand current alternatives
                                let inlined_alts = inline_non_terminal(nt, &rules, &to_inline);

                                // Cross product: each current alternative × each inlined alternative
                                let mut new_alternatives = Vec::new();
                                for current_alt in &current_alternatives {
                                    for inlined_alt in &inlined_alts {
                                        let mut new_alt = current_alt.clone();
                                        new_alt.extend(inlined_alt.clone());
                                        new_alternatives.push(new_alt);
                                    }
                                }
                                current_alternatives = new_alternatives;
                            } else {
                                // Not marked for inlining - keep as reference
                                for alt in &mut current_alternatives {
                                    alt.push(CoreExpr::Rule(nt.clone()));
                                }
                            }
                        }
                    }
                }

                // Convert each alternative to a CoreExpr
                for alt_items in current_alternatives {
                    if alt_items.len() == 1 {
                        alternatives.push(alt_items.into_iter().next().unwrap());
                    } else {
                        alternatives.push(CoreExpr::Seq(alt_items));
                    }
                }
            }

            let rhs = if alternatives.len() == 1 {
                ExtendedExpr::Core(alternatives.into_iter().next().unwrap())
            } else {
                ExtendedExpr::Core(CoreExpr::Choice(alternatives))
            };

            builder = builder.rule(lhs.clone(), rhs);
        }

        builder.build().map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to build inlined grammar: {e:?}"))
        })
    }
}
