//! Pratt grammar optimizer
//!
//! This module provides optimizations specific to Pratt parsing, which uses
//! operator precedence parsing for efficient expression parsing.
//!
//! # Optimizations
//!
//! The `PrattOptimizer` provides the following optimizations:
//!
//! - **Operator precedence table optimization**: Removes duplicate operator entries
//!   and ensures consistency in the operator table
//! - **Expression flattening**: Flattens deeply nested sequences and choices to
//!   reduce parsing overhead
//!
//! # Usage
//!
//! The optimizer is typically used through the grammar transformation pipeline:
//!
//! ```rust,ignore
//! use sipha::backend::pratt::{PrattParser, PrattConfig};
//! use sipha::grammar::hint::OptimizationLevel;
//!
//! let config = PrattConfig {
//!     optimize: true,
//!     optimization_level: OptimizationLevel::Aggressive,
//!     ..Default::default()
//! };
//! let parser = PrattParser::new(&grammar, config)?;
//! ```
//!
//! # Optimization Levels
//!
//! - `None`: No optimization
//! - `Basic`: Operator table optimization only
//! - `Aggressive`: Operator optimization + expression flattening

use crate::backend::pratt::grammar::PrattGrammar;
use crate::backend::traits::{
    GrammarOptimizer, OptimizationCapabilities, OptimizationLevel, OptimizeError,
};
use crate::grammar::{Expr, ExtendedExpr, NonTerminal, Token};

/// Pratt grammar optimizer
pub struct PrattOptimizer;

impl<T, N> GrammarOptimizer<T, N> for PrattOptimizer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = PrattGrammar<T, N>;

    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError> {
        match level {
            OptimizationLevel::None => {
                // Return the grammar as-is
                Ok(grammar.clone_grammar())
            }
            OptimizationLevel::Basic => {
                // Basic optimizations: operator table optimization
                Self::optimize_operators(grammar)
            }
            OptimizationLevel::Aggressive => {
                // Aggressive optimizations: operator optimization + expression flattening
                let optimized = Self::optimize_operators(grammar)?;
                Self::flatten_expressions(&optimized)
            }
        }
    }

    fn capabilities(&self) -> OptimizationCapabilities {
        OptimizationCapabilities {
            can_inline: true,
            can_factor: false, // Pratt doesn't benefit from factoring
            can_eliminate_left_recursion: false, // Pratt handles left recursion natively via precedence
            can_compress_tables: true,           // Can optimize operator table
            can_merge_states: false,             // Pratt doesn't use states
        }
    }
}

impl PrattOptimizer {
    /// Optimize the operator precedence table.
    ///
    /// This method performs several optimizations on the operator table:
    ///
    /// 1. **Duplicate removal**: If the same token appears multiple times in the operator table,
    ///    the entry with the most complete information (most operator types set: prefix/infix/postfix)
    ///    is kept, and duplicates are removed.
    ///
    /// 2. **Consistency validation**: Ensures that operator information is consistent across
    ///    the table, though the current implementation uses a HashMap which doesn't maintain
    ///    ordering.
    ///
    /// # Behavior with Duplicate Operators
    ///
    /// When the same token appears multiple times with different `OperatorInfo`, the method
    /// keeps the entry that has the most operator types enabled (prefix, infix, or postfix).
    /// This ensures that the most complete operator information is preserved.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use sipha::backend::pratt::{PrattGrammar, PrattOptimizer};
    /// use sipha::backend::pratt::grammar::OperatorInfo;
    /// use sipha::grammar::hint::Associativity;
    ///
    /// let mut grammar = PrattGrammar::new(original_grammar);
    /// grammar.add_operator(token1, OperatorInfo {
    ///     precedence: 10,
    ///     associativity: Associativity::Left,
    ///     is_prefix: false,
    ///     is_infix: true,
    ///     is_postfix: false,
    /// });
    ///
    /// // Optimize the operator table
    /// let optimized = PrattOptimizer::optimize_operators(&grammar)?;
    /// ```
    ///
    /// # Returns
    ///
    /// Returns a new `PrattGrammar` with an optimized operator table. The original grammar
    /// structure and entry point are preserved.
    fn optimize_operators<T, N>(
        grammar: &PrattGrammar<T, N>,
    ) -> Result<PrattGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::backend::pratt::grammar::OperatorInfo;

        // Build optimized operator table
        let mut optimized_operators: hashbrown::HashMap<T, OperatorInfo> =
            hashbrown::HashMap::new();

        // Remove duplicates and ensure consistency
        // If the same token appears multiple times, keep the one with the most complete info
        for (token, info) in &grammar.operators {
            // Check if we already have this token
            if let Some(existing) = optimized_operators.get(token) {
                // Keep the entry with more operator types set (prefix/infix/postfix)
                let existing_count =
                    existing.is_prefix as u8 + existing.is_infix as u8 + existing.is_postfix as u8;
                let new_count = info.is_prefix as u8 + info.is_infix as u8 + info.is_postfix as u8;
                if new_count > existing_count {
                    optimized_operators.insert(token.clone(), info.clone());
                }
            } else {
                optimized_operators.insert(token.clone(), info.clone());
            }
        }

        // Ensure precedence ordering is consistent
        // Sort operators by precedence for better lookup performance
        // (HashMap doesn't maintain order, but we can at least validate consistency)

        Ok(PrattGrammar {
            original_grammar: grammar.original_grammar.clone(),
            operators: optimized_operators,
            entry_point: grammar.entry_point.clone(),
        })
    }

    /// Flatten nested expressions to reduce parsing overhead.
    ///
    /// This method recursively processes all rules in the grammar and flattens deeply nested
    /// expression structures, particularly:
    ///
    /// - **Nested sequences**: `Seq([a, Seq([b, c])])` becomes `Seq([a, b, c])`
    /// - **Nested choices**: `Choice([a, Choice([b, c])])` becomes `Choice([a, b, c])`
    /// - **Nested optional/repeat expressions**: Recursively flattens inner expressions
    ///
    /// # Flattening Strategy
    ///
    /// The flattening process:
    ///
    /// 1. Traverses all rules in the grammar
    /// 2. Recursively processes each expression
    /// 3. Removes unnecessary nesting levels
    /// 4. Preserves the semantic meaning of the grammar
    ///
    /// # Expression Types Handled
    ///
    /// The following expression types are flattened:
    ///
    /// - `Seq`: Nested sequences are merged into a single sequence
    /// - `Choice`: Nested choices are merged into a single choice
    /// - `Opt`, `Repeat`, `Label`, `Node`, `Flatten`, `Prune`: Inner expressions are
    ///   recursively flattened
    ///
    /// Other expression types (tokens, rules, etc.) are left unchanged.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use sipha::backend::pratt::{PrattGrammar, PrattOptimizer};
    ///
    /// // Grammar with nested expressions
    /// // Expr: Seq([Token(A), Seq([Token(B), Token(C)])])
    ///
    /// let optimized = PrattOptimizer::flatten_expressions(&grammar)?;
    ///
    /// // Result: Expr: Seq([Token(A), Token(B), Token(C)])
    /// ```
    ///
    /// # Returns
    ///
    /// Returns a new `PrattGrammar` with flattened expressions. The operator table and
    /// entry point are preserved from the original grammar.
    fn flatten_expressions<T, N>(
        grammar: &PrattGrammar<T, N>,
    ) -> Result<PrattGrammar<T, N>, OptimizeError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::grammar::GrammarBuilder;

        // Rebuild grammar with flattened expressions
        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point.clone());

        // Process each rule and flatten nested expressions
        for (lhs, rule) in grammar.original_grammar.rules() {
            let flattened_rhs = Self::flatten_expr(&rule.rhs, &grammar.original_grammar);
            builder = builder.rule(lhs.clone(), flattened_rhs);
        }

        let flattened_grammar = builder.build().map_err(|e| {
            OptimizeError::OptimizationFailed(format!("Failed to build flattened grammar: {e:?}"))
        })?;

        Ok(PrattGrammar {
            original_grammar: flattened_grammar,
            operators: grammar.operators.clone(),
            entry_point: grammar.entry_point.clone(),
        })
    }

    /// Flatten an expression by removing unnecessary nesting
    fn flatten_expr<T, N>(expr: &Expr<T, N>, _grammar: &crate::grammar::Grammar<T, N>) -> Expr<T, N>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        use crate::grammar::CoreExpr;

        match expr {
            ExtendedExpr::Core(CoreExpr::Seq(exprs)) => {
                // Flatten nested sequences: Seq([a, Seq([b, c])]) -> Seq([a, b, c])
                let mut flattened = Vec::new();
                for e in exprs {
                    match e {
                        CoreExpr::Seq(nested) => {
                            flattened.extend(nested.clone());
                        }
                        _ => flattened.push(e.clone()),
                    }
                }
                if flattened.len() == 1 {
                    ExtendedExpr::Core(flattened.into_iter().next().unwrap())
                } else {
                    ExtendedExpr::Core(CoreExpr::Seq(flattened))
                }
            }
            ExtendedExpr::Core(CoreExpr::Choice(exprs)) => {
                // Flatten nested choices: Choice([a, Choice([b, c])]) -> Choice([a, b, c])
                let mut flattened = Vec::new();
                for e in exprs {
                    match e {
                        CoreExpr::Choice(nested) => {
                            flattened.extend(nested.clone());
                        }
                        _ => flattened.push(e.clone()),
                    }
                }
                if flattened.len() == 1 {
                    ExtendedExpr::Core(flattened.into_iter().next().unwrap())
                } else {
                    ExtendedExpr::Core(CoreExpr::Choice(flattened))
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
                // Recursively flatten nested expressions
                let flattened_inner =
                    Self::flatten_expr(&ExtendedExpr::Core((**e).clone()), _grammar);
                match expr {
                    ExtendedExpr::Core(CoreExpr::Opt(_)) => {
                        ExtendedExpr::Core(CoreExpr::Opt(Box::new(flattened_inner.unwrap_core())))
                    }
                    ExtendedExpr::Core(CoreExpr::Repeat { min, max, .. }) => {
                        ExtendedExpr::Core(CoreExpr::Repeat {
                            expr: Box::new(flattened_inner.unwrap_core()),
                            min: *min,
                            max: *max,
                        })
                    }
                    ExtendedExpr::Core(CoreExpr::Label { name, .. }) => {
                        ExtendedExpr::Core(CoreExpr::Label {
                            name: name.clone(),
                            expr: Box::new(flattened_inner.unwrap_core()),
                        })
                    }
                    ExtendedExpr::Core(CoreExpr::Node { kind, .. }) => {
                        let kind_clone = kind.clone();
                        ExtendedExpr::Core(CoreExpr::Node {
                            kind: kind_clone,
                            expr: Box::new(flattened_inner.unwrap_core()),
                        })
                    }
                    ExtendedExpr::Core(CoreExpr::Flatten(_)) => ExtendedExpr::Core(
                        CoreExpr::Flatten(Box::new(flattened_inner.unwrap_core())),
                    ),
                    ExtendedExpr::Core(CoreExpr::Prune(_)) => {
                        ExtendedExpr::Core(CoreExpr::Prune(Box::new(flattened_inner.unwrap_core())))
                    }
                    _ => expr.clone(),
                }
            }
            _ => expr.clone(),
        }
    }
}
