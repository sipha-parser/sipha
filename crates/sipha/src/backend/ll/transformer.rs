//! LL grammar transformer
//!
//! Transforms ExtendedExpr grammars into LlGrammar format suitable for LL parsing.

use crate::backend::ll::grammar::LlGrammar;
use crate::backend::traits::{GrammarTransformer, TransformConfig, TransformError};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};
use hashbrown::HashMap;

/// LL grammar transformer
pub struct LlTransformer;

impl<T, N> GrammarTransformer<T, N> for LlTransformer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = LlGrammar<T, N>;
    type Error = TransformError;

    fn transform(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
    ) -> Result<Self::BackendGrammar, Self::Error> {
        // Convert ExtendedExpr to CoreExpr by transforming unsupported features
        let normalized_grammar = Self::normalize_grammar(grammar)?;

        // Build LL parsing table using ParsingTable::new
        use crate::backend::ll::table::ParsingTable;
        let lookahead_k = config
            .backend_options
            .get("lookahead_k")
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1);

        let table = ParsingTable::new(&normalized_grammar, lookahead_k).map_err(|e| {
            TransformError::TransformationFailed {
                reason: format!("Failed to build LL table: {}", e),
            }
        })?;

        // The ParsingTable already contains FIRST and FOLLOW sets
        // Extract them from the table for caching
        let entry_point = normalized_grammar.entry_point().clone();

        // Extract FIRST and FOLLOW sets from the table
        let mut first_sets = hashbrown::HashMap::with_hasher(ahash::RandomState::new());
        let mut follow_sets = hashbrown::HashMap::with_hasher(ahash::RandomState::new());

        // Populate from table's internal sets (we'll access via table methods in parser)
        // For now, create empty sets - they're accessed via table methods anyway
        for (nt, _) in normalized_grammar.rules() {
            if let Some(first) = table.first_set(nt) {
                first_sets.insert(nt.clone(), first.clone());
            }
            if let Some(follow) = table.follow_set(nt) {
                follow_sets.insert(nt.clone(), follow.clone());
            }
        }

        Ok(LlGrammar::new(
            normalized_grammar,
            table,
            first_sets,
            follow_sets,
            entry_point,
            lookahead_k,
        ))
    }

    fn supports_expr(expr: &Expr<T, N>) -> bool {
        match expr {
            ExtendedExpr::Core(_) => true,
            ExtendedExpr::Lookahead(_) | ExtendedExpr::NotLookahead(_) => false,
            ExtendedExpr::TokenClass { .. } => false,
            ExtendedExpr::Conditional { .. } => false,
            ExtendedExpr::SemanticPredicate { .. } => false,
            ExtendedExpr::Backreference { .. } => false,
            ExtendedExpr::RecoveryPoint { .. } => true,
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => false,
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator { .. } => false,
        }
    }

    fn transform_expr(
        expr: &Expr<T, N>,
        _grammar: &Grammar<T, N>,
    ) -> Result<Option<CoreExpr<T, N>>, TransformError> {
        match expr {
            ExtendedExpr::Core(core) => Ok(Some(core.clone())),
            ExtendedExpr::Lookahead(_) | ExtendedExpr::NotLookahead(_) => {
                Err(TransformError::UnsupportedFeature {
                    feature: "lookahead".to_string(),
                    backend: "LL".to_string(),
                })
            }
            ExtendedExpr::TokenClass { .. } => Err(TransformError::UnsupportedFeature {
                feature: "token_class".to_string(),
                backend: "LL".to_string(),
            }),
            ExtendedExpr::Conditional { .. } => Err(TransformError::UnsupportedFeature {
                feature: "conditional".to_string(),
                backend: "LL".to_string(),
            }),
            ExtendedExpr::SemanticPredicate { .. } => Err(TransformError::UnsupportedFeature {
                feature: "semantic_predicate".to_string(),
                backend: "LL".to_string(),
            }),
            ExtendedExpr::Backreference { .. } => Err(TransformError::UnsupportedFeature {
                feature: "backreference".to_string(),
                backend: "LL".to_string(),
            }),
            ExtendedExpr::RecoveryPoint { expr, .. } => {
                // Strip recovery point wrapper
                Self::transform_expr(expr, _grammar)
            }
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => Err(TransformError::UnsupportedFeature {
                feature: "cut".to_string(),
                backend: "LL".to_string(),
            }),
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator { .. } => Err(TransformError::UnsupportedFeature {
                feature: "pratt_operator".to_string(),
                backend: "LL".to_string(),
            }),
        }
    }
}

impl LlTransformer {
    /// Build LL parsing table
    #[allow(dead_code)] // May be used in future optimizations
    fn build_ll_table<T, N>(
        grammar: &Grammar<T, N>,
    ) -> Result<HashMap<(N, T), usize>, TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        let mut table = HashMap::new();
        let mut production_index = 0;

        for (lhs, rule) in grammar.rules() {
            // For LL parsing, we need to determine which production to use
            // based on the lookahead token. This is a simplified implementation.
            // In a full implementation, we would:
            // 1. Compute FIRST sets for each alternative
            // 2. Build the LL table based on FIRST sets
            // 3. Handle conflicts and left recursion

            // For now, just create entries for the first token of each production
            // This is a placeholder - full implementation would be more complex
            let first_set = rule.rhs.first_set(grammar);
            for token in first_set {
                table.insert((lhs.clone(), token), production_index);
            }
            production_index += 1;
        }

        Ok(table)
    }

    /// Normalize grammar by converting ExtendedExpr to CoreExpr
    fn normalize_grammar<T, N>(grammar: &Grammar<T, N>) -> Result<Grammar<T, N>, TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        // Build grammar from normalized rules using GrammarBuilder
        let mut builder = crate::grammar::GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point().clone());

        for (lhs, rule) in grammar.rules() {
            let normalized_rhs = Self::normalize_expr(&rule.rhs, grammar)?;
            builder = builder.rule(lhs.clone(), ExtendedExpr::Core(normalized_rhs));
        }

        builder
            .build()
            .map_err(|e| TransformError::TransformationFailed {
                reason: format!("Failed to build normalized grammar: {:?}", e),
            })
    }

    /// Normalize an expression by transforming unsupported features
    fn normalize_expr<T, N>(
        expr: &Expr<T, N>,
        grammar: &Grammar<T, N>,
    ) -> Result<CoreExpr<T, N>, TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        match expr {
            ExtendedExpr::Core(core) => Ok(core.clone()),
            ExtendedExpr::RecoveryPoint { expr, .. } => {
                // Strip recovery point, keep inner expression
                Self::normalize_expr(expr, grammar)
            }
            _ => {
                // Try to transform
                Self::transform_expr(expr, grammar)?.ok_or_else(|| {
                    TransformError::TransformationFailed {
                        reason: format!("Cannot transform expression: {:?}", expr),
                    }
                })
            }
        }
    }
}
