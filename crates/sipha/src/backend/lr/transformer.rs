//! LR grammar transformer
//!
//! Transforms `ExtendedExpr` grammars into `LrGrammar` format suitable for LR parsing.

use crate::backend::lr::grammar::LrGrammar;
use crate::backend::lr::table::LrParsingTable;
use crate::backend::traits::{GrammarTransformer, TransformConfig, TransformError};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};

/// LR grammar transformer
pub struct LrTransformer;

impl<T, N> GrammarTransformer<T, N> for LrTransformer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = LrGrammar<T, N>;
    type Error = TransformError;

    fn transform(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
    ) -> Result<Self::BackendGrammar, Self::Error> {
        // Convert ExtendedExpr to CoreExpr by transforming unsupported features
        let normalized_grammar = Self::normalize_grammar(grammar)?;

        // Build LR parsing table
        let use_lalr = config
            .backend_options
            .get("use_lalr")
            .map_or(true, |s| s == "true");

        let table = LrParsingTable::new(&normalized_grammar, use_lalr).map_err(|e| {
            TransformError::TransformationFailed {
                reason: format!("Failed to build LR table: {e}"),
            }
        })?;

        // Compute FIRST and FOLLOW sets
        let follow_sets_raw = normalized_grammar.compute_follow_sets();

        // Convert FOLLOW sets to the expected type
        let follow_sets: hashbrown::HashMap<N, hashbrown::HashSet<T>> = follow_sets_raw
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();

        // Compute FIRST sets manually (Grammar doesn't have compute_first_sets)
        let mut first_sets_map: hashbrown::HashMap<N, hashbrown::HashSet<T>> =
            hashbrown::HashMap::new();
        for (nt, _rule) in normalized_grammar.rules() {
            // Compute FIRST set for this non-terminal
            let first_set = normalized_grammar
                .get_rule(nt)
                .map(|r| r.rhs.first_set(&normalized_grammar))
                .unwrap_or_default();
            first_sets_map.insert(nt.clone(), first_set.into_iter().collect());
        }

        let entry_point = normalized_grammar.entry_point().clone();

        Ok(LrGrammar::new(
            table,
            first_sets_map,
            follow_sets,
            entry_point,
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
            ExtendedExpr::RecoveryPoint { .. } => true, // Can be handled during parsing
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => false,
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator { .. } => false,
        }
    }

    fn transform_expr(
        expr: &Expr<T, N>,
        grammar: &Grammar<T, N>,
    ) -> Result<Option<CoreExpr<T, N>>, TransformError> {
        match expr {
            ExtendedExpr::Core(core) => Ok(Some(core.clone())),
            ExtendedExpr::Lookahead(_) | ExtendedExpr::NotLookahead(_) => {
                Err(TransformError::UnsupportedFeature {
                    feature: "lookahead".to_string(),
                    backend: "LR".to_string(),
                })
            }
            ExtendedExpr::TokenClass { .. } => Err(TransformError::UnsupportedFeature {
                feature: "token_class".to_string(),
                backend: "LR".to_string(),
            }),
            ExtendedExpr::Conditional { .. } => Err(TransformError::UnsupportedFeature {
                feature: "conditional".to_string(),
                backend: "LR".to_string(),
            }),
            ExtendedExpr::SemanticPredicate { .. } => Err(TransformError::UnsupportedFeature {
                feature: "semantic_predicate".to_string(),
                backend: "LR".to_string(),
            }),
            ExtendedExpr::Backreference { .. } => Err(TransformError::UnsupportedFeature {
                feature: "backreference".to_string(),
                backend: "LR".to_string(),
            }),
            ExtendedExpr::RecoveryPoint { expr, .. } => {
                // Strip recovery point wrapper, keep inner expression
                Self::transform_expr(expr, grammar)
            }
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => Err(TransformError::UnsupportedFeature {
                feature: "cut".to_string(),
                backend: "LR".to_string(),
            }),
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator { .. } => Err(TransformError::UnsupportedFeature {
                feature: "pratt_operator".to_string(),
                backend: "LR".to_string(),
            }),
        }
    }
}

impl LrTransformer {
    /// Normalize grammar by converting `ExtendedExpr` to `CoreExpr`
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
                reason: format!("Failed to build normalized grammar: {e:?}"),
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
                        reason: format!("Cannot transform expression: {expr:?}"),
                    }
                })
            }
        }
    }
}
