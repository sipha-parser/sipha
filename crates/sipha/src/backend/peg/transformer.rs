//! PEG grammar transformer
//!
//! Transforms ExtendedExpr grammars into PegGrammar format suitable for PEG parsing.

use crate::backend::peg::grammar::PegGrammar;
use crate::backend::traits::{GrammarTransformer, TransformConfig, TransformError};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};

/// PEG grammar transformer
pub struct PegTransformer;

impl<T, N> GrammarTransformer<T, N> for PegTransformer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = PegGrammar<T, N>;
    type Error = TransformError;

    fn transform(
        grammar: &Grammar<T, N>,
        _config: &TransformConfig,
    ) -> Result<Self::BackendGrammar, Self::Error> {
        // PEG can handle most ExtendedExpr features natively
        // We don't need to normalize as much as LR/LL
        // Just wrap the grammar in PegGrammar
        Ok(PegGrammar::new(grammar.clone()))
    }

    fn supports_expr(expr: &Expr<T, N>) -> bool {
        match expr {
            ExtendedExpr::Core(_) => true,
            ExtendedExpr::Lookahead(_) | ExtendedExpr::NotLookahead(_) => true, // PEG supports lookahead
            ExtendedExpr::TokenClass { .. } => true, // PEG can handle token classes
            ExtendedExpr::Conditional { .. } => true, // PEG supports conditionals
            ExtendedExpr::SemanticPredicate { .. } => true, // PEG supports semantic predicates
            ExtendedExpr::Backreference { .. } => false, // Backreferences are complex
            ExtendedExpr::RecoveryPoint { .. } => true, // Can be handled during parsing
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => true, // PEG supports cut operator
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator { .. } => false, // Not a PEG feature
        }
    }

    fn transform_expr(
        expr: &Expr<T, N>,
        _grammar: &Grammar<T, N>,
    ) -> Result<Option<CoreExpr<T, N>>, TransformError> {
        match expr {
            ExtendedExpr::Core(core) => Ok(Some(core.clone())),
            ExtendedExpr::Lookahead(_) | ExtendedExpr::NotLookahead(_) => {
                // PEG supports lookahead natively, no transformation needed
                // But we can't represent it in CoreExpr, so return None
                // The parser will handle it directly
                Ok(None)
            }
            ExtendedExpr::TokenClass { .. } => {
                // PEG supports token classes natively
                Ok(None)
            }
            ExtendedExpr::Conditional { .. } => {
                // PEG supports conditionals natively
                Ok(None)
            }
            ExtendedExpr::SemanticPredicate { .. } => {
                // PEG supports semantic predicates natively
                Ok(None)
            }
            ExtendedExpr::Backreference { .. } => Err(TransformError::UnsupportedFeature {
                feature: "backreference".to_string(),
                backend: "PEG".to_string(),
            }),
            ExtendedExpr::RecoveryPoint { expr, .. } => {
                // Strip recovery point wrapper, keep inner expression
                Self::transform_expr(expr, _grammar)
            }
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => {
                // PEG supports cut natively
                Ok(None)
            }
            #[cfg(feature = "backend-pratt")]
            ExtendedExpr::PrattOperator { .. } => Err(TransformError::UnsupportedFeature {
                feature: "pratt_operator".to_string(),
                backend: "PEG".to_string(),
            }),
        }
    }
}
