//! GLR grammar transformer
//!
//! Transforms `ExtendedExpr` grammars into `GlrGrammar` format suitable for GLR parsing.
//! Since GLR extends LR, we can reuse the LR transformer.

use crate::backend::glr::grammar::GlrGrammar;
#[cfg(feature = "backend-lr")]
use crate::backend::lr::LrTransformer;
use crate::backend::traits::{GrammarTransformer, TransformConfig, TransformError};
use crate::grammar::{CoreExpr, Expr, Grammar, NonTerminal, Token};

/// GLR grammar transformer
pub struct GlrTransformer;

impl<T, N> GrammarTransformer<T, N> for GlrTransformer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = GlrGrammar<T, N>;
    type Error = TransformError;

    fn transform(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
    ) -> Result<Self::BackendGrammar, Self::Error> {
        #[cfg(feature = "backend-lr")]
        {
            // GLR reuses the LR table, so we can use the LR transformer
            let lr_grammar = LrTransformer::transform(grammar, config)?;

            Ok(GlrGrammar::new(
                grammar.clone(),
                lr_grammar.table,
                lr_grammar.entry_point,
            ))
        }
        #[cfg(not(feature = "backend-lr"))]
        {
            Err(TransformError::TransformationFailed {
                reason: "GLR backend requires backend-lr feature".to_string(),
            })
        }
    }

    fn supports_expr(expr: &Expr<T, N>) -> bool {
        // GLR supports the same expressions as LR
        #[cfg(feature = "backend-lr")]
        {
            LrTransformer::supports_expr(expr)
        }
        #[cfg(not(feature = "backend-lr"))]
        {
            false
        }
    }

    fn transform_expr(
        expr: &Expr<T, N>,
        grammar: &Grammar<T, N>,
    ) -> Result<Option<CoreExpr<T, N>>, TransformError> {
        // GLR uses the same transformations as LR
        #[cfg(feature = "backend-lr")]
        {
            LrTransformer::transform_expr(expr, grammar)
        }
        #[cfg(not(feature = "backend-lr"))]
        {
            Err(TransformError::TransformationFailed {
                reason: "GLR backend requires backend-lr feature".to_string(),
            })
        }
    }
}
