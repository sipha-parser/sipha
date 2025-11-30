//! Grammar transformation pipeline
//!
//! This module provides a structured pipeline for transforming grammars
//! through normalization, validation, optimization, and backend-specific transformation.

use crate::backend::traits::{
    GrammarOptimizer, GrammarTransformer, OptimizationLevel, TransformConfig, TransformError,
};
use crate::grammar::{Grammar, NonTerminal, Token};

/// Grammar transformation pipeline
///
/// Orchestrates the transformation of a grammar through multiple stages:
/// 1. Normalization - Convert ExtendedExpr to CoreExpr where possible
/// 2. Validation - Check grammar properties
/// 3. Backend-specific transformation - Convert to backend IR
/// 4. Optimization (optional) - Optimize the transformed grammar
///
/// Note: Caching is currently handled at the backend level since different
/// backends produce different grammar types that can't be stored generically.
/// The `config.cache` flag is provided for future use or backend-specific caching.
pub struct GrammarTransformPipeline;

impl GrammarTransformPipeline {
    /// Transform a grammar using the specified transformer
    ///
    /// This applies the transformation pipeline:
    /// 1. Validate the grammar
    /// 2. Transform to backend-specific format
    ///
    /// Note: Optimization is not included in this method. Use `transform_with_optimizer`
    /// if optimization is needed.
    ///
    /// Caching: If `config.cache` is true, the transformed grammar may be cached
    /// for reuse. However, since backend grammars are type-specific, caching is
    /// currently implemented at the backend level rather than in the pipeline.
    pub fn transform<T, N, Transformer>(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
    ) -> Result<Transformer::BackendGrammar, TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
        Transformer: GrammarTransformer<T, N>,
    {
        // Step 1: Validate the grammar (basic validation)
        let rules: Vec<_> = grammar.rules().map(|(_, r)| r.clone()).collect();
        if let Err(err) = crate::grammar::validate_grammar(&rules) {
            return Err(TransformError::ValidationError(err.to_string()));
        }

        // Step 2: Transform to backend-specific format
        // TODO: Add caching here if config.cache is true
        // For now, caching is handled at the backend level since we can't
        // store different backend grammar types generically
        Transformer::transform(grammar, config).map_err(|e| TransformError::TransformationFailed {
            reason: e.to_string(),
        })
    }

    /// Transform and optimize a grammar using the specified transformer and optimizer
    ///
    /// This applies the full transformation pipeline:
    /// 1. Validate the grammar
    /// 2. Transform to backend-specific format
    /// 3. Optimize the backend grammar (if enabled in config)
    #[allow(clippy::needless_pass_by_value)] // Optimizers are typically zero-sized types
    pub fn transform_with_optimizer<T, N, Transformer, Optimizer>(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
        optimizer: Optimizer,
    ) -> Result<Transformer::BackendGrammar, TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
        Transformer: GrammarTransformer<T, N>,
        Optimizer: GrammarOptimizer<T, N, BackendGrammar = Transformer::BackendGrammar>,
    {
        // Step 1: Transform first
        let mut backend_grammar = Self::transform::<T, N, Transformer>(grammar, config)?;

        // Step 2: Optimize if enabled and level is not None
        if config.optimize && config.optimization_level != OptimizationLevel::None {
            backend_grammar = optimizer
                .optimize(&backend_grammar, config.optimization_level)
                .map_err(|e| TransformError::TransformationFailed {
                    reason: format!("Optimization failed: {e}"),
                })?;
        }

        Ok(backend_grammar)
    }

    /// Normalize a grammar by converting ExtendedExpr to CoreExpr where possible
    ///
    /// This step attempts to transform unsupported ExtendedExpr features
    /// into CoreExpr equivalents, or rejects them if transformation is impossible.
    ///
    /// Note: This is a helper function. In practice, transformers handle normalization
    /// internally. This function can be used for standalone normalization if needed.
    pub fn normalize<T, N, Transformer>(
        grammar: &Grammar<T, N>,
    ) -> Result<Grammar<T, N>, TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
        Transformer: GrammarTransformer<T, N>,
    {
        use crate::grammar::{ExtendedExpr, GrammarBuilder};

        let mut builder = GrammarBuilder::new();
        builder = builder.entry_point(grammar.entry_point().clone());

        for (lhs, rule) in grammar.rules() {
            // Check if the expression is supported
            if Transformer::supports_expr(&rule.rhs) {
                // Expression is supported, use it as-is
                builder = builder.rule(lhs.clone(), rule.rhs.clone());
            } else {
                // Try to transform the expression
                match Transformer::transform_expr(&rule.rhs, grammar) {
                    Ok(Some(core_expr)) => {
                        // Transformation successful
                        builder = builder.rule(lhs.clone(), ExtendedExpr::Core(core_expr));
                    }
                    Ok(None) => {
                        // Expression should be rejected
                        return Err(TransformError::UnsupportedFeature {
                            feature: format!("{rule:?}", rule = rule.rhs),
                            backend: "unknown".to_string(),
                        });
                    }
                    Err(e) => {
                        // Transformation failed
                        return Err(e);
                    }
                }
            }
        }

        builder
            .build()
            .map_err(|e| TransformError::TransformationFailed {
                reason: format!("Failed to build normalized grammar: {e:?}"),
            })
    }
}
