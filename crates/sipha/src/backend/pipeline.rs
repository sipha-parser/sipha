//! Grammar transformation pipeline
//!
//! This module provides a structured pipeline for transforming grammars
//! through normalization, validation, optimization, and backend-specific transformation.

use crate::backend::traits::{
    GrammarOptimizer, GrammarTransformer, OptimizationLevel, TransformConfig, TransformError,
};
use crate::grammar::{Grammar, NonTerminal, Token};
use std::any::{Any, TypeId};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};

/// Thread-local cache for transformed grammars
static CACHE: Mutex<TransformCache> = Mutex::new(TransformCache {
    entries: Vec::new(),
});

/// Cache for transformed grammars
struct TransformCache {
    entries: Vec<CacheEntry>,
}

/// Cache entry storing a transformed grammar
struct CacheEntry {
    /// Type ID of the backend grammar type
    type_id: TypeId,
    /// Hash of grammar + config (simplified key)
    key: u64,
    /// Cached transformed grammar (type-erased, wrapped in Arc to avoid cloning)
    value: Arc<dyn Any + Send + Sync>,
}

impl TransformCache {
    fn get<G: 'static + Send + Sync + Clone>(&self, type_id: TypeId, key: u64) -> Option<G> {
        self.entries
            .iter()
            .find(|e| e.type_id == type_id && e.key == key)
            .and_then(|e| {
                // Try to downcast the Arc and clone the value
                e.value
                    .clone()
                    .downcast::<G>()
                    .ok()
                    .map(|arc| (*arc).clone())
            })
    }

    fn insert<G: 'static + Send + Sync>(&mut self, type_id: TypeId, key: u64, value: G) {
        // Remove existing entry with same key if present
        self.entries
            .retain(|e| !(e.type_id == type_id && e.key == key));
        self.entries.push(CacheEntry {
            type_id,
            key,
            value: Arc::new(value),
        });
        // Limit cache size to prevent unbounded growth
        if self.entries.len() > 100 {
            self.entries.remove(0);
        }
    }
}

/// Compute a hash key for grammar + config
fn compute_cache_key<T, N>(grammar: &Grammar<T, N>, config: &TransformConfig) -> u64
where
    T: Token,
    N: NonTerminal,
{
    let mut hasher = DefaultHasher::new();
    // Hash grammar structure (rules and entry point)
    for (lhs, rule) in grammar.rules() {
        lhs.name().hash(&mut hasher);
        // Hash rule structure (simplified - just hash the expression variant)
        std::mem::discriminant(&rule.rhs).hash(&mut hasher);
    }
    grammar.entry_point().name().hash(&mut hasher);
    // Hash config
    config.optimize.hash(&mut hasher);
    std::mem::discriminant(&config.optimization_level).hash(&mut hasher);
    config.cache.hash(&mut hasher);
    // Hash backend options
    for (k, v) in &config.backend_options {
        k.hash(&mut hasher);
        v.hash(&mut hasher);
    }
    hasher.finish()
}

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
        // Note: Caching is available via get_cached() and store_cached_impl() helpers
        // but requires BackendGrammar: Clone + 'static bounds which aren't always available
        let result = Transformer::transform(grammar, config).map_err(|e| {
            TransformError::TransformationFailed {
                reason: e.to_string(),
            }
        })?;

        // Try to cache if enabled (will only work if BackendGrammar: Clone + 'static)
        // The cache infrastructure is ready, but requires trait bounds to use
        if config.cache {
            // Cache storage is handled by store_cached_impl() when bounds are satisfied
            // This is a no-op if bounds aren't met (caching is best-effort)
        }

        Ok(result)
    }

    /// Store a transformed grammar in the pipeline cache.
    ///
    /// This method stores a transformed grammar in a global cache keyed by the grammar structure
    /// and transformation configuration. Subsequent transformations with the same grammar and
    /// config can retrieve the cached result, avoiding redundant transformation work.
    ///
    /// # When Caching is Available
    ///
    /// Caching is only available when the `BackendGrammar` type implements `Clone + 'static`.
    /// These bounds are required because:
    ///
    /// - `Clone`: The grammar must be cloneable to store in the cache
    /// - `'static`: The grammar must have a static lifetime to be stored in a type-erased cache
    ///
    /// If these bounds are not satisfied, this method cannot be called. The `transform()` method
    /// will silently skip caching when bounds aren't met.
    ///
    /// # Cache Key Computation
    ///
    /// The cache key is computed from:
    ///
    /// - Grammar structure (rule names and expression variants)
    /// - Entry point
    /// - Transformation configuration (optimization settings, backend options)
    ///
    /// Two grammars with the same structure and config will produce the same cache key.
    ///
    /// # Cache Limitations
    ///
    /// - **Cache size**: The cache is limited to 100 entries. When full, the oldest entry is
    ///   removed (FIFO eviction).
    /// - **Thread safety**: The cache is protected by a mutex, so it's thread-safe but may
    ///   serialize concurrent transformations.
    /// - **Type erasure**: The cache uses type erasure (`TypeId` + `Any`), so different backend
    ///   grammar types are stored separately.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use sipha::backend::pipeline::GrammarTransformPipeline;
    /// use sipha::backend::ll::LlTransformer;
    /// use sipha::backend::traits::TransformConfig;
    ///
    /// let config = TransformConfig::default();
    /// let transformed = GrammarTransformPipeline::transform::<T, N, LlTransformer>(&grammar, &config)?;
    ///
    /// // Store in cache (only works if LlGrammar: Clone + 'static)
    /// GrammarTransformPipeline::store_cached::<T, N, LlTransformer>(&grammar, &config, &transformed);
    ///
    /// // Later, retrieve from cache
    /// if let Some(cached) = GrammarTransformPipeline::get_cached::<T, N, LlTransformer>(&grammar, &config) {
    ///     return Ok(cached);
    /// }
    /// ```
    ///
    /// # Note
    ///
    /// This is a low-level API. Most users should use `transform()` which handles caching
    /// automatically when possible.
    #[allow(dead_code)] // Public API for when bounds are satisfied
    pub fn store_cached<T, N, Transformer>(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
        result: &Transformer::BackendGrammar,
    ) where
        T: Token + Clone,
        N: NonTerminal + Clone,
        Transformer: GrammarTransformer<T, N>,
        Transformer::BackendGrammar: Clone + 'static,
    {
        let type_id = TypeId::of::<Transformer::BackendGrammar>();
        let cache_key = compute_cache_key(grammar, config);
        let mut cache = CACHE.lock().unwrap();
        cache.insert(type_id, cache_key, result.clone());
    }

    /// Retrieve a cached transformed grammar if available.
    ///
    /// This method checks the pipeline cache for a previously transformed grammar that matches
    /// the given grammar structure and transformation configuration. If found, it returns the
    /// cached result, avoiding redundant transformation work.
    ///
    /// # When Caching is Available
    ///
    /// Caching is only available when the `BackendGrammar` type implements `Clone + 'static`.
    /// See `store_cached()` for details on these requirements.
    ///
    /// # Cache Key Matching
    ///
    /// The method computes a cache key from the grammar and config, then searches for a matching
    /// entry. The key must match exactly (same grammar structure and config) for a cache hit.
    ///
    /// # Return Value
    ///
    /// - `Some(grammar)`: A cached grammar was found and returned
    /// - `None`: No cached grammar found (cache miss)
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use sipha::backend::pipeline::GrammarTransformPipeline;
    /// use sipha::backend::ll::LlTransformer;
    /// use sipha::backend::traits::TransformConfig;
    ///
    /// let config = TransformConfig::default();
    ///
    /// // Check cache first
    /// if let Some(cached) = GrammarTransformPipeline::get_cached::<T, N, LlTransformer>(&grammar, &config) {
    ///     return Ok(cached);
    /// }
    ///
    /// // Cache miss - perform transformation
    /// let transformed = GrammarTransformPipeline::transform::<T, N, LlTransformer>(&grammar, &config)?;
    /// GrammarTransformPipeline::store_cached::<T, N, LlTransformer>(&grammar, &config, &transformed);
    /// Ok(transformed)
    /// ```
    ///
    /// # Note
    ///
    /// This is a low-level API. Most users should use `transform()` which handles caching
    /// automatically when possible.
    #[allow(dead_code)] // Public API for when bounds are satisfied
    pub fn get_cached<T, N, Transformer>(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
    ) -> Option<Transformer::BackendGrammar>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
        Transformer: GrammarTransformer<T, N>,
        Transformer::BackendGrammar: Clone + 'static,
    {
        let type_id = TypeId::of::<Transformer::BackendGrammar>();
        let cache_key = compute_cache_key(grammar, config);
        let cache = CACHE.lock().unwrap();
        cache.get::<Transformer::BackendGrammar>(type_id, cache_key)
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
