/// Configuration for LL parser
#[derive(Debug, Clone)]
pub struct LlConfig {
    /// Lookahead depth (k in LL(k))
    pub lookahead: usize,

    /// Enable error recovery
    pub error_recovery: bool,

    /// Maximum number of errors before giving up
    pub max_errors: usize,

    /// Maximum number of cache entries before eviction
    pub max_cache_size: usize,

    /// Number of cache versions to keep in history
    pub cache_history: usize,
}

impl Default for LlConfig {
    fn default() -> Self {
        Self {
            lookahead: 1,
            error_recovery: true,
            max_errors: 100,
            max_cache_size: 2000,
            cache_history: 2,
        }
    }
}
