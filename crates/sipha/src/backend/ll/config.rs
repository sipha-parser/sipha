/// Configuration options for the LL(k) parser backend.
///
/// This struct allows you to customize the behavior of the LL parser,
/// including lookahead depth, error recovery, and caching settings.
///
/// # Example
///
/// ```rust
/// use sipha::backend::ll::LlConfig;
///
/// // Use default configuration
/// let config = LlConfig::default();
///
/// // Or customize it
/// let config = LlConfig {
///     lookahead: 2,  // Use LL(2) instead of LL(1)
///     error_recovery: true,
///     max_errors: 50,
///     max_cache_size: 5000,
///     cache_history: 3,
/// };
/// ```
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
