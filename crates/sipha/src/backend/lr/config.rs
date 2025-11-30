/// Configuration options for the LR(1) / LALR(1) parser backend.
///
/// This struct allows you to customize the behavior of the LR parser,
/// including table construction method, error recovery, and token insertion.
///
/// # Example
///
/// ```rust
/// use sipha::backend::lr::LrConfig;
///
/// // Use default configuration (LALR(1) with error recovery)
/// let config = LrConfig::default();
///
/// // Or customize it
/// let config = LrConfig {
///     error_recovery: true,
///     max_errors: 50,
///     use_lalr: false,  // Use canonical LR(1) instead
///     enable_token_insertion: true,
///     optimize: false,
///     optimization_level: sipha::grammar::hint::OptimizationLevel::None,
/// };
/// ```
#[derive(Debug, Clone)]
pub struct LrConfig {
    /// Enable error recovery
    pub error_recovery: bool,

    /// Maximum number of errors before giving up
    pub max_errors: usize,

    /// Use LALR(1) instead of canonical LR(1) (smaller tables, same power for most grammars)
    pub use_lalr: bool,

    /// Enable token insertion for error recovery
    ///
    /// When enabled, the parser will try inserting expected tokens before skipping tokens.
    /// This can help recover from missing tokens (e.g., missing semicolons) without
    /// skipping valid user code.
    pub enable_token_insertion: bool,

    /// Enable optimizations during transformation
    pub optimize: bool,

    /// Optimization level for grammar transformation
    pub optimization_level: crate::grammar::hint::OptimizationLevel,
}

impl Default for LrConfig {
    fn default() -> Self {
        Self {
            error_recovery: true,
            max_errors: 100,
            use_lalr: true, // LALR(1) is more practical for most grammars
            enable_token_insertion: true, // Enable token insertion by default
            optimize: false,
            optimization_level: crate::grammar::hint::OptimizationLevel::None,
        }
    }
}
