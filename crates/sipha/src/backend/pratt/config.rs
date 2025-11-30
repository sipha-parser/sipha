/// Configuration options for the Pratt parser backend.
///
/// Pratt parsers use recursive descent with operator precedence parsing,
/// making them ideal for parsing expressions with operators. This struct
/// allows you to customize the behavior of the Pratt parser.
///
/// # Example
///
/// ```rust
/// use sipha::backend::pratt::PrattConfig;
///
/// // Use default configuration
/// let config = PrattConfig::default();
///
/// // Or customize it
/// let config = PrattConfig {
///     error_recovery: true,
///     max_errors: 50,
///     optimize: true,
///     optimization_level: crate::grammar::hint::OptimizationLevel::Basic,
/// };
/// ```
#[derive(Debug, Clone)]
pub struct PrattConfig {
    /// Enable error recovery
    pub error_recovery: bool,

    /// Maximum number of errors before giving up
    pub max_errors: usize,

    /// Enable optimizations during transformation
    pub optimize: bool,

    /// Optimization level for grammar transformation
    pub optimization_level: crate::backend::traits::OptimizationLevel,
}

impl Default for PrattConfig {
    fn default() -> Self {
        Self {
            error_recovery: true,
            max_errors: 100,
            optimize: false,
            optimization_level: crate::backend::traits::OptimizationLevel::None,
        }
    }
}
