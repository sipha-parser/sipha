/// Configuration options for the PEG (Parsing Expression Grammar) parser backend.
///
/// PEG parsers use ordered choice (first match wins) and backtracking with
/// memoization (packrat parsing) for efficient parsing. This struct allows you
/// to customize the behavior of the PEG parser.
///
/// # Example
///
/// ```rust
/// use sipha::backend::peg::PegConfig;
///
/// // Use default configuration
/// let config = PegConfig::default();
///
/// // Or customize it
/// let config = PegConfig {
///     enable_memoization: true,  // Enable packrat parsing
///     max_memo_size: 10000,      // Maximum memoization cache size
///     error_recovery: true,
///     max_errors: 50,
///     max_backtrack_depth: 1000,  // Limit backtracking depth
/// };
/// ```
#[derive(Debug, Clone)]
pub struct PegConfig {
    /// Enable memoization (packrat parsing) for O(n) performance
    ///
    /// When enabled, the parser caches parse results at each position for each
    /// non-terminal, allowing linear-time parsing for many grammars.
    pub enable_memoization: bool,

    /// Maximum number of memoization entries before eviction
    pub max_memo_size: usize,

    /// Enable error recovery
    pub error_recovery: bool,

    /// Maximum number of errors before giving up
    pub max_errors: usize,

    /// Maximum depth for backtracking (prevents infinite loops)
    pub max_backtrack_depth: usize,
}

impl Default for PegConfig {
    fn default() -> Self {
        Self {
            enable_memoization: true,
            max_memo_size: 5000,
            error_recovery: true,
            max_errors: 100,
            max_backtrack_depth: 1000,
        }
    }
}
