/// Configuration for LR parser
#[derive(Debug, Clone)]
pub struct LrConfig {
    /// Enable error recovery
    pub error_recovery: bool,

    /// Maximum number of errors before giving up
    pub max_errors: usize,

    /// Use LALR(1) instead of canonical LR(1) (smaller tables, same power for most grammars)
    pub use_lalr: bool,
}

impl Default for LrConfig {
    fn default() -> Self {
        Self {
            error_recovery: true,
            max_errors: 100,
            use_lalr: true, // LALR(1) is more practical for most grammars
        }
    }
}
