/// Configuration for LL parser
#[derive(Debug, Clone)]
pub struct LlConfig {
    /// Lookahead depth (k in LL(k))
    pub lookahead: usize,
    
    /// Enable error recovery
    pub error_recovery: bool,
    
    /// Maximum number of errors before giving up
    pub max_errors: usize,
}

impl Default for LlConfig {
    fn default() -> Self {
        Self {
            lookahead: 1,
            error_recovery: true,
            max_errors: 100,
        }
    }
}

