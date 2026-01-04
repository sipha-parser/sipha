//! Configuration for Earley parser

/// Configuration options for the Earley parser
#[derive(Debug, Clone)]
pub struct EarleyConfig {
    /// Enable error recovery
    pub error_recovery: bool,

    /// Maximum parse depth (for cycle detection)
    pub max_depth: Option<usize>,

    /// Enable ambiguity detection
    pub detect_ambiguity: bool,
}

impl Default for EarleyConfig {
    fn default() -> Self {
        Self {
            error_recovery: true,
            max_depth: Some(10000),
            detect_ambiguity: true,
        }
    }
}

impl EarleyConfig {
    /// Create a new configuration with default settings
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable or disable error recovery
    #[must_use]
    pub fn with_error_recovery(mut self, enabled: bool) -> Self {
        self.error_recovery = enabled;
        self
    }

    /// Set maximum parse depth
    #[must_use]
    pub fn with_max_depth(mut self, depth: Option<usize>) -> Self {
        self.max_depth = depth;
        self
    }

    /// Enable or disable ambiguity detection
    #[must_use]
    pub fn with_ambiguity_detection(mut self, enabled: bool) -> Self {
        self.detect_ambiguity = enabled;
        self
    }
}
