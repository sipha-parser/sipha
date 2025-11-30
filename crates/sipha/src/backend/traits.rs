//! Backend trait definitions for grammar transformation, optimization, and error recovery
//!
//! This module defines the core traits that backends must implement:
//! - `GrammarTransformer`: Transform grammars to backend-specific formats
//! - `GrammarOptimizer`: Optimize backend-specific grammars
//! - `ErrorRecoveryStrategy`: Handle error recovery during parsing

use crate::grammar::{CoreExpr, Expr, Grammar, NonTerminal, Token};

/// Configuration for grammar transformation
#[derive(Debug, Clone)]
pub struct TransformConfig {
    /// Enable optimizations during transformation
    pub optimize: bool,
    /// Optimization level
    pub optimization_level: crate::grammar::hint::OptimizationLevel,
    /// Cache transformed grammars
    pub cache: bool,
    /// Backend-specific transformation options
    pub backend_options: std::collections::HashMap<String, String>,
}

impl Default for TransformConfig {
    fn default() -> Self {
        Self {
            optimize: false,
            optimization_level: crate::grammar::hint::OptimizationLevel::None,
            cache: true,
            backend_options: std::collections::HashMap::new(),
        }
    }
}

/// Error that can occur during grammar transformation
#[derive(Debug, Clone)]
pub enum TransformError {
    /// A feature is not supported by this backend
    UnsupportedFeature {
        /// Name of the unsupported feature
        feature: String,
        /// Backend that doesn't support it
        backend: String,
    },
    /// Transformation failed for another reason
    TransformationFailed {
        /// Reason for failure
        reason: String,
    },
    /// Grammar validation error
    ValidationError(String),
}

impl std::fmt::Display for TransformError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedFeature { feature, backend } => {
                write!(
                    f,
                    "Feature '{}' is not supported by {} backend",
                    feature, backend
                )
            }
            Self::TransformationFailed { reason } => {
                write!(f, "Transformation failed: {}", reason)
            }
            Self::ValidationError(msg) => {
                write!(f, "Validation error: {}", msg)
            }
        }
    }
}

impl std::error::Error for TransformError {}

/// Trait for transforming grammars to backend-specific formats
pub trait GrammarTransformer<T, N>: Send + Sync
where
    T: Token,
    N: NonTerminal,
{
    /// The backend-specific grammar representation
    type BackendGrammar: Send + Sync;

    /// The transformation error type
    type Error: std::error::Error + Send + Sync + 'static;

    /// Transform a grammar to backend-specific format
    fn transform(
        grammar: &Grammar<T, N>,
        config: &TransformConfig,
    ) -> Result<Self::BackendGrammar, Self::Error>;

    /// Check if an expression variant is supported natively
    fn supports_expr(expr: &Expr<T, N>) -> bool;

    /// Attempt to transform an unsupported expression to supported form
    ///
    /// Returns `Ok(Some(expr))` if transformation is possible,
    /// `Ok(None)` if the expression should be rejected,
    /// `Err` if transformation failed.
    fn transform_expr(
        expr: &Expr<T, N>,
        grammar: &Grammar<T, N>,
    ) -> Result<Option<CoreExpr<T, N>>, TransformError>;
}

/// Optimization level for grammar optimizations
///
/// Re-exported from grammar::hint for convenience
pub use crate::grammar::hint::OptimizationLevel;

/// Capabilities of a grammar optimizer
#[derive(Debug, Clone)]
pub struct OptimizationCapabilities {
    /// Can inline small rules
    pub can_inline: bool,
    /// Can factor common prefixes
    pub can_factor: bool,
    /// Can eliminate left recursion
    pub can_eliminate_left_recursion: bool,
    /// Can compress tables (for table-based parsers)
    pub can_compress_tables: bool,
    /// Can merge states (for state machine parsers)
    pub can_merge_states: bool,
}

/// Error that can occur during optimization
#[derive(Debug, Clone)]
pub enum OptimizeError {
    /// Optimization failed
    OptimizationFailed(String),
    /// Invalid optimization level
    InvalidLevel,
}

impl std::fmt::Display for OptimizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OptimizationFailed(reason) => {
                write!(f, "Optimization failed: {}", reason)
            }
            Self::InvalidLevel => {
                write!(f, "Invalid optimization level")
            }
        }
    }
}

impl std::error::Error for OptimizeError {}

/// Trait for optimizing backend-specific grammars
pub trait GrammarOptimizer<T, N>: Send + Sync
where
    T: Token,
    N: NonTerminal,
{
    /// The backend-specific grammar type
    type BackendGrammar: Send + Sync;

    /// Optimize a backend-specific grammar
    fn optimize(
        &self,
        grammar: &Self::BackendGrammar,
        level: OptimizationLevel,
    ) -> Result<Self::BackendGrammar, OptimizeError>;

    /// Get optimization capabilities
    fn capabilities(&self) -> OptimizationCapabilities;
}

/// Recovery action to take when an error occurs
pub enum RecoveryAction<T> {
    /// Skip the current token
    SkipToken,
    /// Insert a token
    InsertToken(T),
    /// Skip to a synchronization point
    SkipToSyncPoint {
        /// Tokens that indicate sync points
        tokens: Vec<T>,
    },
    /// Delete tokens until a condition is met
    DeleteUntil {
        /// Condition function (not Debug/Clone due to function pointer)
        #[allow(missing_docs)]
        condition: Box<dyn Fn(&T) -> bool + Send + Sync>,
    },
    /// Insert expected tokens
    InsertExpected {
        /// Expected tokens to insert
        expected: Vec<T>,
    },
    /// Backtrack to try alternative paths (for backtracking parsers)
    Backtrack,
}

impl<T: std::fmt::Debug> std::fmt::Debug for RecoveryAction<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SkipToken => write!(f, "RecoveryAction::SkipToken"),
            Self::InsertToken(token) => write!(f, "RecoveryAction::InsertToken({:?})", token),
            Self::SkipToSyncPoint { tokens } => {
                write!(
                    f,
                    "RecoveryAction::SkipToSyncPoint {{ tokens: {:?} }}",
                    tokens
                )
            }
            Self::DeleteUntil { .. } => {
                write!(f, "RecoveryAction::DeleteUntil {{ condition: <function> }}")
            }
            Self::InsertExpected { expected } => {
                write!(
                    f,
                    "RecoveryAction::InsertExpected {{ expected: {:?} }}",
                    expected
                )
            }
            Self::Backtrack => write!(f, "RecoveryAction::Backtrack"),
        }
    }
}

/// Context for error recovery
pub struct RecoveryContext<'a, T> {
    /// Input tokens
    pub input: &'a [T],
    /// Current position in input
    pub position: usize,
    /// Expected tokens at this position
    pub expected: Vec<T>,
    /// Backend-specific parser state (type-erased)
    pub parser_state: &'a dyn std::any::Any,
}

/// Error indicating recovery failed
#[derive(Debug, Clone)]
pub struct RecoveryFailed {
    /// Reason recovery failed
    pub reason: String,
}

impl std::fmt::Display for RecoveryFailed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Recovery failed: {}", self.reason)
    }
}

impl std::error::Error for RecoveryFailed {}

/// Capabilities of an error recovery strategy
#[derive(Debug, Clone)]
pub struct RecoveryCapabilities {
    /// Can skip tokens
    pub can_skip_tokens: bool,
    /// Can insert tokens
    pub can_insert_tokens: bool,
    /// Can skip to sync points
    pub can_skip_to_sync: bool,
    /// Can delete tokens
    pub can_delete_tokens: bool,
    /// Can insert expected tokens
    pub can_insert_expected: bool,
}

/// Trait for error recovery strategies
pub trait ErrorRecoveryStrategy<T, N>: Send + Sync
where
    T: Token,
    N: NonTerminal,
{
    /// Attempt to recover from an error
    fn recover(
        &self,
        error: &crate::error::ParseError,
        context: &RecoveryContext<T>,
    ) -> Result<RecoveryAction<T>, RecoveryFailed>;

    /// Get recovery capabilities
    fn capabilities(&self) -> RecoveryCapabilities;
}
