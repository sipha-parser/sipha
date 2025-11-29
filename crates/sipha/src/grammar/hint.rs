use std::any::Any;

/// Trait for backend-specific hints
pub trait BackendHint: std::fmt::Debug + Send + Sync {
    fn as_any(&self) -> &dyn Any;
    fn description(&self) -> String;
}

/// Precedence hint for operator precedence
#[derive(Debug, Clone)]
pub struct PrecedenceHint {
    pub precedence: u32,
    pub associativity: Associativity,
}

impl BackendHint for PrecedenceHint {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!(
            "Precedence: {}, Associativity: {:?}",
            self.precedence, self.associativity
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

/// Strategy for handling left recursion
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LeftRecursionStrategy {
    /// Transform left-recursive rules to right-recursive
    Eliminate,
    /// Use packrat parsing to handle left recursion
    Packrat,
    /// Use operator precedence parsing
    OperatorPrecedence,
}

/// Hint for left recursion handling
#[derive(Debug, Clone)]
pub struct LeftRecursionHint {
    pub strategy: LeftRecursionStrategy,
}

impl BackendHint for LeftRecursionHint {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!("Left recursion strategy: {:?}", self.strategy)
    }
}

/// Scope for memoization
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoizationScope {
    /// Memoize at the rule level
    Rule,
    /// Memoize at the expression level
    Expression,
    /// Memoize globally (all expressions)
    Global,
}

/// Hint for memoization control
#[derive(Debug, Clone)]
pub struct MemoizationHint {
    pub enable: bool,
    pub scope: MemoizationScope,
}

impl BackendHint for MemoizationHint {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!(
            "Memoization: {}, Scope: {:?}",
            if self.enable { "enabled" } else { "disabled" },
            self.scope
        )
    }
}

/// Strategy for backtracking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BacktrackingStrategy {
    /// Full backtracking (try all alternatives)
    Full,
    /// Limited backtracking (with depth limit)
    Limited,
    /// No backtracking (fail fast)
    None,
}

/// Hint for backtracking control
#[derive(Debug, Clone)]
pub struct BacktrackingHint {
    pub max_depth: Option<usize>,
    pub strategy: BacktrackingStrategy,
}

impl BackendHint for BacktrackingHint {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!(
            "Backtracking: {:?}, Max depth: {:?}",
            self.strategy, self.max_depth
        )
    }
}

/// Strategy for ambiguity resolution
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AmbiguityStrategy {
    /// Choose first parse
    First,
    /// Choose longest parse
    Longest,
    /// Choose shortest parse
    Shortest,
    /// Prefer parse with higher priority
    Prefer,
}

/// Hint for ambiguity resolution (primarily for GLR parser)
#[derive(Debug, Clone)]
pub struct AmbiguityHint {
    pub strategy: AmbiguityStrategy,
    pub priority: u32,
}

impl BackendHint for AmbiguityHint {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!(
            "Ambiguity resolution: {:?}, Priority: {}",
            self.strategy, self.priority
        )
    }
}

/// Strategy for error recovery
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecoveryStrategy {
    /// Panic on error (fail immediately)
    Panic,
    /// Skip tokens until sync point
    Skip,
    /// Insert expected tokens
    Insert,
    /// Delete unexpected tokens
    Delete,
}

/// Hint for error recovery behavior
#[derive(Debug, Clone)]
pub struct ErrorRecoveryHint<T>
where
    T: crate::grammar::Token,
{
    pub strategy: RecoveryStrategy,
    pub sync_tokens: Vec<T>,
    pub max_errors: Option<usize>,
}

impl<T: crate::grammar::Token> BackendHint for ErrorRecoveryHint<T> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!(
            "Error recovery: {:?}, Sync tokens: {}, Max errors: {:?}",
            self.strategy,
            self.sync_tokens.len(),
            self.max_errors
        )
    }
}

/// Optimization level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    /// No optimization
    None,
    /// Basic optimizations
    Basic,
    /// Aggressive optimizations
    Aggressive,
}

/// Hint for performance optimization
#[derive(Debug, Clone)]
pub struct PerformanceHint {
    pub inline: bool,
    pub cache: bool,
    pub optimize: OptimizationLevel,
}

impl BackendHint for PerformanceHint {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn description(&self) -> String {
        format!(
            "Performance: inline={}, cache={}, optimize={:?}",
            self.inline, self.cache, self.optimize
        )
    }
}
