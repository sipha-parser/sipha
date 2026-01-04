//! # Unified Parser Driver Trait
//!
//! This module provides an abstract interface for parser backends,
//! enabling unified parsing infrastructure across all algorithms.
//!
//! ## Overview
//!
//! The `ParserDriver` trait abstracts the decision-making logic of different
//! parsing algorithms (LL, LR, GLR, PEG), while the `ParserEngine` handles
//! common concerns like token consumption and tree building.
//!
//! ## Benefits
//!
//! - Shared error recovery infrastructure
//! - Consistent API across backends
//! - Easier testing and benchmarking
//! - Pluggable parsing strategies

use crate::error::ParseError;
use crate::grammar::{NonTerminal, Token};
use crate::lexer::Token as LexerToken;
use crate::syntax::{GreenNodeBuilder, SyntaxKind};
use std::fmt::Debug;

/// The result of a driver decision
#[derive(Debug, Clone)]
pub enum DriverResult {
    /// Continue parsing
    Continue,
    /// Parsing completed successfully
    Accept,
    /// An error occurred
    Error(ParseError),
    /// Need more lookahead
    NeedLookahead(usize),
}

/// Context provided to the driver for making decisions
#[derive(Debug)]
pub struct ParseContext<'a, K: SyntaxKind> {
    /// Current position in the input
    pub position: usize,
    /// Lookahead tokens
    pub lookahead: &'a [LexerToken<K>],
    /// Current nesting depth
    pub depth: usize,
    /// Whether we're in error recovery mode
    pub recovering: bool,
    /// Expected tokens at current position (for error messages)
    pub expected: Vec<K>,
}

impl<'a, K: SyntaxKind> ParseContext<'a, K> {
    /// Create a new parse context
    #[must_use]
    pub fn new(position: usize, lookahead: &'a [LexerToken<K>]) -> Self {
        Self {
            position,
            lookahead,
            depth: 0,
            recovering: false,
            expected: Vec::new(),
        }
    }

    /// Get the current token (if any)
    #[must_use]
    pub fn current(&self) -> Option<&LexerToken<K>> {
        self.lookahead.first()
    }

    /// Get the current token kind (if any)
    #[must_use]
    pub fn current_kind(&self) -> Option<K> {
        self.current().map(|t| t.kind)
    }

    /// Peek ahead by n tokens
    #[must_use]
    pub fn peek(&self, n: usize) -> Option<&LexerToken<K>> {
        self.lookahead.get(n)
    }

    /// Check if at end of input
    #[must_use]
    pub fn is_at_end(&self) -> bool {
        self.lookahead.is_empty()
    }
}

/// Abstract parser driver trait
///
/// This trait encapsulates the decision-making logic of a parser backend.
/// Different parsing algorithms implement this trait to provide their
/// specific behavior.
pub trait ParserDriver<T: Token, N: NonTerminal>: Send {
    /// The parser's internal state
    type State: Default + Clone + Debug;

    /// A parsing decision
    type Decision: Debug;

    /// Initialize the parser state for parsing a given entry point
    fn initial_state(&self, entry: &N) -> Self::State;

    /// Make a parsing decision based on current state and lookahead
    ///
    /// # Errors
    /// Returns an error if the parser cannot make a decision (syntax error).
    fn decide<K: SyntaxKind>(
        &self,
        state: &Self::State,
        ctx: &ParseContext<'_, K>,
    ) -> Result<Self::Decision, ParseError>;

    /// Execute a parsing decision
    fn execute<K: SyntaxKind>(
        &mut self,
        state: &mut Self::State,
        decision: Self::Decision,
        builder: &mut GreenNodeBuilder<K>,
    ) -> DriverResult;

    /// Check if the current state is an accepting state
    fn is_accepting(&self, state: &Self::State) -> bool;

    /// Get the name of this driver (for debugging/logging)
    fn name(&self) -> &'static str;

    /// Get the lookahead requirement for this driver
    fn lookahead_count(&self) -> usize {
        1
    }
}

/// Marker trait for drivers that support incremental parsing
pub trait IncrementalDriver<T: Token, N: NonTerminal>: ParserDriver<T, N> {
    /// Check if a node at the given position can be reused
    fn can_reuse_node<K: SyntaxKind>(
        &self,
        state: &Self::State,
        node_kind: K,
        node_text_len: u32,
    ) -> bool;

    /// Get a cache key for the current parsing context
    fn cache_key(&self, state: &Self::State) -> u64;
}

/// Marker trait for drivers that support parallel parsing
pub trait ParallelDriver<T: Token, N: NonTerminal>: ParserDriver<T, N> + Sync + Clone {
    /// Clone the driver for use in another thread
    fn clone_for_parallel(&self) -> Self
    where
        Self: Sized,
    {
        self.clone()
    }
}

/// Statistics collected during parsing
#[derive(Debug, Clone, Default)]
pub struct ParseStats {
    /// Number of tokens consumed
    pub tokens_consumed: usize,
    /// Number of nodes created
    pub nodes_created: usize,
    /// Number of decisions made
    pub decisions_made: usize,
    /// Number of backtracks (for PEG/GLR)
    pub backtracks: usize,
    /// Maximum stack depth
    pub max_depth: usize,
    /// Cache hits (for incremental)
    pub cache_hits: usize,
    /// Cache misses
    pub cache_misses: usize,
}

impl ParseStats {
    /// Create new empty stats
    #[must_use]
    pub const fn new() -> Self {
        Self {
            tokens_consumed: 0,
            nodes_created: 0,
            decisions_made: 0,
            backtracks: 0,
            max_depth: 0,
            cache_hits: 0,
            cache_misses: 0,
        }
    }

    /// Merge stats from another instance
    pub fn merge(&mut self, other: &Self) {
        self.tokens_consumed += other.tokens_consumed;
        self.nodes_created += other.nodes_created;
        self.decisions_made += other.decisions_made;
        self.backtracks += other.backtracks;
        self.max_depth = self.max_depth.max(other.max_depth);
        self.cache_hits += other.cache_hits;
        self.cache_misses += other.cache_misses;
    }
}

/// A generic parsing event for debugging/tracing
#[derive(Debug, Clone)]
pub enum ParseEvent<K: SyntaxKind, N: NonTerminal> {
    /// Started parsing a rule
    EnterRule { rule: N, position: usize },
    /// Finished parsing a rule
    ExitRule { rule: N, success: bool },
    /// Consumed a token
    ConsumeToken { kind: K, text: String },
    /// Created a node
    CreateNode { kind: K },
    /// Started error recovery
    StartRecovery { position: usize },
    /// Completed error recovery
    EndRecovery { success: bool },
    /// Backtracked to a position
    Backtrack { from: usize, to: usize },
}

/// Trait for receiving parse events (for debugging/tracing)
pub trait ParseEventHandler<K: SyntaxKind, N: NonTerminal>: Send {
    /// Handle a parse event
    fn handle(&mut self, event: ParseEvent<K, N>);
}

/// A no-op event handler
pub struct NullEventHandler;

impl<K: SyntaxKind, N: NonTerminal> ParseEventHandler<K, N> for NullEventHandler {
    fn handle(&mut self, _event: ParseEvent<K, N>) {}
}

/// Configuration for the parser engine
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Maximum parsing depth
    pub max_depth: usize,
    /// Maximum tokens to consume
    pub max_tokens: usize,
    /// Enable event tracing
    pub trace_events: bool,
    /// Enable statistics collection
    pub collect_stats: bool,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            max_depth: 1024,
            max_tokens: 1_000_000,
            trace_events: false,
            collect_stats: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_driver_result() {
        let result = DriverResult::Continue;
        assert!(matches!(result, DriverResult::Continue));

        let result = DriverResult::Accept;
        assert!(matches!(result, DriverResult::Accept));
    }

    #[test]
    fn test_parse_stats() {
        let mut stats1 = ParseStats::new();
        stats1.tokens_consumed = 10;
        stats1.max_depth = 5;

        let mut stats2 = ParseStats::new();
        stats2.tokens_consumed = 20;
        stats2.max_depth = 8;

        stats1.merge(&stats2);

        assert_eq!(stats1.tokens_consumed, 30);
        assert_eq!(stats1.max_depth, 8);
    }

    #[test]
    fn test_engine_config_default() {
        let config = EngineConfig::default();
        assert_eq!(config.max_depth, 1024);
        assert!(config.collect_stats);
    }
}
