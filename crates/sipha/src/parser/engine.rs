//! # Parser Engine
//!
//! The parser engine orchestrates parsing by combining a driver with
//! common infrastructure like token streaming and tree building.

use crate::error::ParseError;
use crate::grammar::NonTerminal;
use crate::lexer::Token;
use crate::lexer::stream::{StreamCheckpoint, TokenStream, VecTokenStream};
use crate::syntax::{GreenNode, GreenNodeBuilder, SyntaxKind};
use std::sync::Arc;

use super::driver::{DriverResult, EngineConfig, ParseContext, ParseStats, ParserDriver};
use super::recovery::ErrorRecoveryStrategy;

/// The main parser engine that combines a driver with parsing infrastructure
pub struct ParserEngine<K: SyntaxKind> {
    /// Tree builder
    builder: GreenNodeBuilder<K>,
    /// Parser configuration
    config: EngineConfig,
    /// Parsing statistics
    stats: ParseStats,
    /// Error recovery strategy
    recovery: Option<Box<dyn ErrorRecoveryStrategy<K>>>,
}

impl<K: SyntaxKind> ParserEngine<K> {
    /// Create a new parser engine
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            config: EngineConfig::default(),
            stats: ParseStats::new(),
            recovery: None,
        }
    }

    /// Create a new parser engine with configuration
    #[must_use]
    pub fn with_config(config: EngineConfig) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            config,
            stats: ParseStats::new(),
            recovery: None,
        }
    }

    /// Set the error recovery strategy
    pub fn with_recovery(mut self, recovery: Box<dyn ErrorRecoveryStrategy<K>>) -> Self {
        self.recovery = Some(recovery);
        self
    }

    /// Get the configuration
    #[must_use]
    pub const fn config(&self) -> &EngineConfig {
        &self.config
    }

    /// Get parsing statistics
    #[must_use]
    pub const fn stats(&self) -> &ParseStats {
        &self.stats
    }

    /// Parse tokens using the given driver
    ///
    /// # Errors
    /// Returns errors if parsing fails.
    pub fn parse<D, T, N>(
        &mut self,
        driver: &mut D,
        tokens: Vec<Token<K>>,
        entry: N,
    ) -> ParseResult<K>
    where
        D: ParserDriver<T, N>,
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        let mut stream = VecTokenStream::new(tokens);
        self.parse_stream(driver, &mut stream, entry)
    }

    /// Parse from a token stream
    ///
    /// # Errors
    /// Returns errors if parsing fails.
    pub fn parse_stream<D, T, N, S>(
        &mut self,
        driver: &mut D,
        stream: &mut S,
        entry: N,
    ) -> ParseResult<K>
    where
        D: ParserDriver<T, N>,
        T: crate::grammar::Token,
        N: NonTerminal,
        S: TokenStream<K>,
    {
        self.stats = ParseStats::new();
        self.builder = GreenNodeBuilder::new();

        let mut errors = Vec::new();
        let mut state = driver.initial_state(&entry);
        let mut depth = 0;

        // Main parsing loop
        loop {
            // Build lookahead
            let lookahead = self.build_lookahead(stream, driver.lookahead_count());
            let ctx = ParseContext {
                position: stream.token_index(),
                lookahead: &lookahead,
                depth,
                recovering: false,
                expected: Vec::new(),
            };

            // Make a decision
            match driver.decide(&state, &ctx) {
                Ok(decision) => {
                    if self.config.collect_stats {
                        self.stats.decisions_made += 1;
                    }

                    // Execute the decision
                    match driver.execute(&mut state, decision, &mut self.builder) {
                        DriverResult::Continue => {
                            // Consume token if needed
                            if let Some(token) = stream.advance() {
                                if self.config.collect_stats {
                                    self.stats.tokens_consumed += 1;
                                }
                                let _ = self.builder.token(token.kind, token.text.as_str());
                            }
                        }
                        DriverResult::Accept => {
                            break;
                        }
                        DriverResult::Error(err) => {
                            errors.push(err);
                            if !self.try_recover(stream, &mut state, driver) {
                                break;
                            }
                        }
                        DriverResult::NeedLookahead(n) => {
                            // Driver needs more lookahead - this shouldn't happen
                            // if lookahead_count() is correct
                            let _ = n;
                        }
                    }
                }
                Err(err) => {
                    errors.push(err);
                    if !self.try_recover(stream, &mut state, driver) {
                        break;
                    }
                }
            }

            // Update depth tracking
            depth = depth.max(self.builder.current_depth());
            if self.config.collect_stats {
                self.stats.max_depth = self.stats.max_depth.max(depth);
            }

            // Check limits
            if depth > self.config.max_depth {
                errors.push(ParseError::depth_exceeded(self.config.max_depth));
                break;
            }

            if self.stats.tokens_consumed > self.config.max_tokens {
                errors.push(ParseError::token_limit_exceeded(self.config.max_tokens));
                break;
            }
        }

        // Build the final tree
        if self.config.collect_stats {
            self.stats.nodes_created = self.builder.node_count();
        }
        let root = std::mem::take(&mut self.builder).finish_unwrap();

        ParseResult {
            root: Some(root),
            errors,
            stats: self.stats.clone(),
        }
    }

    /// Build lookahead array
    #[allow(clippy::unused_self)]
    fn build_lookahead<S: TokenStream<K>>(
        &mut self,
        stream: &mut S,
        count: usize,
    ) -> Vec<Token<K>> {
        let mut lookahead = Vec::with_capacity(count);
        for i in 0..count {
            if let Some(token) = stream.peek(i) {
                lookahead.push(token.clone());
            } else {
                break;
            }
        }
        lookahead
    }

    /// Try to recover from an error
    fn try_recover<D, T, N, S>(
        &mut self,
        stream: &mut S,
        _state: &mut D::State,
        _driver: &D,
    ) -> bool
    where
        D: ParserDriver<T, N>,
        T: crate::grammar::Token,
        N: NonTerminal,
        S: TokenStream<K>,
    {
        if let Some(recovery) = &self.recovery {
            let ctx = super::recovery::RecoveryContext::new(stream.token_index());

            match recovery.recover(&ctx) {
                super::recovery::RecoveryAction::Skip { count } => {
                    for _ in 0..count {
                        stream.advance();
                    }
                    true
                }
                super::recovery::RecoveryAction::Insert { .. } => {
                    // Would need to insert synthetic tokens
                    true
                }
                super::recovery::RecoveryAction::SkipTo { sync_kinds } => {
                    stream.skip_while(|t| !sync_kinds.contains(&t.kind));
                    true
                }
                super::recovery::RecoveryAction::Replace { .. } => {
                    // Would need to replace the current token
                    stream.advance();
                    true
                }
                super::recovery::RecoveryAction::Fail => false,
            }
        } else {
            // No recovery strategy - skip one token and continue
            stream.advance();
            true
        }
    }
}

impl<K: SyntaxKind> Default for ParserEngine<K> {
    fn default() -> Self {
        Self::new()
    }
}

/// Recovery context for error recovery
pub struct RecoveryContext {
    /// Current position
    pub position: usize,
    /// Checkpoint for backtracking
    pub checkpoint: StreamCheckpoint,
}

/// Result of parsing
#[derive(Debug)]
pub struct ParseResult<K: SyntaxKind> {
    /// The root of the syntax tree
    pub root: Option<Arc<GreenNode<K>>>,
    /// Errors encountered during parsing
    pub errors: Vec<ParseError>,
    /// Parsing statistics
    pub stats: ParseStats,
}

impl<K: SyntaxKind> ParseResult<K> {
    /// Check if parsing succeeded without errors
    #[must_use]
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty() && self.root.is_some()
    }

    /// Check if parsing failed
    #[must_use]
    pub fn is_err(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the root node (if successful)
    #[must_use]
    pub fn root(&self) -> Option<&Arc<GreenNode<K>>> {
        self.root.as_ref()
    }

    /// Take the root node
    #[must_use]
    pub fn take_root(self) -> Option<Arc<GreenNode<K>>> {
        self.root
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestKind {
        Root,
        Expr,
        Ident,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            matches!(self, Self::Ident)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    #[test]
    fn test_engine_creation() {
        let engine: ParserEngine<TestKind> = ParserEngine::new();
        assert_eq!(engine.config().max_depth, 1024);
    }

    #[test]
    fn test_engine_with_config() {
        let config = EngineConfig {
            max_depth: 512,
            ..Default::default()
        };
        let engine: ParserEngine<TestKind> = ParserEngine::with_config(config);
        assert_eq!(engine.config().max_depth, 512);
    }
}
