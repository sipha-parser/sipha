//! # Error Recovery Framework
//!
//! This module provides pluggable error recovery strategies for parsing.
//!
//! ## Overview
//!
//! Error recovery allows parsing to continue after encountering syntax errors,
//! producing a partial syntax tree and collecting multiple errors in a single pass.
//!
//! ## Strategies
//!
//! - **Panic Mode**: Skip tokens until a synchronization point is found
//! - **Phrase Recovery**: Try to complete the current phrase
//! - **Minimal Cost**: Find the smallest edit to make input valid

use crate::lexer::Token;
use crate::syntax::SyntaxKind;
use smallvec::SmallVec;

/// Context provided for error recovery
#[derive(Debug)]
pub struct RecoveryContext<K: SyntaxKind> {
    /// Current token (if any)
    pub current_token: Option<Token<K>>,
    /// Expected token kinds
    pub expected: SmallVec<[K; 8]>,
    /// Current position in input
    pub position: usize,
    /// Nesting depth
    pub depth: usize,
    /// Previous tokens for context
    pub previous_tokens: SmallVec<[Token<K>; 4]>,
}

impl<K: SyntaxKind> RecoveryContext<K> {
    /// Create a new recovery context
    #[must_use]
    pub fn new(position: usize) -> Self {
        Self {
            current_token: None,
            expected: SmallVec::new(),
            position,
            depth: 0,
            previous_tokens: SmallVec::new(),
        }
    }

    /// Create a recovery context with full information
    #[must_use]
    pub fn with_details(
        position: usize,
        current_token: Option<Token<K>>,
        expected: impl IntoIterator<Item = K>,
        depth: usize,
        previous_tokens: impl IntoIterator<Item = Token<K>>,
    ) -> Self {
        Self {
            current_token,
            expected: expected.into_iter().collect(),
            position,
            depth,
            previous_tokens: previous_tokens.into_iter().collect(),
        }
    }

    /// Check if any of the expected tokens is trivia
    #[must_use]
    pub fn expects_trivia(&self) -> bool {
        self.expected.iter().any(|k| k.is_trivia())
    }

    /// Get the current token kind
    #[must_use]
    pub fn current_kind(&self) -> Option<K> {
        self.current_token.as_ref().map(|t| t.kind)
    }

    /// Get the current token text (for diagnostics)
    #[must_use]
    pub fn current_text(&self) -> Option<&str> {
        self.current_token.as_ref().map(|t| t.text.as_str())
    }

    /// Check if we're in a nested context (depth > 0)
    #[must_use]
    pub fn is_nested(&self) -> bool {
        self.depth > 0
    }

    /// Get the number of expected tokens
    #[must_use]
    pub fn expected_count(&self) -> usize {
        self.expected.len()
    }

    /// Get recent context tokens (last N tokens)
    #[must_use]
    pub fn recent_tokens(&self, count: usize) -> SmallVec<[Token<K>; 4]> {
        let start = self.previous_tokens.len().saturating_sub(count);
        self.previous_tokens[start..].iter().cloned().collect()
    }
}

/// Action to take during error recovery
#[derive(Debug, Clone)]
pub enum RecoveryAction<K: SyntaxKind> {
    /// Skip tokens until we find a synchronization point
    SkipTo { sync_kinds: SmallVec<[K; 8]> },
    /// Skip a specific number of tokens
    Skip { count: usize },
    /// Insert synthetic tokens
    Insert { tokens: SmallVec<[Token<K>; 2]> },
    /// Replace the current token
    Replace { with: Token<K> },
    /// Recovery failed, stop parsing
    Fail,
}

/// Trait for error recovery strategies
pub trait ErrorRecoveryStrategy<K: SyntaxKind>: Send + Sync {
    /// Attempt to recover from an error
    fn recover(&self, ctx: &RecoveryContext<K>) -> RecoveryAction<K>;

    /// Maximum number of recovery attempts per error
    fn max_attempts(&self) -> usize {
        3
    }

    /// Name of this recovery strategy (for debugging)
    fn name(&self) -> &'static str;
}

/// Panic mode recovery: skip tokens until a synchronization point
#[derive(Debug, Clone)]
pub struct PanicModeRecovery<K: SyntaxKind> {
    /// Token kinds that act as synchronization points
    sync_set: SmallVec<[K; 8]>,
}

impl<K: SyntaxKind> PanicModeRecovery<K> {
    /// Create a new panic mode recovery with the given sync set
    #[must_use]
    pub fn new(sync_set: impl IntoIterator<Item = K>) -> Self {
        Self {
            sync_set: sync_set.into_iter().collect(),
        }
    }

    /// Add a synchronization token
    pub fn add_sync(&mut self, kind: K) {
        if !self.sync_set.contains(&kind) {
            self.sync_set.push(kind);
        }
    }
}

impl<K: SyntaxKind> ErrorRecoveryStrategy<K> for PanicModeRecovery<K> {
    fn recover(&self, _ctx: &RecoveryContext<K>) -> RecoveryAction<K> {
        RecoveryAction::SkipTo {
            sync_kinds: self.sync_set.clone(),
        }
    }

    fn name(&self) -> &'static str {
        "panic_mode"
    }
}

/// Simple skip recovery: just skip one token and continue
#[derive(Debug, Clone, Default)]
pub struct SkipRecovery {
    max_skip: usize,
}

impl SkipRecovery {
    /// Create a new skip recovery with default settings
    #[must_use]
    pub const fn new() -> Self {
        Self { max_skip: 1 }
    }

    /// Create skip recovery that skips up to n tokens
    #[must_use]
    pub const fn with_max(max_skip: usize) -> Self {
        Self { max_skip }
    }
}

impl<K: SyntaxKind> ErrorRecoveryStrategy<K> for SkipRecovery {
    fn recover(&self, _ctx: &RecoveryContext<K>) -> RecoveryAction<K> {
        RecoveryAction::Skip { count: 1 }
    }

    fn max_attempts(&self) -> usize {
        self.max_skip
    }

    fn name(&self) -> &'static str {
        "skip"
    }
}

/// Minimal cost recovery: find the smallest edit to make input valid
///
/// This is more expensive but produces better error messages and recovery.
#[derive(Debug, Clone)]
pub struct MinimalCostRecovery {
    /// Maximum number of edits to consider
    max_edits: usize,
}

impl MinimalCostRecovery {
    /// Create a new minimal cost recovery
    #[must_use]
    pub const fn new(max_edits: usize) -> Self {
        Self { max_edits }
    }
}

impl Default for MinimalCostRecovery {
    fn default() -> Self {
        Self::new(3)
    }
}

impl<K: SyntaxKind> ErrorRecoveryStrategy<K> for MinimalCostRecovery {
    fn recover(&self, ctx: &RecoveryContext<K>) -> RecoveryAction<K> {
        // Try different recovery strategies and pick the best one
        // We score each strategy based on how likely it is to succeed

        let mut best_action: Option<RecoveryAction<K>> = None;
        let mut best_score = usize::MAX;

        // Strategy 1: Try inserting expected tokens
        if !ctx.expected.is_empty() {
            // Insert the first expected token (most likely to be correct)
            let insert_tokens: SmallVec<[Token<K>; 2]> = ctx.expected[0..1.min(ctx.expected.len())]
                .iter()
                .filter_map(|_kind| {
                    // Create a synthetic token - we can't create a real token without text,
                    // so we'll use the current token's text if available, or empty
                    ctx.current_token.clone()
                })
                .collect();

            if !insert_tokens.is_empty() {
                let score = 1; // Insert has low cost
                if score < best_score {
                    best_score = score;
                    best_action = Some(RecoveryAction::Insert {
                        tokens: insert_tokens,
                    });
                }
            }
        }

        // Strategy 2: Try replacing current token with expected token
        if let Some(current) = &ctx.current_token
            && !ctx.expected.is_empty()
            && !ctx.expected.contains(&current.kind)
        {
            // Current token doesn't match expected, try replacing
            if let Some(_expected_kind) = ctx.expected.first() {
                // Create replacement token (simplified - real implementation would need proper token creation)
                let score = 2; // Replace has medium cost
                if score < best_score {
                    // For now, we'll skip since we can't easily create replacement tokens
                    // In a real implementation, we'd create a token with the expected kind
                    best_score = score;
                    best_action = Some(RecoveryAction::Skip { count: 1 });
                }
            }
        }

        // Strategy 3: Skip current token (fallback)
        let skip_score = 3; // Skip has higher cost
        if skip_score < best_score || best_action.is_none() {
            best_action = Some(RecoveryAction::Skip { count: 1 });
        }

        // Strategy 4: If deeply nested, try to skip to reduce depth
        if ctx.depth > 2 {
            // Try skipping more aggressively when deeply nested
            let depth_skip = RecoveryAction::Skip {
                count: ctx.depth.min(3),
            };
            let depth_score = 4;
            if depth_score < best_score {
                best_action = Some(depth_skip);
            }
        }

        best_action.unwrap_or(RecoveryAction::Skip { count: 1 })
    }

    fn max_attempts(&self) -> usize {
        self.max_edits
    }

    fn name(&self) -> &'static str {
        "minimal_cost"
    }
}

/// Phrase recovery: try to complete the current phrase
#[derive(Debug, Clone)]
pub struct PhraseRecovery<K: SyntaxKind> {
    /// Closing tokens for common constructs
    closing_tokens: SmallVec<[(K, K); 8]>, // (open, close) pairs
}

impl<K: SyntaxKind> PhraseRecovery<K> {
    /// Create a new phrase recovery
    #[must_use]
    pub fn new() -> Self {
        Self {
            closing_tokens: SmallVec::new(),
        }
    }

    /// Add a bracket pair (open, close)
    pub fn add_bracket_pair(&mut self, open: K, close: K) {
        self.closing_tokens.push((open, close));
    }
}

impl<K: SyntaxKind> Default for PhraseRecovery<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: SyntaxKind> ErrorRecoveryStrategy<K> for PhraseRecovery<K> {
    fn recover(&self, ctx: &RecoveryContext<K>) -> RecoveryAction<K> {
        // Check if we need to insert a closing token based on bracket matching
        let mut open_count = 0;
        let mut last_open: Option<K> = None;

        // Count unmatched opening brackets
        for token in &ctx.previous_tokens {
            for (open, close) in &self.closing_tokens {
                if token.kind == *open {
                    open_count += 1;
                    last_open = Some(*close);
                } else if token.kind == *close && open_count > 0 {
                    open_count -= 1;
                }
            }
        }

        // If we have unmatched opening brackets, try to insert closing token
        if open_count > 0
            && let Some(close_kind) = last_open
        {
            // Try to insert the closing token
            // Note: In a real implementation, we'd create a proper token
            // For now, we'll use context to suggest the recovery
            if let Some(current) = &ctx.current_token {
                // If current token is the expected close, we're good
                if current.kind == close_kind {
                    return RecoveryAction::Skip { count: 0 };
                }
            }
            // Try inserting the closing token
            // Since we can't easily create tokens here, we'll skip and let the parser handle it
            return RecoveryAction::Skip { count: 1 };
        }

        // Check if current token matches an expected closing token
        if let Some(current) = &ctx.current_token {
            for (_open, close) in &self.closing_tokens {
                if current.kind == *close && ctx.expected.contains(close) {
                    // This is the expected closing token, don't skip it
                    return RecoveryAction::Skip { count: 0 };
                }
            }
        }

        // Default: skip one token
        RecoveryAction::Skip { count: 1 }
    }

    fn name(&self) -> &'static str {
        "phrase"
    }
}

/// Combined recovery strategy that tries multiple strategies in order
pub struct CombinedRecovery<K: SyntaxKind> {
    strategies: Vec<Box<dyn ErrorRecoveryStrategy<K>>>,
}

impl<K: SyntaxKind> CombinedRecovery<K> {
    /// Create a new combined recovery
    #[must_use]
    pub fn new() -> Self {
        Self {
            strategies: Vec::new(),
        }
    }

    /// Add a recovery strategy
    pub fn with_strategy<S: ErrorRecoveryStrategy<K> + 'static>(mut self, strategy: S) -> Self {
        self.strategies.push(Box::new(strategy));
        self
    }
}

impl<K: SyntaxKind> Default for CombinedRecovery<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: SyntaxKind> ErrorRecoveryStrategy<K> for CombinedRecovery<K> {
    fn recover(&self, ctx: &RecoveryContext<K>) -> RecoveryAction<K> {
        for strategy in &self.strategies {
            let action = strategy.recover(ctx);
            if !matches!(action, RecoveryAction::Fail) {
                return action;
            }
        }
        RecoveryAction::Fail
    }

    fn name(&self) -> &'static str {
        "combined"
    }
}

/// Synchronization set recovery: skip to tokens that are likely to be safe recovery points
///
/// This strategy maintains a set of "synchronization tokens" that are likely to appear
/// after an error and can be used as recovery points. Common examples include:
/// - Statement terminators (semicolons, newlines)
/// - Block delimiters (braces, brackets)
/// - Keywords that start new constructs
#[derive(Debug, Clone)]
pub struct SynchronizationSetRecovery<K: SyntaxKind> {
    /// Token kinds that act as synchronization points
    sync_set: SmallVec<[K; 8]>,
    /// Maximum tokens to skip before giving up
    max_skip: usize,
}

impl<K: SyntaxKind> SynchronizationSetRecovery<K> {
    /// Create a new synchronization set recovery
    #[must_use]
    pub fn new(sync_set: impl IntoIterator<Item = K>) -> Self {
        Self {
            sync_set: sync_set.into_iter().collect(),
            max_skip: 10,
        }
    }

    /// Set the maximum number of tokens to skip
    pub fn with_max_skip(mut self, max_skip: usize) -> Self {
        self.max_skip = max_skip;
        self
    }

    /// Add a synchronization token
    pub fn add_sync_token(&mut self, token: K) {
        self.sync_set.push(token);
    }
}

impl<K: SyntaxKind> ErrorRecoveryStrategy<K> for SynchronizationSetRecovery<K> {
    fn recover(&self, _ctx: &RecoveryContext<K>) -> RecoveryAction<K> {
        // Skip to the next synchronization token
        RecoveryAction::SkipTo {
            sync_kinds: self.sync_set.clone(),
        }
    }

    fn max_attempts(&self) -> usize {
        self.max_skip
    }

    fn name(&self) -> &'static str {
        "synchronization_set"
    }
}

/// Builder for creating recovery strategies
pub struct RecoveryBuilder<K: SyntaxKind> {
    sync_set: SmallVec<[K; 8]>,
    bracket_pairs: SmallVec<[(K, K); 4]>,
    max_edits: usize,
}

impl<K: SyntaxKind> RecoveryBuilder<K> {
    /// Create a new recovery builder
    #[must_use]
    pub fn new() -> Self {
        Self {
            sync_set: SmallVec::new(),
            bracket_pairs: SmallVec::new(),
            max_edits: 3,
        }
    }

    /// Add synchronization tokens
    pub fn sync_on(mut self, kinds: impl IntoIterator<Item = K>) -> Self {
        self.sync_set.extend(kinds);
        self
    }

    /// Add a bracket pair
    pub fn bracket_pair(mut self, open: K, close: K) -> Self {
        self.bracket_pairs.push((open, close));
        self
    }

    /// Set maximum edits for minimal cost recovery
    pub fn max_edits(mut self, max: usize) -> Self {
        self.max_edits = max;
        self
    }

    /// Build a panic mode recovery
    #[must_use]
    pub fn build_panic_mode(self) -> PanicModeRecovery<K> {
        PanicModeRecovery::new(self.sync_set)
    }

    /// Build a combined recovery strategy
    #[must_use]
    pub fn build_combined(self) -> CombinedRecovery<K> {
        let mut combined = CombinedRecovery::new();

        // Add phrase recovery if we have bracket pairs
        if !self.bracket_pairs.is_empty() {
            let mut phrase = PhraseRecovery::new();
            for (open, close) in self.bracket_pairs {
                phrase.add_bracket_pair(open, close);
            }
            combined = combined.with_strategy(phrase);
        }

        // Add panic mode if we have sync tokens
        if !self.sync_set.is_empty() {
            combined = combined.with_strategy(PanicModeRecovery::new(self.sync_set));
        }

        // Add skip as fallback
        combined = combined.with_strategy(SkipRecovery::new());

        combined
    }
}

impl<K: SyntaxKind> Default for RecoveryBuilder<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::TextRange;
    use crate::syntax::TextSize;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Ident,
        Semi,
        LBrace,
        RBrace,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            true
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    fn make_token(kind: TestKind, text: &str) -> Token<TestKind> {
        Token {
            kind,
            text: text.into(),
            range: TextRange::at(TextSize::zero(), TextSize::from(text.len() as u32)),
            value: crate::lexer::TokenValue::None,
        }
    }

    #[test]
    fn test_panic_mode_recovery() {
        let recovery = PanicModeRecovery::new([TestKind::Semi]);
        let ctx: RecoveryContext<TestKind> = RecoveryContext::new(0);

        let action = recovery.recover(&ctx);
        assert!(matches!(action, RecoveryAction::SkipTo { .. }));
    }

    #[test]
    fn test_skip_recovery() {
        let recovery = SkipRecovery::new();
        let ctx: RecoveryContext<TestKind> = RecoveryContext::new(0);

        let action = recovery.recover(&ctx);
        assert!(matches!(action, RecoveryAction::Skip { count: 1 }));
    }

    #[test]
    fn test_recovery_builder() {
        let combined: CombinedRecovery<TestKind> = RecoveryBuilder::new()
            .sync_on([TestKind::Semi])
            .bracket_pair(TestKind::LBrace, TestKind::RBrace)
            .build_combined();

        assert_eq!(combined.name(), "combined");
    }
}
