//! # Incremental Lexer
//!
//! This module provides incremental lexing capabilities, allowing efficient
//! re-tokenization when source text changes.
//!
//! ## Overview
//!
//! The incremental lexer maintains the tokenized state and efficiently updates
//! only the affected tokens when edits are made to the source text.
//!
//! Key features:
//! - Efficient delta-based updates
//! - Line number tracking for fast bisection
//! - Token stream compatibility
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::lexer::incremental::{IncrementalLexer, TextEdit};
//!
//! let mut lexer = IncrementalLexer::new(compiled_lexer, "let x = 42;".into());
//!
//! // Make an edit
//! let edit = TextEdit::replace(4..5, "y"); // Change 'x' to 'y'
//! let delta = lexer.update(&edit, "let y = 42;");
//!
//! // The delta tells us what changed
//! println!("Changed tokens: {:?}", delta.changed_range);
//! ```

use crate::lexer::{CompiledLexer, Token, stream::TokenStream};
use crate::syntax::{SyntaxKind, TextRange, TextSize};
use std::ops::Range;

/// A text edit to be applied to the source
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEdit {
    /// The byte range to replace
    pub range: Range<usize>,
    /// The new text to insert
    pub new_text: String,
}

impl TextEdit {
    /// Create a text edit that replaces a range
    #[must_use]
    pub fn replace(range: Range<usize>, new_text: impl Into<String>) -> Self {
        Self {
            range,
            new_text: new_text.into(),
        }
    }

    /// Create an insertion edit
    #[must_use]
    pub fn insert(offset: usize, text: impl Into<String>) -> Self {
        Self {
            range: offset..offset,
            new_text: text.into(),
        }
    }

    /// Create a deletion edit
    #[must_use]
    pub fn delete(range: Range<usize>) -> Self {
        Self {
            range,
            new_text: String::new(),
        }
    }

    /// Get the offset delta (change in length)
    #[must_use]
    pub fn delta(&self) -> isize {
        self.new_text.len() as isize - (self.range.end - self.range.start) as isize
    }

    /// Check if this is an insertion
    #[must_use]
    pub fn is_insert(&self) -> bool {
        self.range.start == self.range.end && !self.new_text.is_empty()
    }

    /// Check if this is a deletion
    #[must_use]
    pub fn is_delete(&self) -> bool {
        self.new_text.is_empty() && self.range.start != self.range.end
    }

    /// Check if this is a replacement
    #[must_use]
    pub fn is_replace(&self) -> bool {
        !self.new_text.is_empty() && self.range.start != self.range.end
    }
}

/// The result of an incremental lex update
#[derive(Debug, Clone)]
pub struct LexerDelta<K: SyntaxKind> {
    /// The range of token indices that changed
    pub changed_range: Range<usize>,
    /// The new tokens that replace the changed range
    pub new_tokens: Vec<Token<K>>,
    /// Number of tokens removed
    pub removed_count: usize,
    /// Number of tokens added
    pub added_count: usize,
}

impl<K: SyntaxKind> LexerDelta<K> {
    /// Check if any tokens changed
    #[must_use]
    pub fn has_changes(&self) -> bool {
        self.removed_count > 0 || self.added_count > 0
    }

    /// Get the net change in token count
    #[must_use]
    pub fn token_count_delta(&self) -> isize {
        self.added_count as isize - self.removed_count as isize
    }
}

/// An incremental lexer that maintains tokenized state
///
/// This lexer efficiently updates tokens when the source text changes,
/// only re-tokenizing the affected region.
pub struct IncrementalLexer<K: SyntaxKind> {
    lexer: CompiledLexer<K>,
    input: String,
    tokens: Vec<Token<K>>,
    /// Line start byte offsets for fast bisection
    line_starts: Vec<TextSize>,
    /// Version counter for cache invalidation
    version: u64,
}

impl<K: SyntaxKind> IncrementalLexer<K> {
    /// Create a new incremental lexer and tokenize the initial input
    #[must_use]
    pub fn new(lexer: CompiledLexer<K>, input: String) -> Self {
        let tokens = lexer.tokenize(&input).unwrap_or_default();
        let line_starts = compute_line_starts(&input);

        Self {
            lexer,
            input,
            tokens,
            line_starts,
            version: 0,
        }
    }

    /// Get the current input text
    #[must_use]
    pub fn input(&self) -> &str {
        &self.input
    }

    /// Get the current tokens
    #[must_use]
    pub fn tokens(&self) -> &[Token<K>] {
        &self.tokens
    }

    /// Get the version number (increments on each update)
    #[must_use]
    pub fn version(&self) -> u64 {
        self.version
    }

    /// Get line starts for line/column calculations
    #[must_use]
    pub fn line_starts(&self) -> &[TextSize] {
        &self.line_starts
    }

    /// Get the line number for a byte offset
    #[must_use]
    pub fn line_at_offset(&self, offset: TextSize) -> usize {
        self.line_starts
            .binary_search(&offset)
            .unwrap_or_else(|i| i.saturating_sub(1))
    }

    /// Update the lexer with a text edit
    pub fn update(&mut self, edit: &TextEdit, new_input: &str) -> LexerDelta<K> {
        self.version += 1;

        // Find the range of tokens affected by the edit
        let edit_start = edit.range.start;
        let edit_end = edit.range.end;

        // Find first affected token (by start position)
        let first_affected = self
            .tokens
            .iter()
            .position(|t| {
                let start = u32::from(t.range.start()) as usize;
                let end = u32::from(t.range.end()) as usize;
                end > edit_start || (start <= edit_start && end >= edit_start)
            })
            .unwrap_or(self.tokens.len());

        // Find last affected token
        // We need to re-lex beyond the edit because token boundaries might shift
        let delta = edit.delta();
        let mut last_affected = first_affected;

        for (i, token) in self.tokens.iter().enumerate().skip(first_affected) {
            let token_end = u32::from(token.range.end()) as usize;
            if token_end < edit_end {
                last_affected = i + 1;
                continue;
            }

            // Check if this token might be affected by shifted boundaries
            // We conservatively include tokens that start near the edit
            let token_start = u32::from(token.range.start()) as usize;
            if token_start <= edit_end {
                last_affected = i + 1;
            } else {
                break;
            }
        }

        last_affected = last_affected.min(self.tokens.len());

        // Compute the byte range to re-lex
        let relex_start = if first_affected > 0 {
            u32::from(self.tokens[first_affected - 1].range.end()) as usize
        } else {
            0
        };

        let relex_end_old = if last_affected < self.tokens.len() {
            u32::from(self.tokens[last_affected].range.start()) as usize
        } else {
            self.input.len()
        };

        // Adjust for the edit delta
        let relex_end_new = if delta >= 0 {
            relex_end_old + delta as usize
        } else {
            relex_end_old.saturating_sub((-delta) as usize)
        };

        // Update internal state
        self.input = new_input.to_string();
        self.line_starts = compute_line_starts(new_input);

        // Re-tokenize the affected region
        let relex_region = &new_input[relex_start..relex_end_new.min(new_input.len())];
        let new_tokens = self
            .lexer
            .tokenize_with_offset(relex_region, TextSize::from(relex_start as u32))
            .unwrap_or_default();

        // Determine how many tokens to remove and add
        let removed_count = last_affected - first_affected;
        let added_count = new_tokens.len();

        // Update token positions for tokens after the edit
        let offset_delta = delta;
        let tokens_after_edit: Vec<_> = self.tokens[last_affected..]
            .iter()
            .map(|t| {
                let new_start = if offset_delta >= 0 {
                    u32::from(t.range.start()) + offset_delta as u32
                } else {
                    u32::from(t.range.start()).saturating_sub((-offset_delta) as u32)
                };
                Token::new(
                    t.kind,
                    t.text.clone(),
                    TextRange::at(TextSize::from(new_start), t.range.len()),
                )
            })
            .collect();

        // Build the new token list
        let mut result_tokens =
            Vec::with_capacity(first_affected + new_tokens.len() + tokens_after_edit.len());
        result_tokens.extend(self.tokens[..first_affected].iter().cloned());
        result_tokens.extend(new_tokens.iter().cloned());
        result_tokens.extend(tokens_after_edit);

        self.tokens = result_tokens;

        LexerDelta {
            changed_range: first_affected..first_affected + removed_count,
            new_tokens,
            removed_count,
            added_count,
        }
    }

    /// Replace the entire input and re-tokenize
    pub fn set_input(&mut self, new_input: String) -> LexerDelta<K> {
        let removed_count = self.tokens.len();

        self.version += 1;
        self.input = new_input;
        self.tokens = self.lexer.tokenize(&self.input).unwrap_or_default();
        self.line_starts = compute_line_starts(&self.input);

        let added_count = self.tokens.len();

        LexerDelta {
            changed_range: 0..removed_count,
            new_tokens: self.tokens.clone(),
            removed_count,
            added_count,
        }
    }

    /// Create a token stream from the current state
    #[must_use]
    pub fn token_stream(&self) -> crate::lexer::stream::VecTokenStream<K> {
        crate::lexer::stream::VecTokenStream::new(self.tokens.clone())
    }

    /// Get the token at a byte offset
    #[must_use]
    pub fn token_at_offset(&self, offset: TextSize) -> Option<&Token<K>> {
        let offset_u32 = u32::from(offset);
        self.tokens.iter().find(|t| {
            let start = u32::from(t.range.start());
            let end = u32::from(t.range.end());
            start <= offset_u32 && offset_u32 < end
        })
    }

    /// Get the token index at a byte offset
    #[must_use]
    pub fn token_index_at_offset(&self, offset: TextSize) -> Option<usize> {
        let offset_u32 = u32::from(offset);
        self.tokens.iter().position(|t| {
            let start = u32::from(t.range.start());
            let end = u32::from(t.range.end());
            start <= offset_u32 && offset_u32 < end
        })
    }
}

impl<K: SyntaxKind> TokenStream<K> for IncrementalLexer<K> {
    fn peek(&mut self, _n: usize) -> Option<&Token<K>> {
        // This is a simplified implementation - for full stream functionality,
        // use token_stream() to get a proper stream
        None
    }

    fn advance(&mut self) -> Option<Token<K>> {
        None
    }

    fn position(&self) -> TextSize {
        TextSize::zero()
    }

    fn checkpoint(&self) -> crate::lexer::stream::StreamCheckpoint {
        crate::lexer::stream::StreamCheckpoint {
            index: 0,
            offset: TextSize::zero(),
        }
    }

    fn restore(&mut self, _checkpoint: crate::lexer::stream::StreamCheckpoint) {}

    fn is_at_end(&mut self) -> bool {
        true
    }

    fn token_index(&self) -> usize {
        0
    }

    fn remaining(&self) -> Option<&[Token<K>]> {
        Some(&self.tokens)
    }
}

/// Compute line start positions for a string
fn compute_line_starts(text: &str) -> Vec<TextSize> {
    let mut starts = vec![TextSize::zero()];
    let mut pos = 0u32;

    for c in text.chars() {
        pos += c.len_utf8() as u32;
        if c == '\n' {
            starts.push(TextSize::from(pos));
        }
    }

    starts
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{CharSet, LexerBuilder, Pattern};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Ident,
        Number,
        Whitespace,
        Eof,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            true
        }

        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
        }
    }

    fn make_lexer() -> CompiledLexer<TestKind> {
        LexerBuilder::new()
            .token(
                TestKind::Ident,
                Pattern::Repeat {
                    pattern: Box::new(Pattern::CharClass(CharSet::new(vec!['a'..='z', 'A'..='Z']))),
                    min: 1,
                    max: None,
                },
            )
            .token(
                TestKind::Number,
                Pattern::Repeat {
                    pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                    min: 1,
                    max: None,
                },
            )
            .token(
                TestKind::Whitespace,
                Pattern::Repeat {
                    pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                    min: 1,
                    max: None,
                },
            )
            .trivia(TestKind::Whitespace)
            .build(TestKind::Eof, TestKind::Ident)
            .unwrap()
    }

    #[test]
    fn test_text_edit_types() {
        let insert = TextEdit::insert(5, "hello");
        assert!(insert.is_insert());
        assert!(!insert.is_delete());
        assert!(!insert.is_replace());

        let delete = TextEdit::delete(5..10);
        assert!(!delete.is_insert());
        assert!(delete.is_delete());
        assert!(!delete.is_replace());

        let replace = TextEdit::replace(5..10, "hi");
        assert!(!replace.is_insert());
        assert!(!replace.is_delete());
        assert!(replace.is_replace());
    }

    #[test]
    fn test_text_edit_delta() {
        let insert = TextEdit::insert(0, "hello");
        assert_eq!(insert.delta(), 5);

        let delete = TextEdit::delete(0..5);
        assert_eq!(delete.delta(), -5);

        let replace = TextEdit::replace(0..3, "hello");
        assert_eq!(replace.delta(), 2);
    }

    #[test]
    fn test_incremental_lexer_new() {
        let lexer = make_lexer();
        let incr = IncrementalLexer::new(lexer, "foo 123".into());

        assert_eq!(incr.version(), 0);
        assert_eq!(incr.input(), "foo 123");
        assert!(!incr.tokens().is_empty());
    }

    #[test]
    fn test_incremental_lexer_update() {
        let lexer = make_lexer();
        let mut incr = IncrementalLexer::new(lexer, "foo 123".into());

        let initial_version = incr.version();

        // Change 'foo' to 'bar'
        let edit = TextEdit::replace(0..3, "bar");
        let delta = incr.update(&edit, "bar 123");

        assert!(delta.has_changes());
        assert_eq!(incr.version(), initial_version + 1);
        assert_eq!(incr.input(), "bar 123");
    }

    #[test]
    fn test_incremental_lexer_line_at_offset() {
        let lexer = make_lexer();
        let incr = IncrementalLexer::new(lexer, "foo\nbar\nbaz".into());

        assert_eq!(incr.line_at_offset(TextSize::from(0)), 0);
        assert_eq!(incr.line_at_offset(TextSize::from(4)), 1);
        assert_eq!(incr.line_at_offset(TextSize::from(8)), 2);
    }

    #[test]
    fn test_compute_line_starts() {
        let starts = compute_line_starts("hello\nworld\n");
        assert_eq!(starts.len(), 3);
        assert_eq!(starts[0], TextSize::from(0));
        assert_eq!(starts[1], TextSize::from(6)); // After "hello\n"
        assert_eq!(starts[2], TextSize::from(12)); // After "world\n"
    }
}
