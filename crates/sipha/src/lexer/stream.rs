//! # Token Stream Abstraction
//!
//! This module provides a streaming/iterator interface for tokenization,
//! enabling both eager and lazy token consumption strategies.
//!
//! ## Overview
//!
//! The `TokenStream` trait abstracts over different ways of providing tokens:
//!
//! - **Eager**: Pre-tokenized Vec of tokens
//! - **Lazy**: On-demand tokenization as parsing progresses
//! - **Incremental**: Reuses tokens from previous parse with delta updates
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::lexer::stream::{TokenStream, VecTokenStream};
//!
//! let tokens = vec![/* tokens from lexer */];
//! let mut stream = VecTokenStream::new(tokens);
//!
//! // Peek ahead without consuming
//! if let Some(token) = stream.peek(0) {
//!     println!("Next token: {:?}", token.kind);
//! }
//!
//! // Consume a token
//! let token = stream.advance();
//!
//! // Create a checkpoint for backtracking
//! let checkpoint = stream.checkpoint();
//!
//! // ... try parsing ...
//!
//! // Backtrack if needed
//! stream.restore(checkpoint);
//! ```

use crate::grammar::Token as GrammarToken;
use crate::lexer::{CompiledLexer, Token};
use crate::syntax::{SyntaxKind, TextRange, TextSize};

/// A checkpoint for restoring stream position
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StreamCheckpoint {
    /// The token index
    pub index: usize,
    /// The byte offset in the source
    pub offset: TextSize,
}

/// Trait for token stream implementations
///
/// This trait abstracts over different ways of providing tokens to a parser.
/// Implementations can be eager (pre-tokenized) or lazy (on-demand).
pub trait TokenStream<K: SyntaxKind> {
    /// Peek at a token without consuming it
    ///
    /// `n` is the lookahead distance (0 = next token, 1 = token after next, etc.)
    fn peek(&mut self, n: usize) -> Option<&Token<K>>;

    /// Consume and return the next token
    fn advance(&mut self) -> Option<Token<K>>;

    /// Get the current byte offset in the source
    fn position(&self) -> TextSize;

    /// Create a checkpoint that can be used to restore position
    fn checkpoint(&self) -> StreamCheckpoint;

    /// Restore to a previously saved checkpoint
    fn restore(&mut self, checkpoint: StreamCheckpoint);

    /// Check if the stream is at the end
    fn is_at_end(&mut self) -> bool;

    /// Get the current token index
    fn token_index(&self) -> usize;

    /// Skip tokens while a predicate is true
    fn skip_while<F>(&mut self, predicate: F) -> usize
    where
        F: Fn(&Token<K>) -> bool,
    {
        let mut count = 0;
        while let Some(token) = self.peek(0) {
            if predicate(token) {
                self.advance();
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    /// Skip trivia tokens (whitespace, comments, etc.)
    fn skip_trivia(&mut self) -> usize {
        self.skip_while(|t| t.kind.is_trivia())
    }

    /// Consume tokens until a specific kind is found
    fn advance_until(&mut self, kind: K) -> Vec<Token<K>> {
        let mut tokens = Vec::new();
        while let Some(token) = self.peek(0) {
            if token.kind == kind {
                break;
            }
            if let Some(t) = self.advance() {
                tokens.push(t);
            }
        }
        tokens
    }

    /// Consume a token if it matches the expected kind
    fn consume(&mut self, expected: K) -> Option<Token<K>> {
        if self.peek(0).map(|t| t.kind) == Some(expected) {
            self.advance()
        } else {
            None
        }
    }

    /// Check if the next token matches the expected kind
    fn check(&mut self, expected: K) -> bool {
        self.peek(0).map(|t| t.kind) == Some(expected)
    }

    /// Get remaining tokens as a slice (if available)
    fn remaining(&self) -> Option<&[Token<K>]> {
        None // Default implementation returns None
    }
}

/// A token stream backed by a pre-tokenized Vec
///
/// This is the simplest and most common token stream implementation.
/// All tokens are materialized upfront.
#[derive(Debug, Clone)]
pub struct VecTokenStream<K: SyntaxKind> {
    tokens: Vec<Token<K>>,
    pos: usize,
    offset: TextSize,
}

impl<K: SyntaxKind> VecTokenStream<K> {
    /// Create a new token stream from a Vec of tokens
    #[must_use]
    pub fn new(tokens: Vec<Token<K>>) -> Self {
        Self {
            tokens,
            pos: 0,
            offset: TextSize::zero(),
        }
    }

    /// Get the underlying tokens
    #[must_use]
    pub fn tokens(&self) -> &[Token<K>] {
        &self.tokens
    }

    /// Get the total number of tokens
    #[must_use]
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Check if the stream is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    /// Reset to the beginning
    pub fn reset(&mut self) {
        self.pos = 0;
        self.offset = TextSize::zero();
    }
}

impl<K: SyntaxKind> TokenStream<K> for VecTokenStream<K> {
    fn peek(&mut self, n: usize) -> Option<&Token<K>> {
        self.tokens.get(self.pos + n)
    }

    fn advance(&mut self) -> Option<Token<K>> {
        if self.pos < self.tokens.len() {
            let token = self.tokens[self.pos].clone();
            self.offset = TextSize::from(u32::from(self.offset) + u32::from(token.text_len()));
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn position(&self) -> TextSize {
        self.offset
    }

    fn checkpoint(&self) -> StreamCheckpoint {
        StreamCheckpoint {
            index: self.pos,
            offset: self.offset,
        }
    }

    fn restore(&mut self, checkpoint: StreamCheckpoint) {
        self.pos = checkpoint.index;
        self.offset = checkpoint.offset;
    }

    fn is_at_end(&mut self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn token_index(&self) -> usize {
        self.pos
    }

    fn remaining(&self) -> Option<&[Token<K>]> {
        Some(&self.tokens[self.pos..])
    }
}

/// A lazy token stream that tokenizes on demand
///
/// This stream delays tokenization until tokens are actually needed,
/// which can be more efficient when parsing fails early or when
/// only part of the input is needed.
pub struct LazyTokenStream<K: SyntaxKind> {
    lexer: CompiledLexer<K>,
    input: String,
    /// Materialized tokens (buffer)
    buffer: Vec<Token<K>>,
    /// Current position in the buffer
    pos: usize,
    /// Current byte offset in the input
    byte_offset: usize,
    /// Whether we've finished tokenizing
    finished: bool,
}

impl<K: SyntaxKind> LazyTokenStream<K> {
    /// Create a new lazy token stream
    #[must_use]
    pub fn new(lexer: CompiledLexer<K>, input: String) -> Self {
        Self {
            lexer,
            input,
            buffer: Vec::new(),
            pos: 0,
            byte_offset: 0,
            finished: false,
        }
    }

    /// Ensure we have at least `n` tokens buffered ahead of current position
    fn ensure_buffered(&mut self, n: usize) {
        let needed = self.pos + n + 1;

        while self.buffer.len() < needed && !self.finished {
            self.tokenize_next();
        }
    }

    /// Tokenize the next token and add it to the buffer
    fn tokenize_next(&mut self) {
        if self.finished {
            return;
        }

        let remaining = &self.input[self.byte_offset..];
        if remaining.is_empty() {
            self.finished = true;
            return;
        }

        // Tokenize using the compiled lexer
        match self.lexer.tokenize(remaining) {
            Ok(tokens) if !tokens.is_empty() => {
                // Adjust ranges for the actual byte offset
                let offset = self.byte_offset as u32;
                for token in tokens {
                    // Skip EOF token at end
                    if token.kind == self.lexer.eof_kind {
                        self.finished = true;
                        break;
                    }
                    let adjusted_range = TextRange::at(
                        TextSize::from(u32::from(token.range.start()) + offset),
                        token.range.len(),
                    );
                    self.byte_offset += token.text.len();
                    self.buffer
                        .push(Token::new(token.kind, token.text, adjusted_range));
                }
            }
            Ok(_) => {
                self.finished = true;
            }
            Err(_) => {
                // On error, skip one byte and try again
                self.byte_offset += 1;
            }
        }
    }

    /// Get the input string
    #[must_use]
    pub fn input(&self) -> &str {
        &self.input
    }

    /// Get the buffered tokens
    #[must_use]
    pub fn buffered_tokens(&self) -> &[Token<K>] {
        &self.buffer
    }
}

impl<K: SyntaxKind> TokenStream<K> for LazyTokenStream<K> {
    fn peek(&mut self, n: usize) -> Option<&Token<K>> {
        self.ensure_buffered(n);
        self.buffer.get(self.pos + n)
    }

    fn advance(&mut self) -> Option<Token<K>> {
        self.ensure_buffered(0);

        if self.pos < self.buffer.len() {
            let token = self.buffer[self.pos].clone();
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn position(&self) -> TextSize {
        if self.pos > 0 && self.pos <= self.buffer.len() {
            self.buffer[self.pos - 1].range.end()
        } else {
            TextSize::zero()
        }
    }

    fn checkpoint(&self) -> StreamCheckpoint {
        StreamCheckpoint {
            index: self.pos,
            offset: self.position(),
        }
    }

    fn restore(&mut self, checkpoint: StreamCheckpoint) {
        self.pos = checkpoint.index;
    }

    fn is_at_end(&mut self) -> bool {
        self.ensure_buffered(0);
        self.pos >= self.buffer.len() && self.finished
    }

    fn token_index(&self) -> usize {
        self.pos
    }
}

/// A peekable wrapper that adds lookahead to any token iterator
pub struct PeekableTokenStream<K: SyntaxKind, I: Iterator<Item = Token<K>>> {
    iter: I,
    buffer: Vec<Token<K>>,
    pos: usize,
    offset: TextSize,
}

impl<K: SyntaxKind, I: Iterator<Item = Token<K>>> PeekableTokenStream<K, I> {
    /// Create a new peekable token stream from an iterator
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            buffer: Vec::new(),
            pos: 0,
            offset: TextSize::zero(),
        }
    }

    fn ensure_buffered(&mut self, n: usize) {
        while self.buffer.len() <= self.pos + n {
            match self.iter.next() {
                Some(token) => self.buffer.push(token),
                None => break,
            }
        }
    }
}

impl<K: SyntaxKind, I: Iterator<Item = Token<K>>> TokenStream<K> for PeekableTokenStream<K, I> {
    fn peek(&mut self, n: usize) -> Option<&Token<K>> {
        self.ensure_buffered(n);
        self.buffer.get(self.pos + n)
    }

    fn advance(&mut self) -> Option<Token<K>> {
        self.ensure_buffered(0);

        if self.pos < self.buffer.len() {
            let token = self.buffer[self.pos].clone();
            self.offset = TextSize::from(u32::from(self.offset) + u32::from(token.text_len()));
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn position(&self) -> TextSize {
        self.offset
    }

    fn checkpoint(&self) -> StreamCheckpoint {
        StreamCheckpoint {
            index: self.pos,
            offset: self.offset,
        }
    }

    fn restore(&mut self, checkpoint: StreamCheckpoint) {
        self.pos = checkpoint.index;
        self.offset = checkpoint.offset;
    }

    fn is_at_end(&mut self) -> bool {
        self.peek(0).is_none()
    }

    fn token_index(&self) -> usize {
        self.pos
    }
}

/// A filtered token stream that automatically skips trivia
pub struct NonTriviaStream<K, S> {
    inner: S,
    _phantom: std::marker::PhantomData<K>,
}

impl<K: SyntaxKind, S: TokenStream<K>> NonTriviaStream<K, S> {
    /// Create a new non-trivia stream wrapper
    #[must_use]
    pub fn new(inner: S) -> Self {
        Self {
            inner,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Get the inner stream
    #[must_use]
    pub fn inner(&self) -> &S {
        &self.inner
    }

    /// Get the inner stream mutably
    pub fn inner_mut(&mut self) -> &mut S {
        &mut self.inner
    }

    /// Skip trivia at current position
    fn skip_trivia_internal(&mut self) {
        while let Some(token) = self.inner.peek(0) {
            if token.kind.is_trivia() {
                self.inner.advance();
            } else {
                break;
            }
        }
    }
}

impl<K: SyntaxKind, S: TokenStream<K>> TokenStream<K> for NonTriviaStream<K, S> {
    fn peek(&mut self, n: usize) -> Option<&Token<K>> {
        self.skip_trivia_internal();

        // For lookahead, we need to count non-trivia tokens
        let mut count = 0;
        let mut idx = 0;

        loop {
            let token = self.inner.peek(idx)?;
            if !token.kind.is_trivia() {
                if count == n {
                    return self.inner.peek(idx);
                }
                count += 1;
            }
            idx += 1;
        }
    }

    fn advance(&mut self) -> Option<Token<K>> {
        self.skip_trivia_internal();
        self.inner.advance()
    }

    fn position(&self) -> TextSize {
        self.inner.position()
    }

    fn checkpoint(&self) -> StreamCheckpoint {
        self.inner.checkpoint()
    }

    fn restore(&mut self, checkpoint: StreamCheckpoint) {
        self.inner.restore(checkpoint);
    }

    fn is_at_end(&mut self) -> bool {
        self.inner.is_at_end()
    }

    fn token_index(&self) -> usize {
        self.inner.token_index()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::TextRange;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Ident,
        Number,
        Whitespace,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            true
        }

        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
        }
    }

    fn make_token(kind: TestKind, text: &str, start: u32) -> Token<TestKind> {
        Token::new(
            kind,
            text,
            TextRange::at(TextSize::from(start), TextSize::from(text.len() as u32)),
        )
    }

    #[test]
    fn test_vec_token_stream_basic() {
        let tokens = vec![
            make_token(TestKind::Ident, "foo", 0),
            make_token(TestKind::Number, "42", 3),
        ];
        let mut stream = VecTokenStream::new(tokens);

        assert!(!stream.is_at_end());
        assert_eq!(stream.peek(0).map(|t| t.kind), Some(TestKind::Ident));
        assert_eq!(stream.peek(1).map(|t| t.kind), Some(TestKind::Number));

        let t = stream.advance().unwrap();
        assert_eq!(t.kind, TestKind::Ident);
        assert_eq!(t.text.as_str(), "foo");

        let t = stream.advance().unwrap();
        assert_eq!(t.kind, TestKind::Number);

        assert!(stream.is_at_end());
        assert!(stream.advance().is_none());
    }

    #[test]
    fn test_vec_token_stream_checkpoint() {
        let tokens = vec![
            make_token(TestKind::Ident, "a", 0),
            make_token(TestKind::Ident, "b", 1),
            make_token(TestKind::Ident, "c", 2),
        ];
        let mut stream = VecTokenStream::new(tokens);

        stream.advance(); // consume 'a'
        let cp = stream.checkpoint();

        stream.advance(); // consume 'b'
        stream.advance(); // consume 'c'

        stream.restore(cp);
        assert_eq!(stream.peek(0).map(|t| &*t.text), Some("b"));
    }

    #[test]
    fn test_skip_trivia() {
        let tokens = vec![
            make_token(TestKind::Whitespace, " ", 0),
            make_token(TestKind::Whitespace, " ", 1),
            make_token(TestKind::Ident, "x", 2),
        ];
        let mut stream = VecTokenStream::new(tokens);

        let skipped = stream.skip_trivia();
        assert_eq!(skipped, 2);
        assert_eq!(stream.peek(0).map(|t| t.kind), Some(TestKind::Ident));
    }

    #[test]
    fn test_consume() {
        let tokens = vec![
            make_token(TestKind::Ident, "x", 0),
            make_token(TestKind::Number, "1", 1),
        ];
        let mut stream = VecTokenStream::new(tokens);

        // Consume matching
        let t = stream.consume(TestKind::Ident);
        assert!(t.is_some());

        // Consume non-matching
        let t = stream.consume(TestKind::Ident);
        assert!(t.is_none());

        // Position unchanged on failed consume
        assert_eq!(stream.peek(0).map(|t| t.kind), Some(TestKind::Number));
    }

    #[test]
    fn test_non_trivia_stream() {
        let tokens = vec![
            make_token(TestKind::Whitespace, " ", 0),
            make_token(TestKind::Ident, "x", 1),
            make_token(TestKind::Whitespace, " ", 2),
            make_token(TestKind::Number, "1", 3),
        ];
        let inner = VecTokenStream::new(tokens);
        let mut stream = NonTriviaStream::new(inner);

        // Should skip initial whitespace
        assert_eq!(stream.peek(0).map(|t| t.kind), Some(TestKind::Ident));

        // Lookahead should also skip trivia
        assert_eq!(stream.peek(1).map(|t| t.kind), Some(TestKind::Number));
    }
}
