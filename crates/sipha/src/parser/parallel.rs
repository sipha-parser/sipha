//! # Parallel Parsing Support
//!
//! This module provides infrastructure for parsing multiple files in parallel.
//!
//! ## Overview
//!
//! When parsing large projects, parsing multiple files in parallel can significantly
//! reduce total parsing time. This module provides utilities for:
//!
//! - Batch parsing of multiple files
//! - Progress reporting
//! - Error aggregation

use crate::error::ParseError;
use crate::grammar::{Grammar, NonTerminal, Token};
use crate::lexer::Token as LexerToken;
use crate::syntax::{GreenNode, SyntaxKind};
use std::sync::Arc;

#[cfg(feature = "parallel")]
use rayon::prelude::*;

/// Result of parsing a single file
#[derive(Debug)]
pub struct FileParseResult<K: SyntaxKind> {
    /// The file identifier (path or index)
    pub file_id: String,
    /// The parsed syntax tree (if successful)
    pub tree: Option<Arc<GreenNode<K>>>,
    /// Errors encountered during parsing
    pub errors: Vec<ParseError>,
    /// Parsing duration
    pub duration: std::time::Duration,
}

impl<K: SyntaxKind> FileParseResult<K> {
    /// Create a successful result
    #[must_use]
    pub fn success(
        file_id: String,
        tree: Arc<GreenNode<K>>,
        duration: std::time::Duration,
    ) -> Self {
        Self {
            file_id,
            tree: Some(tree),
            errors: Vec::new(),
            duration,
        }
    }

    /// Create a failed result
    #[must_use]
    pub fn failure(
        file_id: String,
        errors: Vec<ParseError>,
        duration: std::time::Duration,
    ) -> Self {
        Self {
            file_id,
            tree: None,
            errors,
            duration,
        }
    }

    /// Check if parsing succeeded
    #[must_use]
    pub fn is_ok(&self) -> bool {
        self.tree.is_some() && self.errors.is_empty()
    }
}

/// A batch of files to parse
#[derive(Debug, Clone)]
pub struct ParseBatch<N> {
    /// Files to parse: (file_id, content, entry_point)
    pub files: Vec<(String, String, N)>,
}

impl<N: Clone> ParseBatch<N> {
    /// Create a new empty batch
    #[must_use]
    pub fn new() -> Self {
        Self { files: Vec::new() }
    }

    /// Add a file to the batch
    pub fn add(&mut self, file_id: impl Into<String>, content: impl Into<String>, entry: N) {
        self.files.push((file_id.into(), content.into(), entry));
    }

    /// Get the number of files in the batch
    #[must_use]
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// Check if the batch is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }
}

impl<N: Clone> Default for ParseBatch<N> {
    fn default() -> Self {
        Self::new()
    }
}

/// Progress callback for parallel parsing
pub type ProgressCallback = Box<dyn Fn(usize, usize) + Send + Sync>;

/// Configuration for parallel parsing
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Number of threads to use (0 = auto)
    pub num_threads: usize,
    /// Whether to stop on first error
    pub fail_fast: bool,
    /// Maximum files per thread for load balancing
    pub chunk_size: usize,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        Self {
            num_threads: 0, // Auto-detect
            fail_fast: false,
            chunk_size: 4,
        }
    }
}

/// Parallel parser for batch file parsing
pub struct ParallelParser<T, N, K>
where
    T: Token,
    N: NonTerminal,
    K: SyntaxKind,
{
    /// The shared grammar
    grammar: Arc<Grammar<T, N>>,
    /// Configuration
    #[allow(dead_code)]
    config: ParallelConfig,
    /// Phantom data for K
    _marker: std::marker::PhantomData<K>,
}

impl<T, N, K> ParallelParser<T, N, K>
where
    T: Token + Send + Sync,
    N: NonTerminal + Send + Sync + Clone,
    K: SyntaxKind + Send + Sync,
{
    /// Create a new parallel parser
    #[must_use]
    pub fn new(grammar: Arc<Grammar<T, N>>) -> Self {
        Self {
            grammar,
            config: ParallelConfig::default(),
            _marker: std::marker::PhantomData,
        }
    }

    /// Create a new parallel parser with configuration
    #[must_use]
    pub fn with_config(grammar: Arc<Grammar<T, N>>, config: ParallelConfig) -> Self {
        Self {
            grammar,
            config,
            _marker: std::marker::PhantomData,
        }
    }

    /// Get the grammar
    #[must_use]
    pub fn grammar(&self) -> &Arc<Grammar<T, N>> {
        &self.grammar
    }

    /// Parse a batch of files in parallel
    #[cfg(feature = "parallel")]
    pub fn parse_batch<L, F>(
        &self,
        batch: &ParseBatch<N>,
        lexer_fn: L,
        parse_fn: F,
    ) -> Vec<FileParseResult<K>>
    where
        L: FnMut(&str) -> Result<Vec<LexerToken<K>>, Vec<crate::error::LexerError>>
            + Send
            + Sync
            + Clone,
        F: Fn(&Grammar<T, N>, &[LexerToken<K>], &N) -> (Option<Arc<GreenNode<K>>>, Vec<ParseError>)
            + Send
            + Sync,
    {
        batch
            .files
            .par_iter()
            .map(|(file_id, content, entry)| {
                let start = std::time::Instant::now();
                let mut local_lexer = lexer_fn.clone();

                // Tokenize
                match local_lexer(content) {
                    Ok(tokens) => {
                        // Parse
                        let (tree, errors) = parse_fn(&self.grammar, &tokens, entry);
                        let duration = start.elapsed();

                        if let Some(root) = tree {
                            if errors.is_empty() {
                                FileParseResult::success(file_id.clone(), root, duration)
                            } else {
                                FileParseResult {
                                    file_id: file_id.clone(),
                                    tree: Some(root),
                                    errors,
                                    duration,
                                }
                            }
                        } else {
                            FileParseResult::failure(file_id.clone(), errors, duration)
                        }
                    }
                    Err(lex_errors) => {
                        let errors = lex_errors
                            .iter()
                            .map(ParseError::from_lexer_error)
                            .collect();
                        FileParseResult::failure(file_id.clone(), errors, start.elapsed())
                    }
                }
            })
            .collect()
    }

    /// Parse a batch with progress reporting
    #[cfg(feature = "parallel")]
    pub fn parse_batch_with_progress<L, F>(
        &self,
        batch: &ParseBatch<N>,
        lexer_fn: L,
        parse_fn: F,
        progress: ProgressCallback,
    ) -> Vec<FileParseResult<K>>
    where
        L: FnMut(&str) -> Result<Vec<LexerToken<K>>, Vec<crate::error::LexerError>>
            + Send
            + Sync
            + Clone,
        F: Fn(&Grammar<T, N>, &[LexerToken<K>], &N) -> (Option<Arc<GreenNode<K>>>, Vec<ParseError>)
            + Send
            + Sync,
    {
        let total = batch.len();
        let completed = std::sync::atomic::AtomicUsize::new(0);

        batch
            .files
            .par_iter()
            .map(|(file_id, content, entry)| {
                let start = std::time::Instant::now();
                let mut lexer = lexer_fn.clone();

                // Tokenize
                let result = match lexer(content) {
                    Ok(tokens) => {
                        let (tree, errors) = parse_fn(&self.grammar, &tokens, entry);
                        let duration = start.elapsed();

                        if let Some(root) = tree {
                            if errors.is_empty() {
                                FileParseResult::success(file_id.clone(), root, duration)
                            } else {
                                FileParseResult {
                                    file_id: file_id.clone(),
                                    tree: Some(root),
                                    errors,
                                    duration,
                                }
                            }
                        } else {
                            FileParseResult::failure(file_id.clone(), errors, duration)
                        }
                    }
                    Err(lex_errors) => {
                        let errors = lex_errors
                            .iter()
                            .map(ParseError::from_lexer_error)
                            .collect();
                        FileParseResult::failure(file_id.clone(), errors, start.elapsed())
                    }
                };

                // Report progress
                let done = completed.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
                progress(done, total);

                result
            })
            .collect()
    }

    /// Sequential fallback when parallel feature is disabled
    #[cfg(not(feature = "parallel"))]
    pub fn parse_batch<L, F>(
        &self,
        batch: &ParseBatch<N>,
        mut lexer_fn: L,
        parse_fn: F,
    ) -> Vec<FileParseResult<K>>
    where
        L: FnMut(&str) -> Result<Vec<LexerToken<K>>, Vec<crate::error::LexerError>>,
        F: Fn(&Grammar<T, N>, &[LexerToken<K>], &N) -> (Option<Arc<GreenNode<K>>>, Vec<ParseError>),
    {
        batch
            .files
            .iter()
            .map(|(file_id, content, entry)| {
                let start = std::time::Instant::now();

                match lexer_fn(content) {
                    Ok(tokens) => {
                        let (tree, errors) = parse_fn(&self.grammar, &tokens, entry);
                        let duration = start.elapsed();

                        if let Some(root) = tree {
                            FileParseResult {
                                file_id: file_id.clone(),
                                tree: Some(root),
                                errors,
                                duration,
                            }
                        } else {
                            FileParseResult::failure(file_id.clone(), errors, duration)
                        }
                    }
                    Err(lex_errors) => {
                        let errors = lex_errors
                            .iter()
                            .map(ParseError::from_lexer_error)
                            .collect();
                        FileParseResult::failure(file_id.clone(), errors, start.elapsed())
                    }
                }
            })
            .collect()
    }
}

/// Aggregate results from parallel parsing
pub fn aggregate_results<K: SyntaxKind>(results: &[FileParseResult<K>]) -> ParseSummary {
    let total_files = results.len();
    let successful = results.iter().filter(|r| r.is_ok()).count();
    let failed = total_files - successful;
    let total_errors: usize = results.iter().map(|r| r.errors.len()).sum();
    let total_duration: std::time::Duration = results.iter().map(|r| r.duration).sum();
    let avg_duration = if total_files > 0 {
        total_duration / total_files as u32
    } else {
        std::time::Duration::ZERO
    };

    ParseSummary {
        total_files,
        successful,
        failed,
        total_errors,
        total_duration,
        avg_duration,
    }
}

/// Summary of batch parsing results
#[derive(Debug, Clone)]
pub struct ParseSummary {
    /// Total number of files parsed
    pub total_files: usize,
    /// Number of files parsed successfully
    pub successful: usize,
    /// Number of files that failed to parse
    pub failed: usize,
    /// Total number of errors across all files
    pub total_errors: usize,
    /// Total parsing duration
    pub total_duration: std::time::Duration,
    /// Average parsing duration per file
    pub avg_duration: std::time::Duration,
}

impl ParseSummary {
    /// Get the success rate as a percentage
    #[must_use]
    pub fn success_rate(&self) -> f64 {
        if self.total_files == 0 {
            100.0
        } else {
            (self.successful as f64 / self.total_files as f64) * 100.0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
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
    fn test_parse_batch_new() {
        let batch: ParseBatch<String> = ParseBatch::new();
        assert!(batch.is_empty());
    }

    #[test]
    fn test_parse_batch_add() {
        let mut batch: ParseBatch<String> = ParseBatch::new();
        batch.add("file1.txt", "content", "entry".to_string());

        assert_eq!(batch.len(), 1);
        assert!(!batch.is_empty());
    }

    #[test]
    fn test_file_parse_result_success() {
        use crate::syntax::{GreenElement, GreenNode, GreenToken, TextSize};

        let token = GreenToken::new(TestKind::Ident, "x");
        let node = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token)],
            TextSize::from(1),
        );

        let result = FileParseResult::success(
            "test.txt".to_string(),
            node,
            std::time::Duration::from_millis(10),
        );

        assert!(result.is_ok());
        assert!(result.tree.is_some());
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_aggregate_results() {
        use crate::syntax::{GreenElement, GreenNode, GreenToken, TextSize};

        let token = GreenToken::new(TestKind::Ident, "x");
        let node = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token)],
            TextSize::from(1),
        );

        let results = vec![
            FileParseResult::success(
                "a.txt".to_string(),
                node.clone(),
                std::time::Duration::from_millis(10),
            ),
            FileParseResult::failure(
                "b.txt".to_string(),
                vec![],
                std::time::Duration::from_millis(5),
            ),
        ];

        let summary = aggregate_results(&results);

        assert_eq!(summary.total_files, 2);
        assert_eq!(summary.successful, 1);
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.success_rate(), 50.0);
    }
}
