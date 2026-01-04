//! Tests for parallel parsing functionality

use sipha::grammar::{Expr, Grammar, GrammarBuilder, NonTerminal, Token};
use sipha::parser::parallel::{FileParseResult, ParallelParser, ParseBatch, aggregate_results};
use sipha::syntax::{GreenElement, GreenNode, GreenToken, SyntaxKind, TextSize};
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestKind {
    Root,
    Ident,
    Number,
}

impl SyntaxKind for TestKind {
    fn is_terminal(self) -> bool {
        matches!(self, Self::Ident | Self::Number)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestToken {
    Ident,
    Number,
}

impl Token for TestToken {
    type Kind = TestKind;

    fn kind(&self) -> Self::Kind {
        match self {
            Self::Ident => TestKind::Ident,
            Self::Number => TestKind::Number,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestNonTerminal {
    Root,
}

impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &str {
        "Root"
    }
}

fn create_test_grammar() -> Grammar<TestToken, TestNonTerminal> {
    GrammarBuilder::new()
        .entry_point(TestNonTerminal::Root)
        .rule(TestNonTerminal::Root, Expr::Empty)
        .build()
        .expect("Failed to build test grammar")
}

fn create_test_tree() -> Arc<GreenNode<TestKind>> {
    let token = GreenToken::new(TestKind::Ident, "test");
    GreenNode::new(
        TestKind::Root,
        vec![GreenElement::Token(token)],
        TextSize::from(4),
    )
}

#[test]
fn test_parse_batch_new() {
    let batch: ParseBatch<String> = ParseBatch::new();
    assert!(batch.is_empty());
    assert_eq!(batch.len(), 0);
}

#[test]
fn test_parse_batch_add() {
    let mut batch: ParseBatch<String> = ParseBatch::new();
    batch.add("file1.txt", "content1", "entry1".to_string());
    batch.add("file2.txt", "content2", "entry2".to_string());

    assert_eq!(batch.len(), 2);
    assert!(!batch.is_empty());
}

#[test]
fn test_file_parse_result_success() {
    let tree = create_test_tree();
    let result = FileParseResult::success(
        "test.txt".to_string(),
        tree,
        std::time::Duration::from_millis(10),
    );

    assert!(result.is_ok());
    assert!(result.tree.is_some());
    assert!(result.errors.is_empty());
    assert_eq!(result.file_id, "test.txt");
}

#[test]
fn test_file_parse_result_failure() {
    let result: FileParseResult<TestKind> = FileParseResult::failure(
        "test.txt".to_string(),
        vec![],
        std::time::Duration::from_millis(5),
    );

    assert!(!result.is_ok());
    assert!(result.tree.is_none());
    assert_eq!(result.file_id, "test.txt");
}

#[test]
fn test_aggregate_results() {
    let tree1 = create_test_tree();
    let tree2 = create_test_tree();

    let results = vec![
        FileParseResult::success(
            "a.txt".to_string(),
            tree1,
            std::time::Duration::from_millis(10),
        ),
        FileParseResult::success(
            "b.txt".to_string(),
            tree2,
            std::time::Duration::from_millis(5),
        ),
        FileParseResult::failure(
            "c.txt".to_string(),
            vec![],
            std::time::Duration::from_millis(3),
        ),
    ];

    let summary = aggregate_results(&results);

    assert_eq!(summary.total_files, 3);
    assert_eq!(summary.successful, 2);
    assert_eq!(summary.failed, 1);
    assert!((summary.success_rate() - 66.67).abs() < 0.1); // Allow small floating point error
    assert!(summary.total_duration.as_millis() >= 18);
}

#[test]
fn test_parallel_parser_new() {
    let grammar = create_test_grammar();
    let parser: ParallelParser<TestToken, TestNonTerminal, TestKind> = ParallelParser::new(Arc::new(grammar));
    assert!(parser.grammar().rules().next().is_some());
}

#[test]
fn test_parse_batch_sequential() {
    let grammar = create_test_grammar();
    let parser = ParallelParser::new(Arc::new(grammar));

    let mut batch = ParseBatch::new();
    batch.add("file1.txt", "content1", TestNonTerminal::Root);
    batch.add("file2.txt", "content2", TestNonTerminal::Root);

    // Mock lexer function
    let lexer_fn = |_content: &str| -> Result<
        Vec<sipha::lexer::Token<TestKind>>,
        Vec<sipha::error::LexerError>,
    > { Ok(vec![]) };

    // Mock parser function
    let parse_fn = |_grammar: &Grammar<TestToken, TestNonTerminal>,
                    _tokens: &[sipha::lexer::Token<TestKind>],
                    _entry: &TestNonTerminal|
     -> (
        Option<Arc<GreenNode<TestKind>>>,
        Vec<sipha::error::ParseError>,
    ) {
        let tree = create_test_tree();
        (Some(tree), vec![])
    };

    let results = parser.parse_batch(&batch, lexer_fn, parse_fn);

    assert_eq!(results.len(), 2);
    assert!(results.iter().all(|r| r.is_ok()));
}

#[test]
fn test_parse_batch_with_errors() {
    let grammar = create_test_grammar();
    let parser = ParallelParser::new(Arc::new(grammar));

    let mut batch = ParseBatch::new();
    batch.add("file1.txt", "content1", TestNonTerminal::Root);
    batch.add("file2.txt", "content2", TestNonTerminal::Root);

    // Mock lexer function that fails for file2
    let lexer_fn = |content: &str| -> Result<
        Vec<sipha::lexer::Token<TestKind>>,
        Vec<sipha::error::LexerError>,
    > {
        if content == "content2" {
            Err(vec![sipha::error::LexerError {
                span: sipha::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
                kind: sipha::error::LexerErrorKind::UnexpectedChar { char: 'x' },
            }])
        } else {
            Ok(vec![])
        }
    };

    // Mock parser function
    let parse_fn = |_grammar: &Grammar<TestToken, TestNonTerminal>,
                    _tokens: &[sipha::lexer::Token<TestKind>],
                    _entry: &TestNonTerminal|
     -> (
        Option<Arc<GreenNode<TestKind>>>,
        Vec<sipha::error::ParseError>,
    ) {
        let tree = create_test_tree();
        (Some(tree), vec![])
    };

    let results = parser.parse_batch(&batch, lexer_fn, parse_fn);

    assert_eq!(results.len(), 2);
    assert!(results[0].is_ok());
    assert!(!results[1].is_ok());
    assert!(!results[1].errors.is_empty());
}

#[test]
#[cfg(feature = "parallel")]
fn test_parse_batch_with_progress() {
    let grammar = create_test_grammar();
    let parser = ParallelParser::new(Arc::new(grammar));

    let mut batch = ParseBatch::new();
    batch.add("file1.txt", "content1", TestNonTerminal::Root);
    batch.add("file2.txt", "content2", TestNonTerminal::Root);
    batch.add("file3.txt", "content3", TestNonTerminal::Root);

    let lexer_fn = |_content: &str| -> Result<
        Vec<sipha::lexer::Token<TestKind>>,
        Vec<sipha::error::LexerError>,
    > { Ok(vec![]) };

    let parse_fn = |_grammar: &Grammar<TestToken, TestNonTerminal>,
                    _tokens: &[sipha::lexer::Token<TestKind>],
                    _entry: &TestNonTerminal|
     -> (
        Option<Arc<GreenNode<TestKind>>>,
        Vec<sipha::error::ParseError>,
    ) {
        let tree = create_test_tree();
        (Some(tree), vec![])
    };

    let mut progress_calls = Vec::new();
    let progress = Box::new(|completed: usize, total: usize| {
        progress_calls.push((completed, total));
    });

    let results = parser.parse_batch_with_progress(&batch, lexer_fn, parse_fn, progress);

    assert_eq!(results.len(), 3);
    assert!(progress_calls.len() >= 3); // Progress should be called for each file
    assert!(progress_calls.iter().all(|(c, t)| *c <= *t && *t == 3));
}
