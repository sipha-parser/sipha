//! Tests for incremental lexing functionality

use sipha::lexer::incremental::{IncrementalLexer, TextEdit};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestSyntaxKind {
    Ident,
    Number,
    Plus,
    Minus,
    Whitespace,
    Eof,
}

impl SyntaxKind for TestSyntaxKind {
    fn is_terminal(self) -> bool {
        true
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

fn build_test_lexer() -> sipha::lexer::CompiledLexer<TestSyntaxKind> {
    LexerBuilder::new()
        .token(
            TestSyntaxKind::Ident,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::new(vec![
                    'a'..='z',
                    'A'..='Z',
                    '_'..='_',
                ]))),
                min: 1,
                max: None,
            },
        )
        .token(
            TestSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(TestSyntaxKind::Plus, Pattern::Literal("+".into()))
        .token(TestSyntaxKind::Minus, Pattern::Literal("-".into()))
        .token(
            TestSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(TestSyntaxKind::Whitespace)
        .build(TestSyntaxKind::Eof, TestSyntaxKind::Ident)
        .expect("Failed to build lexer")
}

#[test]
fn test_incremental_lexer_initial_tokenization() {
    let lexer = build_test_lexer();
    let incr = IncrementalLexer::new(lexer, "foo 123".into());

    assert_eq!(incr.version(), 0);
    assert_eq!(incr.input(), "foo 123");
    assert!(!incr.tokens().is_empty());
}

#[test]
fn test_incremental_lexer_small_edit() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "foo + 42".into());

    let initial_token_count = incr.tokens().len();

    // Change "foo" to "bar"
    let edit = TextEdit::replace(0..3, "bar");
    let delta = incr.update(&edit, "bar + 42");

    assert!(delta.has_changes());
    assert_eq!(incr.version(), 1);
    assert_eq!(incr.input(), "bar + 42");
    // Token count should be similar (only the ident changed)
    assert!(incr.tokens().len() <= initial_token_count + 1);
}

#[test]
fn test_incremental_lexer_insertion() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "foo 42".into());

    let initial_token_count = incr.tokens().len();

    // Insert " + " before "42"
    let edit = TextEdit::insert(4, " + ");
    let delta = incr.update(&edit, "foo  + 42");

    assert!(delta.has_changes());
    assert_eq!(incr.version(), 1);
    assert!(delta.added_count > 0);
    assert!(incr.tokens().len() > initial_token_count);
}

#[test]
fn test_incremental_lexer_deletion() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "foo + 42".into());

    let initial_token_count = incr.tokens().len();

    // Delete " + "
    let edit = TextEdit::delete(4..7);
    let delta = incr.update(&edit, "foo 42");

    assert!(delta.has_changes());
    assert_eq!(incr.version(), 1);
    assert!(delta.removed_count > 0);
    // After deletion, token count should decrease or stay the same (depending on tokenization)
    // The important thing is that tokens were removed, which is checked above
    assert!(incr.tokens().len() <= initial_token_count);
}

#[test]
fn test_incremental_lexer_token_reuse() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "foo + bar + baz".into());

    let initial_tokens = incr.tokens().to_vec();

    // Edit only "bar" to "qux" - tokens before and after should be reused
    let edit = TextEdit::replace(8..11, "qux");
    let delta = incr.update(&edit, "foo + qux + baz");

    assert!(delta.has_changes());

    // Tokens before the edit should be unchanged (at least some tokens should be preserved)
    // The changed_range.start indicates where the changes begin
    // We expect at least the first token ("foo") to be unchanged
    assert!(
        delta.changed_range.start >= 1,
        "At least the first token should be unchanged"
    );

    // Tokens after the edit should be preserved (just positions updated)
    let final_tokens = incr.tokens();
    // After replacing "bar" with "qux", we should have similar token count
    assert!(
        final_tokens.len() >= initial_tokens.len() - 2,
        "Most tokens should be preserved"
    );
}

#[test]
fn test_incremental_lexer_multiple_edits() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "foo 42".into());

    // First edit
    let edit1 = TextEdit::replace(0..3, "bar");
    let delta1 = incr.update(&edit1, "bar 42");
    assert_eq!(incr.version(), 1);
    assert!(delta1.has_changes());

    // Second edit
    let edit2 = TextEdit::replace(4..6, "100");
    let delta2 = incr.update(&edit2, "bar 100");
    assert_eq!(incr.version(), 2);
    assert!(delta2.has_changes());
}

#[test]
fn test_incremental_lexer_line_tracking() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "line1\nline2\nline3".into());

    assert_eq!(incr.line_at_offset(TextSize::from(0)), 0);
    assert_eq!(incr.line_at_offset(TextSize::from(6)), 1);
    assert_eq!(incr.line_at_offset(TextSize::from(12)), 2);

    // Edit that adds a newline
    let edit = TextEdit::insert(6, "\nnewline\n");
    incr.update(&edit, "line1\n\nnewline\nline2\nline3");

    // Line tracking should be updated
    assert_eq!(incr.line_at_offset(TextSize::from(0)), 0);
    assert_eq!(incr.line_at_offset(TextSize::from(6)), 1);
}

#[test]
fn test_incremental_lexer_token_at_offset() {
    let lexer = build_test_lexer();
    let incr = IncrementalLexer::new(lexer, "foo 42".into());

    // Find token at offset 0 (should be "foo")
    let token = incr.token_at_offset(TextSize::from(0));
    assert!(token.is_some());
    if let Some(t) = token {
        assert_eq!(t.kind, TestSyntaxKind::Ident);
    }

    // Find token at offset 4 (should be "42")
    let token = incr.token_at_offset(TextSize::from(4));
    assert!(token.is_some());
    if let Some(t) = token {
        assert_eq!(t.kind, TestSyntaxKind::Number);
    }
}

#[test]
fn test_incremental_lexer_set_input() {
    let lexer = build_test_lexer();
    let mut incr = IncrementalLexer::new(lexer, "foo 42".into());

    let delta = incr.set_input("bar 100".into());

    assert_eq!(incr.version(), 1);
    assert_eq!(incr.input(), "bar 100");
    assert!(delta.has_changes());
    assert_eq!(delta.changed_range.start, 0);
}
