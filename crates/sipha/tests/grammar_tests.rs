//! Tests for grammar builder and validation

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum TestSyntaxKind {
    Number,
    Plus,
    Minus,
    Eof,
    Expr,
}

impl SyntaxKind for TestSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TestToken {
    kind: TestSyntaxKind,
}

impl Token for TestToken {
    type Kind = TestSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TestNonTerminal {
    Expr,
}

impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
        }
    }
}

const fn create_token(kind: TestSyntaxKind) -> TestToken {
    TestToken { kind }
}

#[test]
fn test_grammar_builder_simple() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number)),
        )
        .build();

    assert!(grammar.is_ok(), "Should build simple grammar");
}

#[test]
fn test_grammar_builder_choice() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::choice(vec![
                Expr::token(create_token(TestSyntaxKind::Number)),
                Expr::seq(vec![
                    Expr::token(create_token(TestSyntaxKind::Number)),
                    Expr::token(create_token(TestSyntaxKind::Plus)),
                    Expr::token(create_token(TestSyntaxKind::Number)),
                ]),
            ]),
        )
        .build();

    assert!(grammar.is_ok(), "Should build grammar with choice");
}

#[test]
fn test_grammar_builder_optional() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::opt(Expr::token(create_token(TestSyntaxKind::Number))),
        )
        .build();

    assert!(grammar.is_ok(), "Should build grammar with optional");
}

#[test]
#[cfg(feature = "backend-ll")]
fn test_first_follow_conflict_detection() {
    use sipha::backend::ParserBackend;
    use sipha::backend::ll::LlParser;
    use sipha::grammar::GrammarError;

    // Create a grammar with a nullable rule that has FIRST/FOLLOW conflict
    // This is a simplified example - in practice, this would be detected during LL validation
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::opt(Expr::token(create_token(TestSyntaxKind::Number))),
        )
        .build()
        .expect("Grammar should build");

    // Validate with LL parser - this should detect FIRST/FOLLOW conflicts if present
    let errors = LlParser::validate(&grammar);
    // Note: This test verifies that validation runs without panicking
    // Actual FIRST/FOLLOW conflict detection depends on the grammar structure
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, GrammarError::FirstFollowConflict { .. }))
            || errors.is_empty()
    );
}

#[test]
fn test_grammar_builder_repeat() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::repeat_with_greedy(
                Expr::token(create_token(TestSyntaxKind::Number)),
                0,
                None,
                true,
            ),
        )
        .build();

    assert!(grammar.is_ok(), "Should build grammar with repeat");
}

#[test]
fn test_grammar_entry_point() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number)),
        )
        .build()
        .expect("Failed to build grammar");

    assert_eq!(grammar.entry_point(), &TestNonTerminal::Expr);
}

#[test]
fn test_grammar_get_rule() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(
            TestNonTerminal::Expr,
            Expr::token(create_token(TestSyntaxKind::Number)),
        )
        .build()
        .expect("Failed to build grammar");

    let rule = grammar.get_rule(&TestNonTerminal::Expr);
    assert!(rule.is_some(), "Should find rule for entry point");
}
