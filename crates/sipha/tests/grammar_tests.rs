//! Tests for grammar builder and validation

use sipha::grammar::{GrammarBuilder, Expr, Token, NonTerminal};
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        .rule(TestNonTerminal::Expr, Expr::token(create_token(TestSyntaxKind::Number)))
        .build();
    
    assert!(grammar.is_ok(), "Should build simple grammar");
}

#[test]
fn test_grammar_builder_choice() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::Choice(vec![
            Expr::token(create_token(TestSyntaxKind::Number)),
            Expr::Seq(vec![
                Expr::token(create_token(TestSyntaxKind::Number)),
                Expr::token(create_token(TestSyntaxKind::Plus)),
                Expr::token(create_token(TestSyntaxKind::Number)),
            ]),
        ]))
        .build();
    
    assert!(grammar.is_ok(), "Should build grammar with choice");
}

#[test]
fn test_grammar_builder_optional() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::Opt(Box::new(
            Expr::token(create_token(TestSyntaxKind::Number))
        )))
        .build();
    
    assert!(grammar.is_ok(), "Should build grammar with optional");
}

#[test]
fn test_grammar_builder_repeat() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::Repeat {
            expr: Box::new(Expr::token(create_token(TestSyntaxKind::Number))),
            min: 0,
            max: None,
        })
        .build();
    
    assert!(grammar.is_ok(), "Should build grammar with repeat");
}

#[test]
fn test_grammar_entry_point() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::token(create_token(TestSyntaxKind::Number)))
        .build()
        .expect("Failed to build grammar");
    
    assert_eq!(grammar.entry_point(), &TestNonTerminal::Expr);
}

#[test]
fn test_grammar_get_rule() {
    let grammar = GrammarBuilder::new()
        .entry_point(TestNonTerminal::Expr)
        .rule(TestNonTerminal::Expr, Expr::token(create_token(TestSyntaxKind::Number)))
        .build()
        .expect("Failed to build grammar");
    
    let rule = grammar.get_rule(&TestNonTerminal::Expr);
    assert!(rule.is_some(), "Should find rule for entry point");
}

