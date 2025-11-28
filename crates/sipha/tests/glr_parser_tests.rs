//! Tests for GLR parser backend

#[cfg(feature = "backend-glr")]
use proptest::prelude::*;
#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
#[cfg(feature = "backend-glr")]
use sipha::backend::ParserBackend;
#[cfg(feature = "backend-glr")]
use sipha::backend::glr::{ForestNode, GlrConfig, GlrParser, GlrStack, ParseForest};
#[cfg(feature = "backend-glr")]
use sipha::grammar::{
    Associativity, Expr, Grammar, GrammarBuilder, NonTerminal, PrecedenceHint, Token,
};
#[cfg(feature = "backend-glr")]
use sipha::incremental::{IncrementalSession, TextEdit};
#[cfg(feature = "backend-glr")]
use sipha::syntax::{GreenNode, SyntaxKind, TextRange, TextSize};
#[cfg(feature = "backend-glr")]
use std::sync::Arc;

#[cfg(feature = "backend-glr")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[allow(dead_code)]
enum TestSyntaxKind {
    // Terminals
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals
    Expr,
    Term,
    Factor,
}

#[cfg(feature = "backend-glr")]
impl SyntaxKind for TestSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term | Self::Factor)
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[cfg(feature = "backend-glr")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TestToken {
    kind: TestSyntaxKind,
    text: compact_str::CompactString,
}

#[cfg(feature = "backend-glr")]
impl TestToken {
    fn new(kind: TestSyntaxKind, text: &str) -> Self {
        Self {
            kind,
            text: text.into(),
        }
    }
}

#[cfg(feature = "backend-glr")]
impl Token for TestToken {
    type Kind = TestSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(u32::MAX))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
    }
}

#[cfg(feature = "backend-glr")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum TestNonTerminal {
    Expr,
    Term,
    Factor,
}

#[cfg(feature = "backend-glr")]
impl NonTerminal for TestNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Expr => "Expr",
            Self::Term => "Term",
            Self::Factor => "Factor",
        }
    }

    fn to_syntax_kind<K: SyntaxKind>(&self) -> Option<K> {
        None
    }

    fn default_syntax_kind<K: SyntaxKind>(&self) -> Option<K> {
        None
    }
}

#[cfg(feature = "backend-glr")]
fn build_expr_grammar() -> Grammar<TestToken, TestNonTerminal> {
    let mut builder = GrammarBuilder::new()
        .allow_left_recursion()
        .entry_point(TestNonTerminal::Expr);
    builder = builder.rule_with(
        TestNonTerminal::Expr,
        Expr::Seq(vec![
            Expr::Rule(TestNonTerminal::Expr),
            Expr::Token(TestToken::new(TestSyntaxKind::Plus, "+")),
            Expr::Rule(TestNonTerminal::Expr),
        ]),
        |meta| {
            meta.add_hint(PrecedenceHint {
                precedence: 30,
                associativity: Associativity::Left,
            });
        },
    );
    builder = builder.rule_with(
        TestNonTerminal::Expr,
        Expr::Seq(vec![
            Expr::Rule(TestNonTerminal::Expr),
            Expr::Token(TestToken::new(TestSyntaxKind::Minus, "-")),
            Expr::Rule(TestNonTerminal::Expr),
        ]),
        |meta| {
            meta.add_hint(PrecedenceHint {
                precedence: 30,
                associativity: Associativity::Left,
            });
        },
    );
    builder = builder.rule_with(
        TestNonTerminal::Expr,
        Expr::Seq(vec![
            Expr::Rule(TestNonTerminal::Expr),
            Expr::Token(TestToken::new(TestSyntaxKind::Multiply, "*")),
            Expr::Rule(TestNonTerminal::Expr),
        ]),
        |meta| {
            meta.add_hint(PrecedenceHint {
                precedence: 10,
                associativity: Associativity::Left,
            });
        },
    );
    builder = builder.rule(
        TestNonTerminal::Expr,
        Expr::Token(TestToken::new(TestSyntaxKind::Number, "n")),
    );
    builder.build().expect("valid grammar")
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_glr_parser_creation() {
    let mut builder = GrammarBuilder::new().entry_point(TestNonTerminal::Expr);
    builder = builder.rule(
        TestNonTerminal::Expr,
        Expr::Token(TestToken::new(TestSyntaxKind::Number, "1")),
    );

    let grammar = builder.build().expect("valid grammar");
    let config = GlrConfig::default();
    let parser = GlrParser::new(&grammar, config);

    assert!(parser.is_ok());
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_parse_forest_api() {
    // Create a simple forest with one unambiguous node
    let mut forest = ParseForest::new();
    let node = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    forest.add_root(ForestNode::Node(node.clone()));

    assert!(!forest.is_ambiguous());
    assert_eq!(forest.root_count(), 1);
    assert_eq!(forest.count_alternatives(), 1);
    assert_eq!(forest.first_root(), Some(&node));
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_parse_forest_ambiguous() {
    // Create a forest with ambiguous alternatives
    let mut forest = ParseForest::new();
    let node1 = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    let node2 = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    forest.add_root(ForestNode::Ambiguous(vec![node1, node2]));

    assert!(forest.is_ambiguous());
    assert_eq!(forest.root_count(), 1);
    assert_eq!(forest.count_alternatives(), 2);
}

#[test]
#[cfg(all(feature = "backend-glr", feature = "serialize"))]
fn test_parse_forest_serialization_round_trip() {
    let mut forest = ParseForest::new();
    let node = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(2));
    forest.add_root(ForestNode::Node(node));

    let json = serde_json::to_string(&forest).expect("serialize forest");
    let restored: ParseForest<TestSyntaxKind> =
        serde_json::from_str(&json).expect("deserialize forest");

    assert_eq!(restored.root_count(), 1);
    assert!(!restored.is_ambiguous());
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_forest_node_api() {
    // Test unambiguous node
    let node = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    let forest_node = ForestNode::Node(node.clone());

    assert!(!forest_node.is_ambiguous());
    assert_eq!(forest_node.count_alternatives(), 1);
    assert_eq!(forest_node.as_node(), Some(&node));
    assert_eq!(forest_node.get_alternative(0), Some(&node));
    assert_eq!(forest_node.get_alternative(1), None);

    // Test ambiguous node
    let node2 = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    let ambiguous_node = ForestNode::Ambiguous(vec![node.clone(), node2.clone()]);

    assert!(ambiguous_node.is_ambiguous());
    assert_eq!(ambiguous_node.count_alternatives(), 2);
    assert_eq!(ambiguous_node.as_node(), None);
    assert_eq!(ambiguous_node.get_alternative(0), Some(&node));
    assert_eq!(ambiguous_node.get_alternative(1), Some(&node2));
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_glr_incremental_reuses_stack_cache() {
    let grammar = build_expr_grammar();
    let mut parser = GlrParser::new(&grammar, GlrConfig::default()).expect("parser");
    let entry = TestNonTerminal::Expr;

    let original_tokens = vec![
        TestToken::new(TestSyntaxKind::Number, "1"),
        TestToken::new(TestSyntaxKind::Plus, "+"),
        TestToken::new(TestSyntaxKind::Number, "2"),
    ];
    let initial_result = parser.parse(&original_tokens, entry.clone());
    assert!(initial_result.errors.is_empty());

    let edits = vec![TextEdit::replace(
        TextRange::new(TextSize::from(2), TextSize::from(3)),
        "3",
    )];
    let session = IncrementalSession::new(Some(&initial_result.root), &edits);

    let updated_tokens = vec![
        TestToken::new(TestSyntaxKind::Number, "1"),
        TestToken::new(TestSyntaxKind::Plus, "+"),
        TestToken::new(TestSyntaxKind::Number, "3"),
    ];
    let updated_result = parser.parse_with_session(&updated_tokens, entry, &session);

    assert!(updated_result.errors.is_empty());
    assert_eq!(updated_result.metrics.cache_hits, 1);
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_glr_capabilities() {
    let caps = GlrParser::<TestToken, TestNonTerminal>::capabilities();
    assert_eq!(caps.name, "GLR");
    assert!(caps.supports_ambiguity);
    assert!(caps.supports_incremental); // Should be true after our implementation
    assert!(caps.supports_left_recursion);
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_glr_config_default() {
    let config = GlrConfig::default();
    assert_eq!(config.max_stacks, 1000);
    assert!(!config.return_forest);
    assert_eq!(config.max_ambiguity_depth, 10);
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_stack_merging() {
    use sipha::backend::glr::GlrStack;

    // Create two identical stacks
    let mut stack1 = GlrStack::with_initial_state(0);
    let node = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    stack1.push(1, vec![node]);

    let stack2 = stack1.fork();
    assert!(stack1.can_merge(&stack2));
    assert!(stack1.is_identical(&stack2));

    // Merge stacks
    let mut stack3 = stack1.clone();
    stack3.merge(&stack2);
    assert_eq!(stack3.len(), stack1.len());
}

#[test]
#[cfg(feature = "backend-glr")]
fn test_stack_common_prefix() {
    use sipha::backend::glr::GlrStack;

    let mut stack1 = GlrStack::with_initial_state(0);
    let node1 = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    stack1.push(1, vec![node1.clone()]);

    let mut stack2 = GlrStack::with_initial_state(0);
    stack2.push(1, vec![node1]);
    let node2 = GreenNode::new(TestSyntaxKind::Number, vec![], TextSize::from(1));
    stack2.push(2, vec![node2]);

    assert_eq!(stack1.shared_prefix_len(&stack2), 2); // Both have state 0 and state 1
}

#[cfg(feature = "backend-glr")]
mod property_tests {
    use super::*;
    use proptest::collection::vec;

    fn prefix_strategy() -> impl Strategy<Value = Vec<(u16, u8)>> {
        vec((0u16..32, 0u8..3), 0..4)
    }

    fn tail_strategy() -> impl Strategy<Value = Vec<(u16, u8)>> {
        vec((32u16..96, 0u8..3), 0..4)
    }

    fn build_prefix_entries(
        ops: &[(u16, u8)],
    ) -> Vec<(usize, Vec<Arc<GreenNode<TestSyntaxKind>>>)> {
        ops.iter()
            .enumerate()
            .map(|(idx, (state, count))| {
                let nodes = (0..*count)
                    .map(|node_idx| make_leaf(u32::try_from(idx).unwrap_or(u32::MAX), node_idx))
                    .collect();
                (usize::from(*state), nodes)
            })
            .collect()
    }

    fn make_leaf(tag: u32, idx: u8) -> Arc<GreenNode<TestSyntaxKind>> {
        let len = TextSize::from(tag + u32::from(idx) + 1);
        GreenNode::new(TestSyntaxKind::Factor, vec![], len)
    }

    fn apply_tail(stack: &mut GlrStack<TestSyntaxKind>, tail: &[(u16, u8)], tag_offset: u32) {
        for (idx, (state, count)) in tail.iter().enumerate() {
            let nodes = (0..*count)
                .map(|node_idx| {
                    make_leaf(
                        tag_offset + u32::try_from(idx).unwrap_or(u32::MAX) * 10,
                        node_idx,
                    )
                })
                .collect();
            stack.push(usize::from(*state), nodes);
        }
    }

    fn stack_entries(
        stack: &GlrStack<TestSyntaxKind>,
    ) -> Vec<(usize, Vec<Arc<GreenNode<TestSyntaxKind>>>)> {
        let mut clone = stack.clone();
        let mut entries = Vec::new();
        while !clone.is_empty() {
            if let Some(entry) = clone.top_entry() {
                entries.push(entry);
            }
            clone.pop(1);
        }
        entries.reverse();
        entries
    }

    proptest! {
        #[test]
        fn shared_prefix_len_matches_common_prefix(
            prefix in prefix_strategy(),
            tail_a in tail_strategy(),
            tail_b in tail_strategy(),
        ) {
            let prefix_entries = build_prefix_entries(&prefix);

            let mut stack_a = GlrStack::with_initial_state(0);
            let mut stack_b = GlrStack::with_initial_state(0);
            for (state, nodes) in &prefix_entries {
                stack_a.push(*state, nodes.clone());
                stack_b.push(*state, nodes.clone());
            }

            apply_tail(&mut stack_a, &tail_a, 100);
            apply_tail(&mut stack_b, &tail_b, 200);

            let entries_a = stack_entries(&stack_a);
            let entries_b = stack_entries(&stack_b);
            let mut expected_prefix = 0;
            for (entry_a, entry_b) in entries_a.iter().zip(entries_b.iter()) {
                if entry_a.0 != entry_b.0 {
                    break;
                }
                if entry_a.1.len() != entry_b.1.len() {
                    break;
                }
                let nodes_match = entry_a
                    .1
                    .iter()
                    .zip(entry_b.1.iter())
                    .all(|(n1, n2)| Arc::ptr_eq(n1, n2));
                if !nodes_match {
                    break;
                }
                expected_prefix += 1;
            }
            let prefix_len = stack_a.shared_prefix_len(&stack_b);
            prop_assert_eq!(prefix_len, expected_prefix);
            prop_assert_eq!(stack_a.shared_prefix_len(&stack_b), stack_b.shared_prefix_len(&stack_a));
        }
    }

    fn addition_sequence() -> impl Strategy<Value = Vec<u32>> {
        vec(1u32..50, 1..6)
    }

    fn tokens_from_values(values: &[u32]) -> Vec<TestToken> {
        let mut tokens = Vec::new();
        for (idx, value) in values.iter().enumerate() {
            if idx > 0 {
                tokens.push(TestToken::new(TestSyntaxKind::Plus, "+"));
            }
            tokens.push(TestToken::new(TestSyntaxKind::Number, &value.to_string()));
        }
        tokens
    }

    proptest! {
        #[test]
        fn glr_parser_handles_random_additions(values in addition_sequence()) {
            let grammar = build_expr_grammar();
            let mut parser = GlrParser::new(&grammar, GlrConfig::default()).expect("parser");
            let tokens = tokens_from_values(&values);
            let result = parser.parse(&tokens, TestNonTerminal::Expr);
            prop_assert!(result.errors.is_empty());
            prop_assert!(result.warnings.is_empty());
        }
    }
}
