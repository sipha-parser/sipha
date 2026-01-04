//! Additional tests for incremental cache functionality

use sipha::grammar::Token;
use sipha::incremental::cache::{ContentCacheKey, IncrementalCache};
use sipha::syntax::{GreenElement, GreenNode, GreenToken, SyntaxKind, TextSize};
use lasso::Key;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TestToken {
    kind: TestKind,
}

impl Token for TestToken {
    type Kind = TestKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(1)
    }

    fn text(&self) -> compact_str::CompactString {
        "x".into()
    }
}

fn make_node() -> std::sync::Arc<GreenNode<TestKind>> {
    let token = GreenToken::new(TestKind::Ident, "x");
    GreenNode::new(
        TestKind::Root,
        vec![GreenElement::Token(token)],
        TextSize::from(1),
    )
}

/// Test ContentCacheKey::from_tokens creates consistent keys
#[test]
fn test_content_cache_key_from_tokens() {
    let rule = lasso::Spur::try_from_usize(1).unwrap();
    let context = 100;

    let tokens1 = vec![
        TestToken { kind: TestKind::Ident },
        TestToken { kind: TestKind::Ident },
    ];
    let tokens2 = vec![
        TestToken { kind: TestKind::Ident },
        TestToken { kind: TestKind::Ident },
    ];
    let tokens3 = vec![TestToken { kind: TestKind::Ident }];

    let key1 = ContentCacheKey::from_tokens(rule, &tokens1, context);
    let key2 = ContentCacheKey::from_tokens(rule, &tokens2, context);
    let key3 = ContentCacheKey::from_tokens(rule, &tokens3, context);

    // Same tokens should produce same key
    assert_eq!(key1, key2, "Same tokens should produce same cache key");
    
    // Different tokens should produce different key
    assert_ne!(key1, key3, "Different tokens should produce different cache key");
}

/// Test ContentCacheKey::from_tokens with empty token list
#[test]
fn test_content_cache_key_from_empty_tokens() {
    let rule = lasso::Spur::try_from_usize(1).unwrap();
    let context = 100;
    let tokens: Vec<TestToken> = vec![];

    let key = ContentCacheKey::from_tokens(rule, &tokens, context);
    
    // Should not panic and create a valid key
    assert_eq!(key.rule, rule);
    assert_eq!(key.context_hash, context);
}

/// Test that cache handles different rule IDs correctly
#[test]
fn test_cache_different_rules() {
    let mut cache: IncrementalCache<TestKind> = IncrementalCache::new(100);

    let node = make_node();
    let rule1 = lasso::Spur::try_from_usize(1).unwrap();
    let rule2 = lasso::Spur::try_from_usize(2).unwrap();

    let tokens = vec![TestToken { kind: TestKind::Ident }];
    let key1 = ContentCacheKey::from_tokens(rule1, &tokens, 0);
    let key2 = ContentCacheKey::from_tokens(rule2, &tokens, 0);

    let entry = sipha::incremental::cache::CacheEntry::new(
        node.clone(),
        1,
        TextSize::from(1),
        0,
    );

    cache.insert(key1.clone(), entry.clone());
    cache.insert(key2.clone(), entry);

    // Both keys should be in cache
    assert!(cache.contains(&key1));
    assert!(cache.contains(&key2));
    assert_eq!(cache.len(), 2);
}

