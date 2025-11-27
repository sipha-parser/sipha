use crate::syntax::{SyntaxKind, TextSize};
use compact_str::CompactString;
use std::sync::Arc;

/// Immutable, shareable green tree node
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenNode<K: SyntaxKind> {
    kind: K,
    text_len: TextSize,
    children: GreenChildren<K>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum GreenChildren<K: SyntaxKind> {
    Empty,
    One(GreenElement<K>),
    Many(Arc<[GreenElement<K>]>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GreenElement<K: SyntaxKind> {
    Node(Arc<GreenNode<K>>),
    Token(GreenToken<K>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken<K: SyntaxKind> {
    kind: K,
    text: CompactString, // Stack-allocated for short tokens
}

impl<K: SyntaxKind> GreenNode<K> {
    /// Create a new green node.
    ///
    /// # Panics
    ///
    /// Panics if the iterator reports a length of 1 but `next()` returns `None`.
    #[must_use]
    pub fn new<I>(kind: K, children: I, text_len: TextSize) -> Arc<Self>
    where
        I: IntoIterator<Item = GreenElement<K>>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut iter = children.into_iter();
        let len = iter.len();
        let children = match len {
            0 => GreenChildren::Empty,
            1 => GreenChildren::One(iter.next().unwrap()),
            _ => GreenChildren::Many(Arc::from(iter.collect::<Vec<_>>())),
        };

        Arc::new(Self {
            kind,
            text_len,
            children,
        })
    }

    #[inline]
    #[must_use]
    pub const fn kind(&self) -> K {
        self.kind
    }

    #[inline]
    #[must_use]
    pub const fn text_len(&self) -> TextSize {
        self.text_len
    }

    #[inline]
    #[must_use]
    pub fn children(&self) -> &[GreenElement<K>] {
        match &self.children {
            GreenChildren::Empty => &[],
            GreenChildren::One(child) => std::slice::from_ref(child),
            GreenChildren::Many(children) => children,
        }
    }

    #[must_use]
    pub const fn is_leaf(&self) -> bool {
        matches!(self.children, GreenChildren::Empty)
    }
}

impl<K: SyntaxKind> GreenToken<K> {
    #[must_use]
    pub fn new(kind: K, text: impl Into<CompactString>) -> Self {
        Self {
            kind,
            text: text.into(),
        }
    }

    #[inline]
    #[must_use]
    pub const fn kind(&self) -> K {
        self.kind
    }

    #[inline]
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    #[must_use]
    pub fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(u32::MAX))
    }
}

impl<K: SyntaxKind> GreenElement<K> {
    /// Create a green element from a node
    #[must_use]
    pub const fn node(node: Arc<GreenNode<K>>) -> Self {
        Self::Node(node)
    }

    /// Create a green element from a token
    #[must_use]
    pub const fn token(token: GreenToken<K>) -> Self {
        Self::Token(token)
    }

    #[must_use]
    pub fn kind(&self) -> K {
        match self {
            Self::Node(n) => n.kind(),
            Self::Token(t) => t.kind(),
        }
    }

    pub fn text_len(&self) -> TextSize {
        match self {
            Self::Node(n) => n.text_len(),
            Self::Token(t) => t.text_len(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::SyntaxKind;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestKind {
        Root,
        Expr,
        Ident,
        Number,
        Plus,
        Whitespace,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Root | Self::Expr)
        }

        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
        }
    }

    #[test]
    fn test_green_token_new() {
        let token = GreenToken::new(TestKind::Ident, "hello");
        assert_eq!(token.kind(), TestKind::Ident);
        assert_eq!(token.text(), "hello");
    }

    #[test]
    fn test_green_token_text_len() {
        let token = GreenToken::new(TestKind::Ident, "hello");
        assert_eq!(token.text_len(), TextSize::from(5));
    }

    #[test]
    fn test_green_node_empty() {
        let node = GreenNode::new(
            TestKind::Root,
            Vec::<GreenElement<TestKind>>::new(),
            TextSize::zero(),
        );
        assert_eq!(node.kind(), TestKind::Root);
        assert_eq!(node.text_len(), TextSize::zero());
        assert!(node.is_leaf());
        assert_eq!(node.children().len(), 0);
    }

    #[test]
    fn test_green_node_with_one_child() {
        let token = GreenElement::Token(GreenToken::new(TestKind::Ident, "x"));
        let node = GreenNode::new(TestKind::Expr, vec![token], TextSize::from(1));

        assert_eq!(node.kind(), TestKind::Expr);
        assert_eq!(node.text_len(), TextSize::from(1));
        assert!(!node.is_leaf());
        assert_eq!(node.children().len(), 1);
        assert_eq!(node.children()[0].kind(), TestKind::Ident);
    }

    #[test]
    fn test_green_node_with_multiple_children() {
        let token1 = GreenElement::Token(GreenToken::new(TestKind::Ident, "x"));
        let token2 = GreenElement::Token(GreenToken::new(TestKind::Plus, "+"));
        let token3 = GreenElement::Token(GreenToken::new(TestKind::Number, "42"));

        let node = GreenNode::new(
            TestKind::Expr,
            vec![token1, token2, token3],
            TextSize::from(4), // "x+42"
        );

        assert_eq!(node.kind(), TestKind::Expr);
        assert_eq!(node.text_len(), TextSize::from(4));
        assert!(!node.is_leaf());
        assert_eq!(node.children().len(), 3);
    }

    #[test]
    fn test_green_node_nested() {
        let inner_token = GreenElement::Token(GreenToken::new(TestKind::Number, "42"));
        let inner_node = GreenNode::new(TestKind::Expr, vec![inner_token], TextSize::from(2));

        let outer_token = GreenElement::Token(GreenToken::new(TestKind::Plus, "+"));
        let outer_node = GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Node(inner_node), outer_token],
            TextSize::from(3),
        );

        assert_eq!(outer_node.kind(), TestKind::Root);
        assert_eq!(outer_node.children().len(), 2);

        match &outer_node.children()[0] {
            GreenElement::Node(n) => {
                assert_eq!(n.kind(), TestKind::Expr);
                assert_eq!(n.children().len(), 1);
            }
            GreenElement::Token(_) => panic!("Expected node"),
        }
    }

    #[test]
    fn test_green_element_kind() {
        let token = GreenElement::Token(GreenToken::new(TestKind::Ident, "x"));
        assert_eq!(token.kind(), TestKind::Ident);

        let node = GreenNode::new(TestKind::Expr, Vec::new(), TextSize::zero());
        let element = GreenElement::Node(node);
        assert_eq!(element.kind(), TestKind::Expr);
    }

    #[test]
    fn test_green_element_text_len() {
        let token = GreenElement::Token(GreenToken::new(TestKind::Ident, "hello"));
        assert_eq!(token.text_len(), TextSize::from(5));

        let node = GreenNode::new(TestKind::Expr, vec![token], TextSize::from(5));
        let element = GreenElement::Node(node);
        assert_eq!(element.text_len(), TextSize::from(5));
    }

    #[test]
    fn test_green_node_clone_shares_arc() {
        let node = GreenNode::new(TestKind::Root, Vec::new(), TextSize::zero());
        let cloned = node.clone();

        // Both should point to the same data (Arc)
        assert_eq!(Arc::as_ptr(&node), Arc::as_ptr(&cloned));
    }

    #[test]
    fn test_green_node_equality() {
        let node1 = GreenNode::new(TestKind::Root, Vec::new(), TextSize::zero());
        let node2 = GreenNode::new(TestKind::Root, Vec::new(), TextSize::zero());

        // Two separate nodes with same content should be equal
        assert_eq!(*node1, *node2);
    }
}
