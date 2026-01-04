use crate::syntax::{SyntaxKind, TextSize};
use compact_str::CompactString;
#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::sync::Arc;

/// Immutable, shareable green tree node
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct GreenNode<K: SyntaxKind> {
    kind: K,
    text_len: TextSize,
    children: GreenChildren<K>,
}

/// Children storage optimized for different sizes
///
/// This enum provides specialized storage for common cases:
/// - Empty: No allocation needed
/// - One: Single child stored inline
/// - Inline: Small number of children stored inline (no heap allocation)
/// - Many: Large number of children stored in Arc for sharing
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
enum GreenChildren<K: SyntaxKind> {
    /// No children
    Empty,
    /// Single child (common for wrapper nodes)
    One(GreenElement<K>),
    /// 2-8 children stored inline without heap allocation
    Inline(SmallVec<[GreenElement<K>; 8]>),
    /// Many children stored in Arc for sharing
    Many(Arc<[GreenElement<K>]>),
}

/// Structure of Arrays (SoA) layout for cache-friendly child iteration
///
/// This structure stores child metadata in parallel arrays for better
/// cache locality when iterating over kinds or text lengths.
#[derive(Debug, Clone)]
pub struct CompactChildren<K: SyntaxKind> {
    /// Kinds of all children (cache-friendly for kind-based filtering)
    pub kinds: SmallVec<[K; 8]>,
    /// Text lengths of all children
    pub text_lens: SmallVec<[TextSize; 8]>,
    /// The full elements (for when you need the actual data)
    elements: SmallVec<[GreenElement<K>; 8]>,
}

impl<K: SyntaxKind> CompactChildren<K> {
    /// Create empty compact children
    #[must_use]
    pub fn new() -> Self {
        Self {
            kinds: SmallVec::new(),
            text_lens: SmallVec::new(),
            elements: SmallVec::new(),
        }
    }

    /// Create from a slice of elements
    #[must_use]
    pub fn from_elements(elements: &[GreenElement<K>]) -> Self {
        let kinds: SmallVec<[K; 8]> = elements.iter().map(GreenElement::kind).collect();
        let text_lens: SmallVec<[TextSize; 8]> =
            elements.iter().map(GreenElement::text_len).collect();
        let elements: SmallVec<[GreenElement<K>; 8]> = elements.iter().cloned().collect();

        Self {
            kinds,
            text_lens,
            elements,
        }
    }

    /// Add an element
    pub fn push(&mut self, element: GreenElement<K>) {
        self.kinds.push(element.kind());
        self.text_lens.push(element.text_len());
        self.elements.push(element);
    }

    /// Get the number of children
    #[must_use]
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Check if empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    /// Get an element by index
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&GreenElement<K>> {
        self.elements.get(index)
    }

    /// Get all elements
    #[must_use]
    pub fn elements(&self) -> &[GreenElement<K>] {
        &self.elements
    }

    /// Iterate over (kind, text_len) pairs efficiently
    pub fn iter_metadata(&self) -> impl Iterator<Item = (K, TextSize)> + '_ {
        self.kinds
            .iter()
            .copied()
            .zip(self.text_lens.iter().copied())
    }

    /// Find children with a specific kind efficiently
    pub fn find_by_kind(&self, kind: K) -> impl Iterator<Item = (usize, &GreenElement<K>)> {
        self.kinds
            .iter()
            .enumerate()
            .filter(move |(_, k)| **k == kind)
            .map(|(i, _)| (i, &self.elements[i]))
    }

    /// Compute total text length
    #[must_use]
    pub fn total_text_len(&self) -> TextSize {
        self.text_lens.iter().fold(TextSize::zero(), |acc, &len| {
            TextSize::from(u32::from(acc) + u32::from(len))
        })
    }
}

impl<K: SyntaxKind> Default for CompactChildren<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum GreenElement<K: SyntaxKind> {
    Node(Arc<GreenNode<K>>),
    Token(GreenToken<K>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken<K: SyntaxKind> {
    kind: K,
    text: CompactString, // Stack-allocated for short tokens
}

/// Threshold for switching from inline to Arc storage
const INLINE_CHILDREN_THRESHOLD: usize = 8;

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
            2..=INLINE_CHILDREN_THRESHOLD => GreenChildren::Inline(iter.collect()),
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
            GreenChildren::Inline(children) => children,
            GreenChildren::Many(children) => children,
        }
    }

    #[must_use]
    pub const fn is_leaf(&self) -> bool {
        matches!(self.children, GreenChildren::Empty)
    }

    /// Get the number of children
    #[must_use]
    pub fn child_count(&self) -> usize {
        match &self.children {
            GreenChildren::Empty => 0,
            GreenChildren::One(_) => 1,
            GreenChildren::Inline(children) => children.len(),
            GreenChildren::Many(children) => children.len(),
        }
    }

    /// Get a compact view of children for cache-friendly iteration
    #[must_use]
    pub fn compact_children(&self) -> CompactChildren<K> {
        CompactChildren::from_elements(self.children())
    }

    /// Iterate over child kinds without accessing full elements
    pub fn child_kinds(&self) -> impl Iterator<Item = K> + '_ {
        self.children().iter().map(GreenElement::kind)
    }

    /// Find the first child with the given kind
    #[must_use]
    pub fn first_child_by_kind(&self, kind: K) -> Option<&GreenElement<K>> {
        self.children().iter().find(|c| c.kind() == kind)
    }

    /// Find all children with the given kind
    pub fn children_by_kind(&self, kind: K) -> impl Iterator<Item = &GreenElement<K>> {
        self.children().iter().filter(move |c| c.kind() == kind)
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

#[cfg(feature = "serialize")]
impl<K> serde::Serialize for GreenToken<K>
where
    K: SyntaxKind + serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (self.kind, self.text.as_str()).serialize(serializer)
    }
}

#[cfg(feature = "serialize")]
impl<'de, K> serde::Deserialize<'de> for GreenToken<K>
where
    K: SyntaxKind + serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (kind, text): (K, String) = serde::Deserialize::deserialize(deserializer)?;

        Ok(Self {
            kind,
            text: text.into(),
        })
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
