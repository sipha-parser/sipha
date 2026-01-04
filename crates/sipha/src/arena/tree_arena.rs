//! Tree-specific arena allocation
//!
//! This module provides arena allocation specifically designed for syntax trees,
//! with optimized storage for green nodes and tokens.

use crate::syntax::{SyntaxKind, TextSize};
use std::cell::Cell;
use std::sync::Arc;

#[cfg(feature = "arena")]
use super::BumpArena;

/// Arena-allocated green node data
///
/// This is the core data structure for green nodes when using arena allocation.
/// Unlike `GreenNode` which is always wrapped in `Arc`, this can be allocated
/// directly in an arena for better performance in batch parsing.
#[derive(Debug, Clone)]
pub struct GreenNodeData<K: SyntaxKind> {
    kind: K,
    text_len: TextSize,
    children: ChildrenData<K>,
}

/// Children storage for arena-allocated nodes
#[derive(Debug, Clone)]
pub enum ChildrenData<K: SyntaxKind> {
    /// No children (leaf node with no tokens)
    Empty,
    /// Single child (common case optimization)
    One(GreenElementData<K>),
    /// Multiple children stored inline
    Inline(smallvec::SmallVec<[GreenElementData<K>; 4]>),
    /// Multiple children stored in Arc for sharing
    Shared(Arc<[GreenElementData<K>]>),
}

/// Element in a green tree (either node or token)
#[derive(Debug, Clone)]
pub enum GreenElementData<K: SyntaxKind> {
    /// A child node
    Node(Arc<GreenNodeData<K>>),
    /// A token (leaf)
    Token(GreenTokenData<K>),
}

/// Arena-allocated green token data
#[derive(Debug, Clone)]
pub struct GreenTokenData<K: SyntaxKind> {
    kind: K,
    text: compact_str::CompactString,
}

impl<K: SyntaxKind> GreenNodeData<K> {
    /// Create a new green node data
    #[must_use]
    pub fn new<I>(kind: K, children: I, text_len: TextSize) -> Self
    where
        I: IntoIterator<Item = GreenElementData<K>>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut iter = children.into_iter();
        let len = iter.len();
        let children = match len {
            0 => ChildrenData::Empty,
            1 => ChildrenData::One(iter.next().expect("iterator length mismatch")),
            _ => ChildrenData::Inline(iter.collect()),
        };

        Self {
            kind,
            text_len,
            children,
        }
    }

    /// Create a new green node data with shared children
    #[must_use]
    pub fn with_shared_children(
        kind: K,
        children: Arc<[GreenElementData<K>]>,
        text_len: TextSize,
    ) -> Self {
        Self {
            kind,
            text_len,
            children: if children.is_empty() {
                ChildrenData::Empty
            } else if children.len() == 1 {
                ChildrenData::One(children[0].clone())
            } else {
                ChildrenData::Shared(children)
            },
        }
    }

    /// Create a leaf node with no children
    #[must_use]
    pub const fn leaf(kind: K, text_len: TextSize) -> Self {
        Self {
            kind,
            text_len,
            children: ChildrenData::Empty,
        }
    }

    /// Get the syntax kind of this node
    #[inline]
    #[must_use]
    pub const fn kind(&self) -> K {
        self.kind
    }

    /// Get the text length of this node
    #[inline]
    #[must_use]
    pub const fn text_len(&self) -> TextSize {
        self.text_len
    }

    /// Get the children of this node as a slice
    #[must_use]
    pub fn children(&self) -> &[GreenElementData<K>] {
        match &self.children {
            ChildrenData::Empty => &[],
            ChildrenData::One(child) => std::slice::from_ref(child),
            ChildrenData::Inline(children) => children,
            ChildrenData::Shared(children) => children,
        }
    }

    /// Check if this is a leaf node (no children)
    #[must_use]
    pub const fn is_leaf(&self) -> bool {
        matches!(self.children, ChildrenData::Empty)
    }

    /// Get the number of children
    #[must_use]
    pub fn child_count(&self) -> usize {
        match &self.children {
            ChildrenData::Empty => 0,
            ChildrenData::One(_) => 1,
            ChildrenData::Inline(children) => children.len(),
            ChildrenData::Shared(children) => children.len(),
        }
    }

    /// Convert to a shared (Arc-wrapped) node
    #[must_use]
    pub fn into_shared(self) -> Arc<Self> {
        Arc::new(self)
    }

    /// Create from a shared node (clone the data)
    #[must_use]
    pub fn from_shared(shared: &Arc<Self>) -> Self {
        (**shared).clone()
    }
}

impl<K: SyntaxKind> GreenTokenData<K> {
    /// Create a new green token data
    #[must_use]
    pub fn new(kind: K, text: impl Into<compact_str::CompactString>) -> Self {
        Self {
            kind,
            text: text.into(),
        }
    }

    /// Get the syntax kind of this token
    #[inline]
    #[must_use]
    pub const fn kind(&self) -> K {
        self.kind
    }

    /// Get the text of this token
    #[inline]
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Get the text length of this token
    #[must_use]
    pub fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(u32::MAX))
    }
}

impl<K: SyntaxKind> GreenElementData<K> {
    /// Create a node element
    #[must_use]
    pub const fn node(node: Arc<GreenNodeData<K>>) -> Self {
        Self::Node(node)
    }

    /// Create a token element
    #[must_use]
    pub const fn token(token: GreenTokenData<K>) -> Self {
        Self::Token(token)
    }

    /// Get the syntax kind of this element
    #[must_use]
    pub fn kind(&self) -> K {
        match self {
            Self::Node(n) => n.kind(),
            Self::Token(t) => t.kind(),
        }
    }

    /// Get the text length of this element
    #[must_use]
    pub fn text_len(&self) -> TextSize {
        match self {
            Self::Node(n) => n.text_len(),
            Self::Token(t) => t.text_len(),
        }
    }

    /// Check if this is a node
    #[must_use]
    pub const fn is_node(&self) -> bool {
        matches!(self, Self::Node(_))
    }

    /// Check if this is a token
    #[must_use]
    pub const fn is_token(&self) -> bool {
        matches!(self, Self::Token(_))
    }

    /// Get as a node reference
    #[must_use]
    pub fn as_node(&self) -> Option<&Arc<GreenNodeData<K>>> {
        match self {
            Self::Node(n) => Some(n),
            Self::Token(_) => None,
        }
    }

    /// Get as a token reference
    #[must_use]
    pub fn as_token(&self) -> Option<&GreenTokenData<K>> {
        match self {
            Self::Node(_) => None,
            Self::Token(t) => Some(t),
        }
    }
}

/// Arena for allocating green tree nodes
///
/// This arena provides efficient allocation for syntax tree nodes during parsing.
/// All allocations are freed when the arena is dropped.
pub struct TreeArena {
    #[cfg(feature = "arena")]
    bump: BumpArena,
    #[cfg(not(feature = "arena"))]
    _phantom: std::marker::PhantomData<()>,
    node_count: Cell<usize>,
    token_count: Cell<usize>,
}

impl TreeArena {
    /// Create a new tree arena
    #[must_use]
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "arena")]
            bump: BumpArena::new(),
            #[cfg(not(feature = "arena"))]
            _phantom: std::marker::PhantomData,
            node_count: Cell::new(0),
            token_count: Cell::new(0),
        }
    }

    /// Create a new tree arena with pre-allocated capacity
    #[must_use]
    pub fn with_capacity(_capacity: usize) -> Self {
        Self {
            #[cfg(feature = "arena")]
            bump: BumpArena::with_capacity(capacity),
            #[cfg(not(feature = "arena"))]
            _phantom: std::marker::PhantomData,
            node_count: Cell::new(0),
            token_count: Cell::new(0),
        }
    }

    /// Allocate a green node in the arena
    ///
    /// Returns an Arc-wrapped node for compatibility with existing code.
    /// When arena allocation is enabled, this is more efficient than
    /// individual Arc allocations.
    #[must_use]
    pub fn alloc_node<K: SyntaxKind, I>(
        &self,
        kind: K,
        children: I,
        text_len: TextSize,
    ) -> Arc<GreenNodeData<K>>
    where
        I: IntoIterator<Item = GreenElementData<K>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.node_count.set(self.node_count.get() + 1);
        Arc::new(GreenNodeData::new(kind, children, text_len))
    }

    /// Allocate a green token in the arena
    #[must_use]
    pub fn alloc_token<K: SyntaxKind>(
        &self,
        kind: K,
        text: impl Into<compact_str::CompactString>,
    ) -> GreenTokenData<K> {
        self.token_count.set(self.token_count.get() + 1);
        GreenTokenData::new(kind, text)
    }

    /// Get the number of nodes allocated
    #[must_use]
    pub fn node_count(&self) -> usize {
        self.node_count.get()
    }

    /// Get the number of tokens allocated
    #[must_use]
    pub fn token_count(&self) -> usize {
        self.token_count.get()
    }

    /// Get total allocations
    #[must_use]
    pub fn total_allocations(&self) -> usize {
        self.node_count.get() + self.token_count.get()
    }

    /// Get arena statistics
    #[cfg(feature = "arena")]
    #[must_use]
    pub fn stats(&self) -> super::ArenaStats {
        let mut stats = self.bump.stats();
        stats.nodes_allocated = self.node_count.get();
        stats.tokens_allocated = self.token_count.get();
        stats
    }

    /// Reset the arena, freeing all allocations
    #[cfg(feature = "arena")]
    pub fn reset(&mut self) {
        self.bump.reset();
        self.node_count.set(0);
        self.token_count.set(0);
    }
}

impl Default for TreeArena {
    fn default() -> Self {
        Self::new()
    }
}

/// A reference to a green node in an arena
///
/// This is a lightweight reference that doesn't involve reference counting.
/// It's tied to the lifetime of the arena.
#[derive(Clone, Copy)]
pub struct GreenNodeRef<'arena, K: SyntaxKind> {
    data: &'arena GreenNodeData<K>,
}

impl<'arena, K: SyntaxKind> GreenNodeRef<'arena, K> {
    /// Create a new node reference
    #[must_use]
    pub const fn new(data: &'arena GreenNodeData<K>) -> Self {
        Self { data }
    }

    /// Get the syntax kind
    #[inline]
    #[must_use]
    pub const fn kind(&self) -> K {
        self.data.kind()
    }

    /// Get the text length
    #[inline]
    #[must_use]
    pub const fn text_len(&self) -> TextSize {
        self.data.text_len()
    }

    /// Get the children
    #[must_use]
    pub fn children(&self) -> &'arena [GreenElementData<K>] {
        self.data.children()
    }

    /// Check if this is a leaf node
    #[must_use]
    pub const fn is_leaf(&self) -> bool {
        self.data.is_leaf()
    }

    /// Get the underlying data
    #[must_use]
    pub const fn data(&self) -> &'arena GreenNodeData<K> {
        self.data
    }
}

impl<K: SyntaxKind> std::fmt::Debug for GreenNodeRef<'_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GreenNodeRef")
            .field("kind", &self.kind())
            .field("text_len", &self.text_len())
            .field("children", &self.data.child_count())
            .finish()
    }
}

/// Type alias for shared (Arc-wrapped) green nodes
pub type SharedGreenNode<K> = Arc<GreenNodeData<K>>;

/// Builder for constructing green trees using arena allocation
pub struct ArenaTreeBuilder<'arena, K: SyntaxKind> {
    arena: &'arena TreeArena,
    stack: Vec<NodeInProgress<K>>,
}

struct NodeInProgress<K: SyntaxKind> {
    kind: K,
    children: Vec<GreenElementData<K>>,
    text_len: TextSize,
}

impl<'arena, K: SyntaxKind> ArenaTreeBuilder<'arena, K> {
    /// Create a new arena tree builder
    #[must_use]
    pub const fn new(arena: &'arena TreeArena) -> Self {
        Self {
            arena,
            stack: Vec::new(),
        }
    }

    /// Start a new node
    pub fn start_node(&mut self, kind: K) {
        self.stack.push(NodeInProgress {
            kind,
            children: Vec::new(),
            text_len: TextSize::zero(),
        });
    }

    /// Add a token to the current node
    pub fn token(&mut self, kind: K, text: impl Into<compact_str::CompactString>) {
        let text: compact_str::CompactString = text.into();
        let text_len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
        let token = self.arena.alloc_token(kind, text);

        if let Some(parent) = self.stack.last_mut() {
            parent.text_len = TextSize::from(parent.text_len.into() + text_len.into());
            parent.children.push(GreenElementData::Token(token));
        }
    }

    /// Finish the current node
    pub fn finish_node(&mut self) -> Option<Arc<GreenNodeData<K>>> {
        let node = self.stack.pop()?;
        let green = self
            .arena
            .alloc_node(node.kind, node.children, node.text_len);

        if let Some(parent) = self.stack.last_mut() {
            parent.text_len = TextSize::from(parent.text_len.into() + green.text_len().into());
            parent.children.push(GreenElementData::Node(green.clone()));
        }

        Some(green)
    }

    /// Finish building and return the root node
    pub fn finish(mut self) -> Option<Arc<GreenNodeData<K>>> {
        if self.stack.len() != 1 {
            return None;
        }

        let root = self.stack.pop()?;
        Some(
            self.arena
                .alloc_node(root.kind, root.children, root.text_len),
        )
    }

    /// Get the arena
    #[must_use]
    pub const fn arena(&self) -> &'arena TreeArena {
        self.arena
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
        Expr,
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

    #[test]
    fn test_green_node_data_new() {
        let node: GreenNodeData<TestKind> =
            GreenNodeData::new(TestKind::Root, Vec::new(), TextSize::zero());
        assert_eq!(node.kind(), TestKind::Root);
        assert_eq!(node.text_len(), TextSize::zero());
        assert!(node.is_leaf());
    }

    #[test]
    fn test_green_token_data_new() {
        let token = GreenTokenData::new(TestKind::Ident, "hello");
        assert_eq!(token.kind(), TestKind::Ident);
        assert_eq!(token.text(), "hello");
        assert_eq!(token.text_len(), TextSize::from(5));
    }

    #[test]
    fn test_tree_arena_allocation() {
        let arena = TreeArena::new();

        let token = arena.alloc_token(TestKind::Ident, "x");
        assert_eq!(token.kind(), TestKind::Ident);
        assert_eq!(token.text(), "x");

        let node = arena.alloc_node(
            TestKind::Expr,
            vec![GreenElementData::Token(token)],
            TextSize::from(1),
        );
        assert_eq!(node.kind(), TestKind::Expr);
        assert_eq!(node.child_count(), 1);

        assert_eq!(arena.node_count(), 1);
        assert_eq!(arena.token_count(), 1);
    }

    #[test]
    fn test_arena_tree_builder() {
        let arena = TreeArena::new();
        let mut builder = ArenaTreeBuilder::new(&arena);

        builder.start_node(TestKind::Root);
        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Ident, "x");
        builder.finish_node();
        let root = builder.finish().unwrap();

        assert_eq!(root.kind(), TestKind::Root);
        assert_eq!(root.child_count(), 1);
    }

    #[test]
    fn test_children_data_variants() {
        // Empty
        let empty: GreenNodeData<TestKind> =
            GreenNodeData::new(TestKind::Root, Vec::new(), TextSize::zero());
        assert!(empty.children().is_empty());

        // One child
        let token = GreenTokenData::new(TestKind::Ident, "a");
        let one: GreenNodeData<TestKind> = GreenNodeData::new(
            TestKind::Expr,
            vec![GreenElementData::Token(token)],
            TextSize::from(1),
        );
        assert_eq!(one.child_count(), 1);

        // Multiple children
        let tokens: Vec<_> = ["a", "b", "c"]
            .iter()
            .map(|s| GreenElementData::Token(GreenTokenData::new(TestKind::Ident, *s)))
            .collect();
        let multiple: GreenNodeData<TestKind> =
            GreenNodeData::new(TestKind::Root, tokens, TextSize::from(3));
        assert_eq!(multiple.child_count(), 3);
    }
}
