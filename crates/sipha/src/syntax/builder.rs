use crate::syntax::{GreenElement, GreenNode, SyntaxKind, TextSize};
use smallvec::SmallVec;
use thiserror::Error;

/// Error that can occur when building a syntax tree
#[derive(Debug, Clone, Error)]
pub enum BuilderError {
    #[error("Builder must have exactly one root node, but found {stack_size} nodes on stack")]
    InvalidStackSize { stack_size: usize },

    #[error("finish_node() called without a matching start_node()")]
    UnmatchedFinishNode,

    #[error("token() called without a parent node. Call start_node() before adding tokens")]
    TokenWithoutParent,
}

/// Builder for constructing green syntax trees
pub struct GreenNodeBuilder<K: SyntaxKind> {
    stack: SmallVec<[NodeBuilder<K>; 8]>,
}

struct NodeBuilder<K: SyntaxKind> {
    kind: K,
    children: SmallVec<[GreenElement<K>; 4]>,
    text_len: TextSize,
}

impl<K: SyntaxKind> GreenNodeBuilder<K> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            stack: SmallVec::new(),
        }
    }

    pub fn start_node(&mut self, kind: K) {
        self.stack.push(NodeBuilder {
            kind,
            children: SmallVec::new(),
            text_len: TextSize::zero(),
        });
    }

    /// Finish the current node and add it to its parent.
    ///
    /// # Errors
    ///
    /// Returns an error if there's no node on the stack to finish.
    pub fn finish_node(&mut self) -> Result<(), BuilderError> {
        let node = self.stack.pop().ok_or(BuilderError::UnmatchedFinishNode)?;
        let green = GreenNode::new(node.kind, node.children, node.text_len);

        if let Some(parent) = self.stack.last_mut() {
            parent.text_len = TextSize::from(parent.text_len.into() + green.text_len().into());
            parent.children.push(GreenElement::Node(green));
        }
        Ok(())
    }

    /// Add a token to the current node.
    ///
    /// # Errors
    ///
    /// Returns an error if there's no parent node on the stack.
    pub fn token(
        &mut self,
        kind: K,
        text: impl Into<compact_str::CompactString>,
    ) -> Result<(), BuilderError> {
        let text: compact_str::CompactString = text.into();
        let text_len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
        let token = GreenElement::Token(crate::syntax::GreenToken::new(kind, text));

        if let Some(parent) = self.stack.last_mut() {
            parent.text_len = TextSize::from(parent.text_len.into() + text_len.into());
            parent.children.push(token);
            Ok(())
        } else {
            Err(BuilderError::TokenWithoutParent)
        }
    }

    /// Add a pre-built node to the current node.
    ///
    /// This is used for incremental parsing to reuse nodes from previous parses.
    ///
    /// # Errors
    ///
    /// Returns an error if there's no parent node on the stack.
    pub fn reuse_node(&mut self, node: std::sync::Arc<GreenNode<K>>) -> Result<(), BuilderError> {
        if let Some(parent) = self.stack.last_mut() {
            let text_len = node.text_len();
            parent.text_len = TextSize::from(parent.text_len.into() + text_len.into());
            parent.children.push(GreenElement::Node(node));
            Ok(())
        } else {
            Err(BuilderError::TokenWithoutParent)
        }
    }

    /// Finish building the syntax tree.
    ///
    /// Returns an error if the builder doesn't have exactly one root node on the stack.
    /// This typically means there are unmatched `start_node()`/`finish_node()` calls.
    ///
    /// # Errors
    ///
    /// Returns an error if the builder doesn't have exactly one root node on the stack.
    ///
    /// # Panics
    ///
    /// Panics if the stack is empty when trying to pop the root node.
    pub fn finish(mut self) -> Result<std::sync::Arc<GreenNode<K>>, BuilderError> {
        if self.stack.len() != 1 {
            return Err(BuilderError::InvalidStackSize {
                stack_size: self.stack.len(),
            });
        }
        let root = self.stack.pop().unwrap();
        Ok(GreenNode::new(root.kind, root.children, root.text_len))
    }

    /// Finish building the syntax tree, panicking on error.
    ///
    /// This is a convenience method for cases where you're certain the builder
    /// is in a valid state. For better error handling, use [`finish()`](Self::finish) instead.
    ///
    /// # Panics
    ///
    /// Panics if the builder doesn't have exactly one root node on the stack.
    pub fn finish_unwrap(self) -> std::sync::Arc<GreenNode<K>> {
        self.finish()
            .expect("Builder must have exactly one root node")
    }

    pub fn node_count(&self) -> usize {
        self.stack.iter().map(|n| n.children.len()).sum::<usize>()
    }
}

impl<K: SyntaxKind> Default for GreenNodeBuilder<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::SyntaxKind;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Root,
        Expr,
        Ident,
        Number,
        Plus,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Root | Self::Expr)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    #[test]
    fn test_builder_new() {
        let builder = GreenNodeBuilder::<TestKind>::new();
        assert_eq!(builder.stack.len(), 0);
    }

    #[test]
    fn test_builder_default() {
        let builder = GreenNodeBuilder::<TestKind>::default();
        assert_eq!(builder.stack.len(), 0);
    }

    #[test]
    fn test_builder_simple_tree() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);
        builder.token(TestKind::Ident, "x").unwrap();
        // Don't call finish_node() on root - finish() expects it on the stack
        let root = builder.finish().unwrap();
        assert_eq!(root.kind(), TestKind::Root);
        assert_eq!(root.children().len(), 1);
        assert_eq!(root.text_len(), TextSize::from(1));
    }

    #[test]
    fn test_builder_nested_nodes() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Ident, "x").unwrap();
        builder.token(TestKind::Plus, "+").unwrap();
        builder.token(TestKind::Number, "42").unwrap();
        builder.finish_node().unwrap();

        // Don't call finish_node() on root - finish() expects it on the stack
        let root = builder.finish().unwrap();
        assert_eq!(root.kind(), TestKind::Root);
        assert_eq!(root.children().len(), 1);

        match &root.children()[0] {
            GreenElement::Node(expr) => {
                assert_eq!(expr.kind(), TestKind::Expr);
                assert_eq!(expr.children().len(), 3);
            }
            GreenElement::Token(_) => panic!("Expected node"),
        }
    }

    #[test]
    fn test_builder_multiple_siblings() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);
        builder.token(TestKind::Ident, "a").unwrap();
        builder.token(TestKind::Plus, "+").unwrap();
        builder.token(TestKind::Ident, "b").unwrap();
        // Don't call finish_node() on root - finish() expects it on the stack
        let root = builder.finish().unwrap();
        assert_eq!(root.children().len(), 3);
        assert_eq!(root.text_len(), TextSize::from(3)); // "a+b"
    }

    #[test]
    fn test_builder_text_len_accumulation() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Number, "123").unwrap();
        builder.finish_node().unwrap();

        builder.token(TestKind::Plus, "+").unwrap();

        builder.start_node(TestKind::Expr);
        builder.token(TestKind::Number, "456").unwrap();
        builder.finish_node().unwrap();

        // Don't call finish_node() on root - finish() expects it on the stack
        let root = builder.finish().unwrap();
        // "123" + "+" + "456" = 7 bytes
        assert_eq!(root.text_len(), TextSize::from(7));
    }

    #[test]
    fn test_builder_node_count() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);
        builder.token(TestKind::Ident, "x").unwrap();
        builder.token(TestKind::Plus, "+").unwrap();
        builder.token(TestKind::Number, "1").unwrap();

        // node_count counts children in all nodes on the stack
        assert_eq!(builder.node_count(), 3);

        // Don't call finish_node() on root - finish() expects it on the stack
        let _root = builder.finish().unwrap();
    }

    #[test]
    #[should_panic(expected = "UnmatchedFinishNode")]
    fn test_builder_finish_without_start() {
        let mut builder = GreenNodeBuilder::<TestKind>::new();
        builder.finish_node().unwrap();
    }

    #[test]
    fn test_builder_finish_multiple_roots() {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestKind::Root);
        builder.start_node(TestKind::Expr);
        // Don't finish Expr - leave it on the stack
        // Now we have 2 nodes on stack (Root and Expr), so finish() should return error
        assert!(builder.finish().is_err());
    }

    #[test]
    fn test_builder_finish_empty() {
        let builder = GreenNodeBuilder::<TestKind>::new();
        assert!(builder.finish().is_err());
    }

    #[test]
    fn test_builder_token_without_parent() {
        let mut builder = GreenNodeBuilder::<TestKind>::new();
        // Token without parent should return an error
        assert!(builder.token(TestKind::Ident, "x").is_err());
    }
}
