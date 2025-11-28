//! GLR parser stack management
//!
//! This module implements the stack management for GLR parsing, including
//! stack forking and merging operations. Stack prefixes are shared via `Arc`
//! to avoid cloning large stack vectors when exploring multiple parse paths.

use crate::syntax::GreenNode;
use hashbrown::HashSet;
use std::sync::Arc;

/// GLR parser stack entry: (`state_id`, nodes)
pub type GlrStackEntry<K> = (usize, Vec<Arc<GreenNode<K>>>);

#[derive(Debug)]
struct SharedStackPrefix<K: crate::syntax::SyntaxKind> {
    entry: GlrStackEntry<K>,
    prev: Option<Arc<SharedStackPrefix<K>>>,
    depth: usize,
}

impl<K: crate::syntax::SyntaxKind> SharedStackPrefix<K> {
    fn new(entry: GlrStackEntry<K>, prev: Option<Arc<Self>>) -> Arc<Self> {
        let depth = prev.as_ref().map_or(1, |p| p.depth + 1);
        Arc::new(Self { entry, prev, depth })
    }
}

/// GLR parser stack backed by shared prefixes.
///
/// # Examples
///
/// ```
/// use sipha::backend::glr::GlrStack;
/// use sipha::syntax::{GreenNode, SyntaxKind, TextSize};
/// use std::sync::Arc;
///
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// enum Kind {
///     Root,
/// }
///
/// impl SyntaxKind for Kind {
///     fn is_terminal(self) -> bool { true }
///     fn is_trivia(self) -> bool { false }
/// }
///
/// let mut stack = GlrStack::with_initial_state(0);
/// let node = Arc::new(GreenNode::new(Kind::Root, vec![], TextSize::from(0)));
/// stack.push(1, vec![node.clone()]);
///
/// let forked = stack.fork();
/// assert_eq!(stack.shared_prefix_len(&forked), stack.len());
/// ```
#[derive(Debug, Clone)]
pub struct GlrStack<K: crate::syntax::SyntaxKind> {
    head: Option<Arc<SharedStackPrefix<K>>>,
}

impl<K: crate::syntax::SyntaxKind> GlrStack<K> {
    /// Create a new empty stack
    #[must_use]
    pub const fn new() -> Self {
        Self { head: None }
    }

    /// Create a stack with initial state
    #[must_use]
    pub fn with_initial_state(initial_state: usize) -> Self {
        let entry = (initial_state, Vec::new());
        Self {
            head: Some(SharedStackPrefix::new(entry, None)),
        }
    }

    /// Get the current state ID
    #[must_use]
    pub fn current_state(&self) -> Option<usize> {
        self.head.as_ref().map(|node| node.entry.0)
    }

    /// Push a new state and nodes onto the stack
    pub fn push(&mut self, state: usize, nodes: Vec<Arc<GreenNode<K>>>) {
        let entry = (state, nodes);
        let prev = self.head.clone();
        self.head = Some(SharedStackPrefix::new(entry, prev));
    }

    /// Pop entries from the stack
    pub fn pop(&mut self, count: usize) -> Vec<Arc<GreenNode<K>>> {
        let mut result = Vec::new();
        for _ in 0..count {
            let current = self.head.take();
            let Some(current) = current else {
                break;
            };
            result.extend(current.entry.1.iter().cloned());
            // Arc::clone is cheap (just increments reference count, no data copying)
            // Arc doesn't implement CloneFrom, so we can't use clone_from here
            #[allow(clippy::assigning_clones)]
            {
                self.head = current.prev.clone();
            }
        }
        result
    }

    /// Get the number of entries on the stack
    #[must_use]
    pub fn len(&self) -> usize {
        self.head.as_ref().map_or(0, |node| node.depth)
    }

    /// Check if the stack is empty
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    /// Fork this stack to create a new independent copy (shares prefix)
    #[must_use]
    pub fn fork(&self) -> Self {
        Self {
            head: self.head.clone(),
        }
    }

    fn iter_prefixes(&self) -> Vec<Arc<SharedStackPrefix<K>>> {
        let mut prefixes = Vec::new();
        let mut current = self.head.clone();
        while let Some(ref node) = current {
            prefixes.push(node.clone());
            current = node.prev.clone();
        }
        prefixes.reverse();
        prefixes
    }

    /// Check if this stack is identical to another stack
    ///
    /// Two stacks are identical if they share the same prefix chain.
    #[must_use]
    pub fn is_identical(&self, other: &Self) -> bool {
        match (&self.head, &other.head) {
            (None, None) => true,
            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    /// Check if two stacks can be merged (have identical state sequence)
    #[must_use]
    pub fn can_merge(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.iter_prefixes()
            .into_iter()
            .zip(other.iter_prefixes())
            .all(|(a, b)| a.entry.0 == b.entry.0)
    }

    /// Merge another stack into this one by combining node sets.
    ///
    /// The stacks must have identical state sequences.
    ///
    /// # Panics
    ///
    /// This function may panic if the stacks cannot be merged.
    pub fn merge(&mut self, other: &Self) {
        assert!(
            self.can_merge(other),
            "Cannot merge stacks with different states"
        );

        let merged = Self::merge_prefixes(self.head.clone(), other.head.clone());
        self.head = merged;
    }

    /// Attempt to merge in place, reusing existing allocations when this stack
    /// owns its entire prefix chain uniquely. Falls back to cloning if sharing
    /// prevents in-place mutation.
    ///
    /// Returns `true` if the merge happened in place.
    ///
    /// # Panics
    ///
    /// This function may panic if the stacks cannot be merged.
    pub fn merge_in_place(&mut self, other: &Self) -> bool {
        assert!(
            self.can_merge(other),
            "Cannot merge stacks with different states"
        );
        if !Self::has_unique_prefix(self.head.as_ref()) {
            return false;
        }
        Self::merge_prefixes_in_place(&mut self.head, other.head.as_ref());
        true
    }

    fn merge_prefixes(
        left: Option<Arc<SharedStackPrefix<K>>>,
        right: Option<Arc<SharedStackPrefix<K>>>,
    ) -> Option<Arc<SharedStackPrefix<K>>> {
        match (left, right) {
            (None, None) => None,
            (Some(left_node), Some(right_node)) => {
                let prev = Self::merge_prefixes(left_node.prev.clone(), right_node.prev.clone());
                let (changed, combined_nodes) =
                    Self::merged_node_list(&left_node.entry.1, &right_node.entry.1);
                let prev_is_same = prev
                    .as_ref()
                    .zip(left_node.prev.as_ref())
                    .is_some_and(|(p, l)| Arc::ptr_eq(p, l));

                if !changed && prev_is_same {
                    Some(left_node)
                } else {
                    let entry = (left_node.entry.0, combined_nodes);
                    Some(SharedStackPrefix::new(entry, prev))
                }
            }
            (Some(node), None) | (None, Some(node)) => Some(node),
        }
    }

    fn merge_prefixes_in_place(
        left: &mut Option<Arc<SharedStackPrefix<K>>>,
        right: Option<&Arc<SharedStackPrefix<K>>>,
    ) {
        match (left.as_mut(), right) {
            (Some(left_prefix), Some(right_prefix)) => {
                let left_mut = Arc::get_mut(left_prefix).expect("prefix must be unique");
                Self::merge_prefixes_in_place(&mut left_mut.prev, right_prefix.prev.as_ref());
                Self::merge_node_lists_in_place(&mut left_mut.entry.1, &right_prefix.entry.1);
            }
            (None, Some(other)) => {
                *left = Some(other.clone());
            }
            _ => {}
        }
    }

    #[must_use]
    fn has_unique_prefix(node: Option<&Arc<SharedStackPrefix<K>>>) -> bool {
        node.is_none_or(|prefix| {
            Arc::strong_count(prefix) == 1 && Self::has_unique_prefix(prefix.prev.as_ref())
        })
    }

    /// Get the common prefix length with another stack (states and nodes must match)
    #[must_use]
    pub fn shared_prefix_len(&self, other: &Self) -> usize {
        let mut len = 0;
        for (a, b) in self.iter_prefixes().into_iter().zip(other.iter_prefixes()) {
            if a.entry.0 != b.entry.0 {
                break;
            }
            if a.entry.1.len() != b.entry.1.len() {
                break;
            }
            let nodes_match = a
                .entry
                .1
                .iter()
                .zip(b.entry.1.iter())
                .all(|(n1, n2)| Arc::ptr_eq(n1, n2));
            if !nodes_match {
                break;
            }
            len += 1;
        }
        len
    }

    /// Get the top entry (state, nodes) if present.
    #[must_use]
    pub fn top_entry(&self) -> Option<GlrStackEntry<K>> {
        self.head
            .as_ref()
            .map(|node| (node.entry.0, node.entry.1.clone()))
    }

    /// Collect the state sequence from bottom to top for hashing or debugging.
    #[must_use]
    pub fn state_signature(&self) -> Vec<usize> {
        self.iter_prefixes()
            .into_iter()
            .map(|node| node.entry.0)
            .collect()
    }

    fn merged_node_list(
        left: &[Arc<GreenNode<K>>],
        right: &[Arc<GreenNode<K>>],
    ) -> (bool, Vec<Arc<GreenNode<K>>>) {
        if right.is_empty() {
            return (false, left.to_vec());
        }
        let mut combined: Vec<Arc<GreenNode<K>>> = left.to_vec();
        let mut seen: HashSet<usize> = combined
            .iter()
            .map(|node| Arc::as_ptr(node) as usize)
            .collect();
        let mut changed = false;
        for node in right {
            let ptr = Arc::as_ptr(node) as usize;
            if seen.insert(ptr) {
                combined.push(node.clone());
                changed = true;
            }
        }
        (changed, combined)
    }

    fn merge_node_lists_in_place(
        target: &mut Vec<Arc<GreenNode<K>>>,
        source: &[Arc<GreenNode<K>>],
    ) {
        if source.is_empty() {
            return;
        }
        let mut seen: HashSet<usize> = target
            .iter()
            .map(|node| Arc::as_ptr(node) as usize)
            .collect();
        for node in source {
            let ptr = Arc::as_ptr(node) as usize;
            if seen.insert(ptr) {
                target.push(node.clone());
            }
        }
    }
}

impl<K: crate::syntax::SyntaxKind> Default for GlrStack<K> {
    fn default() -> Self {
        Self::new()
    }
}
