//! Parse forest representation for ambiguous results
//!
//! This module provides a parse forest data structure to represent multiple
//! parse trees for ambiguous grammars.

use crate::syntax::{GreenNode, SyntaxKind};
#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Parse forest node representing multiple parse possibilities
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum ForestNode<K: SyntaxKind> {
    /// Single unambiguous node
    Node(Arc<GreenNode<K>>),
    /// Ambiguous node with multiple alternatives
    Ambiguous(Vec<Arc<GreenNode<K>>>),
}

impl<K: SyntaxKind> ForestNode<K> {
    /// Get the single node if this is unambiguous, or None if ambiguous
    #[must_use]
    pub const fn as_node(&self) -> Option<&Arc<GreenNode<K>>> {
        match self {
            Self::Node(node) => Some(node),
            Self::Ambiguous(_) => None,
        }
    }

    /// Get all alternatives if this is ambiguous, or a single-element slice if unambiguous
    #[must_use]
    pub fn alternatives(&self) -> &[Arc<GreenNode<K>>] {
        match self {
            Self::Node(node) => std::slice::from_ref(node),
            Self::Ambiguous(alternatives) => alternatives,
        }
    }

    /// Check if this node is ambiguous
    #[must_use]
    pub const fn is_ambiguous(&self) -> bool {
        matches!(self, Self::Ambiguous(_))
    }

    /// Get the number of alternatives
    #[must_use]
    pub const fn count_alternatives(&self) -> usize {
        match self {
            Self::Node(_) => 1,
            Self::Ambiguous(alternatives) => alternatives.len(),
        }
    }

    /// Get a specific alternative by index
    #[must_use]
    pub fn get_alternative(&self, index: usize) -> Option<&Arc<GreenNode<K>>> {
        match self {
            Self::Node(node) => {
                if index == 0 {
                    Some(node)
                } else {
                    None
                }
            }
            Self::Ambiguous(alternatives) => alternatives.get(index),
        }
    }

    /// Iterate over all alternatives
    #[must_use]
    pub fn iter_alternatives(&self) -> Box<dyn Iterator<Item = &Arc<GreenNode<K>>> + '_> {
        match self {
            Self::Node(node) => {
                // Return a single-element iterator
                Box::new(std::iter::once(node))
            }
            Self::Ambiguous(alternatives) => Box::new(alternatives.iter()),
        }
    }
}

/// Parse forest representing all possible parse trees
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct ParseForest<K: SyntaxKind> {
    /// Root nodes of the forest
    roots: Vec<ForestNode<K>>,
}

impl<K: SyntaxKind> ParseForest<K> {
    /// Create a new empty parse forest
    #[must_use]
    pub const fn new() -> Self {
        Self { roots: Vec::new() }
    }

    /// Add a root node to the forest
    pub fn add_root(&mut self, node: ForestNode<K>) {
        self.roots.push(node);
    }

    /// Get all root nodes
    #[must_use]
    pub fn roots(&self) -> &[ForestNode<K>] {
        &self.roots
    }

    /// Check if the forest is ambiguous (has multiple roots or ambiguous nodes)
    #[must_use]
    pub fn is_ambiguous(&self) -> bool {
        self.roots.len() > 1
            || self
                .roots
                .iter()
                .any(|r| matches!(r, ForestNode::Ambiguous(_)))
    }

    /// Get the first root node if it exists and is unambiguous
    #[must_use]
    pub fn first_root(&self) -> Option<&Arc<GreenNode<K>>> {
        self.roots.first()?.as_node()
    }

    /// Get a specific root by index
    #[must_use]
    pub fn get_root(&self, index: usize) -> Option<&ForestNode<K>> {
        self.roots.get(index)
    }

    /// Get the number of root nodes
    #[must_use]
    pub const fn root_count(&self) -> usize {
        self.roots.len()
    }

    /// Count the total number of parse alternatives across all roots
    #[must_use]
    pub fn count_alternatives(&self) -> usize {
        if self.roots.is_empty() {
            return 0;
        }
        if self.roots.len() == 1 {
            return self.roots[0].count_alternatives();
        }
        // Multiple roots means multiple parse trees
        self.roots.len()
    }

    /// Get a specific alternative parse tree by index
    ///
    /// This flattens the forest structure to provide a single parse tree.
    /// For forests with multiple roots, this selects which root to use.
    /// For ambiguous roots, this selects which alternative to use.
    #[must_use]
    pub fn get_alternative(&self, index: usize) -> Option<&Arc<GreenNode<K>>> {
        if self.roots.is_empty() {
            return None;
        }

        if self.roots.len() == 1 {
            // Single root - get alternative from it
            return self.roots[0].get_alternative(index);
        }

        // Multiple roots - treat each root as a separate alternative
        self.roots.get(index)?.as_node()
    }

    /// Iterate over all root nodes
    pub fn iter_roots(&self) -> impl Iterator<Item = &ForestNode<K>> {
        self.roots.iter()
    }

    /// Iterate over all alternative parse trees
    ///
    /// This flattens the forest to provide an iterator over all possible
    /// parse trees, one per alternative.
    #[must_use]
    pub fn iter_alternatives(&self) -> AlternativesIterator<'_, K> {
        AlternativesIterator {
            roots: &self.roots,
            root_idx: 0,
            alt_idx: 0,
        }
    }
}

/// Iterator over all alternative parse trees in a forest
pub struct AlternativesIterator<'a, K: SyntaxKind> {
    roots: &'a [ForestNode<K>],
    root_idx: usize,
    alt_idx: usize,
}

impl<'a, K: SyntaxKind> Iterator for AlternativesIterator<'a, K> {
    type Item = &'a Arc<GreenNode<K>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.root_idx < self.roots.len() {
            let root = &self.roots[self.root_idx];
            match root {
                ForestNode::Node(node) => {
                    if self.alt_idx == 0 {
                        self.root_idx += 1;
                        self.alt_idx = 0;
                        return Some(node);
                    }
                    self.root_idx += 1;
                    self.alt_idx = 0;
                }
                ForestNode::Ambiguous(alternatives) => {
                    if let Some(node) = alternatives.get(self.alt_idx) {
                        self.alt_idx += 1;
                        return Some(node);
                    }
                    self.root_idx += 1;
                    self.alt_idx = 0;
                }
            }
        }
        None
    }
}

impl<K: SyntaxKind> Default for ParseForest<K> {
    fn default() -> Self {
        Self::new()
    }
}
