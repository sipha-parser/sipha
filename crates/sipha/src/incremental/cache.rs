//! # Incremental Parse Cache
//!
//! This module provides content-based caching for incremental parsing,
//! allowing efficient reuse of parsed nodes across edits.
//!
//! ## Overview
//!
//! The cache uses content-based keys instead of position-based keys,
//! which makes it more robust to edits that shift positions without
//! changing content.

use crate::grammar::Token;
use crate::syntax::{GreenNode, SyntaxKind, TextSize};
use lasso::Spur;
use lru::LruCache;
use std::hash::Hash;
use std::num::NonZeroUsize;
use std::sync::Arc;

/// Content-based cache key
///
/// This key is based on the content being parsed rather than positions,
/// making it more robust to position-shifting edits.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ContentCacheKey {
    /// The rule being parsed (interned string)
    pub rule: Spur,
    /// Hash of the input tokens
    pub input_hash: u64,
    /// Hash of the parsing context
    pub context_hash: u64,
}

impl std::fmt::Debug for ContentCacheKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContentCacheKey")
            .field("input_hash", &self.input_hash)
            .field("context_hash", &self.context_hash)
            .finish()
    }
}

impl ContentCacheKey {
    /// Create a new cache key
    #[must_use]
    pub const fn new(rule: Spur, input_hash: u64, context_hash: u64) -> Self {
        Self {
            rule,
            input_hash,
            context_hash,
        }
    }

    /// Create a key from raw components
    #[must_use]
    pub fn from_parts(rule: Spur, tokens: &[u8], context: u64) -> Self {
        let input_hash = Self::hash_bytes(tokens);
        Self::new(rule, input_hash, context)
    }

    /// Create a key from token sequence (optimized for common case)
    ///
    /// This method provides a fast path for creating cache keys from token sequences.
    #[must_use]
    pub fn from_tokens<T: Token>(rule: Spur, tokens: &[T], context: u64) -> Self {
        use std::hash::Hasher;
        let mut hasher = ahash::AHasher::default();

        // Hash token kinds (more efficient than hashing full token data)
        for token in tokens {
            use std::hash::Hash;
            token.kind().hash(&mut hasher);
        }

        let input_hash = hasher.finish();
        Self::new(rule, input_hash, context)
    }

    fn hash_bytes(bytes: &[u8]) -> u64 {
        use std::hash::Hasher;
        let mut hasher = ahash::AHasher::default();
        hasher.write(bytes);
        hasher.finish()
    }
}

/// A cached parse result
#[derive(Debug, Clone)]
pub struct CacheEntry<K: SyntaxKind> {
    /// The parsed node
    pub node: Arc<GreenNode<K>>,
    /// Number of tokens consumed
    pub tokens_consumed: usize,
    /// The text length of the parsed content
    pub text_len: TextSize,
    /// Version when this was cached
    pub version: u64,
    /// Number of times this entry has been used
    pub hit_count: u32,
}

impl<K: SyntaxKind> CacheEntry<K> {
    /// Create a new cache entry
    #[must_use]
    pub fn new(
        node: Arc<GreenNode<K>>,
        tokens_consumed: usize,
        text_len: TextSize,
        version: u64,
    ) -> Self {
        Self {
            node,
            tokens_consumed,
            text_len,
            version,
            hit_count: 0,
        }
    }

    /// Record a cache hit
    pub fn record_hit(&mut self) {
        self.hit_count = self.hit_count.saturating_add(1);
    }
}

/// Statistics about cache usage
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    /// Number of cache hits
    pub hits: usize,
    /// Number of cache misses
    pub misses: usize,
    /// Number of entries evicted
    pub evictions: usize,
    /// Number of entries invalidated
    pub invalidations: usize,
    /// Current number of entries
    pub entries: usize,
    /// Total bytes used (estimated)
    pub bytes_used: usize,
}

impl CacheStats {
    /// Get the hit ratio
    #[must_use]
    pub fn hit_ratio(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            self.hits as f64 / total as f64
        }
    }

    /// Reset statistics
    pub fn reset(&mut self) {
        *self = Self::default();
    }
}

/// LRU-based incremental parse cache
pub struct IncrementalCache<K: SyntaxKind> {
    /// The LRU cache
    cache: LruCache<ContentCacheKey, CacheEntry<K>>,
    /// Cache statistics
    stats: CacheStats,
    /// Current version
    version: u64,
}

impl<K: SyntaxKind> IncrementalCache<K> {
    /// Create a new cache with the given capacity
    #[must_use]
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: LruCache::new(
                NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(1000).unwrap()),
            ),
            stats: CacheStats::default(),
            version: 0,
        }
    }

    /// Get an entry from the cache
    pub fn get(&mut self, key: &ContentCacheKey) -> Option<&CacheEntry<K>> {
        if let Some(entry) = self.cache.get_mut(key) {
            entry.record_hit();
            self.stats.hits += 1;
            Some(entry)
        } else {
            self.stats.misses += 1;
            None
        }
    }

    /// Check if the cache contains a key (without promoting it)
    #[must_use]
    pub fn contains(&self, key: &ContentCacheKey) -> bool {
        self.cache.contains(key)
    }

    /// Insert an entry into the cache
    pub fn insert(&mut self, key: ContentCacheKey, entry: CacheEntry<K>) {
        if self.cache.len() >= self.cache.cap().get() {
            self.stats.evictions += 1;
        }
        self.cache.put(key, entry);
        self.stats.entries = self.cache.len();
    }

    /// Remove an entry from the cache
    pub fn remove(&mut self, key: &ContentCacheKey) -> Option<CacheEntry<K>> {
        let result = self.cache.pop(key);
        if result.is_some() {
            self.stats.invalidations += 1;
            self.stats.entries = self.cache.len();
        }
        result
    }

    /// Clear all entries
    pub fn clear(&mut self) {
        self.cache.clear();
        self.stats.entries = 0;
        self.stats.invalidations += 1;
    }

    /// Invalidate entries older than the given version
    pub fn invalidate_before(&mut self, version: u64) {
        // LruCache doesn't support predicate-based removal,
        // so we'd need to collect keys first
        let keys_to_remove: Vec<ContentCacheKey> = self
            .cache
            .iter()
            .filter(|(_, entry)| entry.version < version)
            .map(|(key, _)| key.clone())
            .collect();

        for key in keys_to_remove {
            self.cache.pop(&key);
            self.stats.invalidations += 1;
        }
        self.stats.entries = self.cache.len();
    }

    /// Get cache statistics
    #[must_use]
    pub const fn stats(&self) -> &CacheStats {
        &self.stats
    }

    /// Get the current version
    #[must_use]
    pub const fn version(&self) -> u64 {
        self.version
    }

    /// Increment the version
    pub fn bump_version(&mut self) -> u64 {
        self.version += 1;
        self.version
    }

    /// Get the number of entries
    #[must_use]
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Check if the cache is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    /// Get the capacity
    #[must_use]
    pub fn capacity(&self) -> usize {
        self.cache.cap().get()
    }

    /// Resize the cache
    pub fn resize(&mut self, capacity: usize) {
        if let Some(cap) = NonZeroUsize::new(capacity) {
            self.cache.resize(cap);
            self.stats.entries = self.cache.len();
        }
    }
}

impl<K: SyntaxKind> Default for IncrementalCache<K> {
    fn default() -> Self {
        Self::new(10000)
    }
}

/// A lazy node that defers reconstruction
///
/// This allows deferring the work of rebuilding nodes until they're
/// actually needed.
#[derive(Debug, Clone)]
pub enum LazyNode<K: SyntaxKind> {
    /// A fully materialized node
    Materialized(Arc<GreenNode<K>>),
    /// A node that needs reconstruction
    Deferred {
        /// The original node
        original: Arc<GreenNode<K>>,
        /// Pending edits to apply
        pending_offset: i64,
    },
}

impl<K: SyntaxKind> LazyNode<K> {
    /// Create a materialized lazy node
    #[must_use]
    pub const fn materialized(node: Arc<GreenNode<K>>) -> Self {
        Self::Materialized(node)
    }

    /// Create a deferred lazy node with an offset adjustment
    #[must_use]
    pub const fn deferred(original: Arc<GreenNode<K>>, offset: i64) -> Self {
        Self::Deferred {
            original,
            pending_offset: offset,
        }
    }

    /// Force materialization of the node
    #[must_use]
    pub fn materialize(self) -> Arc<GreenNode<K>> {
        match self {
            Self::Materialized(node) => node,
            Self::Deferred {
                original,
                pending_offset: _,
            } => {
                // In a full implementation, we'd adjust text ranges here
                // For now, just return the original
                original
            }
        }
    }

    /// Get the node (materializing if needed)
    #[must_use]
    pub fn get(&self) -> Arc<GreenNode<K>> {
        match self {
            Self::Materialized(node) => node.clone(),
            Self::Deferred { original, .. } => original.clone(),
        }
    }

    /// Check if this node is materialized
    #[must_use]
    pub const fn is_materialized(&self) -> bool {
        matches!(self, Self::Materialized(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{GreenElement, GreenToken};
    use lasso::Key;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestKind {
        Root,
        Expr,
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

    fn make_node() -> Arc<GreenNode<TestKind>> {
        let token = GreenToken::new(TestKind::Ident, "x");
        GreenNode::new(
            TestKind::Root,
            vec![GreenElement::Token(token)],
            TextSize::from(1),
        )
    }

    fn make_key(rule_index: u32) -> ContentCacheKey {
        ContentCacheKey::new(Spur::try_from_usize(rule_index as usize).unwrap(), 123, 456)
    }

    #[test]
    fn test_cache_insert_get() {
        let mut cache: IncrementalCache<TestKind> = IncrementalCache::new(100);

        let node = make_node();
        let key = make_key(1);
        let entry = CacheEntry::new(node.clone(), 1, TextSize::from(1), 0);

        cache.insert(key.clone(), entry);

        assert!(cache.contains(&key));
        assert_eq!(cache.len(), 1);

        let retrieved = cache.get(&key).unwrap();
        assert_eq!(retrieved.tokens_consumed, 1);
    }

    #[test]
    fn test_cache_stats() {
        let mut cache: IncrementalCache<TestKind> = IncrementalCache::new(100);

        let key = make_key(1);

        // Miss
        cache.get(&key);
        assert_eq!(cache.stats().misses, 1);

        // Insert and hit
        let node = make_node();
        let entry = CacheEntry::new(node, 1, TextSize::from(1), 0);
        cache.insert(key.clone(), entry);

        cache.get(&key);
        assert_eq!(cache.stats().hits, 1);
    }

    #[test]
    fn test_cache_eviction() {
        let mut cache: IncrementalCache<TestKind> = IncrementalCache::new(2);

        for i in 0..3 {
            let key = make_key(i);
            let node = make_node();
            let entry = CacheEntry::new(node, 1, TextSize::from(1), 0);
            cache.insert(key, entry);
        }

        // Should have evicted one entry
        assert_eq!(cache.len(), 2);
        assert_eq!(cache.stats().evictions, 1);
    }

    #[test]
    fn test_lazy_node() {
        let node = make_node();

        let lazy = LazyNode::materialized(node.clone());
        assert!(lazy.is_materialized());
        assert_eq!(lazy.get().kind(), TestKind::Root);

        let deferred = LazyNode::deferred(node.clone(), 10);
        assert!(!deferred.is_materialized());
        assert_eq!(deferred.materialize().kind(), TestKind::Root);
    }
}
