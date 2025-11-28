//! Parser state for GLR parser with parse forest caching

use crate::backend::glr::forest::ParseForest;
use crate::backend::glr::stack::GlrStack;
use crate::grammar::{NonTerminal, Token};
use crate::syntax::{TextRange, TextSize};
use hashbrown::HashMap;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct StackSnapshot<K: crate::syntax::SyntaxKind> {
    pub(crate) stacks: Vec<GlrStack<K>>,
    pub(crate) token_pos: usize,
    pub(crate) text_pos: TextSize,
}

/// Parser state for GLR parser
#[derive(Debug)]
pub struct GlrParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Cache for incremental parsing: (`state_id`, `start_pos`, `version`) -> `ParseForest`
    /// This caches parse forest fragments for reuse during incremental parsing
    parse_cache: HashMap<CacheKey, ParseForest<T::Kind>, ahash::RandomState>,
    /// Cache for stack snapshots to resume incremental parses: (`entry_id`, `pos`, `version`)
    stack_cache: HashMap<CacheKey, StackSnapshot<T::Kind>, ahash::RandomState>,
    stack_cache_order: VecDeque<CacheKey>,
    max_stack_snapshots: usize,
    /// Current cache version (incremented on each edit)
    cache_version: usize,
    /// Last parse forest (for access when `return_forest` is true)
    last_forest: Option<ParseForest<T::Kind>>,
    /// Span that was ambiguous in the last parse (if any)
    last_ambiguity_span: Option<TextRange>,
    _phantom: std::marker::PhantomData<(T, N)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey {
    state_id: usize,
    start_pos: usize,
    version: usize,
}

impl CacheKey {
    /// Create a new cache key
    #[must_use]
    const fn new(state_id: usize, start_pos: usize, version: usize) -> Self {
        Self {
            state_id,
            start_pos,
            version,
        }
    }
}

impl<T, N> GlrParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new GLR parser state
    #[must_use]
    pub fn new() -> Self {
        Self {
            parse_cache: HashMap::with_hasher(ahash::RandomState::new()),
            stack_cache: HashMap::with_hasher(ahash::RandomState::new()),
            stack_cache_order: VecDeque::new(),
            max_stack_snapshots: 128,
            cache_version: 0,
            last_forest: None,
            last_ambiguity_span: None,
            _phantom: std::marker::PhantomData,
        }
    }

    const fn cache_key(&self, state_id: usize, start_pos: usize) -> CacheKey {
        CacheKey::new(state_id, start_pos, self.cache_version)
    }

    /// Get a cached parse forest fragment for the given state and position
    #[allow(dead_code)]
    pub(crate) fn get_cached(
        &self,
        state_id: usize,
        start_pos: usize,
    ) -> Option<ParseForest<T::Kind>> {
        let key = self.cache_key(state_id, start_pos);
        self.parse_cache.get(&key).cloned()
    }

    /// Get cached stack snapshots for a given entry/position
    pub(crate) fn get_cached_stacks(
        &self,
        state_id: usize,
        text_offset: usize,
    ) -> Option<StackSnapshot<T::Kind>> {
        let key = self.cache_key(state_id, text_offset);
        self.stack_cache.get(&key).cloned()
    }

    /// Cache a parse forest fragment for incremental parsing performance
    pub(crate) fn cache_result(
        &mut self,
        state_id: usize,
        start_pos: usize,
        forest: ParseForest<T::Kind>,
    ) {
        let key = self.cache_key(state_id, start_pos);
        self.parse_cache.insert(key, forest);
    }

    /// Cache stack snapshots for incremental resume points.
    pub(crate) fn cache_stacks(
        &mut self,
        state_id: usize,
        text_pos: TextSize,
        token_pos: usize,
        stacks: &[GlrStack<T::Kind>],
    ) {
        if stacks.is_empty() {
            return;
        }
        let key = self.cache_key(state_id, text_pos.into() as usize);
        self.stack_cache.insert(
            key.clone(),
            StackSnapshot {
                stacks: stacks.to_vec(),
                token_pos,
                text_pos,
            },
        );
        self.stack_cache_order.push_back(key);
        while self.stack_cache_order.len() > self.max_stack_snapshots {
            if let Some(old_key) = self.stack_cache_order.pop_front() {
                self.stack_cache.remove(&old_key);
            }
        }
    }

    /// Get a cached result for a full parse (using entry point as `state_id`)
    #[allow(dead_code)]
    pub(crate) fn get_cached_parse(
        &self,
        entry_id: usize,
        input_len: usize,
    ) -> Option<ParseForest<T::Kind>> {
        // Use entry_id as state_id and input_len as start_pos to cache full parses
        self.get_cached(entry_id, input_len)
    }

    /// Cache a full parse result
    pub(crate) fn cache_parse_result(
        &mut self,
        entry_id: usize,
        input_len: usize,
        forest: ParseForest<T::Kind>,
    ) {
        self.cache_result(entry_id, input_len, forest.clone());
        self.last_forest = Some(forest);
    }

    /// Remember the ambiguity span of the last parse.
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    pub(crate) fn set_last_ambiguity_span(&mut self, span: Option<TextRange>) {
        self.last_ambiguity_span = span;
    }

    /// Access the ambiguity span of the last parse
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: returns field value
    pub(crate) fn last_ambiguity_span(&self) -> Option<TextRange> {
        self.last_ambiguity_span
    }

    /// Get the last parse forest (if available)
    #[allow(dead_code, clippy::missing_const_for_fn)] // Cannot be const: returns reference
    pub(crate) fn last_forest(&self) -> Option<&ParseForest<T::Kind>> {
        self.last_forest.as_ref()
    }

    /// Set the last parse forest
    pub(crate) fn set_last_forest(&mut self, forest: ParseForest<T::Kind>) {
        self.last_forest = Some(forest);
    }

    /// Invalidate the cache
    #[allow(dead_code)]
    pub(crate) fn invalidate_cache(&mut self) {
        self.cache_version += 1;
        // Clear old cache entries (keep recent ones for potential reuse)
        if self.parse_cache.len() > 1000 {
            self.parse_cache.clear();
        }
        self.stack_cache.clear();
        self.stack_cache_order.clear();
    }

    /// Invalidate cache for a specific range
    /// This is used for incremental parsing to invalidate only affected regions
    pub(crate) fn invalidate_cache_range(&mut self, start_pos: usize, end_pos: usize) {
        // Remove cache entries that overlap with the invalidated range
        self.parse_cache.retain(|key, _| {
            // Keep entries that don't overlap with the invalidated range
            key.start_pos <= start_pos || key.start_pos >= end_pos
        });
        self.stack_cache
            .retain(|key, _| key.start_pos <= start_pos || key.start_pos >= end_pos);
        self.stack_cache_order
            .retain(|key| key.start_pos <= start_pos || key.start_pos >= end_pos);
    }
}

impl<T, N> Default for GlrParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}
