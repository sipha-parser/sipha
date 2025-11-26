use crate::grammar::{Token, NonTerminal};
use crate::syntax::GreenNode;
use hashbrown::HashMap;

/// Parser state for LR parser
#[derive(Debug)]
pub struct LrParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Cache for incremental parsing: (`state_id`, `start_pos`, `version`) -> `node`
    parse_cache: HashMap<CacheKey, std::sync::Arc<GreenNode<T::Kind>>, ahash::RandomState>,
    /// Current cache version (incremented on each edit)
    cache_version: usize,
    _phantom: std::marker::PhantomData<(T, N)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey {
    state_id: usize,
    start_pos: usize,
    version: usize,
}

impl<T, N> LrParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            parse_cache: HashMap::with_hasher(ahash::RandomState::new()),
            cache_version: 0,
            _phantom: std::marker::PhantomData,
        }
    }
    
    const fn cache_key(&self, state_id: usize, start_pos: usize) -> CacheKey {
        CacheKey {
            state_id,
            start_pos,
            version: self.cache_version,
        }
    }
    
    /// Get a cached parse result for the given state and position
    pub(crate) fn get_cached(&self, state_id: usize, start_pos: usize) -> Option<std::sync::Arc<GreenNode<T::Kind>>> {
        let key = self.cache_key(state_id, start_pos);
        self.parse_cache.get(&key).cloned()
    }
    
    /// Cache a parsed node for incremental parsing performance.
    pub(crate) fn cache_result(&mut self, state_id: usize, start_pos: usize, node: std::sync::Arc<GreenNode<T::Kind>>) {
        let key = self.cache_key(state_id, start_pos);
        self.parse_cache.insert(key, node);
    }
    
    /// Get a cached result for a full parse (using entry point as `state_id`)
    pub(crate) fn get_cached_parse(&self, entry_id: usize, input_len: usize) -> Option<std::sync::Arc<GreenNode<T::Kind>>> {
        // Use entry_id as state_id and input_len as start_pos to cache full parses
        self.get_cached(entry_id, input_len)
    }
    
    /// Cache a full parse result
    pub(crate) fn cache_parse_result(&mut self, entry_id: usize, input_len: usize, node: std::sync::Arc<GreenNode<T::Kind>>) {
        self.cache_result(entry_id, input_len, node);
    }
    
    pub(crate) fn invalidate_cache(&mut self) {
        self.cache_version += 1;
        // Clear old cache entries (keep recent ones for potential reuse)
        if self.parse_cache.len() > 1000 {
            self.parse_cache.clear();
        }
    }
}

impl<T, N> Default for LrParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}

