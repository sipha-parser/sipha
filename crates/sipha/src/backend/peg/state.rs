use crate::grammar::{NonTerminal, Token};
use crate::syntax::GreenNode;
use hashbrown::HashMap;

/// Parser state for PEG parser with memoization support.
///
/// PEG parsers use memoization (packrat parsing) to cache parse results at
/// each position for each non-terminal, enabling linear-time parsing for
/// many grammars.
#[derive(Debug)]
pub struct PegParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Memoization cache: (`rule`, `position`) -> `ParseMemo`
    ///
    /// This cache stores the result of parsing a non-terminal at a given
    /// position, allowing the parser to avoid re-parsing the same input
    /// multiple times.
    memo_cache: HashMap<MemoKey<N>, ParseMemo<T::Kind>, ahash::RandomState>,

    /// Current cache version (incremented on each edit for incremental parsing)
    cache_version: usize,

    _phantom: std::marker::PhantomData<(T, N)>,
}

/// Key for memoization cache entries
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MemoKey<N: NonTerminal> {
    rule: N,
    position: usize,
    version: usize,
}

impl<N: NonTerminal> MemoKey<N> {
    /// Create a new memo key
    #[must_use]
    const fn new(rule: N, position: usize, version: usize) -> Self {
        Self {
            rule,
            position,
            version,
        }
    }
}

/// Memoized parse result
#[derive(Debug, Clone)]
pub enum ParseMemo<K: crate::syntax::SyntaxKind> {
    /// Successful parse: (`node`, `end_position`)
    Success {
        node: std::sync::Arc<GreenNode<K>>,
        end_pos: usize,
    },
    /// Failed parse at this position
    Failure,
}

impl<T, N> PegParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            memo_cache: HashMap::with_hasher(ahash::RandomState::new()),
            cache_version: 0,
            _phantom: std::marker::PhantomData,
        }
    }

    fn memo_key(&self, rule: &N, position: usize) -> MemoKey<N> {
        MemoKey::new(rule.clone(), position, self.cache_version)
    }

    /// Get a memoized parse result for the given rule and position
    pub(crate) fn get_memo(&self, rule: &N, position: usize) -> Option<ParseMemo<T::Kind>> {
        let key = self.memo_key(rule, position);
        self.memo_cache.get(&key).cloned()
    }

    /// Store a memoized parse result
    ///
    /// If the cache exceeds `max_memo_size`, evicts oldest entries (by version).
    pub(crate) fn set_memo(
        &mut self,
        rule: &N,
        position: usize,
        memo: ParseMemo<T::Kind>,
        max_memo_size: usize,
    ) {
        let key = self.memo_key(rule, position);
        self.memo_cache.insert(key, memo);

        // Evict oldest entries if cache exceeds limit
        if self.memo_cache.len() > max_memo_size {
            let target_size = max_memo_size;
            let mut entries: Vec<_> = self.memo_cache.drain().collect();
            // Sort by version (newest first) to keep recent entries
            entries.sort_by_key(|(k, _)| std::cmp::Reverse(k.version));
            entries.truncate(target_size);
            for (key, value) in entries {
                self.memo_cache.insert(key, value);
            }
        }
    }

    /// Invalidate the memoization cache (for incremental parsing)
    pub(crate) fn invalidate_cache(&mut self, max_memo_size: usize) {
        self.cache_version += 1;
        // Clear old cache entries if cache is too large
        if self.memo_cache.len() > max_memo_size {
            self.memo_cache.clear();
        }
    }

    /// Invalidate cache entries in a specific range (for incremental parsing)
    ///
    /// This invalidates cache entries that overlap with the given range.
    /// For efficiency, we increment the version and evict old entries.
    #[allow(dead_code)]
    pub(crate) fn invalidate_cache_range(
        &mut self,
        _start: usize,
        _end: usize,
        max_memo_size: usize,
    ) {
        self.cache_version += 1;
        // Evict old entries to keep cache size manageable
        // Keep only recent entries (last 2 versions)
        let min_version = self.cache_version.saturating_sub(2);
        self.memo_cache.retain(|key, _| key.version >= min_version);

        // If still too large, evict oldest entries until under limit
        if self.memo_cache.len() > max_memo_size {
            let target_size = max_memo_size;
            let mut entries: Vec<_> = self.memo_cache.drain().collect();
            entries.sort_by_key(|(k, _)| std::cmp::Reverse(k.version));
            entries.truncate(target_size);
            for (key, value) in entries {
                self.memo_cache.insert(key, value);
            }
        }
    }
}

impl<T, N> Default for PegParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}
