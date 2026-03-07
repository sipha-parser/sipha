//! # Packrat Memoisation
//!
//! Classical packrat parsing caches the result of every rule at every input
//! position in a hash map, trading memory for guaranteed O(n) parse time even
//! on grammars with unbounded backtracking.
//!
//! ## Trade-offs
//!
//! | Property | Without memo | With memo |
//! |---|---|---|
//! | Worst-case time | O(n²) or worse | O(n · |grammar|) |
//! | Memory | O(depth) | O(n · |rules|) |
//! | Per-call overhead | Zero | ~HashMap lookup |
//!
//! For grammars without left-recursion and without pathological alternation
//! the overhead is usually not worth it.  For grammars that re-parse large
//! regions (e.g. deeply nested expressions with many alternatives), packrat
//! makes the complexity predictable.
//!
//! ## Implementation
//!
//! Keys are packed into a `u64`: `(rule_id as u64) << 32 | pos as u64`.
//! Values are [`MemoEntry`]s stored in a plain `HashMap`.
//!
//! Capture events for successful rule invocations are stored in the entry as
//! a `Box<[CaptureEvent]>`.  On a cache hit the events are extended into the
//! engine's event buffer, then position is advanced — no re-parsing required.
//!
//! On a cache miss the entry stores only the `furthest` position reached
//! during the failed attempt, so the error context can be updated correctly.
//!
//! ## Memo and the green/red tree
//!
//! Memoisation stores and replays both **legacy capture events** ([`CaptureEvent`])
//! and **tree events** ([`crate::types::TreeEvent`]).  On a cache hit both are
//! replayed, so the green/red syntax tree remains complete and formatters/tree
//! consumers work correctly with memo enabled.

use std::collections::HashMap;

use crate::types::{CaptureEvent, Pos, RuleId, TreeEvent};

// ─── Entry ────────────────────────────────────────────────────────────────────

/// One cached result for a `(rule, pos)` pair.
#[derive(Clone, Debug)]
pub enum MemoEntry {
    /// The rule matched, consuming input up to `end_pos`.
    /// `events` holds every [`CaptureEvent`], `tree_events` every [`TreeEvent`],
    /// emitted during the match.
    Hit {
        end_pos:     Pos,
        events:      Box<[CaptureEvent]>,
        tree_events: Box<[TreeEvent]>,
    },
    /// The rule failed.  `furthest` is the deepest position reached.
    Miss {
        furthest: Pos,
    },
}

// ─── Query result returned to the caller ─────────────────────────────────────

/// Owned result of a memo lookup (so the borrow on [`MemoTable`] can end).
pub enum MemoQuery {
    /// Not yet cached — caller must run the rule normally.
    Unknown,
    /// Cached failure.
    Miss { furthest: Pos },
    /// Cached success — caller should replay these events and advance pos.
    Hit {
        end_pos:     Pos,
        events:      Vec<CaptureEvent>,
        tree_events: Vec<TreeEvent>,
    },
}

/// Result of a memo lookup that replays events in place (avoids cloning).
/// Use [`MemoTable::query_replay`] for zero-copy replay on hit.
pub enum MemoReplay {
    /// Not yet cached — caller must run the rule normally.
    Unknown,
    /// Cached failure.
    Miss { furthest: Pos },
    /// Cached success; `events` has already been extended in place.
    Hit { end_pos: Pos },
}

// ─── Table ────────────────────────────────────────────────────────────────────

/// The memoisation table.  One instance per parse invocation (cleared between
/// parses via [`MemoTable::clear`]).
pub struct MemoTable {
    table: HashMap<u64, MemoEntry>,
}

impl MemoTable {
    /// Create with a reasonable initial capacity (no resizes for typical grammars).
    pub fn new() -> Self {
        Self {
            table: HashMap::with_capacity(4096),
        }
    }

    /// Clear all entries (cheap — reuses allocation).
    #[inline]
    pub fn clear(&mut self) {
        self.table.clear();
    }

    /// Look up a `(rule, pos)` pair.  Returns an *owned* [`MemoQuery`] so the
    /// borrow on `self` ends before any subsequent mutation.
    #[inline]
    pub fn query(&self, rule: RuleId, pos: Pos) -> MemoQuery {
        match self.table.get(&pack(rule, pos)) {
            None => MemoQuery::Unknown,
            Some(MemoEntry::Miss { furthest }) => MemoQuery::Miss { furthest: *furthest },
            Some(MemoEntry::Hit { end_pos, events, tree_events }) => MemoQuery::Hit {
                end_pos:     *end_pos,
                events:      events.to_vec(),
                tree_events: tree_events.to_vec(),
            },
        }
    }

    /// Look up a `(rule, pos)` pair and, on hit, extend `events` and `tree_events`
    /// in place with the cached data.  Use this in the hot path for zero-copy replay.
    #[inline]
    pub fn query_replay(
        &self,
        rule: RuleId,
        pos: Pos,
        events: &mut Vec<CaptureEvent>,
        tree_events: &mut Vec<TreeEvent>,
    ) -> MemoReplay {
        match self.table.get(&pack(rule, pos)) {
            None => MemoReplay::Unknown,
            Some(MemoEntry::Miss { furthest }) => MemoReplay::Miss { furthest: *furthest },
            Some(MemoEntry::Hit {
                end_pos,
                events: cached_events,
                tree_events: cached_tree,
            }) => {
                events.extend_from_slice(cached_events);
                tree_events.extend_from_slice(cached_tree);
                MemoReplay::Hit { end_pos: *end_pos }
            }
        }
    }

    /// Store a successful result (capture events and tree events produced by the rule).
    #[inline]
    pub fn insert_hit(
        &mut self,
        rule: RuleId,
        pos: Pos,
        end_pos: Pos,
        events: Box<[CaptureEvent]>,
        tree_events: Box<[TreeEvent]>,
    ) {
        self.table.insert(
            pack(rule, pos),
            MemoEntry::Hit {
                end_pos,
                events,
                tree_events,
            },
        );
    }

    /// Store a failed result with the deepest position reached.
    #[inline]
    pub fn insert_miss(&mut self, rule: RuleId, pos: Pos, furthest: Pos) {
        self.table.insert(pack(rule, pos), MemoEntry::Miss { furthest });
    }

    /// Number of cached entries (for diagnostics / benchmarks).
    pub fn len(&self) -> usize {
        self.table.len()
    }

    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }
}

impl Default for MemoTable {
    fn default() -> Self {
        Self::new()
    }
}

// ─── Key packing ─────────────────────────────────────────────────────────────

/// Pack `(rule: u16, pos: u32)` into a single `u64` key.
#[inline(always)]
fn pack(rule: RuleId, pos: Pos) -> u64 {
    ((rule as u64) << 32) | (pos as u64)
}
