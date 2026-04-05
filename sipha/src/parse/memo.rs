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
//! | Per-call overhead | Zero | ~`HashMap` lookup |
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
//! Successful hits append capture and tree event slices into **append-only pools**
//! inside [`MemoTable`] (see `capture_pool` / `tree_pool`).  Each hit entry
//! stores `(start, len)` ranges into those pools instead of a separate
//! `Box<[T]>` per key, which reduces allocator churn when many positions are
//! memoised.  Pools are cleared in [`MemoTable::clear`] while retaining capacity.
//!
//! The VM still keeps a full copy of events in its live buffers so outer rules
//! see one contiguous stream; memo storage is a second copy for replay on hits.
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
    /// Ranges index into [`MemoTable::capture_pool`] and [`MemoTable::tree_pool`].
    Hit {
        end_pos: Pos,
        cap_start: u32,
        cap_len: u32,
        tree_start: u32,
        tree_len: u32,
    },
    /// The rule failed.  `furthest` is the deepest position reached.
    Miss { furthest: Pos },
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
        end_pos: Pos,
        events: Vec<CaptureEvent>,
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
    /// Append-only storage for [`MemoEntry::Hit`] capture suffixes.
    capture_pool: Vec<CaptureEvent>,
    /// Append-only storage for [`MemoEntry::Hit`] tree-event suffixes.
    tree_pool: Vec<TreeEvent>,
}

#[inline]
fn pool_slice<T: Copy>(pool: &[T], start: u32, len: u32) -> Option<&[T]> {
    let s = start as usize;
    let l = len as usize;
    let end = s.checked_add(l)?;
    pool.get(s..end)
}

impl MemoTable {
    /// Create with a reasonable initial capacity (no resizes for typical grammars).
    #[must_use]
    pub fn new() -> Self {
        Self {
            table: HashMap::with_capacity(4096),
            capture_pool: Vec::with_capacity(8192),
            tree_pool: Vec::with_capacity(8192),
        }
    }

    /// Clear all entries (cheap — reuses map and pool allocations).
    #[inline]
    pub fn clear(&mut self) {
        self.table.clear();
        self.capture_pool.clear();
        self.tree_pool.clear();
    }

    /// Look up a `(rule, pos)` pair.  Returns an *owned* [`MemoQuery`] so the
    /// borrow on `self` ends before any subsequent mutation.
    #[must_use]
    #[inline]
    pub fn query(&self, rule: RuleId, pos: Pos) -> MemoQuery {
        match self.table.get(&pack(rule, pos)) {
            None => MemoQuery::Unknown,
            Some(MemoEntry::Miss { furthest }) => MemoQuery::Miss {
                furthest: *furthest,
            },
            Some(MemoEntry::Hit {
                end_pos,
                cap_start,
                cap_len,
                tree_start,
                tree_len,
            }) => {
                let events = pool_slice(&self.capture_pool, *cap_start, *cap_len)
                    .map(|s| s.to_vec())
                    .unwrap_or_default();
                let tree_events = pool_slice(&self.tree_pool, *tree_start, *tree_len)
                    .map(|s| s.to_vec())
                    .unwrap_or_default();
                MemoQuery::Hit {
                    end_pos: *end_pos,
                    events,
                    tree_events,
                }
            }
        }
    }

    /// Look up a `(rule, pos)` pair and, on hit, extend `events` and `tree_events`
    /// in place with the cached data.  Use this in the hot path for zero-copy replay.
    #[must_use]
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
            Some(MemoEntry::Miss { furthest }) => MemoReplay::Miss {
                furthest: *furthest,
            },
            Some(MemoEntry::Hit {
                end_pos,
                cap_start,
                cap_len,
                tree_start,
                tree_len,
            }) => {
                if let Some(cap) = pool_slice(&self.capture_pool, *cap_start, *cap_len) {
                    events.reserve(cap.len());
                    events.extend_from_slice(cap);
                }
                if let Some(tr) = pool_slice(&self.tree_pool, *tree_start, *tree_len) {
                    tree_events.reserve(tr.len());
                    tree_events.extend_from_slice(tr);
                }
                MemoReplay::Hit { end_pos: *end_pos }
            }
        }
    }

    /// Store a successful result (suffixes appended during the rule).
    #[inline]
    pub fn insert_hit(
        &mut self,
        rule: RuleId,
        pos: Pos,
        end_pos: Pos,
        captures: &[CaptureEvent],
        tree: &[TreeEvent],
    ) {
        let cap_len = u32::try_from(captures.len()).expect("memo capture run fits u32");
        let tree_len = u32::try_from(tree.len()).expect("memo tree run fits u32");
        let cap_start = u32::try_from(self.capture_pool.len()).expect("memo pool size fits u32");
        self.capture_pool.extend_from_slice(captures);
        let tree_start = u32::try_from(self.tree_pool.len()).expect("memo pool size fits u32");
        self.tree_pool.extend_from_slice(tree);
        self.table.insert(
            pack(rule, pos),
            MemoEntry::Hit {
                end_pos,
                cap_start,
                cap_len,
                tree_start,
                tree_len,
            },
        );
    }

    /// Store a failed result with the deepest position reached.
    #[inline]
    pub fn insert_miss(&mut self, rule: RuleId, pos: Pos, furthest: Pos) {
        self.table
            .insert(pack(rule, pos), MemoEntry::Miss { furthest });
    }

    /// Number of cached entries (for diagnostics / benchmarks).
    #[must_use]
    pub fn len(&self) -> usize {
        self.table.len()
    }

    #[must_use]
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
#[inline]
fn pack(rule: RuleId, pos: Pos) -> u64 {
    (u64::from(rule) << 32) | u64::from(pos)
}
