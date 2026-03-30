//! # Parse Context — multi-word flag bank
//!
//! A [`ParseContext`] holds an arbitrary number of boolean flags organised
//! into 64-bit words (one `u64` per 64 flags).  Flags are addressed by a
//! flat [`FlagId`] (`u16`):
//!
//! ```text
//! flag_id 0..63   → word 0
//! flag_id 64..127 → word 1
//! flag_id 128..191→ word 2
//!   …
//! ```
//!
//! This gives up to **65,535 flags** while keeping all bit operations as
//! single-word loads, shifts, and OR/AND — no special-casing required.
//!
//! ## Grouping rationale
//!
//! Keeping flags in contiguous `u64` words means:
//! * Testing any single flag is one array-index + one bit-test — O(1).
//! * Applying a set/clear mask to one word is two instructions.
//! * The words are cache-line aligned and SIMD-friendly if you ever want
//!   to test an entire 256-flag bank in one `_mm256_testc_si256`.
//!
//! ## Usage
//!
//! ```rust
//! use sipha::parse::context::{ParseContext, FlagId};
//!
//! // Define flags as named constants — any u16 value is valid.
//! pub const FLAG_IN_LOOP:   FlagId = 0;
//! pub const FLAG_IN_FN:     FlagId = 1;
//! pub const FLAG_STRICT:    FlagId = 64; // lives in word 1
//! pub const FLAG_ASYNC:     FlagId = 65;
//!
//! let ctx = ParseContext::new()
//!     .with_set(FLAG_IN_FN)
//!     .with_set(FLAG_STRICT);
//!
//! assert!(ctx.has(FLAG_STRICT));
//! assert!(!ctx.has(FLAG_IN_LOOP));
//! ```

/// A flag identifier — the flat index of one boolean flag.
///
/// `word = flag_id >> 6`, `bit = flag_id & 63`.
pub type FlagId = u16;

/// A multi-word flag bank.
///
/// The internal word-array grows automatically when flags with high IDs are
/// set.  Zero-words at the tail are always considered clear.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ParseContext {
    words: Vec<u64>,
    /// If set, [`parse_recovering_with_context`](crate::parse::engine::Engine::parse_recovering_with_context)
    /// will insert an error node and close open nodes on failure so the partial tree is well-nested.
    error_node_kind: Option<crate::types::SyntaxKind>,
}

impl ParseContext {
    /// Create an empty context (all flags clear, no words allocated).
    #[must_use]
    pub const fn new() -> Self {
        Self {
            words: Vec::new(),
            error_node_kind: None,
        }
    }

    /// Create with enough pre-allocated words to hold flags up to `max_flag_id`
    /// without reallocation.
    #[must_use]
    pub fn with_capacity_for(max_flag_id: FlagId) -> Self {
        let n = word_index(max_flag_id) + 1;
        Self {
            words: vec![0u64; n],
            error_node_kind: None,
        }
    }

    // ── Mutation ──────────────────────────────────────────────────────────────

    /// Set flag `id` and return `self` (builder style).
    #[must_use]
    pub fn with_set(mut self, id: FlagId) -> Self {
        self.set(id);
        self
    }

    /// Clear flag `id` and return `self` (builder style).
    #[must_use]
    pub fn with_clear(mut self, id: FlagId) -> Self {
        self.clear(id);
        self
    }

    /// Set flag `id` in place, growing the word array if needed.
    #[inline]
    pub fn set(&mut self, id: FlagId) {
        let w = word_index(id);
        if w >= self.words.len() {
            self.words.resize(w + 1, 0);
        }
        self.words[w] |= 1u64 << bit_index(id);
    }

    /// Clear flag `id` in place.  No-op if `id` is beyond the current bank.
    #[inline]
    pub fn clear(&mut self, id: FlagId) {
        let w = word_index(id);
        if w < self.words.len() {
            self.words[w] &= !(1u64 << bit_index(id));
        }
    }

    // ── Query ─────────────────────────────────────────────────────────────────

    /// Test flag `id`.  Returns `false` for any flag beyond the current bank.
    #[must_use]
    #[inline]
    pub fn has(&self, id: FlagId) -> bool {
        let w = word_index(id);
        if w < self.words.len() {
            (self.words[w] >> bit_index(id)) & 1 != 0
        } else {
            false
        }
    }

    /// The number of `u64` words currently allocated.
    #[must_use]
    #[inline]
    pub fn num_words(&self) -> usize {
        self.words.len()
    }

    /// Read-only view of the internal word array.
    #[must_use]
    #[inline]
    pub fn words(&self) -> &[u64] {
        &self.words
    }

    /// Set the syntax kind to use for error nodes when parsing in recovering mode.
    ///
    /// When set, a failed parse will still produce a well-nested partial tree by
    /// closing any open nodes and inserting an error node at the failure position.
    #[must_use]
    pub const fn with_error_node_kind(mut self, kind: crate::types::SyntaxKind) -> Self {
        self.error_node_kind = Some(kind);
        self
    }

    /// Return the error node kind if set.
    #[must_use]
    #[inline]
    pub const fn error_node_kind(&self) -> Option<crate::types::SyntaxKind> {
        self.error_node_kind
    }
}

// ─── Bit addressing helpers ───────────────────────────────────────────────────

/// `flag_id >> 6`
#[inline]
pub(crate) const fn word_index(id: FlagId) -> usize {
    (id >> 6) as usize
}

/// `flag_id & 63`
#[inline]
pub(crate) const fn bit_index(id: FlagId) -> u32 {
    (id & 63) as u32
}

// ─── FlagMask — sparse per-word set/clear descriptor ─────────────────────────

/// One word's worth of set/clear operations within a [`FlagMask`].
///
/// Applying this to `flags[word]`:
/// ```text
/// flags[word] = (flags[word] | set_bits) & !clear_bits
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FlagMaskWord {
    /// Which word in the flags bank this entry applies to.
    pub word: u32,
    /// Bits to set in that word.
    pub set_bits: u64,
    /// Bits to clear in that word.
    pub clear_bits: u64,
}

/// A sparse set/clear mask that may touch multiple words.
///
/// Stored in the grammar's [`FlagMaskTable`](crate::parse::insn::FlagMaskTable) and
/// referenced by index from [`Insn::PushFlags`](crate::parse::insn::Insn::PushFlags).
/// Only words that are actually modified are listed.
#[derive(Clone, Debug)]
pub struct FlagMask<'a> {
    pub entries: &'a [FlagMaskWord],
}

impl FlagMask<'_> {
    /// Apply this mask to a mutable flag bank, growing it if needed.
    #[inline]
    pub fn apply(&self, flags: &mut Vec<u64>) {
        for e in self.entries {
            let w = e.word as usize;
            if w >= flags.len() {
                flags.resize(w + 1, 0);
            }
            flags[w] = (flags[w] | e.set_bits) & !e.clear_bits;
        }
    }
}
