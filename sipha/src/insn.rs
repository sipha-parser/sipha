//! # Instruction Set
//!
//! The parse graph is a flat array of [`Insn`] values.
//!
//! ## Context-flag addressing
//!
//! Flags are indexed by a flat [`FlagId`](crate::context::FlagId) (`u16`).
//! The VM maintains `flags: Vec<u64>` where:
//!
//! ```text
//! word  = flag_id >> 6
//! bit   = flag_id & 63
//! value = (flags[word] >> bit) & 1
//! ```
//!
//! `IfFlag` / `IfNotFlag` address individual bits.
//!
//! `PushFlags` references a [`FlagMaskTable`] entry (a sparse list of
//! `(word, set_bits, clear_bits)` triples) rather than embedding masks
//! inline — this supports masks that span many words without bloating the
//! instruction.

use crate::types::{CharClass, InsnId, RuleId, SyntaxKind, Tag};
use crate::context::FlagMaskWord;

// ─── Instruction enum ─────────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Insn {
    // ── Terminals ────────────────────────────────────────────────────────────

    Byte      { byte: u8,                on_fail: InsnId },
    ByteRange { lo: u8, hi: u8,          on_fail: InsnId },
    Class     { class: CharClass,        on_fail: InsnId },
    /// SIMD-accelerated for literals ≥ 16 bytes.
    Literal   { lit_id: u32,             on_fail: InsnId },
    EndOfInput {                         on_fail: InsnId },
    Fail,

    // ── Unicode codepoint terminals ───────────────────────────────────────────
    //
    // These instructions operate at UTF-8 codepoint level.  The VM decodes
    // one codepoint (1–4 bytes) and tests it.  Invalid UTF-8 always fails.
    //
    // For ASCII-only matching, prefer the byte-level instructions above —
    // they skip the UTF-8 decode and are marginally faster.

    /// Consume any single valid UTF-8 codepoint (1–4 bytes).
    /// Fails on invalid UTF-8 or end-of-input.
    AnyChar { on_fail: InsnId },

    /// Match exactly the Unicode codepoint `codepoint`.
    /// Advances `pos` by the codepoint's UTF-8 byte length (1–4).
    Char { codepoint: u32, on_fail: InsnId },

    /// Match any codepoint in the inclusive range `[lo, hi]`.
    /// Fails if the decoded codepoint is outside the range, or on invalid UTF-8.
    CharRange { lo: u32, hi: u32, on_fail: InsnId },

    // ── Control flow ──────────────────────────────────────────────────────────

    Jump   { target: InsnId },
    Call   { rule: RuleId },
    Return,

    // ── Backtracking ──────────────────────────────────────────────────────────

    Choice        { alt:    InsnId },
    Commit        { target: InsnId },
    BackCommit    { target: InsnId },
    NegBackCommit { target: InsnId },
    PartialCommit { target: InsnId },

    // ── O(1) Dispatch ────────────────────────────────────────────────────────

    ByteDispatch { table_id: u32 },

    // ── Context flags ─────────────────────────────────────────────────────────
    //
    // Flag addressing: word = flag_id >> 6,  bit = flag_id & 63.
    // Supports up to 65,535 flags (FlagId = u16).

    /// Succeed iff `flags[flag_id >> 6] & (1 << (flag_id & 63)) != 0`.
    /// Does not consume input.
    IfFlag    { flag_id: u16, on_fail: InsnId },

    /// Succeed iff the named flag is **clear**.
    IfNotFlag { flag_id: u16, on_fail: InsnId },

    /// Push a snapshot of the affected words onto the context-save arena,
    /// then apply `graph.flag_masks.get(mask_id)` to the live flag bank.
    ///
    /// Uses a table reference instead of inline masks so that masks spanning
    /// many words do not bloat the instruction array.
    PushFlags { mask_id: u32 },

    /// Restore the words saved by the matching `PushFlags`.
    PopFlags,

    // ── Captures ─────────────────────────────────────────────────────────────

    CaptureBegin { tag: Tag },
    CaptureEnd   { tag: Tag },

    // ── Green/Red tree ───────────────────────────────────────────────────────
    //
    // The grammar author wraps matched content in node/token/trivia builders.
    // The VM emits TreeEvents into a side buffer; on success, the caller passes
    // that buffer to `green::build_green_tree`.
    //
    // Backtracking discards uncommitted events exactly as it does CaptureEvents.
    // `PartialCommit` (used by loops) updates the tree mark so each successful
    // loop iteration is permanently committed.

    /// Open a syntax node.  Emits `TreeEvent::NodeOpen`.
    NodeBegin { kind: SyntaxKind },

    /// Close the current syntax node.  Emits `TreeEvent::NodeClose`.
    NodeEnd,

    /// Begin a leaf token; saves `(kind, is_trivia, pos)` on the token-open
    /// side-stack.  Does **not** emit a `TreeEvent` yet.
    TokenBegin { kind: SyntaxKind, is_trivia: bool },

    /// End a leaf token; pops the token-open side-stack and emits
    /// `TreeEvent::Token { start, end, kind, is_trivia }`.
    TokenEnd,

    // ── Accept ───────────────────────────────────────────────────────────────

    Accept,
}

// ─── Literal table ────────────────────────────────────────────────────────────

pub struct LiteralTable {
    pub data:    &'static [u8],
    pub offsets: &'static [u32],
}

impl LiteralTable {
    #[inline(always)]
    pub fn get(&self, id: u32) -> &[u8] {
        let s = self.offsets[id as usize]     as usize;
        let e = self.offsets[id as usize + 1] as usize;
        &self.data[s..e]
    }
}

// ─── Flag-mask table ──────────────────────────────────────────────────────────

/// Stores all sparse flag masks used by [`Insn::PushFlags`].
///
/// Layout mirrors [`LiteralTable`]: all [`FlagMaskWord`] entries are
/// concatenated into `data`; `offsets[m]..offsets[m+1]` gives the slice
/// for mask `m`.
pub struct FlagMaskTable {
    /// All `FlagMaskWord` entries concatenated.
    pub data:    &'static [FlagMaskWord],
    /// Offset into `data` for each mask.  Length = num_masks + 1.
    pub offsets: &'static [u32],
}

impl FlagMaskTable {
    /// Return the entries for mask `id`.
    #[inline(always)]
    pub fn get(&self, id: u32) -> &[FlagMaskWord] {
        let s = self.offsets[id as usize]     as usize;
        let e = self.offsets[id as usize + 1] as usize;
        &self.data[s..e]
    }
}

// ─── Parse graph ─────────────────────────────────────────────────────────────

pub struct ParseGraph {
    pub insns:       &'static [Insn],
    pub rule_entry:  &'static [InsnId],
    pub literals:    LiteralTable,
    pub jump_tables: &'static [[u32; 256]],
    pub flag_masks:  FlagMaskTable,
    pub rule_names:  &'static [&'static str],
    pub tag_names:   &'static [&'static str],
}

impl ParseGraph {
    #[inline(always)]
    pub fn start(&self) -> InsnId { self.rule_entry[0] }

    #[inline(always)]
    pub fn insn(&self, id: InsnId) -> Insn {
        unsafe { *self.insns.get_unchecked(id as usize) }
    }

    #[inline(always)]
    pub fn dispatch(&self, table_id: u32, byte: u8) -> InsnId {
        unsafe {
            *self.jump_tables
                .get_unchecked(table_id as usize)
                .get_unchecked(byte as usize)
        }
    }
}
