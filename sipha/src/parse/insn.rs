//! # Instruction Set
//!
//! The parse graph is a flat array of [`Insn`] values.
//!
//! ## Context-flag addressing
//!
//! Flags are indexed by a flat [`FlagId`](crate::parse::context::FlagId) (`u16`).
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

use crate::diagnostics::grammar_names::GrammarNames;
use crate::parse::context::FlagMaskWord;
use crate::parse::string_table::{StringTable, SymbolId};
use crate::types::{CharClass, FieldId, InsnId, RuleId, SyntaxKind, Tag};

// ─── Instruction enum ─────────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Insn {
    // ── Terminals ────────────────────────────────────────────────────────────
    Byte {
        byte: u8,
        on_fail: InsnId,
    },
    ByteRange {
        lo: u8,
        hi: u8,
        on_fail: InsnId,
    },
    /// Index into the grammar's `class_labels` for diagnostics; 0 = default "character class".
    Class {
        class: CharClass,
        label_id: u32,
        on_fail: InsnId,
    },
    /// SIMD-accelerated for literals ≥ 16 bytes.
    Literal {
        lit_id: u32,
        on_fail: InsnId,
    },
    EndOfInput {
        on_fail: InsnId,
    },
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
    AnyChar {
        on_fail: InsnId,
    },

    /// Match exactly the Unicode codepoint `codepoint`.
    /// Advances `pos` by the codepoint's UTF-8 byte length (1–4).
    Char {
        codepoint: u32,
        on_fail: InsnId,
    },

    /// Match any codepoint in the inclusive range `[lo, hi]`.
    /// Fails if the decoded codepoint is outside the range, or on invalid UTF-8.
    CharRange {
        lo: u32,
        hi: u32,
        on_fail: InsnId,
    },

    // ── Control flow ──────────────────────────────────────────────────────────
    Jump {
        target: InsnId,
    },
    Call {
        rule: RuleId,
    },
    Return,

    // ── Backtracking ──────────────────────────────────────────────────────────
    Choice {
        alt: InsnId,
    },
    Commit {
        target: InsnId,
    },
    BackCommit {
        target: InsnId,
    },
    NegBackCommit {
        target: InsnId,
    },
    PartialCommit {
        target: InsnId,
    },

    // ── O(1) Dispatch ────────────────────────────────────────────────────────
    ByteDispatch {
        table_id: u32,
    },

    // ── Context flags ─────────────────────────────────────────────────────────
    //
    // Flag addressing: word = flag_id >> 6,  bit = flag_id & 63.
    // Supports up to 65,535 flags (FlagId = u16).
    /// Succeed iff `flags[flag_id >> 6] & (1 << (flag_id & 63)) != 0`.
    /// Does not consume input.
    IfFlag {
        flag_id: u16,
        on_fail: InsnId,
    },

    /// Succeed iff the named flag is **clear**.
    IfNotFlag {
        flag_id: u16,
        on_fail: InsnId,
    },

    /// Push a snapshot of the affected words onto the context-save arena,
    /// then apply `graph.flag_masks.get(mask_id)` to the live flag bank.
    ///
    /// Uses a table reference instead of inline masks so that masks spanning
    /// many words do not bloat the instruction array.
    PushFlags {
        mask_id: u32,
    },

    /// Restore the words saved by the matching `PushFlags`.
    PopFlags,

    // ── Captures ─────────────────────────────────────────────────────────────
    CaptureBegin {
        tag: Tag,
    },
    CaptureEnd {
        tag: Tag,
    },

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
    /// `field` labels this node as a named child of its parent when present.
    NodeBegin {
        kind: SyntaxKind,
        field: Option<FieldId>,
    },

    /// Close the current syntax node.  Emits `TreeEvent::NodeClose`.
    NodeEnd,

    /// Begin a leaf token; saves `(kind, is_trivia, pos)` on the token-open
    /// side-stack.  Does **not** emit a `TreeEvent` yet.
    TokenBegin {
        kind: SyntaxKind,
        is_trivia: bool,
    },

    /// End a leaf token; pops the token-open side-stack and emits
    /// `TreeEvent::Token { start, end, kind, is_trivia }`.
    TokenEnd,

    /// Record an expected label at the current position for diagnostics, then continue.
    /// Used by [`expect_label`](crate::parse::builder::GrammarBuilder::expect_label).
    RecordExpectedLabel {
        label_id: u32,
    },

    // ── Error recovery ───────────────────────────────────────────────────────
    //
    // RecoverUntil: try the following "body"; on failure, skip input until
    // sync_rule matches (or EOI), then continue at resume. Used by
    // [`recover_until`](crate::parse::builder::GrammarBuilder::recover_until).
    /// Start a recoverable region. On failure, skip until `sync_rule` matches, then jump to resume.
    RecoverUntil {
        sync_rule: RuleId,
        resume: InsnId,
    },
    /// After sync rule matched during recovery; pops the recovery frame and continues.
    RecoveryResume,

    // ── Accept ───────────────────────────────────────────────────────────────
    Accept,
}

// ─── Literal table ────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
pub struct LiteralTable<'a> {
    pub data: &'a [u8],
    pub offsets: &'a [u32],
}

impl LiteralTable<'_> {
    #[must_use]
    #[inline]
    pub fn get(&self, id: u32) -> &[u8] {
        let s = self.offsets[id as usize] as usize;
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
#[derive(Clone, Copy)]
pub struct FlagMaskTable<'a> {
    /// All `FlagMaskWord` entries concatenated.
    pub data: &'a [FlagMaskWord],
    /// Offset into `data` for each mask.  Length = `num_masks` + 1.
    pub offsets: &'a [u32],
}

impl FlagMaskTable<'_> {
    /// Return the entries for mask `id`.
    #[must_use]
    #[inline]
    pub fn get(&self, id: u32) -> &[FlagMaskWord] {
        let s = self.offsets[id as usize] as usize;
        let e = self.offsets[id as usize + 1] as usize;
        &self.data[s..e]
    }
}

// ─── Parse graph ─────────────────────────────────────────────────────────────

/// VM bytecode and lookup tables, borrowing backing storage (e.g. from [`crate::parse::builder::BuiltGraph`]).
///
/// For generated static grammars, use `ParseGraph<'static>` with `&'static` slices.
#[derive(Clone, Copy)]
pub struct ParseGraph<'a> {
    pub insns: &'a [Insn],
    pub rule_entry: &'a [InsnId],
    pub literals: LiteralTable<'a>,
    pub jump_tables: &'a [[u32; 256]],
    pub flag_masks: FlagMaskTable<'a>,
    /// Shared string pool; [`SymbolId`] values index into it.
    pub strings: &'a StringTable,
    pub rule_names: &'a [SymbolId],
    pub tag_names: &'a [SymbolId],
    /// Labels for [`Insn::Class`] diagnostics; index 0 is the default "character class".
    pub class_labels: &'a [SymbolId],
    /// Labels for [`Expected::Label`](crate::diagnostics::error::Expected::Label) from [`expect_label`](crate::parse::builder::GrammarBuilder::expect_label).
    pub expected_labels: &'a [SymbolId],
    /// Names for named child fields (indexed by [`crate::types::FieldId`]); used by `field_by_id` / name resolution.
    pub field_names: &'a [SymbolId],
}

impl ParseGraph<'_> {
    /// Resolve display name for a rule id.
    #[must_use]
    #[inline]
    pub fn rule_name(&self, id: RuleId) -> Option<&str> {
        self.rule_names
            .get(id as usize)
            .map(|&sym| self.strings.resolve(sym))
    }

    /// Resolve [`Expected::Label`](crate::diagnostics::error::Expected::Label) text.
    #[must_use]
    #[inline]
    pub fn expected_label(&self, id: u32) -> Option<&str> {
        self.expected_labels
            .get(id as usize)
            .map(|&sym| self.strings.resolve(sym))
    }

    /// Resolve class diagnostic label for insn `label_id`.
    #[must_use]
    #[inline]
    pub fn class_label(&self, label_id: u32) -> Option<&str> {
        self.class_labels
            .get(label_id as usize)
            .map(|&sym| self.strings.resolve(sym))
    }

    #[must_use]
    #[inline]
    pub fn start(&self) -> InsnId {
        self.rule_entry[0]
    }

    #[must_use]
    #[inline]
    pub fn insn(&self, id: InsnId) -> Insn {
        unsafe { *self.insns.get_unchecked(id as usize) }
    }

    #[must_use]
    #[inline]
    pub fn dispatch(&self, table_id: u32, byte: u8) -> InsnId {
        unsafe {
            *self
                .jump_tables
                .get_unchecked(table_id as usize)
                .get_unchecked(byte as usize)
        }
    }

    /// Resolve a tag name symbol.
    #[must_use]
    #[inline]
    pub fn tag_name(&self, id: Tag) -> &str {
        self.tag_names
            .get(id as usize)
            .map_or("?", |&s| self.strings.resolve(s))
    }

    /// Resolve a field name symbol.
    #[must_use]
    #[inline]
    pub fn field_name(&self, id: FieldId) -> &str {
        self.field_names
            .get(id as usize)
            .map_or("?", |&s| self.strings.resolve(s))
    }

    /// Look up [`FieldId`] by field name string.
    #[must_use]
    pub fn field_id(&self, name: &str) -> Option<FieldId> {
        self.field_names.iter().enumerate().find_map(|(i, &sym)| {
            (self.strings.resolve(sym) == name)
                .then(|| FieldId::try_from(i).ok())
                .flatten()
        })
    }
}

impl GrammarNames for ParseGraph<'_> {
    fn rule_name(&self, id: RuleId) -> Option<&str> {
        self.rule_names
            .get(id as usize)
            .map(|&sym| self.strings.resolve(sym))
    }

    fn expected_label(&self, id: u32) -> Option<&str> {
        self.expected_labels
            .get(id as usize)
            .map(|&sym| self.strings.resolve(sym))
    }

    fn class_label(&self, label_id: u32) -> Option<&str> {
        self.class_labels
            .get(label_id as usize)
            .map(|&sym| self.strings.resolve(sym))
    }
}

impl<'a> GrammarNames for &'a ParseGraph<'a> {
    fn rule_name(&self, id: RuleId) -> Option<&str> {
        ParseGraph::rule_name(self, id)
    }

    fn expected_label(&self, id: u32) -> Option<&str> {
        ParseGraph::expected_label(self, id)
    }

    fn class_label(&self, label_id: u32) -> Option<&str> {
        ParseGraph::class_label(self, label_id)
    }
}
