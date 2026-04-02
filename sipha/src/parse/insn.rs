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
    /// Match exactly one byte `byte`.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::byte`](crate::parse::builder::GrammarBuilder::byte)
    Byte { byte: u8, on_fail: InsnId },
    /// Match exactly one of two byte values.
    ///
    /// Slightly smaller/faster than `Class` for tiny sets.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::byte_either`](crate::parse::builder::GrammarBuilder::byte_either)
    ByteEither { a: u8, b: u8, on_fail: InsnId },
    /// Match exactly one of three byte values.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::byte_in3`](crate::parse::builder::GrammarBuilder::byte_in3)
    ByteIn3 {
        a: u8,
        b: u8,
        c: u8,
        on_fail: InsnId,
    },
    /// Match any byte in the inclusive range `[lo, hi]`.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::byte_range`](crate::parse::builder::GrammarBuilder::byte_range)
    ByteRange { lo: u8, hi: u8, on_fail: InsnId },
    /// Index into the grammar's `class_labels` for diagnostics; 0 = default "character class".
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::class`](crate::parse::builder::GrammarBuilder::class),
    /// [`GrammarBuilder::class_with_label`](crate::parse::builder::GrammarBuilder::class_with_label)
    Class {
        class: CharClass,
        label_id: u32,
        on_fail: InsnId,
    },
    /// SIMD-accelerated for literals ≥ 16 bytes.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::literal`](crate::parse::builder::GrammarBuilder::literal)
    Literal { lit_id: u32, on_fail: InsnId },
    /// Inline literal of up to 8 bytes.
    ///
    /// Intended for very common short punctuation/keywords where indirection
    /// through the literal table is pure overhead.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::literal`](crate::parse::builder::GrammarBuilder::literal)
    LiteralSmall {
        len: u8,
        bytes: [u8; 8],
        on_fail: InsnId,
    },
    /// Match end-of-input.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::end_of_input`](crate::parse::builder::GrammarBuilder::end_of_input)
    EndOfInput { on_fail: InsnId },
    /// Unconditional failure.
    ///
    /// Builder source: [`GrammarBuilder::fail`](crate::parse::builder::GrammarBuilder::fail),
    /// and synthesized inside lookahead combinators.
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
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::any_char`](crate::parse::builder::GrammarBuilder::any_char)
    AnyChar { on_fail: InsnId },

    /// Match exactly the Unicode codepoint `codepoint`.
    /// Advances `pos` by the codepoint's UTF-8 byte length (1–4).
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::char`](crate::parse::builder::GrammarBuilder::char)
    Char { codepoint: u32, on_fail: InsnId },

    /// Match any codepoint in the inclusive range `[lo, hi]`.
    /// Fails if the decoded codepoint is outside the range, or on invalid UTF-8.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::char_range`](crate::parse::builder::GrammarBuilder::char_range)
    CharRange { lo: u32, hi: u32, on_fail: InsnId },

    // ── Control flow ──────────────────────────────────────────────────────────
    /// Unconditional jump to `target`.
    ///
    /// Builder source: synthesized by higher-level builders (not directly exposed),
    /// most prominently [`GrammarBuilder::byte_dispatch`](crate::parse::builder::GrammarBuilder::byte_dispatch).
    Jump { target: InsnId },
    /// Call a rule by id.
    ///
    /// Builder source: [`GrammarBuilder::call`](crate::parse::builder::GrammarBuilder::call),
    /// [`GrammarBuilder::call_id`](crate::parse::builder::GrammarBuilder::call_id)
    Call { rule: RuleId },
    /// Return from a rule call.
    ///
    /// Builder source: automatically emitted at the end of every rule body by
    /// [`GrammarBuilder::rule`](crate::parse::builder::GrammarBuilder::rule),
    /// [`GrammarBuilder::parser_rule`](crate::parse::builder::GrammarBuilder::parser_rule),
    /// and [`GrammarBuilder::lexer_rule`](crate::parse::builder::GrammarBuilder::lexer_rule).
    Return,

    // ── Backtracking ──────────────────────────────────────────────────────────
    /// Save a backtracking point with an alternate branch at `alt`.
    ///
    /// Builder source: [`GrammarBuilder::choice`](crate::parse::builder::GrammarBuilder::choice),
    /// [`GrammarBuilder::optional`](crate::parse::builder::GrammarBuilder::optional),
    /// [`GrammarBuilder::zero_or_more`](crate::parse::builder::GrammarBuilder::zero_or_more),
    /// [`GrammarBuilder::lookahead`](crate::parse::builder::GrammarBuilder::lookahead),
    /// [`GrammarBuilder::neg_lookahead`](crate::parse::builder::GrammarBuilder::neg_lookahead).
    Choice { alt: InsnId },
    /// Discard the most recent backtracking point and jump to `target`.
    ///
    /// Builder source: [`GrammarBuilder::choice`](crate::parse::builder::GrammarBuilder::choice),
    /// [`GrammarBuilder::optional`](crate::parse::builder::GrammarBuilder::optional),
    /// [`GrammarBuilder::cut`](crate::parse::builder::GrammarBuilder::cut).
    Commit { target: InsnId },
    /// Succeed without consuming input: restore position (and capture/tree state) to the most recent
    /// choice point, then jump to `target`.
    ///
    /// Builder source: [`GrammarBuilder::lookahead`](crate::parse::builder::GrammarBuilder::lookahead).
    BackCommit { target: InsnId },
    /// Negative lookahead commit: succeed iff the body failed, without consuming input, then jump to `target`.
    ///
    /// Builder source: [`GrammarBuilder::neg_lookahead`](crate::parse::builder::GrammarBuilder::neg_lookahead).
    NegBackCommit { target: InsnId },
    /// Commit partial progress and jump to `target`.
    ///
    /// Used by loops to commit events/captures per-iteration while still allowing the loop
    /// itself to terminate via backtracking.
    ///
    /// Builder source: [`GrammarBuilder::zero_or_more`](crate::parse::builder::GrammarBuilder::zero_or_more).
    PartialCommit { target: InsnId },

    // ── O(1) Dispatch ────────────────────────────────────────────────────────
    /// Dispatch to an arm based on the next input byte using a precomputed 256-entry table.
    ///
    /// Builder source: [`GrammarBuilder::byte_dispatch`](crate::parse::builder::GrammarBuilder::byte_dispatch).
    ByteDispatch { table_id: u32 },

    // ── Fused terminals ──────────────────────────────────────────────────────
    /// Consume a run of bytes matching `class`.
    ///
    /// Equivalent to `class` repeated with no backtracking in-between, and is
    /// commonly used for digit/ident/whitespace runs.
    ///
    /// Succeeds iff at least `min` bytes are consumed.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::consume_while_class`](crate::parse::builder::GrammarBuilder::consume_while_class),
    /// [`GrammarBuilder::consume_while_class_with_label`](crate::parse::builder::GrammarBuilder::consume_while_class_with_label),
    /// [`GrammarBuilder::consume_while_class1_with_label`](crate::parse::builder::GrammarBuilder::consume_while_class1_with_label).
    ConsumeWhileClass {
        class: CharClass,
        /// Index into `class_labels` for diagnostics (0 = "character class").
        label_id: u32,
        min: u32,
        on_fail: InsnId,
    },

    // ── Context flags ─────────────────────────────────────────────────────────
    //
    // Flag addressing: word = flag_id >> 6,  bit = flag_id & 63.
    // Supports up to 65,535 flags (FlagId = u16).
    /// Succeed iff `flags[flag_id >> 6] & (1 << (flag_id & 63)) != 0`.
    /// Does not consume input.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::if_flag`](crate::parse::builder::GrammarBuilder::if_flag).
    IfFlag { flag_id: u16, on_fail: InsnId },

    /// Succeed iff the named flag is **clear**.
    ///
    /// On failure, jump to `on_fail`.
    ///
    /// Builder source: [`GrammarBuilder::if_not_flag`](crate::parse::builder::GrammarBuilder::if_not_flag).
    IfNotFlag { flag_id: u16, on_fail: InsnId },

    /// Push a snapshot of the affected words onto the context-save arena,
    /// then apply `graph.flag_masks.get(mask_id)` to the live flag bank.
    ///
    /// Uses a table reference instead of inline masks so that masks spanning
    /// many words do not bloat the instruction array.
    ///
    /// Builder source: [`GrammarBuilder::with_flags`](crate::parse::builder::GrammarBuilder::with_flags).
    PushFlags { mask_id: u32 },

    /// Restore the words saved by the matching `PushFlags`.
    ///
    /// Builder source: [`GrammarBuilder::with_flags`](crate::parse::builder::GrammarBuilder::with_flags).
    PopFlags,

    // ── Captures ─────────────────────────────────────────────────────────────
    /// Begin a legacy capture region tagged with `tag`.
    ///
    /// Builder source: [`GrammarBuilder::capture`](crate::parse::builder::GrammarBuilder::capture).
    CaptureBegin { tag: Tag },
    /// End a legacy capture region tagged with `tag`.
    ///
    /// Builder source: [`GrammarBuilder::capture`](crate::parse::builder::GrammarBuilder::capture).
    CaptureEnd { tag: Tag },

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
    ///
    /// Builder source: [`GrammarBuilder::node`](crate::parse::builder::GrammarBuilder::node),
    /// [`GrammarBuilder::node_with_field`](crate::parse::builder::GrammarBuilder::node_with_field).
    NodeBegin {
        kind: SyntaxKind,
        field: Option<FieldId>,
    },

    /// Close the current syntax node.  Emits `TreeEvent::NodeClose`.
    ///
    /// Builder source: [`GrammarBuilder::node`](crate::parse::builder::GrammarBuilder::node),
    /// [`GrammarBuilder::node_with_field`](crate::parse::builder::GrammarBuilder::node_with_field).
    NodeEnd,

    /// Begin a leaf token; saves `(kind, is_trivia, pos)` on the token-open
    /// side-stack.  Does **not** emit a `TreeEvent` yet.
    ///
    /// Builder source: [`GrammarBuilder::token`](crate::parse::builder::GrammarBuilder::token),
    /// [`GrammarBuilder::trivia`](crate::parse::builder::GrammarBuilder::trivia).
    TokenBegin { kind: SyntaxKind, is_trivia: bool },

    /// End a leaf token; pops the token-open side-stack and emits
    /// `TreeEvent::Token { start, end, kind, is_trivia }`.
    ///
    /// Builder source: [`GrammarBuilder::token`](crate::parse::builder::GrammarBuilder::token),
    /// [`GrammarBuilder::trivia`](crate::parse::builder::GrammarBuilder::trivia).
    TokenEnd,

    /// Record an expected label at the current position for diagnostics, then continue.
    /// Used by [`expect_label`](crate::parse::builder::GrammarBuilder::expect_label).
    ///
    /// Builder source: [`GrammarBuilder::expect_label`](crate::parse::builder::GrammarBuilder::expect_label).
    RecordExpectedLabel { label_id: u32 },

    // ── Diagnostic context ("while parsing") ─────────────────────────────────
    /// Push a "while parsing ..." label onto the diagnostic context stack.
    /// Paired with [`Insn::PopDiagnosticContext`]; usually emitted as a bracket
    /// pair around rule bodies by `GrammarBuilder::context_rule`.
    PushDiagnosticContext { label_id: u32 },

    /// Pop the innermost diagnostic context entry.
    PopDiagnosticContext,

    // ── Dynamic hints ────────────────────────────────────────────────────────
    /// Attach a human-readable hint at this position if it is at (or beyond)
    /// the current furthest error position.
    ///
    /// The hint is an interned string ID in the grammar's shared string pool.
    SetHint { hint_id: SymbolId },

    // ── Optional runtime tracing ─────────────────────────────────────────────
    /// Runtime trace waypoint (only emitted when builder trace mode is enabled).
    ///
    /// `label_id` is an interned string ID in the grammar's shared string pool.
    TracePoint { label_id: SymbolId },

    // ── Error recovery ───────────────────────────────────────────────────────
    //
    // RecoverUntil: try the following "body"; on failure, skip input until
    // sync_rule matches (or EOI), then continue at resume. Used by
    // [`recover_until`](crate::parse::builder::GrammarBuilder::recover_until).
    /// Start a recoverable region. On failure, skip until `sync_rule` matches, then jump to resume.
    ///
    /// Builder source: [`GrammarBuilder::recover_until`](crate::parse::builder::GrammarBuilder::recover_until).
    RecoverUntil { sync_rule: RuleId, resume: InsnId },
    /// After sync rule matched during recovery; pops the recovery frame and continues.
    ///
    /// Builder source: [`GrammarBuilder::recover_until`](crate::parse::builder::GrammarBuilder::recover_until).
    RecoveryResume,

    // ── Accept ───────────────────────────────────────────────────────────────
    /// Accept the parse successfully.
    ///
    /// Builder source: [`GrammarBuilder::accept`](crate::parse::builder::GrammarBuilder::accept).
    Accept,
}

// ─── Literal table ────────────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
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

    /// Look up a [`RuleId`] by rule name.
    ///
    /// This is the inverse of [`rule_name`](Self::rule_name).
    #[must_use]
    pub fn rule_id(&self, name: &str) -> Option<RuleId> {
        self.rule_names.iter().enumerate().find_map(|(i, &sym)| {
            (self.strings.resolve(sym) == name)
                .then(|| RuleId::try_from(i).ok())
                .flatten()
        })
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

#[cfg(test)]
mod tests {
    use super::{FlagMaskTable, LiteralTable, ParseGraph};
    use crate::parse::string_table::{StringTable, SymbolId};

    #[test]
    fn rule_id_roundtrips_with_rule_name() {
        let pool: &'static [&'static str] = &["", "start", "expr", "stmt"];
        let strings = StringTable::from_static_pool(pool);
        let rule_names: &[SymbolId] = &[SymbolId(1), SymbolId(2), SymbolId(3)];

        let graph = ParseGraph {
            insns: &[],
            rule_entry: &[0],
            literals: LiteralTable {
                data: &[],
                offsets: &[0],
            },
            jump_tables: &[],
            flag_masks: FlagMaskTable {
                data: &[],
                offsets: &[0],
            },
            strings: &strings,
            rule_names,
            tag_names: &[],
            class_labels: &[],
            expected_labels: &[],
            field_names: &[],
        };

        for (id, &sym) in rule_names.iter().enumerate() {
            let name = graph.rule_name(id as u16).unwrap();
            assert_eq!(name, strings.resolve(sym));
            assert_eq!(graph.rule_id(name), Some(id as u16));
        }

        assert_eq!(graph.rule_id("does_not_exist"), None);
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

    fn resolve_symbol(&self, id: SymbolId) -> Option<&str> {
        Some(self.strings.resolve(id))
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

    fn resolve_symbol(&self, id: SymbolId) -> Option<&str> {
        Some(self.strings.resolve(id))
    }
}
