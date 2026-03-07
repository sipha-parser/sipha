//! # Grammar Builder
//!
//! Constructs a [`ParseGraph`] by composing PEG combinators.
//!
//! ## Usage pattern
//!
//! ```rust,ignore
//! let mut g = GrammarBuilder::new();
//!
//! g.rule("expr", |g| {
//!     g.node(NODE_EXPR, |g| {
//!         g.token(TOK_NUM, |g| g.repeat(1.., |g| g.class(classes::DIGIT)));
//!     });
//! });
//!
//! let graph = g.finish()?.as_graph();
//! ```
//!
//! Every combinator takes a closure body — the builder emits instructions
//! immediately as closures are called, so instruction addresses are known
//! at build time with no second pass.
//!
//! ## Terminal hierarchy
//!
//! | Method | Level | Matches |
//! |---|---|---|
//! | `byte(b)` | byte | Exactly one byte |
//! | `byte_range(lo, hi)` | byte | Byte in `[lo, hi]` |
//! | `class(cls)` | byte | Membership in a 256-bit `CharClass` |
//! | `literal(bytes)` | byte | Exact byte string (SIMD-accelerated ≥16 bytes) |
//! | `any_char()` | codepoint | Any valid UTF-8 codepoint |
//! | `char(c)` | codepoint | Exactly one codepoint |
//! | `char_range(lo, hi)` | codepoint | Codepoint in `[lo, hi]` |
//!
//! Byte-level terminals skip the UTF-8 decode; prefer them for ASCII grammars.
//!
//! ## Repetition
//!
//! [`GrammarBuilder::repeat`] expands at build time — no counter register
//! in the VM.  Accepts any `Into<Repeat>`, so Rust range syntax works:
//!
//! ```rust,ignore
//! g.repeat(3,     body);   // Exact(3)
//! g.repeat(1..,   body);   // AtLeast(1)
//! g.repeat(..=5,  body);   // AtMost(5)
//! g.repeat(2..=4, body);   // Between(2, 4)
//! ```
//!
//! ## Automatic trivia skipping
//!
//! Real grammars must allow whitespace and comments between almost every pair
//! of tokens.  Writing `g.call("ws")` between every element is verbose and
//! error-prone.  The builder's **trivia system** automates this:
//!
//! 1. **Register a trivia rule** — call [`set_trivia_rule`] once with the
//!    name of the rule that matches whitespace / comments.
//!
//! 2. **Declare parser rules** — use [`parser_rule`] instead of [`rule`] for
//!    structural rules.  Inside a parser rule, every [`call`], [`token`], and
//!    [`byte_dispatch`] automatically emits a trivia-skip first.
//!
//! 3. **Declare lexer rules** — use [`lexer_rule`] for token-matching rules
//!    (strings, numbers, identifiers).  No trivia is injected inside lexer
//!    rules, keeping the byte-level pattern intact.
//!
//! ```rust,ignore
//! g.set_trivia_rule("ws");
//!
//! g.parser_rule("object", |g| {
//!     g.node(NODE_OBJECT, |g| {
//!         g.token(TOK_LBRACE, |g| g.byte(b'{'));   // ws + {
//!         g.optional(|g| {
//!             g.call("member");                     // ws + member
//!             g.zero_or_more(|g| {
//!                 g.token(TOK_COMMA, |g| g.byte(b',')); // ws + ,
//!                 g.call("member");                 // ws + member
//!             });
//!         });
//!         g.token(TOK_RBRACE, |g| g.byte(b'}'));   // ws + }
//!     });
//! });
//!
//! // Lexer rules match bytes directly; no trivia is injected.
//! g.lexer_rule("ws", |g| {
//!     g.trivia(TRIVIA_WS, |g| {
//!         g.zero_or_more(|g| g.class(classes::WHITESPACE));
//!     });
//! });
//! ```
//!
//! ### What auto-skips and what doesn't
//!
//! Inside a `parser_rule`, trivia is injected **before**:
//! - [`call`] and [`call_id`] — calling another rule
//! - [`token`] — starting a semantic leaf token
//! - [`byte_dispatch`] — dispatching on the next byte
//!
//! Trivia is **never** injected inside:
//! - [`token`] bodies — the token's interior is lexer-level
//! - [`trivia`] bodies — trivia cannot contain more trivia
//! - [`lexer_rule`] bodies — the whole rule is byte-level
//! - [`no_skip`] blocks — explicit suppression
//!
//! Structural combinators ([`choice`], [`optional`], [`zero_or_more`],
//! [`one_or_more`], [`lookahead`], [`neg_lookahead`], [`node`]) are
//! transparent: they pass the current auto-skip mode through to their bodies.
//!
//! Use explicit [`skip()`] for points that aren't covered by auto-injection —
//! most commonly, trailing trivia before [`end_of_input`].
//!
//! [`set_trivia_rule`]: GrammarBuilder::set_trivia_rule
//! [`parser_rule`]: GrammarBuilder::parser_rule
//! [`lexer_rule`]: GrammarBuilder::lexer_rule
//! [`rule`]: GrammarBuilder::rule
//! [`call`]: GrammarBuilder::call
//! [`call_id`]: GrammarBuilder::call_id
//! [`token`]: GrammarBuilder::token
//! [`trivia`]: GrammarBuilder::trivia
//! [`byte_dispatch`]: GrammarBuilder::byte_dispatch
//! [`no_skip`]: GrammarBuilder::no_skip
//! [`skip()`]: GrammarBuilder::skip
//! [`end_of_input`]: GrammarBuilder::end_of_input
//! [`node`]: GrammarBuilder::node
//! [`choice`]: GrammarBuilder::choice
//! [`optional`]: GrammarBuilder::optional
//! [`zero_or_more`]: GrammarBuilder::zero_or_more
//! [`one_or_more`]: GrammarBuilder::one_or_more
//! [`lookahead`]: GrammarBuilder::lookahead
//! [`neg_lookahead`]: GrammarBuilder::neg_lookahead

use std::collections::{BTreeMap, HashMap, HashSet};
use crate::{
    context::{FlagId, FlagMaskWord},
    insn::{FlagMaskTable, Insn, LiteralTable, ParseGraph},
    types::{CharClass, FieldId, InsnId, IntoSyntaxKind, RuleId, Tag},
};

/// N-way choice without `Vec<Box<dyn FnOnce>>`: `choices!(g, |g| g.literal(b"a"), |g| g.literal(b"b"))`.
#[macro_export]
macro_rules! choices {
    ($g:expr, $first:expr $(, $rest:expr)* $(,)?) => {
        $g.choices(vec![
            Box::new($first),
            $(Box::new($rest),)*
        ])
    };
}

// ─── Repeat ───────────────────────────────────────────────────────────────────

/// Repetition count for [`GrammarBuilder::repeat`].
///
/// Construct directly or via the `From` impls that accept Rust range literals:
///
/// | Value | Meaning |
/// |---|---|
/// | `3u32` or `Exact(3)` | Exactly 3 |
/// | `1u32..` or `AtLeast(1)` | At least 1 |
/// | `..=5u32` or `AtMost(5)` | At most 5 |
/// | `2u32..=4` or `Between(2, 4)` | Between 2 and 4 (inclusive) |
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Repeat {
    /// `e{n}` — exactly n times.
    Exact(u32),
    /// `e{n,}` — at least n times.
    AtLeast(u32),
    /// `e{,n}` — at most n times.
    AtMost(u32),
    /// `e{min,max}` — between min and max times (inclusive).
    Between(u32, u32),
}

impl From<u32>                             for Repeat {
    fn from(n: u32) -> Self { Self::Exact(n) }
}
impl From<std::ops::RangeFrom<u32>>        for Repeat {
    fn from(r: std::ops::RangeFrom<u32>) -> Self { Self::AtLeast(r.start) }
}
impl From<std::ops::RangeToInclusive<u32>> for Repeat {
    fn from(r: std::ops::RangeToInclusive<u32>) -> Self { Self::AtMost(r.end) }
}
impl From<std::ops::RangeInclusive<u32>>   for Repeat {
    fn from(r: std::ops::RangeInclusive<u32>) -> Self { Self::Between(*r.start(), *r.end()) }
}

// ─── Internal: LiteralInterner ────────────────────────────────────────────────

/// Appends byte strings into a flat blob and records offsets.
///
/// Layout: `data[offsets[i]..offsets[i+1]]` is the bytes for literal `i`.
struct LiteralInterner {
    data:    Vec<u8>,
    offsets: Vec<u32>,
}

impl LiteralInterner {
    fn new() -> Self {
        Self { data: Vec::new(), offsets: vec![0] }
    }

    /// Append `bytes` and return the id of the new literal.
    fn intern(&mut self, bytes: &[u8]) -> u32 {
        let id = (self.offsets.len() - 1) as u32;
        self.data.extend_from_slice(bytes);
        self.offsets.push(self.data.len() as u32);
        id
    }

    /// Ensure the sentinel offset is present (called by `finish`).
    fn seal(&mut self) {
        if self.offsets.last() != Some(&(self.data.len() as u32)) {
            self.offsets.push(self.data.len() as u32);
        }
    }
}

// ─── Internal: FlagMaskInterner ───────────────────────────────────────────────

/// Stores sparse `(word, set_bits, clear_bits)` entries for flag masks.
///
/// Each call to [`intern`](FlagMaskInterner::intern) groups the provided flag
/// IDs by their 64-bit word, appends the resulting [`FlagMaskWord`] entries,
/// and records the slice offset.
struct FlagMaskInterner {
    data:    Vec<FlagMaskWord>,
    offsets: Vec<u32>,
}

impl FlagMaskInterner {
    fn new() -> Self {
        Self { data: Vec::new(), offsets: vec![0] }
    }

    /// Intern a mask defined by `set_ids` and `clear_ids` and return its id.
    ///
    /// IDs in `set_ids` are set first, then IDs in `clear_ids` are cleared —
    /// so if an ID appears in both, the net effect is clear.
    fn intern(&mut self, set_ids: &[FlagId], clear_ids: &[FlagId]) -> u32 {
        // Group by word index (BTreeMap gives deterministic order).
        let mut by_word: BTreeMap<u32, (u64, u64)> = BTreeMap::new();
        for &id in set_ids {
            by_word.entry((id >> 6) as u32).or_default().0 |= 1u64 << (id & 63);
        }
        for &id in clear_ids {
            by_word.entry((id >> 6) as u32).or_default().1 |= 1u64 << (id & 63);
        }

        let id = (self.offsets.len() - 1) as u32;
        for (word, (set_bits, clear_bits)) in by_word {
            self.data.push(FlagMaskWord { word, set_bits, clear_bits });
        }
        self.offsets.push(self.data.len() as u32);
        id
    }

    /// Ensure the sentinel offset is present (called by `finish`).
    fn seal(&mut self) {
        if self.offsets.last() != Some(&(self.data.len() as u32)) {
            self.offsets.push(self.data.len() as u32);
        }
    }
}

// ─── BuiltGraph ───────────────────────────────────────────────────────────────

/// The fully-built grammar tables, ready to be handed to the parse engine.
///
/// Call [`as_graph`](BuiltGraph::as_graph) to obtain a [`ParseGraph`] that
/// borrows from this value.  Keep `BuiltGraph` alive for as long as the
/// `ParseGraph` (or any derived references) are in use.
pub struct BuiltGraph {
    pub insns:             Vec<Insn>,
    pub rule_entry:        Vec<InsnId>,
    pub literal_data:      Vec<u8>,
    pub literal_offsets:   Vec<u32>,
    pub jump_tables:       Vec<[u32; 256]>,
    pub flag_mask_data:    Vec<FlagMaskWord>,
    pub flag_mask_offsets: Vec<u32>,
    pub rule_names:        Vec<&'static str>,
    pub tag_names:         Vec<&'static str>,
    /// Labels for [`Insn::Class`] diagnostics; index 0 is the default "character class".
    pub class_labels:      Vec<&'static str>,
    /// Labels for [`expect_label`](GrammarBuilder::expect_label).
    pub expected_labels:   Vec<&'static str>,
    /// Names for named child fields (indexed by [`FieldId`]); used by `field_by_id` / name resolution.
    pub field_names:       Vec<&'static str>,
}

impl BuiltGraph {
    /// Return a [`ParseGraph`] that borrows all tables from this `BuiltGraph`.
    ///
    /// # Safety and lifetime
    ///
    /// `ParseGraph` uses `&'static [T]` slices so it can be `Copy` and
    /// embedded in static grammars.  Here those references are produced by
    /// casting the backing `Vec` pointers.  **You must ensure:**
    ///
    /// - The `BuiltGraph` is not mutated after `as_graph` is called, and
    /// - The `BuiltGraph` outlives every use of the returned `ParseGraph`
    ///   (do not drop or move the `BuiltGraph` while the graph is in use).
    ///
    /// Violating these conditions is undefined behavior.  In typical usage
    /// the `BuiltGraph` is stored in a variable that outlives the parse
    /// call (e.g. `let built = g.finish()?; let graph = built.as_graph();`).
    pub fn as_graph(&self) -> ParseGraph {
        // SAFETY: see doc comment above.
        unsafe fn to_static<T>(v: &[T]) -> &'static [T] {
            std::slice::from_raw_parts(v.as_ptr(), v.len())
        }
        unsafe {
            ParseGraph {
                insns:       to_static(&self.insns),
                rule_entry:  to_static(&self.rule_entry),
                jump_tables: to_static(&self.jump_tables),
                literals: LiteralTable {
                    data:    to_static(&self.literal_data),
                    offsets: to_static(&self.literal_offsets),
                },
                flag_masks: FlagMaskTable {
                    data:    to_static(&self.flag_mask_data),
                    offsets: to_static(&self.flag_mask_offsets),
                },
                rule_names:    to_static(&self.rule_names),
                tag_names:     to_static(&self.tag_names),
                class_labels:    if self.class_labels.is_empty() {
                    const DEFAULT: &[&'static str] = &["character class"];
                    DEFAULT
                } else {
                    to_static(&self.class_labels)
                },
                expected_labels: to_static(&self.expected_labels),
                field_names:     to_static(&self.field_names),
            }
        }
    }
}

// ─── GrammarBuilder ───────────────────────────────────────────────────────────

/// Builds a [`ParseGraph`] by composing PEG combinators.
///
/// The builder is stateful: it appends instructions as combinators are called.
/// Call [`finish`](GrammarBuilder::finish) when the grammar is complete.
pub struct GrammarBuilder {
    // ── Instruction stream ────────────────────────────────────────────────────
    insns: Vec<Insn>,

    // ── Rule registry ─────────────────────────────────────────────────────────
    rule_entry:    Vec<InsnId>,
    rule_by_name:  HashMap<String, RuleId>,
    rule_names:    Vec<&'static str>,
    /// `(insn_addr, rule_name)` pairs resolved during [`finish`](Self::finish).
    pending_calls: Vec<(usize, String)>,
    /// `(insn_addr, sync_rule_name)` for [`RecoverUntil`](Insn::RecoverUntil); resolved in [`finish`](Self::finish).
    pending_recover: Vec<(usize, String)>,

    // ── Tag registry ──────────────────────────────────────────────────────────
    tag_by_name: HashMap<String, Tag>,
    tag_names:   Vec<&'static str>,

    // ── Field names (for named child access) ─────────────────────────────────
    field_by_name: HashMap<String, FieldId>,

    // ── Interners ─────────────────────────────────────────────────────────────
    literals:   LiteralInterner,
    flag_masks: FlagMaskInterner,

    // ── Jump tables (for ByteDispatch) ────────────────────────────────────────
    jump_tables: Vec<[u32; 256]>,

    // ── Trivia / auto-skip ────────────────────────────────────────────────────
    /// Name of the rule to call when skipping trivia.
    /// `None` if [`set_trivia_rule`](Self::set_trivia_rule) has not been called.
    trivia_rule: Option<String>,

    /// Whether the builder is currently in auto-skip mode.
    ///
    /// When `true`, [`call`](Self::call), [`token`](Self::token), and
    /// [`byte_dispatch`](Self::byte_dispatch) emit a trivia-skip before
    /// themselves.  Toggled by [`parser_rule`](Self::parser_rule),
    /// [`lexer_rule`](Self::lexer_rule), and [`no_skip`](Self::no_skip), and
    /// suppressed inside [`token`](Self::token) and [`trivia`](Self::trivia)
    /// bodies.
    auto_trivia: bool,

    /// Labels for [`Insn::Class`] diagnostics; index 0 is "character class".
    class_labels: Vec<&'static str>,

    /// Labels for [`expect_label`](Self::expect_label).
    expected_labels: Vec<&'static str>,

    /// Names for named child fields (indexed by [`FieldId`]).
    field_names: Vec<&'static str>,

    /// If `true`, [`finish`](Self::finish) will not report an error for
    /// left-recursive or mutually recursive rule cycles. Use when the grammar
    /// uses packrat memoization and intentionally has indirect recursion
    /// (e.g. expr → primary → object_pair → expr). Default is `false`.
    allow_rule_cycles: bool,

    /// If `false`, [`finish`](Self::finish) will return an error when any rule
    /// is unreachable from the start rule. Default is `true` (allow unreachable).
    allow_unreachable_rules: bool,
}

impl GrammarBuilder {
    pub fn new() -> Self {
        Self {
            insns:         Vec::with_capacity(1024),
            rule_entry:    Vec::new(),
            rule_by_name:  HashMap::new(),
            rule_names:    Vec::new(),
            pending_calls:   Vec::new(),
            pending_recover: Vec::new(),
            tag_by_name:     HashMap::new(),
            tag_names:     Vec::new(),
            field_by_name:   HashMap::new(),
            literals:      LiteralInterner::new(),
            flag_masks:    FlagMaskInterner::new(),
            jump_tables:       Vec::new(),
            trivia_rule:       None,
            auto_trivia:       false,
            class_labels:      vec!["character class"],
            expected_labels:       Vec::new(),
            field_names:       Vec::new(),
            allow_rule_cycles:     false,
            allow_unreachable_rules: true,
        }
    }

    /// Allow rule cycles (left-recursion or mutual recursion) when finishing.
    ///
    /// When enabled, [`finish`](Self::finish) will not return an error if the
    /// call graph contains a cycle. Use for grammars that run with
    /// [`Engine::with_memo`](crate::engine::Engine::with_memo) and have
    /// intentional indirect recursion (e.g. expression precedence chains that
    /// cycle back through primary → object_pair → expr).
    pub fn allow_rule_cycles(&mut self, allow: bool) -> &mut Self {
        self.allow_rule_cycles = allow;
        self
    }

    /// If `false`, [`finish`](Self::finish) will error when any rule is unreachable
    /// from the start rule. Default is `true`.
    pub fn allow_unreachable_rules(&mut self, allow: bool) -> &mut Self {
        self.allow_unreachable_rules = allow;
        self
    }

    // ── Trivia registration ───────────────────────────────────────────────────

    /// Register the rule to call whenever trivia should be skipped.
    ///
    /// `name` must match a rule defined via [`rule`](Self::rule),
    /// [`parser_rule`](Self::parser_rule), or [`lexer_rule`](Self::lexer_rule)
    /// (order does not matter — the reference is resolved at
    /// [`finish`](Self::finish) time).
    ///
    /// After calling this, use [`parser_rule`](Self::parser_rule) for
    /// structural rules that should skip trivia automatically, and
    /// [`lexer_rule`](Self::lexer_rule) for token-matching rules that must not.
    ///
    /// ```rust,ignore
    /// g.set_trivia_rule("ws");
    ///
    /// g.lexer_rule("ws", |g| {
    ///     g.trivia(TRIVIA_WS, |g| {
    ///         g.zero_or_more(|g| g.class(classes::WHITESPACE));
    ///     });
    /// });
    /// ```
    pub fn set_trivia_rule(&mut self, name: impl Into<String>) {
        self.trivia_rule = Some(name.into());
    }

    /// Emit a call to the registered trivia rule right now.
    ///
    /// This is a no-op if no trivia rule has been set.
    ///
    /// Trivia is injected automatically before [`call`](Self::call),
    /// [`token`](Self::token), and [`byte_dispatch`](Self::byte_dispatch)
    /// inside [`parser_rule`](Self::parser_rule) bodies.  Use `skip()`
    /// explicitly for positions not covered by auto-injection — most commonly,
    /// trailing trivia before [`end_of_input`](Self::end_of_input):
    ///
    /// ```rust,ignore
    /// g.parser_rule("document", |g| {
    ///     g.node(NODE_DOCUMENT, |g| {
    ///         g.call("value");  // auto: ws + value
    ///         g.skip();         // explicit trailing ws before EOF
    ///     });
    ///     g.end_of_input();
    ///     g.accept();
    /// });
    /// ```
    pub fn skip(&mut self) {
        if let Some(name) = self.trivia_rule.clone() {
            // Disable auto_trivia while emitting the skip call itself to
            // prevent infinite recursion (skip → call → skip → call → …).
            let prev = self.auto_trivia;
            self.auto_trivia = false;
            self.call(name);
            self.auto_trivia = prev;
        }
    }

    // ── Rules ─────────────────────────────────────────────────────────────────

    /// Define a **parser-level** rule with automatic trivia skipping.
    ///
    /// Inside the body, [`call`], [`call_id`], [`token`], and
    /// [`byte_dispatch`] automatically emit a [`skip()`] before themselves.
    /// The auto-skip is suppressed inside [`token`] and [`trivia`] bodies
    /// (which are always lexer-level) and inside [`no_skip`] blocks.
    ///
    /// Structural combinators ([`choice`], [`optional`], [`zero_or_more`],
    /// [`one_or_more`], [`lookahead`], [`neg_lookahead`], [`node`]) are
    /// transparent: they pass the current auto-skip mode through.
    ///
    /// ```rust,ignore
    /// g.set_trivia_rule("ws");
    ///
    /// g.parser_rule("object", |g| {
    ///     g.node(NODE_OBJECT, |g| {
    ///         g.token(TOK_LBRACE, |g| g.byte(b'{'));  // ws + {
    ///         g.optional(|g| {
    ///             g.call("member");                    // ws + member
    ///             g.zero_or_more(|g| {
    ///                 g.token(TOK_COMMA,  |g| g.byte(b',')); // ws + ,
    ///                 g.call("member");                // ws + member
    ///             });
    ///         });
    ///         g.token(TOK_RBRACE, |g| g.byte(b'}'));  // ws + }
    ///     });
    /// });
    /// ```
    ///
    /// [`call`]: Self::call
    /// [`call_id`]: Self::call_id
    /// [`token`]: Self::token
    /// [`byte_dispatch`]: Self::byte_dispatch
    /// [`skip()`]: Self::skip
    /// [`trivia`]: Self::trivia
    /// [`no_skip`]: Self::no_skip
    /// [`choice`]: Self::choice
    /// [`optional`]: Self::optional
    /// [`zero_or_more`]: Self::zero_or_more
    /// [`one_or_more`]: Self::one_or_more
    /// [`lookahead`]: Self::lookahead
    /// [`neg_lookahead`]: Self::neg_lookahead
    /// [`node`]: Self::node
    pub fn parser_rule<F>(&mut self, name: &'static str, body: F) -> RuleId
    where F: FnOnce(&mut Self)
    {
        self.rule_impl(name, body, true)
    }

    /// Define a **lexer-level** rule with trivia skipping suppressed.
    ///
    /// No auto-skip is injected anywhere inside the body.  Use this for
    /// token-matching rules (strings, numbers, identifiers, whitespace) that
    /// operate at byte level and must not be interrupted by trivia.
    ///
    /// ```rust,ignore
    /// g.lexer_rule("number", |g| {
    ///     g.token(TOK_NUMBER, |g| {
    ///         g.optional(|g| g.byte(b'-'));
    ///         g.one_or_more(|g| g.class(classes::DIGIT));
    ///     });
    /// });
    /// ```
    pub fn lexer_rule<F>(&mut self, name: &'static str, body: F) -> RuleId
    where F: FnOnce(&mut Self)
    {
        self.rule_impl(name, body, false)
    }

    /// Define a rule with neutral trivia behaviour (inherits the current mode).
    ///
    /// Prefer [`parser_rule`](Self::parser_rule) or [`lexer_rule`](Self::lexer_rule)
    /// to make intent explicit.  `rule` is retained for backward compatibility
    /// and for shared helper rules that can be embedded in either context.
    ///
    /// ```rust,ignore
    /// g.rule("digit", |g| {
    ///     g.class(classes::DIGIT);
    /// });
    /// ```
    pub fn rule<F>(&mut self, name: &'static str, body: F) -> RuleId
    where F: FnOnce(&mut Self)
    {
        let id = self.begin_rule(name);
        body(self);
        self.end_rule();
        id
    }

    /// Begin a rule manually.  Prefer the closure forms when possible.
    ///
    /// Every `begin_rule` must be followed by exactly one
    /// [`end_rule`](Self::end_rule).
    pub fn begin_rule(&mut self, name: &'static str) -> RuleId {
        let id = self.rule_entry.len() as RuleId;
        self.rule_entry.push(self.insns.len() as InsnId);
        self.rule_by_name.insert(name.to_string(), id);
        self.rule_names.push(name);
        id
    }

    /// End a rule opened by [`begin_rule`](Self::begin_rule).
    pub fn end_rule(&mut self) {
        self.emit(Insn::Return);
    }

    /// Suppress automatic trivia skipping for the duration of `body`.
    ///
    /// Useful inside a [`parser_rule`](Self::parser_rule) when a sub-expression
    /// must not skip trivia — for example, a tight multi-token keyword that
    /// would be corrupted if whitespace were inserted mid-match.
    ///
    /// Trivia is already suppressed automatically inside [`token`](Self::token)
    /// and [`trivia`](Self::trivia) bodies, so `no_skip` is mainly needed for
    /// sequences outside those wrappers.
    ///
    /// ```rust,ignore
    /// g.parser_rule("range_expr", |g| {
    ///     g.call("expr");
    ///     // ".." must not have whitespace inside it.
    ///     g.no_skip(|g| {
    ///         g.literal(b"..");
    ///     });
    ///     g.call("expr");
    /// });
    /// ```
    pub fn no_skip<F>(&mut self, body: F)
    where F: FnOnce(&mut Self)
    {
        let prev = self.auto_trivia;
        self.auto_trivia = false;
        body(self);
        self.auto_trivia = prev;
    }

    // ── Tags ──────────────────────────────────────────────────────────────────

    /// Intern a tag name and return its [`Tag`] id.  Idempotent: calling with
    /// the same string twice returns the same id.
    pub fn tag(&mut self, name: &'static str) -> Tag {
        if let Some(&id) = self.tag_by_name.get(name) {
            return id;
        }
        let id = self.tag_names.len() as Tag;
        self.tag_by_name.insert(name.to_string(), id);
        self.tag_names.push(name);
        id
    }

    // ── Low-level emission ────────────────────────────────────────────────────

    /// Append a raw instruction and return its address.
    ///
    /// Prefer the higher-level combinators.  This is exposed for grammars that
    /// need to emit instructions that have no combinator wrapper.
    pub fn emit(&mut self, insn: Insn) -> InsnId {
        let id = self.insns.len() as InsnId;
        self.insns.push(insn);
        id
    }

    /// Return the address the *next* emitted instruction will have.
    fn current_ip(&self) -> InsnId {
        self.insns.len() as InsnId
    }

    /// Back-patch a forward reference in an already-emitted instruction.
    fn patch(&mut self, addr: InsnId, target: InsnId) {
        match &mut self.insns[addr as usize] {
            // Terminals — patch their failure branch.
            Insn::Byte         { on_fail, .. }
            | Insn::ByteRange  { on_fail, .. }
            | Insn::Class      { on_fail, .. }  // label_id unchanged
            | Insn::Literal    { on_fail, .. }
            | Insn::EndOfInput { on_fail }
            | Insn::AnyChar    { on_fail }
            | Insn::Char       { on_fail, .. }
            | Insn::CharRange  { on_fail, .. }
            | Insn::IfFlag     { on_fail, .. }
            | Insn::IfNotFlag  { on_fail, .. }
                => *on_fail = target,

            // Control-flow — patch their jump target.
            Insn::Jump            { target: t }
            | Insn::Commit        { target: t }
            | Insn::BackCommit    { target: t }
            | Insn::NegBackCommit { target: t }
            | Insn::PartialCommit { target: t }
                => *t = target,

            // Choice — patch its alternate branch.
            Insn::Choice { alt } => *alt = target,

            // Error recovery — patch resume address (after body + RecoveryResume).
            Insn::RecoverUntil { resume, .. } => *resume = target,

            other => panic!("patch: {other:?} at {addr} has no patchable field"),
        }
    }

    fn patch_table_id(&mut self, addr: InsnId, table_id: u32) {
        match &mut self.insns[addr as usize] {
            Insn::ByteDispatch { table_id: t } => *t = table_id,
            other => panic!("patch_table_id: expected ByteDispatch, got {other:?} at {addr}"),
        }
    }

    // ── Byte-level terminals ──────────────────────────────────────────────────

    /// Match exactly one byte value.
    pub fn byte(&mut self, b: u8) -> InsnId {
        self.emit(Insn::Byte { byte: b, on_fail: u32::MAX })
    }

    /// Match any byte in the inclusive range `[lo, hi]`.
    pub fn byte_range(&mut self, lo: u8, hi: u8) -> InsnId {
        self.emit(Insn::ByteRange { lo, hi, on_fail: u32::MAX })
    }

    /// Match any byte whose bit is set in `class`.
    pub fn class(&mut self, class: CharClass) -> InsnId {
        self.emit(Insn::Class {
            class,
            label_id: 0,
            on_fail:  u32::MAX,
        })
    }

    /// Like [`class`](Self::class) but use `label` in diagnostics (e.g. "digit", "whitespace").
    pub fn class_with_label(&mut self, class: CharClass, label: &'static str) -> InsnId {
        let label_id = self.class_labels.len() as u32;
        self.class_labels.push(label);
        self.emit(Insn::Class {
            class,
            label_id,
            on_fail: u32::MAX,
        })
    }

    /// Match the exact byte string `bytes` (SIMD-accelerated for ≥16 bytes).
    pub fn literal(&mut self, bytes: &[u8]) -> InsnId {
        let lit_id = self.literals.intern(bytes);
        self.emit(Insn::Literal { lit_id, on_fail: u32::MAX })
    }

    /// Match end-of-input.
    ///
    /// Note: `end_of_input` is **not** preceded by an auto-skip even inside a
    /// [`parser_rule`](Self::parser_rule).  Call [`skip()`](Self::skip)
    /// explicitly if trailing trivia should be consumed before the end marker.
    pub fn end_of_input(&mut self) -> InsnId {
        self.emit(Insn::EndOfInput { on_fail: u32::MAX })
    }

    // ── Unicode codepoint terminals ───────────────────────────────────────────

    /// Match any single valid UTF-8 codepoint (1–4 bytes).
    ///
    /// Fails on invalid UTF-8 or end-of-input.  For ASCII-only input,
    /// `class(CharClass::ANY)` is equivalent and faster (no decode step).
    pub fn any_char(&mut self) -> InsnId {
        self.emit(Insn::AnyChar { on_fail: u32::MAX })
    }

    /// Match exactly the Unicode codepoint `c`.
    ///
    /// For ASCII codepoints (U+0000–U+007F), `byte(c as u8)` is equivalent
    /// and avoids the UTF-8 decode.
    pub fn char(&mut self, c: char) -> InsnId {
        self.emit(Insn::Char { codepoint: c as u32, on_fail: u32::MAX })
    }

    /// Match any codepoint in the inclusive range `[lo, hi]`.
    ///
    /// ```rust,ignore
    /// g.char_range('a', 'z')          // ASCII lowercase
    /// g.char_range('α', 'ω')          // Greek lowercase
    /// g.char_range('\u{1F600}', '\u{1F64F}')  // Smiley emoji block
    /// ```
    pub fn char_range(&mut self, lo: char, hi: char) -> InsnId {
        self.emit(Insn::CharRange { lo: lo as u32, hi: hi as u32, on_fail: u32::MAX })
    }

    // ── Control flow ──────────────────────────────────────────────────────────

    /// Emit an unconditional failure.
    pub fn fail(&mut self) -> InsnId {
        self.emit(Insn::Fail)
    }

    /// Emit the `Accept` instruction, ending the parse successfully.
    ///
    /// Only needed as the final instruction of the grammar's start rule; all
    /// other rules end with `Return` (emitted by [`end_rule`](Self::end_rule)).
    pub fn accept(&mut self) {
        self.emit(Insn::Accept);
    }

    /// Call a rule by name.
    ///
    /// The reference is resolved during [`finish`](Self::finish), so forward
    /// references are fine.
    ///
    /// Inside a [`parser_rule`](Self::parser_rule), a [`skip()`](Self::skip)
    /// is emitted **before** the call.
    pub fn call(&mut self, rule_name: impl Into<String>) -> InsnId {
        if self.auto_trivia { self.skip(); }
        let addr = self.current_ip();
        self.pending_calls.push((addr as usize, rule_name.into()));
        self.emit(Insn::Call { rule: u16::MAX })
    }

    /// Call a rule by its already-known [`RuleId`].
    ///
    /// Inside a [`parser_rule`](Self::parser_rule), a [`skip()`](Self::skip)
    /// is emitted **before** the call.
    pub fn call_id(&mut self, rule: RuleId) -> InsnId {
        if self.auto_trivia { self.skip(); }
        self.emit(Insn::Call { rule })
    }

    // ── Legacy captures ───────────────────────────────────────────────────────

    /// Wrap `body` in a legacy `CaptureBegin`/`CaptureEnd` pair.
    ///
    /// Prefer [`node`](Self::node) / [`token`](Self::token) for new grammars —
    /// they produce the green/red tree.  `capture` is retained for grammars
    /// that use the flat [`CaptureEvent`](crate::types::CaptureEvent) log.
    pub fn capture<F>(&mut self, tag: Tag, body: F)
    where F: FnOnce(&mut Self)
    {
        self.emit(Insn::CaptureBegin { tag });
        body(self);
        self.emit(Insn::CaptureEnd { tag });
    }

    // ── Green/Red tree ────────────────────────────────────────────────────────

    /// Wrap `body` in a syntax node of `kind`.
    ///
    /// Emits `NodeBegin` before the body and `NodeEnd` after.  Backtracking
    /// automatically discards uncommitted node events.
    ///
    /// `node` is transparent to auto-skip mode: the current trivia mode is
    /// passed through to the body unchanged.
    ///
    /// ```rust,ignore
    /// g.node(NODE_EXPR, |g| {
    ///     g.token(TOK_NUM, |g| g.repeat(1.., |g| g.class(classes::DIGIT)));
    /// });
    /// ```
    pub fn node<K: IntoSyntaxKind, F>(&mut self, kind: K, body: F)
    where F: FnOnce(&mut Self)
    {
        self.emit(Insn::NodeBegin { kind: kind.into_syntax_kind(), field: None });
        body(self);
        self.emit(Insn::NodeEnd);
    }

    /// Wrap `body` in a syntax node of `kind` with a named field label.
    ///
    /// The label is interned in the grammar's `field_names` table; use
    /// [`SyntaxNode::field_by_id`](crate::red::SyntaxNode::field_by_id) with the
    /// corresponding [`FieldId`](crate::types::FieldId) to access this child by name.
    pub fn node_with_field<K: IntoSyntaxKind, F>(&mut self, kind: K, field: &'static str, body: F)
    where F: FnOnce(&mut Self)
    {
        let field_id = self.intern_field(field);
        self.emit(Insn::NodeBegin { kind: kind.into_syntax_kind(), field: Some(field_id) });
        body(self);
        self.emit(Insn::NodeEnd);
    }

    /// Intern a field name and return its [`FieldId`]. Same name returns the same id.
    pub fn intern_field(&mut self, name: &'static str) -> FieldId {
        if let Some(&id) = self.field_by_name.get(name) {
            return id;
        }
        let id = self.field_names.len() as FieldId;
        self.field_names.push(name);
        self.field_by_name.insert(name.to_string(), id);
        id
    }

    /// Wrap `body` in a semantic (non-trivia) leaf token of `kind`.
    ///
    /// Inside a [`parser_rule`](Self::parser_rule), a [`skip()`](Self::skip)
    /// is emitted **before** the token.  The token's own body always runs in
    /// lexer mode (no auto-skip), since token interiors are byte-level.
    ///
    /// ```rust,ignore
    /// g.token(TOK_IDENT, |g| {
    ///     g.class(classes::IDENT_START);
    ///     g.zero_or_more(|g| g.class(classes::IDENT_CONT));
    /// });
    /// ```
    pub fn token<K: IntoSyntaxKind, F>(&mut self, kind: K, body: F)
    where F: FnOnce(&mut Self)
    {
        if self.auto_trivia { self.skip(); }
        self.emit(Insn::TokenBegin { kind: kind.into_syntax_kind(), is_trivia: false });
        // Token interiors are always lexer-level: no auto-skip inside.
        let prev = self.auto_trivia;
        self.auto_trivia = false;
        body(self);
        self.auto_trivia = prev;
        self.emit(Insn::TokenEnd);
    }

    /// Wrap `body` in a trivia leaf token of `kind`.
    ///
    /// Trivia (whitespace, comments) is stored in the green tree but filtered
    /// out by [`SyntaxNode::non_trivia_tokens`](crate::red::SyntaxNode::non_trivia_tokens).
    ///
    /// Trivia tokens are never preceded by an auto-skip (trivia cannot contain
    /// more trivia), and their body always runs in lexer mode.
    ///
    /// ```rust,ignore
    /// g.trivia(TRIVIA_WS, |g| {
    ///     g.zero_or_more(|g| g.class(classes::WHITESPACE));
    /// });
    /// ```
    pub fn trivia<K: IntoSyntaxKind, F>(&mut self, kind: K, body: F)
    where F: FnOnce(&mut Self)
    {
        // Never inject a skip before trivia, and always run the body in
        // lexer mode — trivia patterns are byte-level by definition.
        self.emit(Insn::TokenBegin { kind: kind.into_syntax_kind(), is_trivia: true });
        let prev = self.auto_trivia;
        self.auto_trivia = false;
        body(self);
        self.auto_trivia = prev;
        self.emit(Insn::TokenEnd);
    }

    // ── PEG combinators ───────────────────────────────────────────────────────

    /// `e1 / e2` — try `e1`; if it fails, try `e2`.
    ///
    /// Auto-skip mode is passed through transparently to both branches.
    /// For many alternatives, use [`choices`](Self::choices) with a vec.
    pub fn choice<F1, F2>(&mut self, e1: F1, e2: F2)
    where F1: FnOnce(&mut Self), F2: FnOnce(&mut Self)
    {
        let choice_ip = self.emit(Insn::Choice { alt: u32::MAX });
        e1(self);
        let commit_ip = self.emit(Insn::Commit { target: u32::MAX });
        let e2_start  = self.current_ip();
        self.patch(choice_ip, e2_start);
        e2(self);
        let after = self.current_ip();
        self.patch(commit_ip, after);
    }

    /// N-way choice from a vector of alternatives.
    ///
    /// Tries each closure in order; the first that matches succeeds. Equivalent to
    /// nesting [`choice`](Self::choice) for each pair. Empty vec is a no-op; single
    /// element just runs that alternative.
    ///
    /// ```rust,ignore
    /// g.choices(vec![
    ///     Box::new(|g| g.literal(b"a")),
    ///     Box::new(|g| g.literal(b"b")),
    ///     Box::new(|g| g.literal(b"c")),
    /// ]);
    /// ```
    pub fn choices(&mut self, mut alternatives: Vec<Box<dyn FnOnce(&mut Self)>>) {
        match alternatives.len() {
            0 => {}
            1 => {
                let f = alternatives.pop().unwrap();
                f(self);
            }
            _ => {
                let first = alternatives.remove(0);
                self.choice(first, |g| g.choices(alternatives));
            }
        }
    }

    /// Three-way choice without allocation. Tries `e1`, then `e2`, then `e3`.
    pub fn choice3<F1, F2, F3>(&mut self, e1: F1, e2: F2, e3: F3)
    where
        F1: FnOnce(&mut Self),
        F2: FnOnce(&mut Self),
        F3: FnOnce(&mut Self),
    {
        self.choice(e1, |g| g.choice(e2, e3));
    }

    /// Four-way choice without allocation. Tries `e1`, then `e2`, then `e3`, then `e4`.
    pub fn choice4<F1, F2, F3, F4>(&mut self, e1: F1, e2: F2, e3: F3, e4: F4)
    where
        F1: FnOnce(&mut Self),
        F2: FnOnce(&mut Self),
        F3: FnOnce(&mut Self),
        F4: FnOnce(&mut Self),
    {
        self.choice(e1, |g| g.choice3(e2, e3, e4));
    }

    /// `e?` — match `body` zero or one time.
    pub fn optional<F>(&mut self, body: F)
    where F: Fn(&mut Self)
    {
        let choice_ip = self.emit(Insn::Choice { alt: u32::MAX });
        body(self);
        let commit_ip = self.emit(Insn::Commit { target: u32::MAX });
        let after     = self.current_ip();
        self.patch(choice_ip, after);
        self.patch(commit_ip, after);
    }

    /// `e*` — match `body` zero or more times.
    pub fn zero_or_more<F>(&mut self, body: F)
    where F: Fn(&mut Self)
    {
        let choice_ip  = self.emit(Insn::Choice { alt: u32::MAX });
        let loop_start = self.current_ip();
        body(self);
        self.emit(Insn::PartialCommit { target: loop_start });
        let after = self.current_ip();
        self.patch(choice_ip, after);
    }

    /// `e+` — match `body` one or more times.
    pub fn one_or_more<F>(&mut self, body: F)
    where F: Fn(&mut Self)
    {
        body(self);
        self.zero_or_more(body);
    }

    /// `&e` — succeed iff `body` matches, without consuming input.
    pub fn lookahead<F>(&mut self, body: F)
    where F: FnOnce(&mut Self)
    {
        let choice_ip  = self.emit(Insn::Choice { alt: u32::MAX });
        body(self);
        let backcommit = self.emit(Insn::BackCommit { target: u32::MAX });
        let fail_ip    = self.current_ip();
        self.patch(choice_ip, fail_ip);
        self.emit(Insn::Fail);
        let after = self.current_ip();
        self.patch(backcommit, after);
    }

    /// `!e` — succeed iff `body` does **not** match, without consuming input.
    pub fn neg_lookahead<F>(&mut self, body: F)
    where F: FnOnce(&mut Self)
    {
        let choice_ip = self.emit(Insn::Choice { alt: u32::MAX });
        body(self);
        let neg_ip    = self.emit(Insn::NegBackCommit { target: u32::MAX });
        let after     = self.current_ip();
        self.patch(choice_ip, after);
        self.patch(neg_ip, after);
    }

    /// Run `body` and record `label` as the expected value at this position if matching fails.
    ///
    /// When a parse fails, the diagnostic will show `label` (e.g. "statement", "expression")
    /// in the expected set when the failure occurred at the start of this region.
    pub fn expect_label<F>(&mut self, label: &'static str, body: F)
    where F: FnOnce(&mut Self)
    {
        let label_id = self.expected_labels.len() as u32;
        self.expected_labels.push(label);
        self.emit(Insn::RecordExpectedLabel { label_id });
        body(self);
    }

    /// Run `body` and commit to this alternative on success (no backtracking past this point).
    ///
    /// Like a cut in Prolog or "commit" in PEG: if `body` succeeds, the parser will not
    /// backtrack to before this point. Use inside a [`choice`](Self::choice) when the
    /// first successful match should be final. On failure, backtracking proceeds as usual.
    pub fn cut<F>(&mut self, body: F)
    where F: FnOnce(&mut Self)
    {
        body(self);
        let commit_ip = self.emit(Insn::Commit { target: u32::MAX });
        self.patch(commit_ip, self.current_ip()); // target = next instruction after Commit
    }

    /// Left-associative infix chain: `operand (op operand)*`.
    ///
    /// Use for expression precedence: define a primary rule, then build each level
    /// with this helper. Example: `expr_add` = `infix_left("expr_mul", "add_op")` gives
    /// `expr_mul (add_op expr_mul)*`; then `expr_mul` = `infix_left("primary", "mul_op")`.
    pub fn infix_left(&mut self, operand_rule: &str, op_rule: &str) {
        self.call(operand_rule);
        self.zero_or_more(|g| {
            g.call(op_rule);
            g.call(operand_rule);
        });
    }

    /// Error recovery: try `body`; on failure, skip input until `sync_rule` matches (or EOI), then continue.
    ///
    /// `sync_rule` must be the name of a rule that matches the sync token (e.g. `;` or `}`).
    /// It is resolved at [`finish`](Self::finish) like [`call`](Self::call). Use inside a
    /// list rule to report multiple errors in one pass: after a statement fails, skip to the
    /// next sync token and keep parsing.
    pub fn recover_until<F>(&mut self, sync_rule: &str, body: F)
    where
        F: FnOnce(&mut Self),
    {
        let recover_ip = self.current_ip();
        self.emit(Insn::RecoverUntil {
            sync_rule: 0, // patched at finish()
            resume:     u32::MAX,
        });
        self.pending_recover.push((recover_ip as usize, sync_rule.to_string()));
        body(self);
        let resume_ip = self.current_ip();
        self.patch(recover_ip, resume_ip);
        self.emit(Insn::RecoveryResume);
    }

    // ── Repetition ────────────────────────────────────────────────────────────

    /// Repeat `body` according to `range`.
    ///
    /// Expands at **build time** into straight-line instruction sequences —
    /// no counter register in the VM.
    ///
    /// | Variant | Expansion |
    /// |---|---|
    /// | `Exact(n)` | `body` × n |
    /// | `AtLeast(n)` | `body` × n, then `zero_or_more(body)` |
    /// | `AtMost(n)` | `optional(body)` × n |
    /// | `Between(m, n)` | `body` × m, `optional(body)` × (n − m) |
    ///
    /// `body` must be `Fn` (not just `FnOnce`) so it can be called multiple
    /// times.  Grammar closures are almost always `Fn`.
    pub fn repeat<F, R>(&mut self, range: R, body: F)
    where
        F: Fn(&mut Self),
        R: Into<Repeat>,
    {
        match range.into() {
            Repeat::Exact(n)       => { for _ in 0..n { body(self); } }
            Repeat::AtLeast(n)     => { for _ in 0..n { body(self); } self.zero_or_more(body); }
            Repeat::AtMost(n)      => { for _ in 0..n { self.optional(&body); } }
            Repeat::Between(lo, hi) => {
                for _ in 0..lo                    { body(self); }
                for _ in 0..hi.saturating_sub(lo) { self.optional(&body); }
            }
        }
    }

    // ── Context flags ─────────────────────────────────────────────────────────

    /// Succeed iff flag `id` is **set**; does not consume input.
    pub fn if_flag(&mut self, id: FlagId) -> InsnId {
        self.emit(Insn::IfFlag { flag_id: id, on_fail: u32::MAX })
    }

    /// Succeed iff flag `id` is **clear**; does not consume input.
    pub fn if_not_flag(&mut self, id: FlagId) -> InsnId {
        self.emit(Insn::IfNotFlag { flag_id: id, on_fail: u32::MAX })
    }

    /// Run `body` with `set_ids` set and `clear_ids` cleared, then restore.
    ///
    /// On any exit — success or backtrack — the previous flag values are
    /// automatically restored via the snapshot arena.
    pub fn with_flags<F>(&mut self, set_ids: &[FlagId], clear_ids: &[FlagId], body: F)
    where F: FnOnce(&mut Self)
    {
        let mask_id = self.flag_masks.intern(set_ids, clear_ids);
        self.emit(Insn::PushFlags { mask_id });
        body(self);
        self.emit(Insn::PopFlags);
    }

    // ── O(1) byte dispatch ────────────────────────────────────────────────────

    /// Emit an O(1) dispatch on the next byte.
    ///
    /// Each arm is a `(CharClass, body)` pair.  The dispatch table maps each
    /// byte to the start address of its arm's body.  `fallback` is taken for
    /// any byte not covered by any arm, or on end-of-input.
    ///
    /// Inside a [`parser_rule`](Self::parser_rule), a [`skip()`](Self::skip)
    /// is emitted before the dispatch table lookup.  The arm bodies themselves
    /// always run without auto-skip (since trivia before them was already
    /// consumed before the dispatch).
    pub fn byte_dispatch(
        &mut self,
        arms:     Vec<(CharClass, Box<dyn FnOnce(&mut Self)>)>,
        fallback: Option<Box<dyn FnOnce(&mut Self)>>,
    ) {
        if self.auto_trivia { self.skip(); }
        // Arm bodies run without auto-skip: trivia is already consumed above,
        // and the dispatch table peeks at the next byte after trivia.
        let prev = self.auto_trivia;
        self.auto_trivia = false;

        let dispatch_ip = self.emit(Insn::ByteDispatch { table_id: u32::MAX });
        let mut table   = [u32::MAX; 256];
        let mut exits   = Vec::new();

        for (class, body) in arms {
            let arm_start = self.current_ip();
            for b in 0u8..=255 {
                if class.contains(b) { table[b as usize] = arm_start; }
            }
            body(self);
            exits.push(self.emit(Insn::Jump { target: u32::MAX }));
        }

        if let Some(body) = fallback {
            let fallback_start = self.current_ip();
            for slot in table.iter_mut() {
                if *slot == u32::MAX { *slot = fallback_start; }
            }
            body(self);
            exits.push(self.emit(Insn::Jump { target: u32::MAX }));
        }

        let after    = self.current_ip();
        let table_id = self.jump_tables.len() as u32;
        for jmp in exits { self.patch(jmp, after); }
        self.jump_tables.push(table);
        self.patch_table_id(dispatch_ip, table_id);

        self.auto_trivia = prev;
    }

    // ── Finalisation ──────────────────────────────────────────────────────────

    /// Resolve all pending rule references and return the completed [`BuiltGraph`].
    ///
    /// Returns `Err` if any [`call`](Self::call) referred to a rule name that
    /// was never defined, or if the grammar contains a left-recursive (or mutually
    /// recursive) cycle.
    pub fn finish(mut self) -> Result<BuiltGraph, String> {
        for (addr, rule_name) in self.pending_calls.drain(..) {
            let rule_id = self.rule_by_name.get(&rule_name).copied()
                .ok_or_else(|| format!("undefined rule: {rule_name}"))?;
            if let Insn::Call { rule } = &mut self.insns[addr] {
                *rule = rule_id;
            }
        }
        for (addr, sync_rule_name) in self.pending_recover.drain(..) {
            let rule_id = self.rule_by_name.get(&sync_rule_name).copied()
                .ok_or_else(|| format!("undefined sync rule: {sync_rule_name}"))?;
            if let Insn::RecoverUntil { sync_rule, .. } = &mut self.insns[addr] {
                *sync_rule = rule_id;
            }
        }

        if !self.allow_rule_cycles {
            if let Err(msg) = self.check_rule_cycles() {
                return Err(msg);
            }
        }

        if !self.allow_unreachable_rules {
            if let Err(msg) = self.check_unreachable_rules() {
                return Err(msg);
            }
        }

        self.literals.seal();
        self.flag_masks.seal();

        Ok(BuiltGraph {
            insns:             self.insns,
            rule_entry:        self.rule_entry,
            literal_data:      self.literals.data,
            literal_offsets:   self.literals.offsets,
            jump_tables:       self.jump_tables,
            flag_mask_data:    self.flag_masks.data,
            flag_mask_offsets: self.flag_masks.offsets,
            rule_names:        self.rule_names,
            tag_names:         self.tag_names,
            class_labels:      self.class_labels,
            expected_labels:   self.expected_labels,
            field_names:       self.field_names,
        })
    }

    // ── Private helpers ───────────────────────────────────────────────────────

    /// Build rule→rule call graph and detect cycles (left-recursion or mutual recursion).
    fn check_rule_cycles(&self) -> Result<(), String> {
        let num_rules = self.rule_entry.len();
        if num_rules == 0 {
            return Ok(());
        }

        // For each rule r, collect all rules it calls (directly) by walking reachable insns.
        let mut edges: Vec<Vec<RuleId>> = (0..num_rules).map(|_| Vec::new()).collect();
        let insns = &self.insns;
        let rule_entry = &self.rule_entry;
        let jump_tables = &self.jump_tables;

        for r in 0..num_rules {
            let start = rule_entry[r] as usize;
            let mut stack = vec![start];
            let mut visited = HashSet::new();
            while let Some(ip) = stack.pop() {
                if ip >= insns.len() || !visited.insert(ip) {
                    continue;
                }
                match &insns[ip] {
                    Insn::Call { rule } => {
                        let callee = *rule;
                        if (callee as usize) < num_rules && !edges[r].contains(&callee) {
                            edges[r].push(callee);
                        }
                        stack.push(ip + 1);
                    }
                    Insn::Return | Insn::Fail | Insn::Accept => {}
                    Insn::Jump { target } => stack.push(*target as usize),
                    Insn::Choice { alt } => {
                        stack.push(ip + 1);
                        stack.push(*alt as usize);
                    }
                    Insn::Commit { target }
                    | Insn::BackCommit { target }
                    | Insn::NegBackCommit { target }
                    | Insn::PartialCommit { target } => stack.push(*target as usize),
                    Insn::ByteDispatch { table_id } => {
                        let tid = *table_id as usize;
                        if tid < jump_tables.len() {
                            for target in &jump_tables[tid] {
                                if *target != u32::MAX {
                                    stack.push(*target as usize);
                                }
                            }
                        }
                        stack.push(ip + 1);
                    }
                    Insn::RecoverUntil { resume, .. } => {
                        stack.push(ip + 1);
                        if *resume != u32::MAX {
                            stack.push(*resume as usize);
                        }
                    }
                    Insn::RecoveryResume => stack.push(ip + 1),
                    _ => {
                        stack.push(ip + 1);
                        if let Some(on_fail) = self.insn_on_fail(ip as u32) {
                            if on_fail != u32::MAX {
                                stack.push(on_fail as usize);
                            }
                        }
                    }
                }
            }
        }

        // Find a cycle with DFS and recursion stack.
        let mut in_stack = vec![false; num_rules];
        let mut order = vec![None; num_rules];
        let mut cycle = Vec::new();

        fn visit(
            r: RuleId,
            r_usize: usize,
            edges: &[Vec<RuleId>],
            rule_names: &[&'static str],
            in_stack: &mut [bool],
            order: &mut [Option<u32>],
            cycle: &mut Vec<RuleId>,
            counter: &mut u32,
        ) -> bool {
            if order[r_usize].is_some() {
                if in_stack[r_usize] {
                    cycle.push(r);
                    return true;
                }
                return false;
            }
            in_stack[r_usize] = true;
            *counter += 1;
            order[r_usize] = Some(*counter);
            for &s in &edges[r_usize] {
                let s_usize = s as usize;
                if visit(s, s_usize, edges, rule_names, in_stack, order, cycle, counter) {
                    if cycle.len() == 1 || cycle[0] != r {
                        cycle.push(r);
                    }
                    in_stack[r_usize] = false;
                    return true;
                }
            }
            in_stack[r_usize] = false;
            false
        }

        let mut counter = 0u32;
        for r in 0..num_rules {
            if order[r].is_none()
                && visit(
                    r as RuleId,
                    r,
                    &edges,
                    &self.rule_names,
                    &mut in_stack,
                    &mut order,
                    &mut cycle,
                    &mut counter,
                )
            {
                cycle.reverse();
                let names: Vec<&str> = cycle
                    .iter()
                    .map(|&id| {
                        self.rule_names
                            .get(id as usize)
                            .copied()
                            .unwrap_or("?")
                    })
                    .collect();
                let closed: Vec<&str> = names
                    .iter()
                    .copied()
                    .chain(names.first().copied())
                    .collect();
                return Err(format!(
                    "left-recursive or mutually recursive rules: {}",
                    closed.join(" -> ")
                ));
            }
        }
        Ok(())
    }

    /// Check that every rule is reachable from rule 0 (start). Returns Err with first unreachable rule name.
    fn check_unreachable_rules(&self) -> Result<(), String> {
        let num_rules = self.rule_entry.len();
        if num_rules <= 1 {
            return Ok(());
        }
        let edges = self.build_rule_call_edges();
        let mut reachable = vec![false; num_rules];
        let mut stack: Vec<RuleId> = vec![0];
        reachable[0] = true;
        while let Some(r) = stack.pop() {
            let r_usize = r as usize;
            for &s in &edges[r_usize] {
                let s_usize = s as usize;
                if s_usize < num_rules && !reachable[s_usize] {
                    reachable[s_usize] = true;
                    stack.push(s);
                }
            }
        }
        for (r, &ok) in reachable.iter().enumerate() {
            if !ok {
                let name = self.rule_names.get(r).copied().unwrap_or("?");
                return Err(format!("unreachable rule: {name}"));
            }
        }
        Ok(())
    }

    /// Build direct rule→rule call edges (same logic as cycle check, but no cycle detection).
    fn build_rule_call_edges(&self) -> Vec<Vec<RuleId>> {
        let num_rules = self.rule_entry.len();
        if num_rules == 0 {
            return vec![];
        }
        let mut edges: Vec<Vec<RuleId>> = (0..num_rules).map(|_| Vec::new()).collect();
        let insns = &self.insns;
        let rule_entry = &self.rule_entry;
        let jump_tables = &self.jump_tables;

        for r in 0..num_rules {
            let start = rule_entry[r] as usize;
            let mut stack = vec![start];
            let mut visited = HashSet::new();
            while let Some(ip) = stack.pop() {
                if ip >= insns.len() || !visited.insert(ip) {
                    continue;
                }
                match &insns[ip] {
                    Insn::Call { rule } => {
                        let callee = *rule;
                        if (callee as usize) < num_rules && !edges[r].contains(&callee) {
                            edges[r].push(callee);
                        }
                        stack.push(ip + 1);
                    }
                    Insn::Return | Insn::Fail | Insn::Accept => {}
                    Insn::Jump { target } => stack.push(*target as usize),
                    Insn::Choice { alt } => {
                        stack.push(ip + 1);
                        stack.push(*alt as usize);
                    }
                    Insn::Commit { target }
                    | Insn::BackCommit { target }
                    | Insn::NegBackCommit { target }
                    | Insn::PartialCommit { target } => stack.push(*target as usize),
                    Insn::ByteDispatch { table_id } => {
                        let tid = *table_id as usize;
                        if tid < jump_tables.len() {
                            for target in &jump_tables[tid] {
                                if *target != u32::MAX {
                                    stack.push(*target as usize);
                                }
                            }
                        }
                        stack.push(ip + 1);
                    }
                    Insn::RecoverUntil { resume, .. } => {
                        stack.push(ip + 1);
                        if *resume != u32::MAX {
                            stack.push(*resume as usize);
                        }
                    }
                    Insn::RecoveryResume => stack.push(ip + 1),
                    _ => {
                        stack.push(ip + 1);
                        if let Some(on_fail) = self.insn_on_fail(ip as u32) {
                            if on_fail != u32::MAX {
                                stack.push(on_fail as usize);
                            }
                        }
                    }
                }
            }
        }
        edges
    }

    /// Return the on_fail target for a terminal-like instruction at the given address.
    fn insn_on_fail(&self, addr: u32) -> Option<u32> {
        let ip = addr as usize;
        if ip >= self.insns.len() {
            return None;
        }
        match &self.insns[ip] {
            Insn::Byte { on_fail, .. }
            | Insn::ByteRange { on_fail, .. }
            | Insn::Class { on_fail, .. }  // label_id unchanged
            | Insn::Literal { on_fail, .. }
            | Insn::EndOfInput { on_fail }
            | Insn::AnyChar { on_fail }
            | Insn::Char { on_fail, .. }
            | Insn::CharRange { on_fail, .. }
            | Insn::IfFlag { on_fail, .. }
            | Insn::IfNotFlag { on_fail, .. } => Some(*on_fail),
            _ => None,
        }
    }

    /// Common implementation for [`parser_rule`] and [`lexer_rule`].
    fn rule_impl<F>(&mut self, name: &'static str, body: F, with_trivia: bool) -> RuleId
    where F: FnOnce(&mut Self)
    {
        let prev = self.auto_trivia;
        self.auto_trivia = with_trivia;
        let id = self.begin_rule(name);
        body(self);
        self.end_rule();
        self.auto_trivia = prev;
        id
    }
}

impl Default for GrammarBuilder {
    fn default() -> Self { Self::new() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finish_rejects_left_recursive_cycle_by_default() {
        let mut g = GrammarBuilder::new();
        g.parser_rule("a", |g| {
            g.call("a");
            g.byte(b'x');
        });
        let res = g.finish();
        assert!(res.is_err());
        let err = res.err().unwrap();
        assert!(err.contains("left-recursive") && err.contains("a"));
    }

    #[test]
    fn finish_accepts_cycle_when_allow_rule_cycles() {
        let mut g = GrammarBuilder::new();
        g.allow_rule_cycles(true);
        g.parser_rule("a", |g| {
            g.call("a");
            g.byte(b'x');
        });
        assert!(g.finish().is_ok());
    }

    #[test]
    fn choices_macro() {
        use crate::engine::Engine;
        let mut g = GrammarBuilder::new();
        g.parser_rule("start", |g| {
            crate::choices!(g, |g| { g.byte(b'a'); }, |g| { g.byte(b'b'); }, |g| { g.byte(b'c'); });
            g.end_of_input();
            g.accept();
        });
        let built = g.finish().unwrap();
        let graph = built.as_graph();
        let mut engine = Engine::new();
        assert!(engine.parse(&graph, b"a").is_ok());
        assert!(engine.parse(&graph, b"b").is_ok());
        assert!(engine.parse(&graph, b"c").is_ok());
        assert!(engine.parse(&graph, b"d").is_err());
    }
}
