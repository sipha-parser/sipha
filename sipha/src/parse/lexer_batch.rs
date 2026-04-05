//! Batch helpers for registering many small lexer rules (keyword literals, fixed token spellings).
//!
//! # How this relates to `ByteDispatch` / prefix tries
//!
//! After [`GrammarBuilder::finish`](crate::parse::builder::GrammarBuilder::finish), the graph
//! optimizer ([`super::graph_optimize`]) can fuse a right-nested [`Insn::Choice`](crate::parse::insn::Insn::Choice)
//! spine into [`Insn::ByteDispatch`](crate::parse::insn::Insn::ByteDispatch) or a shared-prefix trie
//! when each alternative starts with byte-level terminals (`byte`, `literal`, …).
//!
//! - **One [`lexer_rule`](crate::parse::builder::GrammarBuilder::lexer_rule) per spelling** (what
//!   these helpers emit) keeps separate rule names for [`GrammarBuilder::call`]. Parser-side
//!   dispatch is usually [`byte_dispatch`](crate::parse::builder::GrammarBuilder::byte_dispatch) +
//!   `call`; that does **not** merge distinct rules into one trie.
//! - **One lexer rule** whose body is N-way [`choices`](crate::parse::builder::GrammarBuilder::choices)
//! / [`crate::choices!`] over `keyword` / `literal` arms **does** give the optimizer a single spine
//! to trie. Use that when you want one entry point; register **longer** shared prefixes before
//! shorter ones (e.g. `integer` before `int`).
//!
//! Optional [`FlagId`](crate::parse::context::FlagId) guards are emitted as a sequence of
//! [`GrammarBuilder::if_flag`] (logical AND), matching common “language level + feature flag”
//! patterns.

use crate::parse::builder::GrammarBuilder;
use crate::parse::context::FlagId;
use crate::types::LexKind;

/// Describes one `lexer_rule` that matches [`GrammarBuilder::keyword`] after optional flag guards.
#[derive(Clone, Copy, Debug)]
pub struct LexerKeywordSpec<K> {
    pub name: &'static str,
    pub kind: K,
    pub word: &'static [u8],
    /// [`GrammarBuilder::if_flag`] for each id, in order, before the keyword match.
    pub require_flags: &'static [FlagId],
}

/// Describes one `lexer_rule` that matches `token(kind, literal(lit))` after optional guards.
#[derive(Clone, Copy, Debug)]
pub struct LexerTokenLiteralSpec<K> {
    pub name: &'static str,
    pub kind: K,
    pub lit: &'static [u8],
    pub require_flags: &'static [FlagId],
}

impl<K> LexerKeywordSpec<K> {
    #[must_use]
    pub const fn new(name: &'static str, kind: K, word: &'static [u8]) -> Self {
        Self {
            name,
            kind,
            word,
            require_flags: &[],
        }
    }

    #[must_use]
    pub const fn with_flags(mut self, flags: &'static [FlagId]) -> Self {
        self.require_flags = flags;
        self
    }
}

impl<K> LexerTokenLiteralSpec<K> {
    #[must_use]
    pub const fn new(name: &'static str, kind: K, lit: &'static [u8]) -> Self {
        Self {
            name,
            kind,
            lit,
            require_flags: &[],
        }
    }

    #[must_use]
    pub const fn with_flags(mut self, flags: &'static [FlagId]) -> Self {
        self.require_flags = flags;
        self
    }
}

impl GrammarBuilder {
    /// Register many keyword lexer rules from a table (one rule per entry, same as N× `lexer_rule`).
    pub fn lexer_rule_keywords_batch<K: LexKind + Copy>(
        &mut self,
        entries: &[LexerKeywordSpec<K>],
    ) {
        for spec in entries {
            let LexerKeywordSpec {
                name,
                kind,
                word,
                require_flags,
            } = *spec;
            self.lexer_rule(name, |g| {
                for &f in require_flags {
                    g.if_flag(f);
                }
                g.keyword(kind, word);
            });
        }
    }

    /// Register many fixed-spelling token rules (one rule per entry).
    pub fn lexer_rule_token_literals_batch<K: LexKind + Copy>(
        &mut self,
        entries: &[LexerTokenLiteralSpec<K>],
    ) {
        for spec in entries {
            let LexerTokenLiteralSpec {
                name,
                kind,
                lit,
                require_flags,
            } = *spec;
            self.lexer_rule(name, |g| {
                for &f in require_flags {
                    g.if_flag(f);
                }
                g.token(kind, |g| {
                    g.literal(lit);
                });
            });
        }
    }
}
