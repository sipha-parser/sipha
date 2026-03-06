//! # Error Diagnostics
//!
//! The engine maintains an [`ErrorContext`] that records what the parser
//! *expected* at the furthest byte position it reached before failing.
//! This produces error messages on the order of:
//!
//! ```text
//! parse error at byte 42: expected one of: '"', digit, end-of-input
//! ```
//!
//! ## Algorithm
//!
//! * Every terminal instruction records its expectation **before** jumping to
//!   its `on_fail` target.
//! * If the current position is *strictly past* the recorded furthest, we
//!   clear the expected set and start fresh.
//! * If the current position *equals* the recorded furthest, we append to the
//!   expected set (deduplicating).
//! * Positions strictly *before* the furthest are silently ignored.
//!
//! This is the classic "rightmost error" heuristic from PEG literature.
//!
//! For richer messages (line/column, source snippet), use
//! [`Diagnostic::format_with_source`] with a [`crate::line_index::LineIndex`].

use crate::line_index::LineIndex;
use crate::{insn::LiteralTable, types::Pos};

// ─── Expected token ───────────────────────────────────────────────────────────

/// One item in the "expected" set at the error position.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expected {
    /// A specific byte value, e.g. `b'"'`.
    Byte(u8),
    /// Any byte in `[lo, hi]` inclusive.
    ByteRange(u8, u8),
    /// Membership in a character class (described by `label`).
    Class(&'static str),
    /// A literal byte string, identified by its `lit_id` in the grammar.
    Literal(u32),
    /// The end-of-input sentinel.
    EndOfInput,
    /// Any valid Unicode codepoint was expected (from `Insn::AnyChar`).
    AnyChar,
    /// A specific Unicode codepoint, identified by its scalar value.
    Char(u32),
    /// Any Unicode codepoint in the inclusive range `[lo, hi]`.
    CharRange(u32, u32),
    /// A context flag was required (`required=true` means it must be set;
    /// `required=false` means it must be clear).
    /// A context-flag gate failed.  `id` is the [`FlagId`]; `required` is
    /// true for `IfFlag`, false for `IfNotFlag`.
    Flag { id: u16, required: bool },
}

impl Expected {
    /// Format a single `Expected` item into a human-readable string.
    pub fn display(&self, literals: Option<&LiteralTable>) -> String {
        match self {
            Expected::Byte(b) => {
                if b.is_ascii_graphic() {
                    format!("'{}'", *b as char)
                } else {
                    format!("0x{b:02X}")
                }
            }
            Expected::ByteRange(lo, hi) => {
                format!("'{}'–'{}'", *lo as char, *hi as char)
            }
            Expected::Class(label) => label.to_string(),
            Expected::Literal(lit_id) => {
                if let Some(tbl) = literals {
                    let bytes = tbl.get(*lit_id);
                    if let Ok(s) = std::str::from_utf8(bytes) {
                        return format!("{s:?}");
                    }
                    format!("0x{:X?}", bytes)
                } else {
                    format!("literal#{lit_id}")
                }
            }
            Expected::EndOfInput => "end-of-input".to_string(),
            Expected::AnyChar => "any Unicode character".to_string(),
            Expected::Char(cp) => {
                if let Some(c) = char::from_u32(*cp) {
                    if c.is_alphanumeric() || c.is_ascii_punctuation() {
                        format!("'{c}'")
                    } else {
                        format!("U+{cp:04X}")
                    }
                } else {
                    format!("U+{cp:04X}")
                }
            }
            Expected::CharRange(lo, hi) => {
                let fmt = |cp: u32| -> String {
                    char::from_u32(cp)
                        .filter(|c| c.is_alphanumeric() || c.is_ascii_punctuation())
                        .map(|c| format!("'{c}'"))
                        .unwrap_or_else(|| format!("U+{cp:04X}"))
                };
                format!("{}–{}", fmt(*lo), fmt(*hi))
            }
            Expected::Flag { id, required } => {
                let word = id >> 6;
                let bit  = id & 63;
                if *required {
                    format!("flag {id} (word {word} bit {bit}) to be set")
                } else {
                    format!("flag {id} (word {word} bit {bit}) to be clear")
                }
            }
        }
    }
}

// ─── Diagnostic ──────────────────────────────────────────────────────────────

/// The structured parse error produced when parsing fails.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// The furthest byte offset the parser reached before failing.
    pub furthest: Pos,
    /// What the parser expected to find at [`furthest`].
    pub expected: Vec<Expected>,
}

impl Diagnostic {
    /// Format the diagnostic using the grammar's literal table (optional).
    pub fn message(&self, literals: Option<&LiteralTable>) -> String {
        let items: Vec<String> = self
            .expected
            .iter()
            .map(|e| e.display(literals))
            .collect();

        let expected_str = match items.len() {
            0 => "nothing".to_string(),
            1 => items[0].clone(),
            _ => {
                let (last, rest) = items.split_last().unwrap();
                format!("{}, or {}", rest.join(", "), last)
            }
        };

        format!(
            "parse error at byte {}: expected {}",
            self.furthest, expected_str
        )
    }

    /// Format with line/column and source snippet for compiler/IDE use.
    ///
    /// Uses `line_index` to resolve `furthest` to a line and column, then
    /// appends the line of source and a caret. If `source` is empty or
    /// `line_index` is for a different file, falls back to [`message`].
    pub fn format_with_source(
        &self,
        source: &[u8],
        line_index: &LineIndex,
        literals: Option<&LiteralTable>,
    ) -> String {
        let expected_str = {
            let items: Vec<String> = self
                .expected
                .iter()
                .map(|e| e.display(literals))
                .collect();
            match items.len() {
                0 => "nothing".to_string(),
                1 => items[0].clone(),
                _ => {
                    let (last, rest) = items.split_last().unwrap();
                    format!("{}, or {}", rest.join(", "), last)
                }
            }
        };

        let (line_1, col_1) = line_index.line_col_1based(self.furthest);
        let header = format!(
            "parse error at {}:{} (byte {}): expected {}",
            line_1, col_1, self.furthest, expected_str
        );

        if source.is_empty() || self.furthest as usize > source.len() {
            return header;
        }

        let snippet = line_index.snippet_at(source, self.furthest);
        format!("{}\n{}", header, snippet)
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message(None))
    }
}

#[cfg(feature = "miette")]
impl Diagnostic {
    /// Convert this diagnostic into a [`miette::Diagnostic`] for pretty-printed
    /// reports with source snippets. Requires the `miette` feature.
    ///
    /// Use with [`miette::Report::new`]:
    ///
    /// ```ignore
    /// let report = miette::Report::new(diag.into_miette(source, "file.txt", Some(&graph.literals)));
    /// eprintln!("{:?}", report);
    /// ```
    pub fn into_miette(
        &self,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&LiteralTable>,
    ) -> crate::miette_support::MietteParseDiagnostic {
        crate::miette_support::MietteParseDiagnostic::new(self, source, name, literals)
    }
}

// ─── ErrorContext ─────────────────────────────────────────────────────────────

/// Accumulated error state maintained by the VM during a parse.
///
/// Cheap to allocate; call [`clear`] before each parse.
pub struct ErrorContext {
    /// The furthest byte position at which any terminal failed.
    pub furthest: Pos,
    /// What the parser expected at `furthest` (deduplicated).
    pub expected: Vec<Expected>,
    /// Scratch set for dedup (reused across `record` calls).
    seen: std::collections::HashSet<Expected>,
}

impl ErrorContext {
    pub fn new() -> Self {
        Self {
            furthest:  0,
            expected:  Vec::with_capacity(8),
            seen:      std::collections::HashSet::with_capacity(8),
        }
    }

    /// Reset for a new parse.
    #[inline]
    pub fn clear(&mut self) {
        self.furthest = 0;
        self.expected.clear();
        self.seen.clear();
    }

    /// Record a terminal failure at `pos` expecting `what`.
    ///
    /// Only updates if `pos >= self.furthest`; clears existing expectations
    /// if `pos > self.furthest`.
    #[inline(always)]
    pub fn record(&mut self, pos: Pos, what: Expected) {
        if pos > self.furthest {
            self.furthest = pos;
            self.expected.clear();
            self.seen.clear();
            self.seen.insert(what.clone());
            self.expected.push(what);
        } else if pos == self.furthest && self.seen.insert(what.clone()) {
            self.expected.push(what);
        }
    }

    /// Build a [`Diagnostic`] from the accumulated context.
    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            furthest: self.furthest,
            expected: self.expected.clone(),
        }
    }
}

impl Default for ErrorContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_context_record_furthest() {
        let mut ctx = ErrorContext::new();
        ctx.record(0, Expected::Byte(b'a'));
        ctx.record(1, Expected::Byte(b'b'));
        ctx.record(0, Expected::Byte(b'c')); // earlier, ignored
        assert_eq!(ctx.furthest, 1);
        assert_eq!(ctx.expected.len(), 1);
        assert!(matches!(ctx.expected[0], Expected::Byte(b'b')));
    }

    #[test]
    fn error_context_record_same_pos_dedup() {
        let mut ctx = ErrorContext::new();
        ctx.record(5, Expected::Byte(b'x'));
        ctx.record(5, Expected::EndOfInput);
        ctx.record(5, Expected::Byte(b'x')); // duplicate, not added
        assert_eq!(ctx.furthest, 5);
        assert_eq!(ctx.expected.len(), 2);
    }

    #[test]
    fn error_context_to_diagnostic() {
        let mut ctx = ErrorContext::new();
        ctx.record(10, Expected::Literal(0));
        ctx.record(10, Expected::EndOfInput);
        let diag = ctx.to_diagnostic();
        assert_eq!(diag.furthest, 10);
        assert_eq!(diag.expected.len(), 2);
    }
}
