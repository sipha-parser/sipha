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
//! [`Diagnostic::format_with_source`] with a [`LineIndex`].

use super::grammar_names::GrammarNames;
use super::line_index::LineIndex;
use crate::{parse::insn::LiteralTable, types::Pos, types::RuleId, types::Span};

// ─── Expected token ───────────────────────────────────────────────────────────

/// One item in the "expected" set at the error position.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expected {
    /// A specific byte value, e.g. `b'"'`.
    Byte(u8),
    /// Any byte in `[lo, hi]` inclusive.
    ByteRange(u8, u8),
    /// Membership in a character class; `label_id` indexes the grammar's class-label table.
    ClassLabel(u32),
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
    /// A context-flag gate failed.  `id` is the [`FlagId`](crate::parse::context::FlagId); `required` is
    /// true for `IfFlag`, false for `IfNotFlag`.
    Flag { id: u16, required: bool },
    /// A named rule was expected (e.g. "statement", "expr").  Display uses
    /// `rule_names` to show the rule name when available.
    Rule(RuleId),
    /// A custom expectation label (e.g. "statement", "expression") from
    /// [`expect_label`](crate::parse::builder::GrammarBuilder::expect_label).  Display
    /// uses `expected_labels` when available.
    Label(u32),
}

impl Expected {
    /// Format a single `Expected` item into a human-readable string.
    ///
    /// `rule_names` is used for [`Expected::Rule`]; `expected_labels` for
    /// [`Expected::Label`]. Pass the grammar's tables when formatting.
    #[must_use]
    pub fn display(
        &self,
        literals: Option<&LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> String {
        match self {
            Self::Byte(b) => {
                if (*b).is_ascii_graphic() {
                    format!("'{}'", char::from(*b))
                } else {
                    format!("0x{b:02X}")
                }
            }
            Self::ByteRange(lo, hi) => {
                format!("'{}'-'{}'", char::from(*lo), char::from(*hi))
            }
            Self::ClassLabel(label_id) => {
                if let Some(n) = names {
                    if let Some(s) = n.class_label(*label_id) {
                        return s.to_string();
                    }
                }
                format!("class#{label_id}")
            }
            Self::Literal(lit_id) => {
                if let Some(tbl) = literals {
                    let bytes = tbl.get(*lit_id);
                    if let Ok(s) = std::str::from_utf8(bytes) {
                        return format!("{s:?}");
                    }
                    format!("0x{bytes:X?}")
                } else {
                    format!("literal#{lit_id}")
                }
            }
            Self::EndOfInput => "end-of-input".to_string(),
            Self::AnyChar => "any Unicode character".to_string(),
            Self::Char(cp) => char::from_u32(*cp).map_or_else(
                || format!("U+{cp:04X}"),
                |c| {
                    if c.is_alphanumeric() || c.is_ascii_punctuation() {
                        format!("'{c}'")
                    } else {
                        format!("U+{cp:04X}")
                    }
                },
            ),
            Self::CharRange(lo, hi) => {
                let fmt = |cp: u32| -> String {
                    char::from_u32(cp)
                        .filter(|c| c.is_alphanumeric() || c.is_ascii_punctuation())
                        .map_or_else(|| format!("U+{cp:04X}"), |c| format!("'{c}'"))
                };
                format!("{}–{}", fmt(*lo), fmt(*hi))
            }
            Self::Flag { id, required } => {
                let word = id >> 6;
                let bit = id & 63;
                if *required {
                    format!("flag {id} (word {word} bit {bit}) to be set")
                } else {
                    format!("flag {id} (word {word} bit {bit}) to be clear")
                }
            }
            Self::Rule(rule_id) => {
                if let Some(n) = names {
                    if let Some(s) = n.rule_name(*rule_id) {
                        return s.to_string();
                    }
                }
                format!("rule#{rule_id}")
            }
            Self::Label(label_id) => {
                if let Some(n) = names {
                    if let Some(s) = n.expected_label(*label_id) {
                        return s.to_string();
                    }
                }
                format!("expected#{label_id}")
            }
        }
    }
}

// ─── Helpers for expected list formatting ─────────────────────────────────────

pub(crate) fn format_expected_list(items: &[String]) -> String {
    if items.is_empty() {
        return "nothing".to_string();
    }
    let (display, more) = if items.len() <= MAX_EXPECTED_DISPLAY {
        (items, 0)
    } else {
        (
            &items[..MAX_EXPECTED_DISPLAY],
            items.len() - MAX_EXPECTED_DISPLAY,
        )
    };
    let list = if display.len() == 1 {
        display[0].clone()
    } else {
        let (last, rest) = display.split_last().unwrap();
        format!("{}, or {last}", rest.join(", "))
    };
    if more > 0 {
        format!("{list} (and {more} more)")
    } else {
        list
    }
}

// ─── Diagnostic ──────────────────────────────────────────────────────────────

/// The structured parse error produced when parsing fails.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// The furthest byte offset the parser reached before failing.
    pub furthest: Pos,
    /// What the parser expected to find at [`furthest`](Diagnostic::furthest).
    pub expected: Vec<Expected>,
    /// Optional hints (e.g. "did you mean 'x'?", "add a semicolon here") shown after the expected message.
    pub hints: Vec<&'static str>,
}

/// Maximum number of expected items to show before truncating with "(and N more)".
const MAX_EXPECTED_DISPLAY: usize = 10;

impl Diagnostic {
    /// Format the diagnostic using the grammar's literal, rule-name, and label tables (optional).
    ///
    /// If there are more than `MAX_EXPECTED_DISPLAY` (10) expected items, only the first
    /// ones are shown and the message ends with "(and N more)".
    #[must_use]
    pub fn message(
        &self,
        literals: Option<&LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> String {
        let items: Vec<String> = self
            .expected
            .iter()
            .map(|e| e.display(literals, names))
            .collect();

        let expected_str = format_expected_list(&items);

        let mut out = format!(
            "parse error at byte {}: expected {expected_str}",
            self.furthest
        );
        for hint in &self.hints {
            out.push_str("\n  hint: ");
            out.push_str(hint);
        }
        out
    }

    /// Format with line/column and source snippet for compiler/IDE use.
    ///
    /// Uses `line_index` to resolve `furthest` to a line and column, then
    /// appends the line of source and a caret. If `source` is empty or
    /// `line_index` is for a different file, falls back to [`message`](Diagnostic::message).
    #[must_use]
    pub fn format_with_source(
        &self,
        source: &[u8],
        line_index: &LineIndex,
        literals: Option<&LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> String {
        let expected_str = {
            let items: Vec<String> = self
                .expected
                .iter()
                .map(|e| e.display(literals, names))
                .collect();
            format_expected_list(&items)
        };

        let (line_1, col_1) = line_index.line_col_1based(self.furthest);
        let header = format!(
            "parse error at {line_1}:{col_1} (byte {}): expected {expected_str}",
            self.furthest
        );

        if source.is_empty() || self.furthest as usize > source.len() {
            let mut out = header;
            for hint in &self.hints {
                out.push_str("\n  hint: ");
                out.push_str(hint);
            }
            return out;
        }

        let mut out = header;
        for hint in &self.hints {
            out.push_str("\n  hint: ");
            out.push_str(hint);
        }
        out.push('\n');
        out.push_str(&line_index.snippet_at(source, self.furthest));
        out
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message(None, None))
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
    #[must_use]
    pub fn into_miette(
        &self,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> super::miette_support::MietteParseDiagnostic {
        super::miette_support::MietteParseDiagnostic::new(self, source, name, literals, names)
    }
}

// ─── ErrorContext ─────────────────────────────────────────────────────────────

/// Accumulated error state maintained by the VM during a parse.
///
/// Cheap to allocate; call [`ErrorContext::clear`] before each parse.
pub struct ErrorContext {
    /// The furthest byte position at which any terminal failed.
    pub furthest: Pos,
    /// What the parser expected at `furthest` (deduplicated).
    pub expected: Vec<Expected>,
    /// Scratch set for dedup (reused across `record` calls).
    seen: std::collections::HashSet<Expected>,
}

impl ErrorContext {
    #[must_use]
    pub fn new() -> Self {
        Self {
            furthest: 0,
            expected: Vec::with_capacity(8),
            seen: std::collections::HashSet::with_capacity(8),
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
    #[inline]
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
    #[must_use]
    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            furthest: self.furthest,
            expected: self.expected.clone(),
            hints: Vec::new(),
        }
    }
}

impl Default for ErrorContext {
    fn default() -> Self {
        Self::new()
    }
}

// ─── Semantic (analysis) diagnostics ─────────────────────────────────────────

/// Severity for analysis/validation diagnostics (errors, warnings, etc.).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Severity {
    /// Error: invalid program (e.g. undefined variable, type mismatch).
    Error,
    /// Warning: likely mistake or bad style (e.g. unused variable).
    Warning,
    /// Deprecation: use of deprecated syntax or API (e.g. deprecated operators).
    Deprecation,
    /// Informational note.
    Note,
}

/// Related location for a diagnostic (e.g. "first declared here" for duplicate name).
#[derive(Clone, Debug)]
pub struct RelatedLocation {
    /// Span of the related occurrence.
    pub span: Span,
    /// Short message to show at that location (e.g. "first declared here").
    pub message: String,
}

/// A single diagnostic from semantic analysis or validation (e.g. undefined
/// variable, type error).
///
/// Use with [`SemanticDiagnostic::format_with_source`] or
/// [`SemanticDiagnostic::into_miette`] (with the `miette` feature) so tools
/// and IDEs can show consistent, pluggable error output.
#[derive(Clone, Debug)]
pub struct SemanticDiagnostic {
    /// Source span (half-open byte range) for highlighting.
    pub span: Span,
    /// Human-readable message.
    pub message: String,
    /// Error, warning, or note.
    pub severity: Severity,
    /// Optional code (e.g. `"E0425"`, `"unused_variable"`) for filtering and docs.
    pub code: Option<String>,
    /// Optional file id or path for multi-file compilers (e.g. `"src/main.leek"`).
    pub file_id: Option<String>,
    /// Optional related locations (e.g. first declaration for duplicate name errors).
    pub related: Vec<RelatedLocation>,
}

impl SemanticDiagnostic {
    /// Format this diagnostic with line/column and source snippet.
    ///
    /// Uses `line_index` to resolve `span.start` to a line and column, then
    /// appends the line of source and a caret. Callers can use [`ParsedDoc`](crate::diagnostics::parsed_doc::ParsedDoc)
    /// to get `source` and `line_index` from a successful parse.
    #[must_use]
    pub fn format_with_source(&self, source: &[u8], line_index: &LineIndex) -> String {
        let (line_1, col_1) = line_index.line_col_1based(self.span.start);
        let severity_label = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Deprecation => "deprecation",
            Severity::Note => "note",
        };
        let code_part = self
            .code
            .as_deref()
            .map(|c| format!(" [{c}]"))
            .unwrap_or_default();
        let location = self.file_id.as_deref().map_or_else(
            || format!("{line_1}:{col_1}"),
            |f| format!("{f}:{line_1}:{col_1}"),
        );
        let header = format!(
            "{}: {}{}: {}",
            location, severity_label, code_part, self.message
        );

        if source.is_empty() || self.span.start as usize >= source.len() {
            return header;
        }

        let snippet = line_index.snippet_at(source, self.span.start);
        format!("{header}\n{snippet}")
    }

    /// Build a diagnostic for an error at the given span.
    pub fn error(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            severity: Severity::Error,
            code: None,
            file_id: None,
            related: Vec::new(),
        }
    }

    /// Build a diagnostic for a warning at the given span.
    pub fn warning(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            severity: Severity::Warning,
            code: None,
            file_id: None,
            related: Vec::new(),
        }
    }

    /// Build a diagnostic for use of deprecated syntax or API.
    pub fn deprecation(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            severity: Severity::Deprecation,
            code: None,
            file_id: None,
            related: Vec::new(),
        }
    }

    /// Set an optional code (e.g. for filtering or docs).
    #[must_use]
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Set an optional file id or path (for multi-file diagnostics).
    #[must_use]
    pub fn with_file_id(mut self, file_id: impl Into<String>) -> Self {
        self.file_id = Some(file_id.into());
        self
    }

    /// Add related locations (e.g. "first declared here" for duplicate name errors).
    #[must_use]
    pub fn with_related(mut self, related: Vec<RelatedLocation>) -> Self {
        self.related = related;
        self
    }
}

#[cfg(feature = "miette")]
impl SemanticDiagnostic {
    /// Convert this diagnostic into a value implementing [`miette::Diagnostic`]
    /// for pretty-printed reports with source snippets.
    #[must_use]
    pub fn into_miette(
        &self,
        source: impl Into<String>,
        name: impl Into<String>,
    ) -> super::miette_support::MietteSemanticDiagnostic {
        super::miette_support::MietteSemanticDiagnostic::new(self, source, name)
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

    #[test]
    fn expected_rule_display() {
        let rule = Expected::Rule(0);
        let tables = super::super::grammar_names::SliceGrammarNames {
            rule_names: &["start"],
            expected_labels: &[],
            class_labels: &[],
        };
        assert_eq!(rule.display(None, Some(&tables)), "start");
        assert_eq!(rule.display(None, None), "rule#0");
    }

    #[test]
    fn expected_label_display() {
        let label = Expected::Label(0);
        let tables = super::super::grammar_names::SliceGrammarNames {
            rule_names: &[],
            expected_labels: &["statement"],
            class_labels: &[],
        };
        assert_eq!(label.display(None, Some(&tables)), "statement");
        assert_eq!(label.display(None, None), "expected#0");
    }
}
