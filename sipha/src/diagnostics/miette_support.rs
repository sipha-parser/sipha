//! # Miette support (optional)
//!
//! When the `miette` feature is enabled, parse diagnostics can be converted
//! into [`miette::Report`] for rich, pretty-printed error output with source
//! snippets.
//!
//! ## Example
//!
//! ```ignore
//! if let Err(e) = engine.parse(&graph, &source) {
//!     if let Some(report) = e.to_miette_report(&source, "script.xy", Some(&graph.literals)) {
//!         eprintln!("{:?}", report);  // or use miette's fancy handler
//!     }
//! }
//! ```
//!
//! For the graphical source snippet view, depend on `miette` with the `fancy` feature
//! and install the default hook (e.g. `miette::set_hook(Box::new(|_| ...))` or use
//! a crate that does so).
//!
//! Semantic (analysis) diagnostics can be converted via [`MietteSemanticDiagnostic`]
//! and [`SemanticDiagnostic::into_miette`].

use super::error::{Diagnostic, SemanticDiagnostic};
use super::grammar_names::GrammarNames;
use crate::parse::insn::LiteralTable;
use std::fmt;

/// Wrapper that implements [`miette::Diagnostic`] for a sipha parse error with source.
///
/// Construct via [`Diagnostic::into_miette`] when the `miette` feature is enabled.
#[derive(Debug)]
pub struct MietteParseDiagnostic {
    /// Pre-formatted "expected X, Y, or Z" message for the label.
    expected_msg: String,
    /// Source code for snippet display. Stored as `NamedSource<String>` so we can
    /// return `&dyn SourceCode` from the trait.
    source: miette::NamedSource<String>,
    /// Byte offset of the error (used for the primary label span).
    offset: usize,
    /// Hints from [`Diagnostic::hints`] (e.g. "did you mean 'x'?").
    hints: Vec<&'static str>,
}

impl MietteParseDiagnostic {
    /// Build a miette diagnostic from a sipha diagnostic and the source buffer.
    ///
    /// `source` is the input that was parsed (will be used for the code snippet).
    /// `name` is the source name shown in the report (e.g. filename).
    /// `literals` is the grammar's literal table for readable "expected" strings.
    /// `rule_names` is the grammar's rule names for [`Expected::Rule`](crate::diagnostics::error::Expected::Rule) display.
    /// `expected_labels` is for [`Expected::Label`](crate::diagnostics::error::Expected::Label) from [`expect_label`](crate::parse::builder::GrammarBuilder::expect_label).
    pub fn new(
        diagnostic: &Diagnostic,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> Self {
        let expected_msg = {
            let items: Vec<String> = diagnostic
                .expected
                .iter()
                .map(|e| e.display(literals, names))
                .collect();
            let list = super::error::format_expected_list(&items);
            if list == "nothing" {
                "expected nothing".to_string()
            } else {
                format!("expected {list}")
            }
        };
        let source_str = source.into();
        let name_str = name.into();
        let offset = diagnostic.furthest as usize;
        let source = miette::NamedSource::new(name_str, source_str);
        let hints = diagnostic.hints.clone();
        Self {
            expected_msg,
            source,
            offset,
            hints,
        }
    }
}

impl std::error::Error for MietteParseDiagnostic {}

impl fmt::Display for MietteParseDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error: {}", self.expected_msg)
    }
}

impl miette::Diagnostic for MietteParseDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(Box::new("sipha::parse"))
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(miette::Severity::Error)
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let offset = self.offset;
        let msg = self.expected_msg.clone();
        let span = miette::SourceSpan::from((offset, 1));
        let label = miette::LabeledSpan::new_with_span(Some(msg), span);
        Some(Box::new(std::iter::once(label)))
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        if self.hints.is_empty() {
            None
        } else {
            let s = self
                .hints
                .iter()
                .map(|h| format!("  hint: {h}"))
                .collect::<Vec<_>>()
                .join("\n");
            Some(Box::new(HintDisplay(s)))
        }
    }
}

/// Helper to display hints in miette's help.
struct HintDisplay(String);

impl fmt::Display for HintDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

// ─── Semantic diagnostic (analysis / validation) ──────────────────────────────

/// Wrapper that implements [`miette::Diagnostic`] for a semantic (analysis)
/// diagnostic with source. Construct via [`SemanticDiagnostic::into_miette`].
#[derive(Debug)]
pub struct MietteSemanticDiagnostic {
    message: String,
    source: miette::NamedSource<String>,
    span: miette::SourceSpan,
    severity: miette::Severity,
    code: Option<String>,
}

impl MietteSemanticDiagnostic {
    /// Build a miette diagnostic from a semantic diagnostic and the source buffer.
    pub fn new(
        diagnostic: &SemanticDiagnostic,
        source: impl Into<String>,
        name: impl Into<String>,
    ) -> Self {
        let source_str = source.into();
        let name_str = name.into();
        let start = diagnostic.span.start as usize;
        let len = diagnostic.span.len().max(1);
        let span = miette::SourceSpan::from((start, len));
        let severity = match diagnostic.severity {
            super::error::Severity::Error => miette::Severity::Error,
            super::error::Severity::Warning | super::error::Severity::Deprecation => {
                miette::Severity::Warning
            }
            super::error::Severity::Note => miette::Severity::Advice,
        };
        Self {
            message: diagnostic.message.clone(),
            source: miette::NamedSource::new(name_str, source_str),
            span,
            severity,
            code: diagnostic.code.clone(),
        }
    }
}

impl std::error::Error for MietteSemanticDiagnostic {}

impl fmt::Display for MietteSemanticDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl miette::Diagnostic for MietteSemanticDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code
            .as_ref()
            .map(|c| Box::new(c.as_str()) as Box<dyn fmt::Display>)
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(self.severity)
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let msg = self.message.clone();
        let span = self.span;
        let label = miette::LabeledSpan::new_with_span(Some(msg), span);
        Some(Box::new(std::iter::once(label)))
    }
}
