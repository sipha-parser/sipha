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

use crate::error::Diagnostic;
use crate::insn::LiteralTable;
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
}

impl MietteParseDiagnostic {
    /// Build a miette diagnostic from a sipha diagnostic and the source buffer.
    ///
    /// `source` is the input that was parsed (will be used for the code snippet).
    /// `name` is the source name shown in the report (e.g. filename).
    /// `literals` is the grammar's literal table for readable "expected" strings.
    pub fn new(
        diagnostic: &Diagnostic,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&LiteralTable>,
    ) -> Self {
        let expected_msg = {
            let items: Vec<String> = diagnostic
                .expected
                .iter()
                .map(|e| e.display(literals))
                .collect();
            match items.len() {
                0 => "expected nothing".to_string(),
                1 => format!("expected {}", items[0]),
                _ => {
                    let (last, rest) = items.split_last().unwrap();
                    format!("expected {}, or {}", rest.join(", "), last)
                }
            }
        };
        let source_str = source.into();
        let name_str = name.into();
        let offset = diagnostic.furthest as usize;
        let source = miette::NamedSource::new(name_str, source_str);
        Self {
            expected_msg,
            source,
            offset,
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
}
