//! LSP (Language Server Protocol) conversion helpers.
//!
//! Enable the `lsp` feature to use this module. This feature depends on `utf16`
//! because LSP `Position.character` is defined in UTF-16 code units.

use super::parsed_doc::ParsedDoc;
use crate::diagnostics::error::{Diagnostic, SemanticDiagnostic, Severity};
use crate::parse::engine::ParseError;
use crate::types::{Pos, Span};

pub use lsp_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity, Position, Range};

/// Convert a byte offset to an LSP `Position` (0-based line, UTF-16 character).
#[must_use]
pub fn byte_offset_to_position(doc: &ParsedDoc, offset: Pos) -> Position {
    let (line, character) = doc.offset_to_line_col_utf16(offset);
    Position { line, character }
}

/// Convert a byte span to an LSP `Range` using UTF-16 character offsets.
#[must_use]
pub fn span_to_range(doc: &ParsedDoc, span: Span) -> Range {
    let start = byte_offset_to_position(doc, span.start);
    let end = byte_offset_to_position(doc, span.end);
    Range { start, end }
}

/// Convert an LSP `Position` to a byte offset.
///
/// Returns `None` if the position is out of bounds for the document.
#[must_use]
pub fn position_to_byte_offset(doc: &ParsedDoc, pos: Position) -> Option<Pos> {
    doc.line_index()
        .line_col_utf16_to_byte(doc.source_str(), pos.line, pos.character)
}

fn severity_to_lsp(sev: Severity) -> Option<DiagnosticSeverity> {
    match sev {
        Severity::Error => Some(DiagnosticSeverity::ERROR),
        Severity::Warning => Some(DiagnosticSeverity::WARNING),
        Severity::Deprecation => Some(DiagnosticSeverity::HINT),
        Severity::Note => Some(DiagnosticSeverity::INFORMATION),
    }
}

/// Convert a parse `Diagnostic` (furthest/expected) into an LSP diagnostic.
///
/// The range is derived from `diag.primary_span(source_len)`.
#[must_use]
pub fn parse_diagnostic_to_lsp(doc: &ParsedDoc, diag: &Diagnostic) -> LspDiagnostic {
    let primary = diag.primary_span(doc.source().len());
    LspDiagnostic {
        range: span_to_range(doc, primary),
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("sipha".to_string()),
        message: diag.message(None, None),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a semantic diagnostic into an LSP diagnostic.
#[must_use]
pub fn semantic_diagnostic_to_lsp(doc: &ParsedDoc, diag: &SemanticDiagnostic) -> LspDiagnostic {
    LspDiagnostic {
        range: span_to_range(doc, diag.span),
        severity: severity_to_lsp(diag.severity),
        code: diag
            .code
            .clone()
            .map(lsp_types::NumberOrString::String),
        code_description: None,
        source: Some("sipha".to_string()),
        message: diag.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a `ParseError` into an LSP diagnostic when possible.
///
/// Returns `None` for non-parse failures (`BadGraph`, `UnknownRuleName`), since those
/// are typically configuration/infra errors rather than source diagnostics.
#[must_use]
pub fn parse_error_to_lsp(doc: &ParsedDoc, err: &ParseError) -> Option<LspDiagnostic> {
    match err {
        ParseError::NoMatch(d) => Some(parse_diagnostic_to_lsp(doc, d)),
        ParseError::BadGraph(_) | ParseError::UnknownRuleName(_) => None,
    }
}

