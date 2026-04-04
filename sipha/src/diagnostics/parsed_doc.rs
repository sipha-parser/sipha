//! # Parsed document
//!
//! Single type that holds source, line index, and syntax root after a
//! successful parse. Use this as the main handle for the compiler (errors,
//! spans, line/col) and formatter (tokens, trivia, source slices).

use super::error::{Diagnostic, SemanticDiagnostic};
use super::grammar_names::GrammarNames;
use super::line_index::LineIndex;
use crate::parse::engine::ParseOutput;
use crate::parse::insn::LiteralTable;
use crate::tree::red::SyntaxNode;
use crate::types::{Pos, Span};

/// A successfully parsed document: source buffer, line index, and syntax root.
///
/// Use this as the single entry point for:
/// - **Compiler**: `root()` to walk the tree, `offset_to_line_col()` and
///   `format_diagnostic()` for errors, `source()` for span slices.
/// - **Formatter**: `root()` for `token_groups()` and trivia, `source()` or
///   token/node text for rewriting.
#[derive(Debug)]
pub struct ParsedDoc {
    source: Vec<u8>,
    line_index: LineIndex,
    root: SyntaxNode,
}

impl ParsedDoc {
    /// Build a parsed document from the raw source and parse output.
    ///
    /// Returns `None` if the output has no or invalid tree events (e.g. empty
    /// or malformed). The `source` slice used during parsing should match what
    /// you pass here so that token text and spans are correct.
    #[must_use]
    pub fn new(source: Vec<u8>, output: &ParseOutput) -> Option<Self> {
        let line_index = LineIndex::new(&source);
        let root = output.syntax_root(&source)?;
        Some(Self {
            source,
            line_index,
            root,
        })
    }

    /// Build a parsed document from a source slice and parse output.
    ///
    /// Clones `source` into a `Vec<u8>`. Prefer [`new`](Self::new) when you
    /// already own a `Vec<u8>`. Returns `None` if the output has no or invalid
    /// tree events.
    #[must_use]
    pub fn from_slice(source: &[u8], output: &ParseOutput) -> Option<Self> {
        let mut v = Vec::with_capacity(source.len());
        v.extend_from_slice(source);
        Self::new(v, output)
    }

    /// Recover the owned UTF-8 buffer (e.g. to reuse its capacity for another parse).
    #[must_use]
    pub fn into_bytes(self) -> Vec<u8> {
        self.source
    }

    /// Reference to the syntax tree root.
    #[inline]
    #[must_use]
    pub const fn root(&self) -> &SyntaxNode {
        &self.root
    }

    /// Raw source bytes.
    #[inline]
    #[must_use]
    pub fn source(&self) -> &[u8] {
        &self.source
    }

    /// Source as UTF-8 str, or empty if invalid UTF-8.
    #[inline]
    #[must_use]
    pub fn source_str(&self) -> &str {
        std::str::from_utf8(&self.source).unwrap_or("")
    }

    /// Line index for this document (offset → line/column, snippets).
    #[inline]
    #[must_use]
    pub const fn line_index(&self) -> &LineIndex {
        &self.line_index
    }

    /// Line and column (0-based) for a byte offset.
    #[inline]
    #[must_use]
    pub fn offset_to_line_col(&self, offset: Pos) -> (u32, u32) {
        self.line_index.line_col(offset)
    }

    /// Line and column (1-based) for error messages and IDE.
    #[inline]
    #[must_use]
    pub fn offset_to_line_col_1based(&self, offset: Pos) -> (u32, u32) {
        self.line_index.line_col_1based(offset)
    }

    /// Single-line snippet with caret at the given offset (for ad-hoc errors).
    #[inline]
    #[must_use]
    pub fn snippet_at(&self, offset: Pos) -> String {
        self.line_index.snippet_at(&self.source, offset)
    }

    /// Format a parse diagnostic with line/column and source snippet.
    ///
    /// Pass the grammar's literal table and name tables (e.g. `graph` as [`GrammarNames`]) for readable
    /// "expected" strings; otherwise literal/rule/label ids are shown.
    #[must_use]
    pub fn format_diagnostic(
        &self,
        diagnostic: &Diagnostic,
        literals: Option<&LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> String {
        diagnostic.format_with_source(&self.source, &self.line_index, literals, names)
    }

    /// Byte slice for a span (convenience for compiler/formatter).
    #[inline]
    #[must_use]
    pub fn span_slice(&self, span: Span) -> &[u8] {
        span.as_slice(&self.source)
    }

    /// Format a semantic (analysis) diagnostic with line/column and source snippet.
    ///
    /// Use this to report validation/analysis errors and warnings in the same
    /// style as parse errors. For miette reports, use
    /// [`SemanticDiagnostic::into_miette`] with `self.source_str()` and your
    /// file name.
    #[must_use]
    pub fn format_semantic_diagnostic(&self, diagnostic: &SemanticDiagnostic) -> String {
        diagnostic.format_with_source(&self.source, &self.line_index)
    }

    /// Return the smallest syntax node whose range contains the given byte offset.
    #[inline]
    #[must_use]
    pub fn node_at_offset(&self, offset: Pos) -> Option<SyntaxNode> {
        self.root.node_at_offset(offset)
    }

    /// Return the deepest token at the given byte offset (including trivia).
    #[inline]
    #[must_use]
    pub fn token_at_offset(&self, offset: Pos) -> Option<crate::tree::red::SyntaxToken> {
        self.root.token_at_offset(offset)
    }

    /// Line and column (0-based) with column in UTF-16 code units.
    /// Requires the `utf16` feature.
    #[cfg(feature = "utf16")]
    #[inline]
    #[must_use]
    pub fn offset_to_line_col_utf16(&self, offset: Pos) -> (u32, u32) {
        self.line_index.line_col_utf16(self.source_str(), offset)
    }

    /// Line and column (1-based) in UTF-16 for LSP and error messages.
    #[cfg(feature = "utf16")]
    #[inline]
    #[must_use]
    pub fn offset_to_line_col_utf16_1based(&self, offset: Pos) -> (u32, u32) {
        self.line_index
            .line_col_utf16_1based(self.source_str(), offset)
    }

    /// UTF-16 code unit range `(start, end)` for the given span.
    #[cfg(feature = "utf16")]
    #[inline]
    #[must_use]
    pub fn span_to_utf16_range(&self, span: Span) -> (u32, u32) {
        super::utf16::span_to_utf16_range(span, self.source_str())
    }
}
