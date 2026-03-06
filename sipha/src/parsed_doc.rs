//! # Parsed document
//!
//! Single type that holds source, line index, and syntax root after a
//! successful parse. Use this as the main handle for the compiler (errors,
//! spans, line/col) and formatter (tokens, trivia, source slices).

use crate::engine::ParseOutput;
use crate::error::Diagnostic;
use crate::insn::LiteralTable;
use crate::line_index::LineIndex;
use crate::red::SyntaxNode;
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
    source:      Vec<u8>,
    line_index:  LineIndex,
    root:        SyntaxNode,
}

impl ParsedDoc {
    /// Build a parsed document from the raw source and parse output.
    ///
    /// Returns `None` if the output has no or invalid tree events (e.g. empty
    /// or malformed). The `source` slice used during parsing should match what
    /// you pass here so that token text and spans are correct.
    pub fn new(source: Vec<u8>, output: ParseOutput) -> Option<Self> {
        let line_index = LineIndex::new(&source);
        let root = output.syntax_root(&source)?;
        Some(Self {
            source,
            line_index,
            root,
        })
    }

    /// Reference to the syntax tree root.
    #[inline]
    pub fn root(&self) -> &SyntaxNode {
        &self.root
    }

    /// Raw source bytes.
    #[inline]
    pub fn source(&self) -> &[u8] {
        &self.source
    }

    /// Source as UTF-8 str, or empty if invalid UTF-8.
    #[inline]
    pub fn source_str(&self) -> &str {
        std::str::from_utf8(&self.source).unwrap_or("")
    }

    /// Line index for this document (offset → line/column, snippets).
    #[inline]
    pub fn line_index(&self) -> &LineIndex {
        &self.line_index
    }

    /// Line and column (0-based) for a byte offset.
    #[inline]
    pub fn offset_to_line_col(&self, offset: Pos) -> (u32, u32) {
        self.line_index.line_col(offset)
    }

    /// Line and column (1-based) for error messages and IDE.
    #[inline]
    pub fn offset_to_line_col_1based(&self, offset: Pos) -> (u32, u32) {
        self.line_index.line_col_1based(offset)
    }

    /// Single-line snippet with caret at the given offset (for ad-hoc errors).
    #[inline]
    pub fn snippet_at(&self, offset: Pos) -> String {
        self.line_index.snippet_at(&self.source, offset)
    }

    /// Format a parse diagnostic with line/column and source snippet.
    ///
    /// Pass the grammar's literal table for readable "expected" strings;
    /// otherwise literal ids are shown.
    pub fn format_diagnostic(
        &self,
        diagnostic: &Diagnostic,
        literals: Option<&LiteralTable>,
    ) -> String {
        diagnostic.format_with_source(&self.source, &self.line_index, literals)
    }

    /// Byte slice for a span (convenience for compiler/formatter).
    #[inline]
    pub fn span_slice(&self, span: Span) -> &[u8] {
        span.as_slice(&self.source)
    }
}
