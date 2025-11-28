//! # Sipha LSP Integration
//!
//! This crate provides conversion traits and utilities for integrating Sipha parsers
//! with Language Server Protocol (LSP) implementations.
//!
//! ## Features
//!
//! - Convert `ParseError` to `lsp_types::Diagnostic`
//! - Convert `SyntaxNode` to `lsp_types::DocumentSymbol` (for outlines/breadcrumbs)
//! - Convert `TextRange` to `lsp_types::Range`
//! - Helper functions for common LSP operations
//!
//! ## Usage
//!
//! ```rust,no_run
//! use sipha_lsp::{ToDiagnostic, ToRange, ToDocumentSymbol};
//! use sipha::error::ParseError;
//! use sipha::syntax::{SyntaxNode, SyntaxKind};
//! use sipha::syntax::TextRange;
//!
//! // Convert parse error to LSP diagnostic
//! let error: ParseError = /* ... */;
//! let diagnostic = error.to_diagnostic();
//!
//! // Convert text range to LSP range
//! let range: TextRange = /* ... */;
//! let lsp_range = range.to_range();
//!
//! // Convert syntax node to document symbol
//! let node: SyntaxNode<MySyntaxKind> = /* ... */;
//! let symbol = node.to_document_symbol();
//! ```

pub mod conversions;

pub use conversions::{
    SyntaxKindToSymbolKind, ToDiagnostic, ToDocumentSymbol, ToRange, ToRangeWithSource,
    map_common_symbol_kinds,
};
