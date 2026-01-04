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
//! # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! # enum MySyntaxKind { Expr, Number }
//! # impl SyntaxKind for MySyntaxKind {
//! #     fn is_terminal(self) -> bool { matches!(self, Self::Number) }
//! #     fn is_trivia(self) -> bool { false }
//! # }
//! # fn example() {
//! // Convert parse error to LSP diagnostic
//! # let error: ParseError = todo!();
//! let diagnostic = error.to_diagnostic();
//!
//! // Convert text range to LSP range
//! # let range: TextRange = todo!();
//! let lsp_range = range.to_range();
//!
//! // Convert syntax node to document symbol
//! # let node: SyntaxNode<MySyntaxKind> = todo!();
//! let symbol = node.to_document_symbol();
//! # }
//! ```

pub mod conversions;

pub use conversions::{
    // Semantic tokens
    SemanticTokenInfo,
    StandardSemanticModifier,
    StandardSemanticType,
    SyntaxKindToSemanticType,
    // Core traits
    SyntaxKindToSymbolKind,
    // Completion helpers
    ToCompletionItem,
    ToDiagnostic,
    ToDocumentSymbol,
    ToRange,
    ToRangeWithSource,
    // Hover helpers
    code_block,
    create_hover,
    function_completion,
    keyword_completion,
    map_common_symbol_kinds,
    snippet_completion,
};
