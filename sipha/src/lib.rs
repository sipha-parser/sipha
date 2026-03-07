//! # sipha
//!
//! A PEG (Parsing Expression Grammar) parser with a stack-based VM, green/red
//! syntax trees, and optional packrat memoisation.
//!
//! ## Quick start
//!
//! ```rust
//! use sipha::prelude::*;
//!
//! let mut g = GrammarBuilder::new();
//! g.begin_rule("start");
//! g.byte(b'a');
//! g.end_of_input();
//! g.accept();
//! let built = g.finish().unwrap();
//! let graph = built.as_graph();
//!
//! let mut engine = Engine::new();
//! let out = engine.parse(&graph, b"a").unwrap();
//! assert_eq!(out.consumed, 1);
//! ```
//!
//! ## Compiler / formatter API
//!
//! For a single handle to source, tree, and line info after a successful parse,
//! use [`ParsedDoc`]: build it from `ParseOutput` and then use `doc.root()`,
//! `doc.offset_to_line_col_1based()`, and `doc.format_diagnostic()` for errors.
//! See `examples/parsed_doc_errors.rs` and the [`line_index`] and [`parsed_doc`]
//! modules.
//!
//! ## Tree walk (visitor, optional)
//!
//! With the default `walk` feature, use [`SyntaxNode::walk`] (or the free
//! function [`walk`]) with a [`Visitor`] and [`WalkOptions`] to traverse the
//! red tree for formatting, scope analysis, type checking, or linting. See the
//! [`walk`] module.
//!
//! ## Semantic (analysis) diagnostics
//!
//! For validation and other post-parse analyses, use [`SemanticDiagnostic`] and
//! [`Severity`]. Report span, message, and severity; then format with
//! [`SemanticDiagnostic::format_with_source`] or convert to miette via
//! [`SemanticDiagnostic::into_miette`] (with the `miette` feature). [`ParsedDoc`]
//! provides [`ParsedDoc::format_semantic_diagnostic`] so you can plug the same
//! output style as parse errors.
//!
//! ## Miette integration (optional)
//!
//! Enable the `miette` feature for pretty-printed diagnostics. Then use
//! [`ParseError::to_miette_report`] or [`Diagnostic::into_miette`] for parse
//! errors, and [`SemanticDiagnostic::into_miette`] for analysis diagnostics.
//! See `examples/miette_errors.rs`.
//!
//! See the individual modules for details and the `examples/` directory for
//! full grammars (e.g. JSON). For common patterns (expression precedence,
//! error recovery, byte_dispatch, trivia), see `docs/COOKBOOK.md` in the crate.

#![allow(clippy::module_name_repetitions)]

pub use sipha_macros::SyntaxKinds;

pub mod types;
pub mod context;
pub mod insn;
pub mod simd;
pub mod error;
pub mod memo;
pub mod engine;
pub mod capture;
pub mod green;
pub mod green_builder;
pub mod red;
pub mod builder;

pub mod codegen;
pub mod incremental;
pub mod sexp;

#[cfg(feature = "utf16")]
pub mod utf16;
/// Helpers for building expression grammars with precedence (left/right-assoc infix levels).
pub mod expr;
pub mod line_index;
pub mod parsed_doc;
pub mod trivia;
pub mod source_map;

#[cfg(feature = "walk")]
pub mod walk;

#[cfg(feature = "emit")]
pub mod emit;

#[cfg(feature = "transform")]
pub mod transform;

#[cfg(feature = "miette")]
pub mod miette_support;

pub mod prelude {
    pub use crate::types::{CharClass, FromSyntaxKind, IntoSyntaxKind, Span, SyntaxKind, Tag, TreeEvent, classes, sort_spans};
    pub use crate::context::{FlagId, ParseContext};
    pub use crate::insn::{Insn, FlagMaskTable, LiteralTable, ParseGraph};
    pub use crate::engine::{Engine, ParseError, ParseOutput, RecoverMultiResult};
    pub use crate::error::{Diagnostic, ErrorContext, Expected, SemanticDiagnostic, Severity};
    pub use crate::capture::CaptureNode;
    pub use crate::green::{GreenElement, GreenNode, GreenToken, build_green_tree};
    pub use crate::green_builder::GreenBuilder;
    pub use crate::red::{SyntaxElement, SyntaxNode, SyntaxToken, TokenWithTrivia};
    pub use crate::builder::{BuiltGraph, GrammarBuilder, Repeat};
    pub use crate::line_index::LineIndex;
    pub use crate::memo::MemoTable;
    pub use crate::parsed_doc::ParsedDoc;
    pub use crate::trivia::{newline, replace_leading_trivia, replace_trailing_trivia, space};
    pub use crate::source_map::{map_offset, SpanMap, SpanMapping};

    #[cfg(feature = "walk")]
    pub use crate::walk::{walk, Visitor, WalkOptions, WalkResult};

    #[cfg(feature = "emit")]
    pub use crate::emit::{syntax_root_to_string, EmitOptions};

    #[cfg(feature = "transform")]
    pub use crate::transform::{transform, TransformResult, Transformer};

    #[cfg(feature = "miette")]
    pub use crate::miette_support::{MietteParseDiagnostic, MietteSemanticDiagnostic};

    #[cfg(feature = "utf16")]
    pub use crate::utf16::{byte_offset_to_utf16, span_to_utf16_range, utf16_len};
}
