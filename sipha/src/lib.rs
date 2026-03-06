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
//! ## Miette integration (optional)
//!
//! Enable the `miette` feature for pretty-printed diagnostics. Then use
//! [`ParseError::to_miette_report`] or [`Diagnostic::into_miette`] to get a
//! [`miette::Report`]. See `examples/miette_errors.rs`.
//!
//! See the individual modules for details and the `examples/` directory for
//! full grammars (e.g. JSON).

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
pub mod red;
pub mod builder;
pub mod codegen;
pub mod line_index;
pub mod parsed_doc;

#[cfg(feature = "miette")]
pub mod miette_support;

pub mod prelude {
    pub use crate::types::{CharClass, FromSyntaxKind, IntoSyntaxKind, Span, SyntaxKind, Tag, TreeEvent, classes};
    pub use crate::context::{FlagId, ParseContext};
    pub use crate::insn::{Insn, FlagMaskTable, LiteralTable, ParseGraph};
    pub use crate::engine::{Engine, ParseError, ParseOutput};
    pub use crate::error::{Diagnostic, ErrorContext, Expected};
    pub use crate::capture::CaptureNode;
    pub use crate::green::{GreenElement, GreenNode, GreenToken, build_green_tree};
    pub use crate::red::{SyntaxElement, SyntaxNode, SyntaxToken, TokenWithTrivia};
    pub use crate::builder::{BuiltGraph, GrammarBuilder, Repeat};
    pub use crate::line_index::LineIndex;
    pub use crate::memo::MemoTable;
    pub use crate::parsed_doc::ParsedDoc;

    #[cfg(feature = "miette")]
    pub use crate::miette_support::MietteParseDiagnostic;
}
