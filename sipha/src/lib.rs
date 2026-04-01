//! # sipha
//!
//! A PEG (Parsing Expression Grammar) parser with a stack-based VM, green/red
//! syntax trees, and optional packrat memoisation.
//!
//! ## Crate layout
//!
//! - [`parse`] — grammar builder, VM engine, IR ([`Insn`](parse::insn::Insn)),
//!   codegen, memoisation.
//! - [`tree`] — green/red syntax trees, trivia, walking, optional emit/transform.
//! - [`diagnostics`] — spans, line index, [`ParsedDoc`](diagnostics::parsed_doc::ParsedDoc),
//!   parse and semantic diagnostics.
//! - [`types`] — [`Span`](types::Span), [`SyntaxKind`](types::SyntaxKind), char classes, tree events.
//! - [`extras`] — optional tools (display, fmt, diff, …), feature-gated.
//!
//! Use [`prelude`] for the usual names in one import, or import from the modules above.
//!
//! ## Quick start
//!
//! ```rust
//! # #[cfg(feature = "std")]
//! # {
//! use sipha::prelude::*;
//!
//! let mut g = GrammarBuilder::new();
//! g.rule("start", |g| {
//!     g.byte(b'a');
//!     g.end_of_input();
//!     g.accept();
//! });
//! let built = g.finish().unwrap();
//! let graph = built.as_graph();
//!
//! let mut engine = Engine::new();
//! let out = engine.parse(&graph, b"a").unwrap();
//! assert_eq!(out.consumed, 1);
//! # }
//! ```
//!
//! ## Compiler / formatter API
//!
//! For a single handle to source, tree, and line info after a successful parse,
//! use [`ParsedDoc`](diagnostics::parsed_doc::ParsedDoc): build it from `ParseOutput` and then use `doc.root()`,
//! `doc.offset_to_line_col_1based()`, and `doc.format_diagnostic()` for errors.
//! See `examples/parsed_doc_errors.rs` and the [`diagnostics::line_index`] and
//! [`diagnostics::parsed_doc`] modules.
//!
//! ## Tree walk (visitor, optional)
//!
//! With the default `walk` feature, use [`SyntaxNode::walk`](tree::red::SyntaxNode::walk) (or the free
//! function [`walk`](tree::walk::walk)) with a [`Visitor`](tree::walk::Visitor) and [`WalkOptions`](tree::walk::WalkOptions) to traverse the
//! red tree for formatting, scope analysis, type checking, or linting. See the
//! [`tree::walk`] module.
//!
//! ## Semantic (analysis) diagnostics
//!
//! For validation and other post-parse analyses, use [`SemanticDiagnostic`](diagnostics::error::SemanticDiagnostic) and
//! [`Severity`](diagnostics::error::Severity). Report span, message, and severity; then format with
//! [`SemanticDiagnostic::format_with_source`](diagnostics::error::SemanticDiagnostic::format_with_source) or convert to miette via
//! [`SemanticDiagnostic::into_miette`](diagnostics::error::SemanticDiagnostic::into_miette) (with the `miette` feature). [`ParsedDoc`](diagnostics::parsed_doc::ParsedDoc)
//! provides [`ParsedDoc::format_semantic_diagnostic`](diagnostics::parsed_doc::ParsedDoc::format_semantic_diagnostic) so you can plug the same
//! output style as parse errors.
//!
//! ## Miette integration (optional)
//!
//! Enable the `miette` feature for pretty-printed diagnostics. Then use
//! [`ParseError::to_miette_report`](parse::engine::ParseError::to_miette_report) or [`Diagnostic::into_miette`](diagnostics::error::Diagnostic::into_miette) for parse
//! errors, and [`SemanticDiagnostic::into_miette`](diagnostics::error::SemanticDiagnostic::into_miette) for analysis diagnostics.
//! See `examples/miette_errors.rs`.
//!
//! See the individual modules for details and the `examples/` directory for
//! full grammars (e.g. JSON). For common patterns (expression precedence,
//! error recovery, `byte_dispatch`, trivia), see the **Cookbook** in the repository
//! at `sipha/docs/COOKBOOK.md` ([rendered on GitHub](https://github.com/sipha-parser/sipha/blob/main/sipha/docs/COOKBOOK.md)).

#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub use sipha_macros::AstNode;
pub use sipha_macros::AstEnum;
pub use sipha_macros::SyntaxKinds;
pub use sipha_macros::sipha_grammar;

pub mod diagnostics;
#[cfg(feature = "std")]
pub mod extras;
pub mod parse;
#[cfg(feature = "std")]
pub mod tree;
pub mod types;

pub mod prelude {
    //! Commonly used types. For granular imports, use [`parse`](crate::parse),
    //! [`tree`](crate::tree), or [`diagnostics`](crate::diagnostics).

    /// Parsing: grammar builder, VM, IR, memo.
    ///
    /// N-way [`GrammarBuilder::choices`] without a `Vec`: use the [`choices`](crate::choices) macro
    /// at the crate root, for example `sipha::choices!(g, |g| g.literal(b"a"), |g| g.literal(b"b"))`.
    pub mod parse {
        #[cfg(feature = "std")]
        pub use crate::parse::builder::{BuiltGraph, GrammarBuilder, GrammarChoiceFn, Repeat};
        pub use crate::parse::capture::CaptureNode;
        pub use crate::parse::context::{FlagId, ParseContext};
        pub use crate::parse::engine::{
            BadGraphKind, Engine, ParseError, ParseOutput, RecoverMultiResult,
        };
        #[cfg(feature = "std")]
        pub use crate::parse::expr::{
            LeftAssocInfixLevel, left_assoc_infix_level, postfix_chain, right_assoc_infix_level,
            separated_rule_list, separated1_rule_list,
        };
        pub use crate::parse::insn::{FlagMaskTable, Insn, LiteralTable, ParseGraph};
        #[cfg(feature = "std")]
        pub use crate::parse::memo::MemoTable;
        pub use crate::parse::sublanguage::{
            EmbeddedSpan, SubLanguage, SubLanguageError, apply_sublanguages,
        };

        #[cfg_attr(docsrs, doc(cfg(feature = "incremental")))]
        #[cfg(feature = "incremental")]
        pub use crate::parse::incremental::{
            ReparseResult, ReuseStats, TextEdit, build_green_tree_with_reuse,
            build_green_tree_with_reuse_stats, reparse, reparse_with_output,
            reparse_with_output_and_stats,
        };

        #[cfg(feature = "parallel")]
        pub use crate::parse::parallel::{parse_many, parse_many_syntax_roots, parse_many_with};

        #[cfg(feature = "partial-reparse")]
        pub use crate::parse::partial_reparse::{
            PartialReparseConfig, PartialReparseResult, reparse_partial_or_fallback,
        };

        #[cfg(feature = "experimental_parallel_units")]
        pub use crate::parse::parallel_units::{
            UnitRange, parse_units_parallel, stitch_unit_syntax_root, stitch_unit_trees,
        };
    }

    /// Syntax trees: green layer, red layer, trivia, display helpers.
    pub mod tree {
        #[cfg(feature = "std")]
        pub use crate::tree::ast::{AstNode, AstNodeExt};
        #[cfg(feature = "std")]
        pub use crate::tree::green::{GreenElement, GreenNode, GreenToken, build_green_tree};
        #[cfg(feature = "std")]
        pub use crate::tree::green_builder::GreenBuilder;
        #[cfg(feature = "std")]
        pub use crate::tree::red::{SyntaxElement, SyntaxNode, SyntaxToken, TokenWithTrivia};
        #[cfg(feature = "std")]
        pub use crate::tree::tree_display::{TreeDisplayOptions, format_syntax_tree};
        #[cfg(feature = "std")]
        pub use crate::tree::trivia::{
            newline, replace_leading_trivia, replace_trailing_trivia, space,
        };

        #[cfg(feature = "walk")]
        pub use crate::tree::walk::{Visitor, WalkOptions, WalkResult, walk};

        #[cfg(feature = "emit")]
        pub use crate::tree::emit::{EmitOptions, syntax_root_to_string};

        #[cfg(feature = "transform")]
        pub use crate::tree::transform::{TransformResult, Transformer, transform};
    }

    /// Line index, parsed document, span maps, and diagnostic types.
    pub mod diagnostics {
        pub use crate::diagnostics::error::{
            Diagnostic, ErrorContext, Expected, RelatedLocation, SemanticDiagnostic, Severity,
        };
        #[cfg(feature = "std")]
        pub use crate::diagnostics::line_index::LineIndex;
        #[cfg(feature = "std")]
        pub use crate::diagnostics::parsed_doc::ParsedDoc;
        #[cfg(feature = "std")]
        pub use crate::diagnostics::source_map::{SpanMap, SpanMapping, map_offset};
        pub use crate::diagnostics::{GrammarNames, SliceGrammarNames};

        #[cfg(feature = "miette")]
        pub use crate::diagnostics::miette_support::{
            MietteParseDiagnostic, MietteSemanticDiagnostic,
        };

        #[cfg(feature = "lsp")]
        pub use crate::diagnostics::lsp::{
            LspDiagnostic, Position, Range, byte_offset_to_position, parse_diagnostic_to_lsp,
            parse_error_to_lsp, position_to_byte_offset, semantic_diagnostic_to_lsp, span_to_range,
        };

        #[cfg(feature = "utf16")]
        pub use crate::diagnostics::utf16::{byte_offset_to_utf16, span_to_utf16_range, utf16_len};
    }

    /// Optional add-ons (feature-gated); same symbols as `sipha::extras` when enabled.
    pub mod extras {
        #[cfg(feature = "patterns")]
        pub use crate::extras::patterns;

        #[cfg(feature = "analysis")]
        pub use crate::extras::analysis::{
            build_scope_extents, collect_definitions, scope_at_offset,
        };

        #[cfg(feature = "display")]
        pub use crate::extras::display::{
            RuleDepDotOptions, to_cfg_dot, to_peg, to_rule_dep_dot, to_rule_dep_dot_with_options,
        };

        #[cfg(feature = "sourcemap")]
        pub use crate::extras::sourcemap::transform_with_mapping;

        #[cfg(feature = "fmt")]
        pub use crate::extras::fmt::{format_full, format_semantic_only, format_with_skip};

        #[cfg(feature = "diff")]
        pub use crate::extras::diff::{
            SexpOptions, assert_parse_eq, format_diff, syntax_node_to_sexp, trees_equal,
            trees_equal_semantic,
        };
    }

    #[cfg(feature = "std")]
    pub use self::diagnostics::{
        Diagnostic, ErrorContext, Expected, GrammarNames, LineIndex, ParsedDoc, RelatedLocation,
        SemanticDiagnostic, Severity, SliceGrammarNames,
    };
    #[cfg(not(feature = "std"))]
    pub use self::diagnostics::{
        Diagnostic, ErrorContext, Expected, GrammarNames, SliceGrammarNames,
    };
    #[cfg(feature = "std")]
    pub use self::diagnostics::{SpanMap, SpanMapping, map_offset};
    #[cfg(feature = "std")]
    pub use self::parse::{
        BadGraphKind, BuiltGraph, CaptureNode, EmbeddedSpan, Engine, FlagId, FlagMaskTable,
        GrammarBuilder, Insn, LeftAssocInfixLevel, LiteralTable, MemoTable, ParseContext,
        ParseError, ParseGraph, ParseOutput, RecoverMultiResult, Repeat, SubLanguage,
        SubLanguageError, apply_sublanguages, left_assoc_infix_level, right_assoc_infix_level,
    };
    #[cfg(not(feature = "std"))]
    pub use self::parse::{
        BadGraphKind, CaptureNode, EmbeddedSpan, Engine, FlagId, FlagMaskTable, Insn, LiteralTable,
        ParseContext, ParseError, ParseGraph, ParseOutput, RecoverMultiResult, SubLanguage,
        SubLanguageError, apply_sublanguages,
    };
    #[cfg(feature = "std")]
    pub use crate::parse::parse_to_doc::parse_to_doc;

    #[cfg(feature = "std")]
    pub use self::tree::{
        GreenBuilder, GreenElement, GreenNode, GreenToken, SyntaxElement, SyntaxNode, SyntaxToken,
        TokenWithTrivia, TreeDisplayOptions, build_green_tree, format_syntax_tree, newline,
        replace_leading_trivia, replace_trailing_trivia, space,
    };
    #[cfg(feature = "std")]
    pub use crate::tree::ast::{AstNode, AstNodeExt};
    pub use crate::types::{
        CharClass, FromSyntaxKind, IntoSyntaxKind, Span, SyntaxKind, Tag, TreeEvent, classes,
        sort_spans,
    };

    #[cfg(feature = "walk")]
    pub use self::tree::{Visitor, WalkOptions, WalkResult, walk};

    #[cfg(feature = "emit")]
    pub use self::tree::{EmitOptions, syntax_root_to_string};

    #[cfg(feature = "transform")]
    pub use self::tree::{TransformResult, Transformer, transform};

    #[cfg(feature = "miette")]
    pub use self::diagnostics::{MietteParseDiagnostic, MietteSemanticDiagnostic};

    #[cfg(feature = "utf16")]
    pub use self::diagnostics::{byte_offset_to_utf16, span_to_utf16_range, utf16_len};

    #[cfg(feature = "analysis")]
    pub use self::extras::{build_scope_extents, collect_definitions, scope_at_offset};

    #[cfg(all(feature = "std", feature = "patterns"))]
    pub use self::extras::patterns;

    #[cfg(all(feature = "std", feature = "display"))]
    pub use self::extras::{
        RuleDepDotOptions, to_cfg_dot, to_peg, to_rule_dep_dot, to_rule_dep_dot_with_options,
    };

    #[cfg(feature = "sourcemap")]
    pub use self::extras::transform_with_mapping;

    #[cfg(feature = "fmt")]
    pub use self::extras::{format_full, format_semantic_only, format_with_skip};

    #[cfg(feature = "diff")]
    pub use self::extras::{
        SexpOptions, assert_parse_eq, format_diff, syntax_node_to_sexp, trees_equal,
        trees_equal_semantic,
    };

    #[cfg_attr(docsrs, doc(cfg(feature = "incremental")))]
    #[cfg(feature = "incremental")]
    pub use self::parse::{
        ReparseResult, ReuseStats, TextEdit, build_green_tree_with_reuse,
        build_green_tree_with_reuse_stats, reparse, reparse_with_output,
        reparse_with_output_and_stats,
    };

    #[cfg(feature = "parallel")]
    pub use self::parse::{parse_many, parse_many_syntax_roots, parse_many_with};

    #[cfg(feature = "partial-reparse")]
    pub use self::parse::{
        PartialReparseConfig, PartialReparseResult, reparse_partial_or_fallback,
    };

    #[cfg(feature = "experimental_parallel_units")]
    pub use self::parse::{
        UnitRange, parse_units_parallel, stitch_unit_syntax_root, stitch_unit_trees,
    };
}
