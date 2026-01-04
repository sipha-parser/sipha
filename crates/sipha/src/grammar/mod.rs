//! # Grammar Module
//!
//! Grammar definition and validation for context-free grammars.
//!
//! ## Overview
//!
//! This module provides types and utilities for defining and working with context-free grammars.
//! It supports:
//!
//! - **Production rules**: Define grammar rules with expressions
//! - **Expression combinators**: Sequence, choice, repetition, etc.
//! - **Grammar validation**: Detect left recursion, conflicts, etc.
//! - **Backend hints**: Provide hints to parser backends
//!
//! ## Usage
//!
//! ```rust,no_run
//! use sipha::{GrammarBuilder, Expr, grammar::{Token, NonTerminal}, syntax::SyntaxKind};
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MyNonTerminal {
//!     Expr,
//!     Term,
//! }
//!
//! impl NonTerminal for MyNonTerminal {
//!     fn name(&self) -> &str {
//!         match self {
//!             MyNonTerminal::Expr => "Expr",
//!             MyNonTerminal::Term => "Term",
//!         }
//!     }
//! }
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MyTokenKind {
//!     Plus,
//! }
//!
//! impl SyntaxKind for MyTokenKind {
//!     fn is_terminal(self) -> bool {
//!         true
//!     }
//!     
//!     fn is_trivia(self) -> bool {
//!         false
//!     }
//! }
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MyToken {
//!     Plus,
//! }
//!
//! impl Token for MyToken {
//!     type Kind = MyTokenKind;
//!     fn kind(&self) -> Self::Kind {
//!         MyTokenKind::Plus
//!     }
//! }
//!
//! let grammar = GrammarBuilder::new()
//!     .entry_point(MyNonTerminal::Expr)
//!     .rule(MyNonTerminal::Expr, Expr::Choice(vec![
//!         Expr::Seq(vec![
//!             Expr::Rule(MyNonTerminal::Term),
//!             Expr::Token(MyToken::Plus),
//!             Expr::Rule(MyNonTerminal::Expr),
//!         ]),
//!         Expr::Rule(MyNonTerminal::Term),
//!     ]))
//!     .build()?;
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ## Expression Types
//!
//! - **Primitives**: `Token`, `Rule`, `Any`, `Eof`, `Empty`
//! - **Combinators**: `Seq`, `Choice`, `Opt`, `Repeat`
//! - **Advanced**: `Separated`, `Delimited`, `Node`, `Label`
//! - **Predicates**: `Lookahead`, `NotLookahead`, `SemanticPredicate`
//! - **Advanced**: `Cut`, `TokenClass`, `Conditional`, `Backreference`
//! - **Error Recovery**: `RecoveryPoint`
//!
//! See [`Expr`] for the full list of expression types.
//!
//! ## Additional Modules
//!
//! - **Token Classes**: [`token_class`] - Match tokens by class (digit, letter, etc.)
//! - **Captures**: [`capture`] - Capture groups for backreferences
//! - **Semantic Predicates**: [`predicate`] - Context-sensitive parsing predicates

pub mod analysis;
pub mod analyzer;
pub mod builder;
pub mod capture;
pub mod expr;
pub mod hint;
pub mod predicate;
pub mod token_class;
pub mod validate;

#[cfg(feature = "grammar-docs")]
pub mod docs;

pub use analysis::*;
pub use analyzer::*;
pub use builder::*;
pub use capture::*;
pub use expr::*;
pub use hint::*;
pub use predicate::*;
pub use token_class::*;
pub use validate::*;

#[cfg(feature = "grammar-docs")]
pub use docs::*;

// Re-exported types for convenience

/// Trait for token types
pub trait Token: Clone + std::fmt::Debug + std::hash::Hash + Eq + Send + Sync + 'static {
    /// The syntax kind type for this token
    type Kind: crate::syntax::SyntaxKind;

    /// Get the syntax kind for this token
    fn kind(&self) -> Self::Kind;

    /// Get the text length of this token in bytes
    ///
    /// This is used for accurate text position tracking during parsing.
    /// The default implementation returns 1, but implementations should
    /// provide the actual token length for proper error reporting.
    fn text_len(&self) -> crate::syntax::TextSize {
        crate::syntax::TextSize::from(1)
    }

    /// Get the text representation of this token
    ///
    /// This is used for syntax tree construction. The default implementation
    /// uses Debug formatting, but implementations should provide the actual
    /// token text when available.
    fn text(&self) -> compact_str::CompactString {
        format!("{self:?}").into()
    }
}

/// Trait for non-terminal types
pub trait NonTerminal:
    Clone + std::fmt::Debug + std::hash::Hash + Eq + Send + Sync + 'static
{
    /// Get the name of this non-terminal
    fn name(&self) -> &str;

    /// Convert this non-terminal to a syntax kind
    ///
    /// This is used for syntax tree construction. Users should implement this
    /// for their specific types to provide a mapping from non-terminals to syntax kinds.
    ///
    /// Returns None if no mapping is available, in which case the parser will
    /// attempt to infer the kind from context (e.g., from tokens).
    fn to_syntax_kind<K: crate::syntax::SyntaxKind>(&self) -> Option<K> {
        let _ = self;
        None
    }

    /// Get a default syntax kind for this non-terminal
    ///
    /// This is used as a fallback when [`to_syntax_kind()`](Self::to_syntax_kind) returns `None` and
    /// no tokens are available. Users should implement this to provide a
    /// default kind (e.g., a generic "node" kind).
    ///
    /// The default implementation returns None, which will cause parsing
    /// to fail if no other kind can be determined.
    fn default_syntax_kind<K: crate::syntax::SyntaxKind>(&self) -> Option<K> {
        let _ = self;
        None
    }
}
