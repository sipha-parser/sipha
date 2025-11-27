//! # Lexer Module
//!
//! Tokenization and lexical analysis for source code.
//!
//! ## Overview
//!
//! The lexer converts source text into a stream of tokens. It supports:
//!
//! - **Pattern matching**: Literal strings, character classes, regex patterns
//! - **Keywords**: Reserved word recognition
//! - **Trivia handling**: Automatic skipping of whitespace and comments
//! - **Custom matchers**: User-defined token recognition logic
//!
//! ## Usage
//!
//! ```rust,no_run
//! use sipha::{LexerBuilder, lexer::Pattern, syntax::SyntaxKind};
//!
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MyKind {
//!     // Terminals
//!     Ident,
//!     Number,
//!     If,
//!     Whitespace,
//!     Eof,
//!     // Non-terminals
//!     Expr,
//! }
//!
//! impl SyntaxKind for MyKind {
//!     fn is_terminal(self) -> bool {
//!         !matches!(self, MyKind::Expr)
//!     }
//!     
//!     fn is_trivia(self) -> bool {
//!         matches!(self, MyKind::Whitespace)
//!     }
//! }
//!
//! let eof_kind = MyKind::Eof;
//! let ident_kind = MyKind::Ident;
//!
//! let lexer = LexerBuilder::new()
//!     .token(MyKind::Ident, Pattern::Any)
//!     .token(MyKind::Number, Pattern::Regex(r"\d+".into()))
//!     .keyword("if", MyKind::If)
//!     .trivia(MyKind::Whitespace)
//!     .build(eof_kind, ident_kind)?;
//!
//! let tokens = lexer.tokenize("if x == 42").unwrap();
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ## Error Handling
//!
//! The lexer returns [`LexerError`] for invalid input. Errors include:
//!
//! - Unexpected characters
//! - Unterminated strings
//! - Invalid escape sequences
//! - Invalid number formats
//!
//! See [`crate::error::LexerError`] for details.

pub mod builder;
pub mod dfa;
pub mod token;

pub use builder::{CharSet, LexRule, LexerBuilder, Pattern};
pub use dfa::*;
pub use token::*;
