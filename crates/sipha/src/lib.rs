// Allow false positive warnings from derive(Clone) on enums with named fields
#![allow(unused_assignments)]
//! # Sipha
//!
//! A flexible, incremental parsing library for Rust with support for multiple parsing algorithms.
//!
//! ## Overview
//!
//! Sipha provides the foundational types and traits for building parsers. It supports:
//!
//! - **Multiple parsing backends**: LL(k), LR, and more (via feature flags)
//! - **Incremental parsing**: Efficient re-parsing of edited code
//! - **Syntax trees**: Immutable green/red tree representation
//! - **Error recovery**: Configurable error recovery strategies
//! - **Grammar definition**: Flexible grammar builder API
//!
//! ## Quick Start
//!
//! This example shows how to create a simple arithmetic expression parser:
//!
//! ```rust,no_run
//! use sipha::grammar::{GrammarBuilder, Token, NonTerminal, Expr};
//! use sipha::syntax::SyntaxKind as SyntaxKindTrait;
//! use sipha::backend::ll::{LlParser, LlConfig};
//! use sipha::backend::ParserBackend;
//! use sipha::syntax::SyntaxNode;
//!
//! // 1. Define your syntax kinds (unified enum for both terminals and non-terminals)
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! enum MySyntaxKind {
//!     // Terminals (produced by lexer)
//!     Number,
//!     Plus,
//!     Minus,
//!     Whitespace,  // Trivia: whitespace that should be ignored
//!     Eof,
//!     // Non-terminals (produced by parser)
//!     Expr,
//! }
//!
//! // Implement SyntaxKind for all variants
//! impl SyntaxKindTrait for MySyntaxKind {
//!     fn is_terminal(self) -> bool {
//!         !matches!(self, MySyntaxKind::Expr)
//!     }
//!     
//!     fn is_trivia(self) -> bool {
//!         matches!(self, MySyntaxKind::Whitespace)
//!     }
//! }
//!
//! // 2. Build a lexer to tokenize text input
//! use sipha::{LexerBuilder, lexer::{Pattern, CharSet}};
//!
//! let lexer = LexerBuilder::new()
//!     // Match numbers using repeating character class [0-9]+ (one or more digits)
//!     .token(MySyntaxKind::Number, Pattern::Repeat {
//!         pattern: Box::new(Pattern::CharClass(CharSet::digits())),
//!         min: 1,
//!         max: None, // No maximum - match as many as possible
//!     })
//!     .token(MySyntaxKind::Plus, Pattern::Literal("+".into()))
//!     .token(MySyntaxKind::Minus, Pattern::Literal("-".into()))
//!     // Match whitespace using repeating character class (one or more whitespace chars)
//!     .token(MySyntaxKind::Whitespace, Pattern::Repeat {
//!         pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
//!         min: 1,
//!         max: None,
//!     })
//!     // Mark whitespace as trivia so it's automatically skipped during parsing
//!     .trivia(MySyntaxKind::Whitespace)
//!     .build(MySyntaxKind::Eof, MySyntaxKind::Number)
//!     .expect("Failed to build lexer");
//!
//! // 3. Tokenize input text (with whitespace - trivia will be automatically skipped)
//! // Start with a simple expression: just a number
//! let input_text = "1";
//! let lexer_tokens = lexer.tokenize(input_text)
//!     .expect("Failed to tokenize input");
//! // Verify lexer produced tokens
//! assert!(!lexer_tokens.is_empty(), "Lexer should produce at least one token");
//!
//! // 4. Use lexer tokens directly - they implement grammar::Token
//! // Filter out Eof token (parser handles EOF automatically)
//! let grammar_tokens: Vec<sipha::lexer::Token<MySyntaxKind>> = lexer_tokens
//!     .into_iter()
//!     .filter(|t| t.kind != MySyntaxKind::Eof)
//!     .collect();
//!
//! // 5. Define your non-terminals
//! #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//! enum MyNonTerminal {
//!     Expr,
//! }
//!
//! impl NonTerminal for MyNonTerminal {
//!     fn name(&self) -> &str {
//!         match self {
//!             MyNonTerminal::Expr => "Expr",
//!         }
//!     }
//! }
//!
//! // 6. Build the grammar using GrammarBuilder
//! use sipha::lexer::Token as LexerToken;
//! let grammar = GrammarBuilder::new()
//!     .entry_point(MyNonTerminal::Expr)
//!     .rule(MyNonTerminal::Expr, Expr::token(LexerToken::new(
//!         MySyntaxKind::Number,
//!         "",
//!         sipha::syntax::TextRange::at(sipha::syntax::TextSize::zero(), sipha::syntax::TextSize::zero()),
//!     )))
//!     .build()
//!     .expect("Failed to build grammar");
//!
//! // 7. Create the parser with default configuration
//! let config = LlConfig::default();
//! let mut parser = LlParser::new(&grammar, config)
//!     .expect("Failed to create parser");
//!
//! // 8. Verify we have tokens to parse
//! assert!(!grammar_tokens.is_empty(), "Should have at least one token to parse");
//!
//! // 9. Parse using the tokenized input
//! let result = parser.parse(&grammar_tokens, MyNonTerminal::Expr);
//!
//! // 10. Verify parsing succeeded (no errors)
//! if !result.errors.is_empty() {
//!     panic!("Parsing failed with errors: {:?}", result.errors);
//! }
//! // Verify we consumed the expected token
//! assert!(result.metrics.tokens_consumed >= 1, "Should consume at least 1 token");
//!
//! // 11. Verify syntax tree structure
//! let red_tree = SyntaxNode::new_root(result.root.clone());
//! // The root kind should be Expr or Number (depending on implementation)
//! assert!(matches!(red_tree.kind(), MySyntaxKind::Expr | MySyntaxKind::Number));
//!
//! // 12. Verify we can traverse the syntax tree
//! let children: Vec<_> = red_tree.children().collect();
//! assert!(!children.is_empty(), "Syntax tree should have children");
//!
//! // 13. Verify metrics
//! assert!(result.metrics.nodes_created > 0, "Should create at least one node");
//! assert!(result.metrics.parse_time.as_nanos() > 0, "Parse time should be recorded");
//! ```
//!
//! ## Modules
//!
//! - [`syntax`] - Syntax tree types (green/red trees, nodes, tokens)
//! - [`grammar`] - Grammar definition and validation
//! - [`lexer`] - Tokenization and lexing
//! - [`parser`] - Parser traits and interfaces
//! - [`backend`] - Parser backend implementations
//! - [`error`] - Error types and diagnostics
//! - [`incremental`] - Incremental parsing support

pub mod arena;
pub mod backend;
pub mod error;
pub mod grammar;
pub mod incremental;
pub mod intern;
pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod testing;

// Re-export commonly used types
pub use arena::{GreenElementData, GreenNodeData, GreenTokenData, SharedGreenNode, TreeArena};
pub use error::{LexerError, ParseError, ParseMetrics, ParseResult};
pub use grammar::{Expr, Grammar, GrammarBuilder, NonTerminal, Rule, Token as GrammarToken};
pub use intern::{InternedStr, InternedToken, Interner, ThreadSafeInterner};
pub use lexer::{CompiledLexer, LexerBuilder, Token};
pub use syntax::{
    GreenNode, GreenNodeBuilder, GreenToken, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken,
    TextRange, TextSize,
};

// Conditionally re-export feature-gated modules
#[cfg(feature = "visitor")]
pub use syntax::{SyntaxVisitor, SyntaxWalker};

#[cfg(feature = "query")]
pub use syntax::{QueryBuilder, XPathQuery};

#[cfg(feature = "tree-utils")]
pub use syntax::{TreeDiff, TreeStats, ValidationResult};
