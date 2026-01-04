//! # Sipha LSP Server Framework
//!
//! High-level framework for building LSP servers with Sipha parsers.
//!
//! This crate provides:
//!
//! - Built-in incremental parsing integration
//! - Automatic textDocument/didChange handling
//! - Common LSP feature implementations
//! - Easy-to-use server builder API
//!
//! ## Example
//!
//! ```rust,no_run
//! use sipha_lsp_server::{LspServer, ServerConfig};
//! use sipha::grammar::{Grammar, NonTerminal, Token};
//!
//! # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//! # enum MyNonTerminal { Expr }
//! # impl NonTerminal for MyNonTerminal {
//! #     fn name(&self) -> &str { "Expr" }
//! # }
//! # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! # enum MyToken { Plus }
//! # impl Token for MyToken {
//! #     type Kind = ();
//! #     fn kind(&self) -> Self::Kind { () }
//! # }
//! # let grammar: Grammar<MyToken, MyNonTerminal> = todo!();
//! let config = ServerConfig::default();
//! let server = LspServer::new(grammar, config);
//! server.run()?;
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

pub mod server;
pub mod handlers;
pub mod session;

pub use server::{LspServer, ServerConfig};
pub use handlers::*;
pub use session::LspSession;

