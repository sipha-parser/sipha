//! LSP Server implementation

use crate::handlers::*;
use crate::session::LspSession;
use lsp_server::{Connection, Message, Notification, Request};
use lsp_types::{
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use sipha::backend::ParserBackend;
use sipha::grammar::{Grammar, NonTerminal, Token};
use std::sync::Arc;

/// Configuration for the LSP server
#[derive(Debug, Clone)]
pub struct ServerConfig {
    /// Enable incremental parsing (default: true)
    pub incremental_parsing: bool,
    /// Maximum cache size for incremental parsing
    pub cache_size: usize,
    /// Enable diagnostics
    pub diagnostics: bool,
    /// Enable document symbols
    pub document_symbols: bool,
    /// Enable semantic tokens
    pub semantic_tokens: bool,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            incremental_parsing: true,
            cache_size: 10000,
            diagnostics: true,
            document_symbols: true,
            semantic_tokens: false,
        }
    }
}

/// High-level LSP server for Sipha parsers
pub struct LspServer<T, N, P, L>
where
    T: Token,
    N: NonTerminal,
    P: ParserBackend<T, N>,
    L: crate::handlers::Lexer<T> + Clone,
{
    grammar: Arc<Grammar<T, N>>,
    config: ServerConfig,
    session: LspSession<T, N>,
    parser: P,
    lexer: L,
    entry_point: N,
}

impl<T, N, P, L> LspServer<T, N, P, L>
where
    T: Token,
    N: NonTerminal,
    P: ParserBackend<T, N>,
    L: crate::handlers::Lexer<T> + Clone,
{
    /// Create a new LSP server
    pub fn new(
        grammar: Grammar<T, N>,
        parser: P,
        lexer: L,
        entry_point: N,
        config: ServerConfig,
    ) -> Self {
        let cache_size = config.cache_size;
        Self {
            grammar: Arc::new(grammar),
            config,
            session: LspSession::new(cache_size),
            parser,
            lexer,
            entry_point,
        }
    }

    /// Run the LSP server
    ///
    /// This method blocks and handles LSP requests/notifications.
    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let (connection, io_threads) = Connection::stdio();

        // Wait for initialization
        let server_capabilities = self.get_server_capabilities();
        let initialization_params = connection.initialize(serde_json::to_value(server_capabilities)?)?;

        // Initialize the session
        let init_params: lsp_types::InitializeParams = serde_json::from_value(initialization_params)?;
        self.session.initialize(init_params);

        // Main event loop
        for msg in &connection.receiver {
            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    self.handle_request(req, &connection)?;
                }
                Message::Notification(not) => {
                    self.handle_notification(not, &connection)?;
                }
                Message::Response(_) => {
                    // Responses are handled by the connection
                }
            }
        }

        io_threads.join()?;
        Ok(())
    }

    fn get_server_capabilities(&self) -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                if self.config.incremental_parsing {
                    TextDocumentSyncKind::INCREMENTAL
                } else {
                    TextDocumentSyncKind::FULL
                },
            )),
            hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
            completion_provider: Some(lsp_types::CompletionOptions {
                trigger_characters: Some(vec![".".to_string(), "(".to_string(), "[".to_string()]),
                all_commit_characters: None,
                resolve_provider: Some(false),
                work_done_progress_options: Default::default(),
                completion_item: None,
            }),
            document_symbol_provider: if self.config.document_symbols {
                Some(lsp_types::OneOf::Right(lsp_types::DocumentSymbolOptions {
                    work_done_progress_options: Default::default(),
                    label: None,
                }))
            } else {
                None
            },
            semantic_tokens_provider: if self.config.semantic_tokens {
                Some(lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                    lsp_types::SemanticTokensOptions {
                        work_done_progress_options: Default::default(),
                        legend: lsp_types::SemanticTokensLegend {
                            token_types: sipha_lsp::StandardSemanticType::standard_types()
                                .into_iter()
                                .map(|s| lsp_types::SemanticTokenType::from(s))
                                .collect(),
                            token_modifiers: sipha_lsp::StandardSemanticModifier::standard_modifiers()
                                .into_iter()
                                .map(|s| lsp_types::SemanticTokenModifier::from(s))
                                .collect(),
                        },
                        range: Some(true),
                        full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                    },
                ))
            } else {
                None
            },
            ..Default::default()
        }
    }

    fn handle_request(&mut self, req: Request, connection: &Connection) -> Result<(), Box<dyn std::error::Error>> {
        match req.method.as_str() {
            "textDocument/hover" => {
                handle_hover(&self.grammar, &mut self.session, req, connection)?;
            }
            "textDocument/completion" => {
                handle_completion(&self.grammar, &mut self.session, req, connection)?;
            }
            "textDocument/documentSymbol" => {
                handle_document_symbol(&self.grammar, &mut self.session, req, connection)?;
            }
            "textDocument/semanticTokens/full" => {
                if self.config.semantic_tokens {
                    handle_semantic_tokens_full(&self.grammar, &mut self.session, req, connection)?;
                }
            }
            _ => {
                // Unknown request - log and continue
                eprintln!("Unknown request: {}", req.method);
            }
        }
        Ok(())
    }

    fn handle_notification(&mut self, not: Notification, connection: &Connection) -> Result<(), Box<dyn std::error::Error>> {
        match not.method.as_str() {
            "textDocument/didOpen" => {
                handle_did_open(
                    &self.grammar,
                    &mut self.session,
                    &mut self.parser,
                    &self.lexer,
                    &self.entry_point,
                    not,
                    connection,
                    &self.config,
                )?;
            }
            "textDocument/didChange" => {
                if self.config.incremental_parsing {
                    handle_did_change_incremental(
                        &self.grammar,
                        &mut self.session,
                        &mut self.parser,
                        &self.lexer,
                        &self.entry_point,
                        not,
                        connection,
                        &self.config,
                    )?;
                } else {
                    handle_did_change_full(&self.grammar, &mut self.session, not, connection)?;
                }
            }
            "textDocument/didClose" => {
                handle_did_close(&mut self.session, not)?;
            }
            _ => {
                // Unknown notification - log and continue
                eprintln!("Unknown notification: {}", not.method);
            }
        }
        Ok(())
    }
}

