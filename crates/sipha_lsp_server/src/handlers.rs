//! LSP request and notification handlers

use crate::session::LspSession;
use crate::server::ServerConfig;
use lsp_server::{Connection, Notification, Request, Response};
use lsp_types::*;
use sipha::error::{LexerError, ParseError};
use sipha::grammar::{Grammar, NonTerminal, Token};
use sipha::incremental::TextEdit;
use sipha::syntax::red::SyntaxNode;
use sipha::syntax::{SyntaxKind, TextRange, TextSize};
use sipha_lsp::{ToDiagnostic, ToDocumentSymbol, ToRangeWithSource, create_hover};
use std::sync::Arc;

/// Trait for lexers that can tokenize text
pub trait Lexer<T: Token> {
    fn tokenize(&self, input: &str) -> Result<Vec<T>, Vec<LexerError>>;
}

// Implement for CompiledLexer when T::Kind matches the lexer's kind
impl<K: sipha::syntax::SyntaxKind> Lexer<sipha::lexer::Token<K>> for sipha::lexer::CompiledLexer<K> {
    fn tokenize(&self, input: &str) -> Result<Vec<sipha::lexer::Token<K>>, Vec<LexerError>> {
        self.tokenize(input)
    }
}

/// Send diagnostics to the LSP client
fn send_diagnostics(
    connection: &Connection,
    uri: &lsp_types::Uri,
    diagnostics: &[lsp_types::Diagnostic],
) -> Result<(), Box<dyn std::error::Error>> {
    let params = lsp_types::PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics: diagnostics.to_vec(),
        version: None,
    };
    let notification = Notification::new(
        "textDocument/publishDiagnostics".into(),
        params,
    );
    connection.sender.send(notification.into())?;
    Ok(())
}

/// Handle textDocument/didOpen notification
pub fn handle_did_open<T, N, P, L>(
    grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    parser: &mut P,
    lexer: &L,
    entry_point: &N,
    not: Notification,
    connection: &Connection,
    config: &ServerConfig,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
    P: sipha::backend::ParserBackend<T, N>,
    L: Lexer<T> + Clone,
{
    let params: DidOpenTextDocumentParams = serde_json::from_value(not.params)?;
    let doc = session.get_or_create_document(&params.text_document.uri);
    doc.text = params.text_document.text.clone();
    
    // Tokenize the document
    let tokens = match lexer.tokenize(&doc.text) {
        Ok(tokens) => tokens,
        Err(errors) => {
            // Send lexer errors as diagnostics
            if config.diagnostics {
                let diagnostics: Vec<lsp_types::Diagnostic> = errors
                    .iter()
                    .map(|e| {
                        let parse_error = ParseError::from_lexer_error(e);
                        parse_error.to_diagnostic()
                    })
                    .collect::<Vec<_>>();
                send_diagnostics(connection, &params.text_document.uri, &diagnostics)?;
            }
            return Ok(());
        }
    };
    
    // Parse the document
    let result = parser.parse(&tokens, entry_point.clone());
    doc.tree = Some(result.root.clone());
    
    // Send diagnostics if configured
    if config.diagnostics {
        let diagnostics: Vec<lsp_types::Diagnostic> = result
            .errors
            .iter()
            .map(|e| e.to_diagnostic())
            .collect::<Vec<_>>();
        send_diagnostics(connection, &params.text_document.uri, &diagnostics)?;
    }
    
    Ok(())
}

/// Convert LSP range to TextRange
fn lsp_range_to_text_range(range: &Range, line_index: &sipha::syntax::line_col::LineIndex) -> TextRange {
    let start = line_index.offset(range.start.line, range.start.character);
    let end = line_index.offset(range.end.line, range.end.character);
    TextRange::new(start, end)
}

/// Handle textDocument/didChange notification with incremental parsing
pub fn handle_did_change_incremental<T, N, P, L>(
    grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    parser: &mut P,
    lexer: &L,
    entry_point: &N,
    not: Notification,
    connection: &Connection,
    config: &ServerConfig,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
    P: sipha::backend::ParserBackend<T, N>,
    L: Lexer<T> + Clone,
{
    let params: DidChangeTextDocumentParams = serde_json::from_value(not.params)?;
    let doc = session.get_document_mut(&params.text_document.uri);
    
    if let Some(doc) = doc {
        let line_index = sipha::syntax::line_col::LineIndex::new(&doc.text);
        let mut edits = Vec::new();
        
        // Apply incremental changes
        for change in &params.content_changes {
            match change {
                TextDocumentContentChangeEvent {
                    range: Some(range),
                    range_length: _,
                    text,
                } => {
                    // Incremental change - convert to TextEdit
                    let text_range = lsp_range_to_text_range(range, &line_index);
                    edits.push(TextEdit {
                        range: text_range,
                        new_text: text.clone(),
                    });
                }
                TextDocumentContentChangeEvent {
                    range: None,
                    range_length: _,
                    text,
                } => {
                    // Full document replacement
                    doc.text = text.clone();
                    doc.tree = None; // Invalidate tree
                    // Re-parse the entire document
                    let tokens = match lexer.tokenize(&doc.text) {
                        Ok(tokens) => tokens,
                        Err(_) => return Ok(()),
                    };
                    let result = parser.parse(&tokens, entry_point.clone());
                    doc.tree = Some(result.root.clone());
                    
                    if config.diagnostics {
                        let diagnostics: Vec<lsp_types::Diagnostic> = result
                            .errors
                            .iter()
                            .map(|e| e.to_diagnostic())
                            .collect::<Vec<_>>();
                        send_diagnostics(connection, &params.text_document.uri, &diagnostics)?;
                    }
                    return Ok(());
                }
            }
        }
        
        // Apply edits to text
        for edit in &edits {
            // Simple text replacement - in a real implementation, use a proper text editor
            let start = u64::from(edit.range.start().into()) as usize;
            let end = u64::from(edit.range.end().into()) as usize;
            if start <= doc.text.len() && end <= doc.text.len() {
                doc.text.replace_range(start..end, &edit.new_text);
            }
        }
        
        // Update parse tree incrementally
        let old_tree: Option<&sipha::syntax::GreenNode<T::Kind>> = doc.tree.as_ref().map(|t| t.as_ref());
        let incremental_session = sipha::incremental::IncrementalSession::new(old_tree, &edits);
        
        // Tokenize the updated text
        let tokens = match lexer.tokenize(&doc.text) {
            Ok(tokens) => tokens,
            Err(_) => {
                doc.tree = None;
                return Ok(());
            }
        };
        
        // Parse with incremental session
        let result = parser.parse_with_session(&tokens, entry_point.clone(), &incremental_session);
        doc.tree = Some(result.root.clone());
        
        // Send diagnostics if configured
        if config.diagnostics {
            let diagnostics: Vec<lsp_types::Diagnostic> = result
                .errors
                .iter()
                .map(|e| e.to_diagnostic())
                .collect();
            send_diagnostics(connection, &params.text_document.uri, &diagnostics)?;
        }
    }
    
    Ok(())
}

/// Handle textDocument/didChange notification with full re-parse
pub fn handle_did_change_full<T, N>(
    _grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    not: Notification,
    _connection: &Connection,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
{
    let params: DidChangeTextDocumentParams = serde_json::from_value(not.params)?;
    let doc = session.get_document_mut(&params.text_document.uri);
    
    if let Some(doc) = doc {
        // Apply changes to text
        for change in &params.content_changes {
            match change {
                TextDocumentContentChangeEvent {
                    range: Some(range),
                    range_length: _,
                    text,
                } => {
                    // Apply text edit
                    let line_index = sipha::syntax::line_col::LineIndex::new(&doc.text);
                    let text_range = lsp_range_to_text_range(range, &line_index);
                    let start = u64::from(text_range.start().into()) as usize;
                    let end = u64::from(text_range.end().into()) as usize;
                    if start <= doc.text.len() && end <= doc.text.len() {
                        doc.text.replace_range(start..end, text);
                    }
                }
                TextDocumentContentChangeEvent {
                    range: None,
                    range_length: _,
                    text,
                } => {
                    doc.text = text.clone();
                }
            }
        }
        doc.tree = None; // Invalidate tree for full re-parse
    }
    
    Ok(())
}

/// Handle textDocument/didClose notification
pub fn handle_did_close<T, N>(
    session: &mut LspSession<T, N>,
    not: Notification,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
{
    let params: DidCloseTextDocumentParams = serde_json::from_value(not.params)?;
    session.remove_document(&params.text_document.uri);
    Ok(())
}

/// Handle textDocument/hover request
pub fn handle_hover<T, N>(
    _grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    req: Request,
    connection: &Connection,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
{
    let params: HoverParams = serde_json::from_value(req.params)?;
    let uri = &params.text_document_position_params.text_document.uri;
    let position = &params.text_document_position_params.position;
    
    // Get hover information from parse tree
    let hover = if let Some(doc) = session.get_document(uri) {
        if let Some(tree) = &doc.tree {
            // Convert tree to red tree for navigation
            let root = sipha::syntax::red::SyntaxNode::new_root(tree.clone());
            
            // Find node at cursor position
            let line_index = sipha::syntax::line_col::LineIndex::new(&doc.text);
            let offset = line_index.offset(position.line, position.character);
            
            // Find the smallest node containing this offset
            let mut node_at_cursor = root.clone();
            for child in root.descendants() {
                let range = child.text_range();
                if range.contains(offset) && range.len() < node_at_cursor.text_range().len() {
                    node_at_cursor = child;
                }
            }
            
            // Generate hover content from node
            let kind_str = format!("{:?}", node_at_cursor.kind());
            let text = node_at_cursor.text();
            let content = format!("**Kind**: {}\n\n**Text**: `{}`", kind_str, text);
            
            let range = node_at_cursor.text_range().to_range_with_source(&doc.text);
            create_hover(&content, Some(range))
        } else {
            // No parse tree available
            create_hover("No parse information available", None)
        }
    } else {
        create_hover("Document not found", None)
    };
    
    let response = Response::new_ok(req.id, hover);
    connection.sender.send(response.into())?;
    
    Ok(())
}

/// Handle textDocument/completion request
pub fn handle_completion<T, N>(
    grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    req: Request,
    connection: &Connection,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
{
    let params: CompletionParams = serde_json::from_value(req.params)?;
    
    // Generate completion items based on grammar and context
    let mut items = Vec::new();
    
    // Add completions from grammar rules
    for (nt, _rule) in grammar.rules() {
        items.push(sipha_lsp::function_completion(
            nt.name(),
            Some(&format!("Rule: {}", nt.name())),
        ));
    }
    
    // Add keyword completions if available
    // Note: This would require access to token definitions
    // For now, we provide a basic set
    
    let response = Response::new_ok(req.id, CompletionResponse::Array(items));
    connection.sender.send(response.into())?;
    
    Ok(())
}

/// Handle textDocument/documentSymbol request
pub fn handle_document_symbol<T, N>(
    _grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    req: Request,
    connection: &Connection,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
{
    let params: DocumentSymbolParams = serde_json::from_value(req.params)?;
    let uri = &params.text_document.uri;
    
    let symbols: Vec<DocumentSymbol> = if let Some(doc) = session.get_document(uri) {
        if let Some(tree) = &doc.tree {
            // Convert tree to document symbols using ToDocumentSymbol trait
            let root = sipha::syntax::red::SyntaxNode::new_root(tree.clone());
            
            // Collect all symbols from the tree
            let mut symbols = Vec::new();
            for node in root.descendants() {
                // Only include non-trivial nodes (skip tokens and trivia)
                if !node.kind().is_terminal() && !node.kind().is_trivia() {
                    let symbol = node.to_document_symbol();
                    symbols.push(symbol);
                }
            }
            symbols
        } else {
            vec![]
        }
    } else {
        vec![]
    };
    
    let response = Response::new_ok(req.id, Some(DocumentSymbolResponse::Nested(symbols)));
    connection.sender.send(response.into())?;
    
    Ok(())
}

/// Handle textDocument/semanticTokens/full request
pub fn handle_semantic_tokens_full<T, N>(
    _grammar: &Arc<Grammar<T, N>>,
    session: &mut LspSession<T, N>,
    req: Request,
    connection: &Connection,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: Token,
    N: NonTerminal,
{
    let params: SemanticTokensParams = serde_json::from_value(req.params)?;
    let uri = &params.text_document.uri;
    
    // Generate semantic tokens from parse tree
    let tokens = if let Some(doc) = session.get_document(uri) {
        if let Some(tree) = &doc.tree {
            let root = sipha::syntax::red::SyntaxNode::new_root(tree.clone());
            let line_index = sipha::syntax::line_col::LineIndex::new(&doc.text);
            
            // Collect semantic tokens from the tree
            let mut semantic_tokens = Vec::new();
            let mut prev_line = 0u32;
            let mut prev_char = 0u32;
            
            for node in root.descendants() {
                // Only include terminal nodes (actual tokens)
                if node.kind().is_terminal() && !node.kind().is_trivia() {
                    let range = node.text_range();
                    let start = line_index.line_col(range.start());
                    let end = line_index.line_col(range.end());
                    
                    // Calculate delta encoding
                    let delta_line = (start.line as u32).saturating_sub(prev_line);
                    let delta_start = if delta_line == 0 {
                        (start.column as u32).saturating_sub(prev_char)
                    } else {
                        start.column as u32
                    };
                    
                    // Token type and modifiers (simplified - would use SyntaxKindToSemanticType trait)
                    let token_type = 0u32; // Default to variable
                    let token_modifiers_bitset = 0u32;
                    
                    // Length of token
                    let length = u32::try_from(u64::from(range.len().into())).unwrap_or(0);
                    
                    semantic_tokens.push(lsp_types::SemanticToken {
                        delta_line,
                        delta_start,
                        length,
                        token_type,
                        token_modifiers_bitset,
                    });
                    
                    prev_line = end.line as u32;
                    prev_char = end.column as u32;
                }
            }
            
            SemanticTokens {
                result_id: None,
                data: semantic_tokens,
            }
        } else {
            SemanticTokens {
                result_id: None,
                data: vec![],
            }
        }
    } else {
        SemanticTokens {
            result_id: None,
            data: vec![],
        }
    };
    
    let response = Response::new_ok(req.id, Some(SemanticTokensResult::Tokens(tokens)));
    connection.sender.send(response.into())?;
    
    Ok(())
}

