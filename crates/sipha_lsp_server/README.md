# Sipha LSP Server Framework

High-level framework for building Language Server Protocol (LSP) servers with Sipha parsers.

## Features

- **Built-in Incremental Parsing**: Automatic integration with Sipha's incremental parsing
- **Text Document Sync**: Handles `didOpen`, `didChange`, and `didClose` notifications
- **Common LSP Features**: Hover, completion, document symbols, semantic tokens
- **Easy-to-Use API**: Simple server builder pattern

## Usage

```rust
use sipha_lsp_server::{LspServer, ServerConfig};
use sipha::grammar::{Grammar, GrammarBuilder, Expr, NonTerminal, Token};

// Define your grammar
let grammar = GrammarBuilder::new()
    .entry_point(MyNonTerminal::Expr)
    .rule(MyNonTerminal::Expr, Expr::Empty)
    .build()?;

// Configure the server
let config = ServerConfig {
    incremental_parsing: true,
    cache_size: 10000,
    diagnostics: true,
    document_symbols: true,
    semantic_tokens: false,
};

// Create and run the server
let mut server = LspServer::new(grammar, config);
server.run()?;
```

## Architecture

The framework consists of:

- **LspServer**: Main server that handles LSP protocol
- **LspSession**: Manages per-document state and incremental parsing
- **Handlers**: Request/notification handlers for LSP features

## Integration with Parsers

The framework is designed to work with any Sipha parser backend:

- LL parser
- LR parser
- GLR parser
- PEG parser

Simply provide your grammar and the framework handles the rest.

## Status

This is a foundational framework. Full integration with parsing backends is in progress.

