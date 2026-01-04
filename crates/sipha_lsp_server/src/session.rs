//! LSP session management with incremental parsing

use sipha::grammar::{NonTerminal, Token};
use sipha::incremental::{IncrementalSession, IncrementalCache};
use sipha::syntax::SyntaxKind;
use lsp_types::InitializeParams;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;

/// Per-document state
pub struct DocumentState<'a, K: SyntaxKind> {
    /// Current parse tree
    pub tree: Option<Arc<sipha::syntax::GreenNode<K>>>,
    /// Current text content
    pub text: String,
    /// Incremental session for this document
    pub session: Option<IncrementalSession<'a, K>>,
}

/// LSP session managing multiple documents
pub struct LspSession<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Per-document state
    documents: HashMap<lsp_types::Uri, DocumentState<'static, T::Kind>>,
    /// Incremental cache (shared across documents)
    cache: IncrementalCache<T::Kind>,
    /// Initialization parameters
    init_params: Option<InitializeParams>,
    /// Phantom data for unused type parameter N
    _phantom: PhantomData<N>,
}

impl<T, N> LspSession<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new LSP session
    pub fn new(cache_size: usize) -> Self {
        Self {
            documents: HashMap::new(),
            cache: IncrementalCache::new(cache_size),
            init_params: None,
            _phantom: PhantomData,
        }
    }

    /// Initialize the session
    pub fn initialize(&mut self, params: InitializeParams) {
        self.init_params = Some(params);
    }

    /// Get or create document state
    pub fn get_or_create_document(&mut self, uri: &lsp_types::Uri) -> &mut DocumentState<'static, T::Kind> {
        self.documents.entry(uri.clone()).or_insert_with(|| {
            DocumentState {
                tree: None,
                text: String::new(),
                session: None,
            }
        })
    }

    /// Get document state
    pub fn get_document(&self, uri: &lsp_types::Uri) -> Option<&DocumentState<'static, T::Kind>> {
        self.documents.get(uri)
    }

    /// Get document state mutably
    pub fn get_document_mut(&mut self, uri: &lsp_types::Uri) -> Option<&mut DocumentState<'static, T::Kind>> {
        self.documents.get_mut(uri)
    }

    /// Remove a document
    pub fn remove_document(&mut self, uri: &lsp_types::Uri) {
        self.documents.remove(uri);
    }

    /// Get the incremental cache
    pub fn cache(&mut self) -> &mut IncrementalCache<T::Kind> {
        &mut self.cache
    }
}

