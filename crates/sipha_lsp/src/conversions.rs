//! Conversion traits for Sipha types to LSP types

use lsp_types::{Diagnostic, DiagnosticSeverity, DocumentSymbol, Position, Range, SymbolKind};
use sipha::error::ParseError;
use sipha::syntax::{SyntaxKind, SyntaxNode, TextRange, TextSize, line_col::LineIndex};

/// Convert a `ParseError` to an LSP `Diagnostic`
pub trait ToDiagnostic {
    /// Convert to LSP diagnostic
    fn to_diagnostic(&self) -> Diagnostic;
}

impl ToDiagnostic for ParseError {
    fn to_diagnostic(&self) -> Diagnostic {
        let (message, severity) = match self {
            Self::UnexpectedToken { expected, .. } => {
                let msg = format!("Unexpected token. Expected: {}", expected.join(", "));
                (msg, DiagnosticSeverity::ERROR)
            }
            Self::UnexpectedEof { expected, .. } => {
                let msg = format!("Unexpected end of file. Expected: {}", expected.join(", "));
                (msg, DiagnosticSeverity::ERROR)
            }
            Self::InvalidSyntax { message, .. } => (message.clone(), DiagnosticSeverity::ERROR),
            Self::Ambiguity { alternatives, .. } => {
                let msg = format!("Ambiguous parse. Alternatives: {}", alternatives.join(", "));
                (msg, DiagnosticSeverity::WARNING)
            }
            Self::DepthExceeded { max_depth } => {
                let msg = format!("Maximum parse depth exceeded: {max_depth}");
                (msg, DiagnosticSeverity::ERROR)
            }
            Self::TokenLimitExceeded { max_tokens } => {
                let msg = format!("Token limit exceeded: {max_tokens}");
                (msg, DiagnosticSeverity::ERROR)
            }
            Self::LexerError { message, .. } => (message.clone(), DiagnosticSeverity::ERROR),
        };

        let range = self.span().map_or_else(
            || Range::new(Position::new(0, 0), Position::new(0, 0)),
            |s| s.to_range(),
        );

        Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some("sipha".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        }
    }
}

/// Convert a `TextRange` to an LSP `Range`
///
/// **Note**: This trait provides a simplified conversion that doesn't require source text.
/// For accurate line/column positions, use [`ToRangeWithSource`] instead.
pub trait ToRange {
    /// Convert to LSP range
    ///
    /// This is a simplified conversion that uses byte offsets as character positions.
    /// For accurate line/column calculation, use [`ToRangeWithSource::to_range_with_source`].
    fn to_range(&self) -> Range;
}

impl ToRange for TextRange {
    fn to_range(&self) -> Range {
        Range {
            start: text_size_to_position_simple(self.start()),
            end: text_size_to_position_simple(self.end()),
        }
    }
}

/// Convert a `TextRange` to an LSP `Range` with source text for accurate line/column calculation
pub trait ToRangeWithSource {
    /// Convert to LSP range using source text for accurate line/column positions
    fn to_range_with_source(&self, source_text: &str) -> Range;
}

impl ToRangeWithSource for TextRange {
    fn to_range_with_source(&self, source_text: &str) -> Range {
        let index = LineIndex::new(source_text);
        Range {
            start: text_size_to_position_with_index(&index, self.start()),
            end: text_size_to_position_with_index(&index, self.end()),
        }
    }
}

/// Convert a `SyntaxNode` to an LSP `DocumentSymbol`
pub trait ToDocumentSymbol<K: SyntaxKind> {
    /// Convert to LSP document symbol
    fn to_document_symbol(&self) -> DocumentSymbol;
}

impl<K: SyntaxKind> ToDocumentSymbol<K> for SyntaxNode<K> {
    fn to_document_symbol(&self) -> DocumentSymbol {
        let kind = node_kind_to_symbol_kind(self.kind());
        let range = self.text_range().to_range();
        let selection_range = range;

        let children: Vec<DocumentSymbol> = self
            .children()
            .filter_map(|child| {
                if let sipha::syntax::SyntaxElement::Node(child_node) = child {
                    Some(child_node.to_document_symbol())
                } else {
                    None
                }
            })
            .collect();

        DocumentSymbol {
            name: format!("{:?}", self.kind()), // Use Debug format for name
            detail: Some(self.text()),
            kind,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
            range,
            selection_range,
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        }
    }
}

/// Helper function to convert `TextSize` to LSP Position (simplified version)
///
/// This is a simplified conversion that doesn't require source text.
/// For accurate line/column positions, use `text_size_to_position_with_index` instead.
fn text_size_to_position_simple(size: TextSize) -> Position {
    let offset = u32::try_from(u64::from(size.into())).unwrap_or(u32::MAX);
    Position {
        line: 0,           // Simplified: always line 0
        character: offset, // Simplified: use offset as character
    }
}

/// Helper function to convert `TextSize` to LSP Position using a `LineIndex`
fn text_size_to_position_with_index(index: &LineIndex, size: TextSize) -> Position {
    let line_col = index.line_col(size);
    Position {
        line: line_col.line,
        character: line_col.column,
    }
}

/// Trait for mapping syntax kinds to LSP symbol kinds
///
/// Implement this trait for your syntax kind type to provide custom mapping
/// from syntax kinds to LSP symbol kinds. If not implemented, a default
/// mapping will be used.
pub trait SyntaxKindToSymbolKind<K: SyntaxKind> {
    /// Convert a syntax kind to an LSP symbol kind
    fn to_symbol_kind(kind: K) -> SymbolKind;
}

/// Default implementation that maps all syntax kinds to VARIABLE
impl<K: SyntaxKind> SyntaxKindToSymbolKind<K> for K {
    fn to_symbol_kind(_kind: K) -> SymbolKind {
        SymbolKind::VARIABLE
    }
}

/// Convert syntax kind to LSP symbol kind
///
/// This function uses the `SyntaxKindToSymbolKind` trait if implemented,
/// otherwise falls back to a default mapping.
fn node_kind_to_symbol_kind<K>(kind: K) -> SymbolKind
where
    K: SyntaxKind + SyntaxKindToSymbolKind<K>,
{
    K::to_symbol_kind(kind)
}

/// Helper function for common symbol kind mappings
///
/// This can be used in custom `SyntaxKindToSymbolKind` implementations
/// to map common patterns (e.g., function names, class names, etc.)
#[must_use]
pub fn map_common_symbol_kinds(name: &str) -> SymbolKind {
    let lower = name.to_lowercase();
    if lower.contains("function") || lower.contains("fn") || lower.contains("func") {
        SymbolKind::FUNCTION
    } else if lower.contains("class") || lower.contains("struct") || lower.contains("type") {
        SymbolKind::CLASS
    } else if lower.contains("interface") || lower.contains("trait") {
        SymbolKind::INTERFACE
    } else if lower.contains("enum") {
        SymbolKind::ENUM
    } else if lower.contains("module") || lower.contains("mod") || lower.contains("namespace") {
        SymbolKind::MODULE
    } else if lower.contains("variable") || lower.contains("var") || lower.contains("let") {
        SymbolKind::VARIABLE
    } else if lower.contains("constant") || lower.contains("const") {
        SymbolKind::CONSTANT
    } else if lower.contains("property") || lower.contains("field") {
        SymbolKind::PROPERTY
    } else if lower.contains("method") {
        SymbolKind::METHOD
    } else {
        SymbolKind::VARIABLE // Default fallback
    }
}

// ==================== Semantic Tokens Support ====================

/// Trait for mapping syntax kinds to semantic token types
pub trait SyntaxKindToSemanticType<K: SyntaxKind> {
    /// Convert a syntax kind to semantic token type and modifiers
    fn to_semantic_type(kind: K) -> Option<SemanticTokenInfo>;
}

/// Information about a semantic token
#[derive(Debug, Clone)]
pub struct SemanticTokenInfo {
    /// The token type index (into the legend)
    pub token_type: u32,
    /// Bitmask of token modifiers
    pub token_modifiers: u32,
}

/// Standard semantic token types for common language constructs
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StandardSemanticType {
    Namespace,
    Type,
    Class,
    Enum,
    Interface,
    Struct,
    TypeParameter,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Event,
    Function,
    Method,
    Macro,
    Keyword,
    Modifier,
    Comment,
    String,
    Number,
    Regexp,
    Operator,
}

impl StandardSemanticType {
    /// Get the standard token type name
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Namespace => "namespace",
            Self::Type => "type",
            Self::Class => "class",
            Self::Enum => "enum",
            Self::Interface => "interface",
            Self::Struct => "struct",
            Self::TypeParameter => "typeParameter",
            Self::Parameter => "parameter",
            Self::Variable => "variable",
            Self::Property => "property",
            Self::EnumMember => "enumMember",
            Self::Event => "event",
            Self::Function => "function",
            Self::Method => "method",
            Self::Macro => "macro",
            Self::Keyword => "keyword",
            Self::Modifier => "modifier",
            Self::Comment => "comment",
            Self::String => "string",
            Self::Number => "number",
            Self::Regexp => "regexp",
            Self::Operator => "operator",
        }
    }

    /// Get standard token types for semantic token legend
    #[must_use]
    pub fn standard_types() -> Vec<&'static str> {
        vec![
            "namespace",
            "type",
            "class",
            "enum",
            "interface",
            "struct",
            "typeParameter",
            "parameter",
            "variable",
            "property",
            "enumMember",
            "event",
            "function",
            "method",
            "macro",
            "keyword",
            "modifier",
            "comment",
            "string",
            "number",
            "regexp",
            "operator",
        ]
    }
}

/// Standard semantic token modifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StandardSemanticModifier {
    Declaration,
    Definition,
    Readonly,
    Static,
    Deprecated,
    Abstract,
    Async,
    Modification,
    Documentation,
    DefaultLibrary,
}

impl StandardSemanticModifier {
    /// Get the modifier name
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Declaration => "declaration",
            Self::Definition => "definition",
            Self::Readonly => "readonly",
            Self::Static => "static",
            Self::Deprecated => "deprecated",
            Self::Abstract => "abstract",
            Self::Async => "async",
            Self::Modification => "modification",
            Self::Documentation => "documentation",
            Self::DefaultLibrary => "defaultLibrary",
        }
    }

    /// Get standard modifiers for semantic token legend
    #[must_use]
    pub fn standard_modifiers() -> Vec<&'static str> {
        vec![
            "declaration",
            "definition",
            "readonly",
            "static",
            "deprecated",
            "abstract",
            "async",
            "modification",
            "documentation",
            "defaultLibrary",
        ]
    }

    /// Get the bitmask value for this modifier
    #[must_use]
    pub const fn bitmask(&self) -> u32 {
        1 << (*self as u32)
    }
}

// ==================== Completion Helpers ====================

/// Trait for generating completion items from grammar
pub trait ToCompletionItem {
    /// Convert to LSP completion item
    fn to_completion_item(&self) -> lsp_types::CompletionItem;
}

/// Helper to create a completion item for a keyword
#[must_use]
pub fn keyword_completion(keyword: &str) -> lsp_types::CompletionItem {
    lsp_types::CompletionItem {
        label: keyword.into(),
        kind: Some(lsp_types::CompletionItemKind::KEYWORD),
        detail: Some("keyword".into()),
        ..Default::default()
    }
}

/// Helper to create a completion item for a snippet
#[must_use]
pub fn snippet_completion(
    label: &str,
    snippet: &str,
    detail: Option<&str>,
) -> lsp_types::CompletionItem {
    lsp_types::CompletionItem {
        label: label.into(),
        kind: Some(lsp_types::CompletionItemKind::SNIPPET),
        insert_text: Some(snippet.into()),
        insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
        detail: detail.map(Into::into),
        ..Default::default()
    }
}

/// Helper to create a completion item for a function
#[must_use]
pub fn function_completion(name: &str, signature: Option<&str>) -> lsp_types::CompletionItem {
    lsp_types::CompletionItem {
        label: name.into(),
        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
        detail: signature.map(Into::into),
        ..Default::default()
    }
}

// ==================== Hover Helpers ====================

/// Helper to create hover content
#[must_use]
pub fn create_hover(content: &str, range: Option<Range>) -> lsp_types::Hover {
    lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: content.into(),
        }),
        range,
    }
}

/// Helper to create code block for hover
#[must_use]
pub fn code_block(language: &str, code: &str) -> String {
    format!("```{language}\n{code}\n```")
}
