//! # Pretty Printing Framework
//!
//! This module provides a framework for pretty printing syntax trees.
//!
//! ## Features
//!
//! - Configurable indentation and line width
//! - Smart line breaking based on available width
//! - Custom formatters per syntax kind
//! - Whitespace normalization

use crate::syntax::{GreenElement, GreenNode, SyntaxKind, SyntaxNode};
use std::sync::Arc;

/// Configuration for pretty printing
#[derive(Debug, Clone)]
pub struct PrettyConfig {
    /// Maximum line width before breaking
    pub max_width: usize,
    /// Indentation string (e.g., "  " or "\t")
    pub indent: String,
    /// Whether to insert trailing commas
    pub trailing_comma: bool,
    /// Whether to normalize whitespace
    pub normalize_whitespace: bool,
    /// Line ending style
    pub line_ending: LineEnding,
}

impl Default for PrettyConfig {
    fn default() -> Self {
        Self {
            max_width: 80,
            indent: "  ".into(),
            trailing_comma: false,
            normalize_whitespace: true,
            line_ending: LineEnding::Lf,
        }
    }
}

/// Line ending style
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineEnding {
    /// Unix-style line endings (LF)
    Lf,
    /// Windows-style line endings (CRLF)
    Crlf,
    /// Classic Mac-style line endings (CR)
    Cr,
}

impl LineEnding {
    /// Get the line ending as a string
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Lf => "\n",
            Self::Crlf => "\r\n",
            Self::Cr => "\r",
        }
    }
}

/// A document element for the pretty printing algorithm
#[derive(Debug, Clone)]
pub enum Doc {
    /// Plain text
    Text(String),
    /// A line break (soft or hard)
    Line { hard: bool },
    /// Indentation increase
    Indent(Box<Doc>),
    /// Concatenation of documents
    Concat(Vec<Doc>),
    /// Group - may be printed in a single line or broken
    Group(Box<Doc>),
    /// If-break - different content when broken vs not broken
    IfBreak { broken: Box<Doc>, flat: Box<Doc> },
}

impl Doc {
    /// Create a text document
    #[must_use]
    pub fn text(s: impl Into<String>) -> Self {
        Self::Text(s.into())
    }

    /// Create a soft line break (space if fits, newline if not)
    #[must_use]
    pub const fn soft_line() -> Self {
        Self::Line { hard: false }
    }

    /// Create a hard line break (always newline)
    #[must_use]
    pub const fn hard_line() -> Self {
        Self::Line { hard: true }
    }

    /// Create an indented document
    #[must_use]
    pub fn indent(doc: Doc) -> Self {
        Self::Indent(Box::new(doc))
    }

    /// Create a concatenation of documents
    #[must_use]
    pub fn concat(docs: Vec<Doc>) -> Self {
        Self::Concat(docs)
    }

    /// Create a group that may be broken
    #[must_use]
    pub fn group(doc: Doc) -> Self {
        Self::Group(Box::new(doc))
    }

    /// Create an if-break document
    #[must_use]
    pub fn if_break(broken: Doc, flat: Doc) -> Self {
        Self::IfBreak {
            broken: Box::new(broken),
            flat: Box::new(flat),
        }
    }

    /// Render the document to a string
    #[must_use]
    pub fn render(&self, config: &PrettyConfig) -> String {
        let mut output = String::new();
        let mut state = RenderState {
            indent_level: 0,
            column: 0,
            broken: false,
        };
        self.render_to(&mut output, config, &mut state);
        output
    }

    fn render_to(&self, out: &mut String, config: &PrettyConfig, state: &mut RenderState) {
        match self {
            Self::Text(text) => {
                out.push_str(text);
                state.column += text.len();
            }
            Self::Line { hard } => {
                if *hard || state.broken || state.column > config.max_width {
                    out.push_str(config.line_ending.as_str());
                    for _ in 0..state.indent_level {
                        out.push_str(&config.indent);
                    }
                    state.column = state.indent_level * config.indent.len();
                } else {
                    out.push(' ');
                    state.column += 1;
                }
            }
            Self::Indent(inner) => {
                state.indent_level += 1;
                inner.render_to(out, config, state);
                state.indent_level -= 1;
            }
            Self::Concat(docs) => {
                for doc in docs {
                    doc.render_to(out, config, state);
                }
            }
            Self::Group(inner) => {
                // Try to fit in one line first
                let flat_len = inner.flat_length();
                let fits = state.column + flat_len <= config.max_width;

                let old_broken = state.broken;
                state.broken = !fits;
                inner.render_to(out, config, state);
                state.broken = old_broken;
            }
            Self::IfBreak { broken, flat } => {
                if state.broken {
                    broken.render_to(out, config, state);
                } else {
                    flat.render_to(out, config, state);
                }
            }
        }
    }

    /// Calculate the flat length of this document
    fn flat_length(&self) -> usize {
        match self {
            Self::Text(text) => text.len(),
            Self::Line { hard: false } => 1, // Space in flat mode
            Self::Line { hard: true } => usize::MAX, // Hard breaks prevent flattening
            Self::Indent(inner) => inner.flat_length(),
            Self::Concat(docs) => docs.iter().map(Doc::flat_length).sum(),
            Self::Group(inner) => inner.flat_length(),
            Self::IfBreak { flat, .. } => flat.flat_length(),
        }
    }
}

/// State for rendering
struct RenderState {
    indent_level: usize,
    column: usize,
    broken: bool,
}

/// Pretty printer for syntax trees
pub struct PrettyPrinter<K: SyntaxKind> {
    config: PrettyConfig,
    formatters: Vec<Box<dyn Fn(&SyntaxNode<K>) -> Option<Doc>>>,
}

impl<K: SyntaxKind> Default for PrettyPrinter<K> {
    fn default() -> Self {
        Self::new(PrettyConfig::default())
    }
}

impl<K: SyntaxKind> PrettyPrinter<K> {
    /// Create a new pretty printer with default configuration
    #[must_use]
    pub fn new(config: PrettyConfig) -> Self {
        Self {
            config,
            formatters: Vec::new(),
        }
    }

    /// Add a custom formatter for specific syntax kinds
    pub fn add_formatter<F>(&mut self, formatter: F)
    where
        F: Fn(&SyntaxNode<K>) -> Option<Doc> + 'static,
    {
        self.formatters.push(Box::new(formatter));
    }

    /// Pretty print a syntax node
    #[must_use]
    pub fn print(&self, node: &SyntaxNode<K>) -> String {
        let doc = self.node_to_doc(node);
        doc.render(&self.config)
    }

    /// Convert a syntax node to a document
    fn node_to_doc(&self, node: &SyntaxNode<K>) -> Doc {
        // Check custom formatters first
        for formatter in &self.formatters {
            if let Some(doc) = formatter(node) {
                return doc;
            }
        }

        // Default: concatenate children with appropriate spacing
        let children: Vec<_> = node.children().collect();
        if children.is_empty() {
            return Doc::text(node.text());
        }

        let mut docs = Vec::new();
        for (i, child) in children.iter().enumerate() {
            let child_doc = match child {
                crate::syntax::SyntaxElement::Node(n) => self.node_to_doc(n),
                crate::syntax::SyntaxElement::Token(t) => Doc::text(t.text()),
            };
            docs.push(child_doc);

            // Add spacing between children (simplified)
            if i < children.len() - 1 {
                docs.push(Doc::soft_line());
            }
        }

        Doc::group(Doc::concat(docs))
    }

    /// Pretty print a green node
    #[must_use]
    pub fn print_green(&self, node: &Arc<GreenNode<K>>) -> String {
        let doc = self.green_to_doc(node);
        doc.render(&self.config)
    }

    /// Convert a green node to a document
    fn green_to_doc(&self, node: &Arc<GreenNode<K>>) -> Doc {
        let children = node.children();
        if children.is_empty() {
            // Leaf node or token
            return Doc::text(children.first().map_or(String::new(), |c| match c {
                GreenElement::Token(t) => t.text().to_string(),
                GreenElement::Node(n) => self.green_to_doc(&n.clone()).render(&self.config),
            }));
        }

        let mut docs = Vec::new();
        for (i, child) in children.iter().enumerate() {
            let child_doc = match child {
                GreenElement::Node(n) => self.green_to_doc(&n.clone()),
                GreenElement::Token(t) => Doc::text(t.text()),
            };
            docs.push(child_doc);

            if i < children.len() - 1 {
                docs.push(Doc::soft_line());
            }
        }

        Doc::group(Doc::concat(docs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_doc_rendering() {
        let config = PrettyConfig::default();

        let doc = Doc::concat(vec![
            Doc::text("hello"),
            Doc::soft_line(),
            Doc::text("world"),
        ]);

        let result = doc.render(&config);
        assert!(result.contains("hello"));
        assert!(result.contains("world"));
    }

    #[test]
    fn test_hard_line() {
        let config = PrettyConfig::default();

        let doc = Doc::concat(vec![
            Doc::text("hello"),
            Doc::hard_line(),
            Doc::text("world"),
        ]);

        let result = doc.render(&config);
        assert!(result.contains('\n'));
    }
}
