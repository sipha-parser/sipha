//! Capture system for backreferences
//!
//! This module provides types for capturing matched content during parsing
//! and referencing it later via backreferences. This enables patterns like
//! matching paired delimiters or repeating previously matched content.
//!
//! # Example
//!
//! ```rust,no_run
//! use sipha::grammar::capture::{CaptureId, Capture};
//!
//! // Use numeric capture IDs
//! let capture_0 = CaptureId::Numeric(0);
//!
//! // Or use named captures for better readability
//! let capture_name = CaptureId::Named("delimiter".to_string());
//!
//! // Captures store the matched content
//! // Note: In actual usage, you would provide concrete token types
//! // let capture: Capture<MyToken> = Capture::new(
//! //     capture_0,
//! //     vec![/* tokens */],
//! //     "matched content".to_string(),
//! // );
//! ```

use crate::grammar::Token;

/// Identifier for a capture group, used in backreferences.
///
/// Captures can be identified either by numeric index (0-based) or by name.
/// Named captures are more readable and maintainable, while numeric captures
/// are more compact.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CaptureId {
    /// Numeric capture ID (0-based index)
    ///
    /// The first capture group is 0, the second is 1, etc.
    Numeric(usize),
    /// Named capture ID
    ///
    /// Uses a string identifier for better readability and maintainability.
    Named(String),
}

impl CaptureId {
    /// Get a string representation of the capture ID.
    #[must_use]
    pub fn as_str(&self) -> String {
        match self {
            Self::Numeric(n) => format!("{n}"),
            Self::Named(s) => s.clone(),
        }
    }
}

/// A captured match from parsing.
///
/// Captures store the matched content (tokens and text) for later use
/// in backreferences. The capture system is primarily used by PEG parsers
/// and other backends that support backtracking and capture groups.
#[derive(Debug, Clone)]
pub struct Capture<T: Token> {
    /// The capture ID that identifies this capture
    pub id: CaptureId,
    /// The tokens that were matched
    pub content: Vec<T>,
    /// The text representation of the matched content
    pub text: String,
}

impl<T: Token> Capture<T> {
    /// Create a new capture.
    #[must_use]
    pub const fn new(id: CaptureId, content: Vec<T>, text: String) -> Self {
        Self { id, content, text }
    }

    /// Get the length of the captured content in tokens.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.content.len()
    }

    /// Check if the capture is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.content.is_empty()
    }
}
