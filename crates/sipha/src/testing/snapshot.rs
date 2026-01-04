//! # Snapshot Testing Utilities
//!
//! This module provides utilities for snapshot testing of parse results.
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::testing::SnapshotTester;
//!
//! let tester = SnapshotTester::new("snapshots");
//! tester.assert_snapshot("basic_expr", &parse_result.root);
//! ```

use crate::syntax::{GreenNode, SyntaxKind, SyntaxNode};
use std::fmt::Write;
use std::path::PathBuf;
use std::sync::Arc;

/// Snapshot tester for parse trees
pub struct SnapshotTester {
    snapshot_dir: PathBuf,
    update_mode: bool,
}

impl SnapshotTester {
    /// Create a new snapshot tester
    #[must_use]
    pub fn new(snapshot_dir: impl Into<PathBuf>) -> Self {
        let update_mode = std::env::var("UPDATE_SNAPSHOTS").is_ok()
            || std::env::var("SIPHA_UPDATE_SNAPSHOTS").is_ok();
        Self {
            snapshot_dir: snapshot_dir.into(),
            update_mode,
        }
    }

    /// Set update mode (for updating existing snapshots)
    #[must_use]
    pub const fn with_update_mode(mut self, update: bool) -> Self {
        self.update_mode = update;
        self
    }

    /// Assert that a green node matches the snapshot
    ///
    /// # Panics
    /// Panics if the snapshot doesn't match (and update mode is disabled)
    pub fn assert_snapshot<K: SyntaxKind>(&self, name: &str, node: &Arc<GreenNode<K>>) {
        let actual = format_green_node(node, 0);
        self.check_snapshot(name, &actual);
    }

    /// Assert that a syntax node matches the snapshot
    ///
    /// # Panics
    /// Panics if the snapshot doesn't match (and update mode is disabled)
    pub fn assert_syntax_snapshot<K: SyntaxKind>(&self, name: &str, node: &SyntaxNode<K>) {
        let actual = format_syntax_node(node, 0);
        self.check_snapshot(name, &actual);
    }

    /// Assert that an error list matches the snapshot
    ///
    /// # Panics
    /// Panics if the snapshot doesn't match (and update mode is disabled)
    pub fn assert_errors_snapshot(&self, name: &str, errors: &[crate::error::ParseError]) {
        let mut actual = String::new();
        for error in errors {
            writeln!(actual, "{error}").unwrap();
        }
        self.check_snapshot(name, &actual);
    }

    fn check_snapshot(&self, name: &str, actual: &str) {
        let path = self.snapshot_dir.join(format!("{name}.snap"));

        if self.update_mode {
            std::fs::create_dir_all(&self.snapshot_dir).ok();
            std::fs::write(&path, actual).expect("Failed to write snapshot");
            return;
        }

        if path.exists() {
            let expected = std::fs::read_to_string(&path).expect("Failed to read snapshot");
            assert!(
                actual == expected,
                "Snapshot mismatch for '{name}':\n\
                --- Expected ---\n{expected}\n\
                --- Actual ---\n{actual}\n\
                \n\
                To update snapshots, run with UPDATE_SNAPSHOTS=1"
            );
        } else {
            panic!(
                "Snapshot '{name}' not found at {}.\n\
                To create it, run with UPDATE_SNAPSHOTS=1",
                path.display()
            );
        }
    }
}

/// Format a green node as a tree string
fn format_green_node<K: SyntaxKind>(node: &Arc<GreenNode<K>>, indent: usize) -> String {
    let mut result = String::new();
    let indent_str = "  ".repeat(indent);

    writeln!(
        result,
        "{}{:?}@{:?}",
        indent_str,
        node.kind(),
        node.text_len()
    )
    .unwrap();

    for child in node.children() {
        match child {
            crate::syntax::GreenElement::Node(n) => {
                result.push_str(&format_green_node(&n.clone(), indent + 1));
            }
            crate::syntax::GreenElement::Token(t) => {
                writeln!(result, "{}  {:?} {:?}", indent_str, t.kind(), t.text()).unwrap();
            }
        }
    }

    result
}

/// Format a syntax node as a tree string
fn format_syntax_node<K: SyntaxKind>(node: &SyntaxNode<K>, indent: usize) -> String {
    let mut result = String::new();
    let indent_str = "  ".repeat(indent);

    writeln!(
        result,
        "{}{:?}@{:?}",
        indent_str,
        node.kind(),
        node.text_range()
    )
    .unwrap();

    for child in node.children() {
        match child {
            crate::syntax::SyntaxElement::Node(n) => {
                result.push_str(&format_syntax_node(&n, indent + 1));
            }
            crate::syntax::SyntaxElement::Token(t) => {
                writeln!(
                    result,
                    "{}  {:?}@{:?} {:?}",
                    indent_str,
                    t.kind(),
                    t.text_range(),
                    t.text()
                )
                .unwrap();
            }
        }
    }

    result
}

/// Parse result assertion helpers
pub trait ParseResultAssertions {
    /// Assert that parsing succeeded with no errors
    fn assert_ok(&self);
    /// Assert that parsing produced exactly n errors
    fn assert_error_count(&self, count: usize);
    /// Assert that an error message contains the given substring
    fn assert_error_contains(&self, substring: &str);
}

impl<T: crate::grammar::Token, N: crate::grammar::NonTerminal> ParseResultAssertions
    for crate::error::ParseResult<T, N>
{
    fn assert_ok(&self) {
        assert!(
            self.errors.is_empty(),
            "Expected no errors, got: {:?}",
            self.errors
        );
    }

    fn assert_error_count(&self, count: usize) {
        assert_eq!(
            self.errors.len(),
            count,
            "Expected {} errors, got {}: {:?}",
            count,
            self.errors.len(),
            self.errors
        );
    }

    fn assert_error_contains(&self, substring: &str) {
        let has_match = self
            .errors
            .iter()
            .any(|e| e.to_string().contains(substring));
        assert!(
            has_match,
            "No error containing '{}' found in: {:?}",
            substring, self.errors
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snapshot_tester_creation() {
        let tester = SnapshotTester::new("test_snapshots");
        assert!(!tester.update_mode);
    }
}
