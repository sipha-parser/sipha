//! Helper functions for error handling and validation
//!
//! This module provides utilities for working with `ParseResult` instances
//! in tests and other contexts where error handling and validation is needed.

use sipha::error::{ParseError, ParseResult, ParseWarning};
use sipha::grammar::Token;
use sipha::syntax::TextRange;

/// Check if parsing was successful (no errors)
///
/// # Example
/// ```
/// # use sipha::error::ParseResult;
/// # use sipha::tests::error_helpers::is_successful;
/// # // Assuming you have a ParseResult instance
/// # // assert!(is_successful(&result));
/// ```
#[must_use]
pub const fn is_successful<T, N>(result: &ParseResult<T, N>) -> bool
where
    T: Token,
{
    result.errors.is_empty()
}

/// Get the number of errors in the parse result
#[must_use]
pub const fn error_count<T, N>(result: &ParseResult<T, N>) -> usize
where
    T: Token,
{
    result.errors.len()
}

/// Get the number of warnings in the parse result
#[must_use]
pub const fn warning_count<T, N>(result: &ParseResult<T, N>) -> usize
where
    T: Token,
{
    result.warnings.len()
}

/// Check if the parse result has any warnings
#[must_use]
pub const fn has_warnings<T, N>(result: &ParseResult<T, N>) -> bool
where
    T: Token,
{
    !result.warnings.is_empty()
}

/// Get all error messages as strings
///
/// # Example
/// ```
/// # use sipha::error::ParseResult;
/// # use sipha::tests::error_helpers::error_messages;
/// # // let messages = error_messages(&result);
/// # // assert_eq!(messages.len(), 1);
/// ```
#[must_use]
pub fn error_messages<T, N>(result: &ParseResult<T, N>) -> Vec<String>
where
    T: Token,
{
    result.errors.iter().map(|e| format!("{e}")).collect()
}

/// Get all warning messages as strings
#[must_use]
pub fn warning_messages<T, N>(result: &ParseResult<T, N>) -> Vec<String>
where
    T: Token,
{
    result.warnings.iter().map(|w| w.message.clone()).collect()
}

/// Get an iterator over error messages (more efficient than collecting to Vec)
pub fn error_messages_iter<T, N>(result: &ParseResult<T, N>) -> impl Iterator<Item = String> + '_
where
    T: Token,
{
    result.errors.iter().map(|e| format!("{e}"))
}

/// Find errors of a specific variant
#[must_use]
pub fn find_errors<T, N, F>(result: &ParseResult<T, N>, predicate: F) -> Vec<&ParseError>
where
    T: Token,
    F: Fn(&ParseError) -> bool,
{
    result.errors.iter().filter(|e| predicate(e)).collect()
}

/// Find errors that overlap with a given span
#[must_use]
pub fn errors_at_span<T, N>(result: &ParseResult<T, N>, span: TextRange) -> Vec<&ParseError>
where
    T: Token,
{
    result
        .errors
        .iter()
        .filter(|e| e.span().intersect(span).is_some())
        .collect()
}

/// Find warnings that overlap with a given span
#[must_use]
pub fn warnings_at_span<T, N>(result: &ParseResult<T, N>, span: TextRange) -> Vec<&ParseWarning>
where
    T: Token,
{
    result
        .warnings
        .iter()
        .filter(|w| w.span.intersect(span).is_some())
        .collect()
}

/// Check if there are any errors of a specific variant
pub fn has_error_type<T, N, F>(result: &ParseResult<T, N>, predicate: F) -> bool
where
    T: Token,
    F: Fn(&ParseError) -> bool,
{
    result.errors.iter().any(predicate)
}

/// Format parse result for display
///
/// # Example
/// ```
/// # use sipha::error::ParseResult;
/// # use sipha::tests::error_helpers::format_result;
/// # // println!("{}", format_result(&result));
/// ```
#[must_use]
pub fn format_result<T, N>(result: &ParseResult<T, N>) -> String
where
    T: Token,
{
    if is_successful(result) {
        let mut output = format!(
            "Parse successful: {} nodes created, {} tokens consumed in {:?}",
            result.metrics.nodes_created, result.metrics.tokens_consumed, result.metrics.parse_time
        );

        if has_warnings(result) {
            use std::fmt::Write;
            write!(
                &mut output,
                "\nWarnings: {}",
                warning_messages(result).join(", ")
            )
            .unwrap();
        }

        output
    } else {
        format!(
            "Parse failed with {} errors:\n{}",
            result.errors.len(),
            error_messages(result).join("\n")
        )
    }
}

/// Assert that parsing was successful, panicking with a formatted message if not
///
/// # Panics
/// Panics if the parse result contains any errors, displaying all error messages.
pub fn assert_parse_success<T, N>(result: &ParseResult<T, N>)
where
    T: Token,
{
    assert!(
        is_successful(result),
        "Expected parse to succeed, but got {} errors:\n{}",
        result.errors.len(),
        error_messages(result).join("\n")
    );
}

/// Assert that parsing failed with a specific number of errors
///
/// # Panics
/// Panics if the error count doesn't match the expected count.
pub fn assert_error_count<T, N>(result: &ParseResult<T, N>, expected: usize)
where
    T: Token,
{
    let actual = error_count(result);
    assert!(
        actual == expected,
        "Expected {} errors, but got {}:\n{}",
        expected,
        actual,
        error_messages(result).join("\n")
    );
}

/// Assert that the parse result contains at least one error matching a predicate
///
/// # Panics
/// Panics if no errors match the predicate.
pub fn assert_has_error<T, N, F>(result: &ParseResult<T, N>, predicate: F)
where
    T: Token,
    F: Fn(&ParseError) -> bool,
{
    assert!(
        has_error_type(result, &predicate),
        "Expected to find a matching error, but got {} errors:\n{}",
        result.errors.len(),
        error_messages(result).join("\n")
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use sipha::error::{ParseError, ParseMetrics, ParseResult, ParseWarning, Severity};
    use sipha::grammar::Token;
    use sipha::syntax::{GreenNodeBuilder, SyntaxKind, TextRange, TextSize};

    // Test token type for error helper tests
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestSyntaxKind {
        Root,
    }

    impl SyntaxKind for TestSyntaxKind {
        fn is_terminal(self) -> bool {
            true // Root is the only kind, treated as terminal for this test
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    struct TestToken {
        kind: TestSyntaxKind,
    }

    impl Token for TestToken {
        type Kind = TestSyntaxKind;

        fn kind(&self) -> Self::Kind {
            self.kind
        }
    }

    // Helper to create a minimal ParseResult for testing
    fn create_test_result(
        errors: Vec<ParseError>,
        warnings: Vec<ParseWarning>,
    ) -> ParseResult<TestToken, ()> {
        // Create a dummy root node using the builder
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(TestSyntaxKind::Root);
        let root = builder.finish().unwrap();

        ParseResult::new(root, errors, warnings, ParseMetrics::default())
    }

    #[test]
    fn test_is_successful() {
        let result = create_test_result(vec![], vec![]);
        assert!(is_successful(&result));

        let error = ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        };
        let result_with_error = create_test_result(vec![error], vec![]);
        assert!(!is_successful(&result_with_error));
    }

    #[test]
    fn test_error_count() {
        let result = create_test_result(vec![], vec![]);
        assert_eq!(error_count(&result), 0);

        let errors = vec![
            ParseError::UnexpectedToken {
                span: TextRange::new(TextSize::from(0), TextSize::from(5)),
                expected: vec!["number".to_string()],
            },
            ParseError::InvalidSyntax {
                span: TextRange::new(TextSize::from(10), TextSize::from(15)),
                message: "Test error".to_string(),
            },
        ];
        let result_with_errors = create_test_result(errors, vec![]);
        assert_eq!(error_count(&result_with_errors), 2);
    }

    #[test]
    fn test_warning_count() {
        let result = create_test_result(vec![], vec![]);
        assert_eq!(warning_count(&result), 0);

        let warnings = vec![
            ParseWarning {
                span: TextRange::new(TextSize::from(0), TextSize::from(5)),
                message: "Warning 1".to_string(),
                severity: Severity::Warning,
            },
            ParseWarning {
                span: TextRange::new(TextSize::from(10), TextSize::from(15)),
                message: "Warning 2".to_string(),
                severity: Severity::Info,
            },
        ];
        let result_with_warnings = create_test_result(vec![], warnings);
        assert_eq!(warning_count(&result_with_warnings), 2);
    }

    #[test]
    fn test_has_warnings() {
        let result = create_test_result(vec![], vec![]);
        assert!(!has_warnings(&result));

        let warnings = vec![ParseWarning {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            message: "Test warning".to_string(),
            severity: Severity::Warning,
        }];
        let result_with_warnings = create_test_result(vec![], warnings);
        assert!(has_warnings(&result_with_warnings));
    }

    #[test]
    fn test_error_messages() {
        let errors = vec![
            ParseError::UnexpectedToken {
                span: TextRange::new(TextSize::from(0), TextSize::from(5)),
                expected: vec!["number".to_string()],
            },
            ParseError::InvalidSyntax {
                span: TextRange::new(TextSize::from(10), TextSize::from(15)),
                message: "Invalid syntax".to_string(),
            },
        ];
        let result = create_test_result(errors, vec![]);
        let messages = error_messages(&result);

        assert_eq!(messages.len(), 2);
        assert!(messages.iter().any(|m| m.contains("Unexpected")));
        assert!(messages.iter().any(|m| m.contains("Invalid")));
    }

    #[test]
    fn test_warning_messages() {
        let warnings = vec![
            ParseWarning {
                span: TextRange::new(TextSize::from(0), TextSize::from(5)),
                message: "Warning 1".to_string(),
                severity: Severity::Warning,
            },
            ParseWarning {
                span: TextRange::new(TextSize::from(10), TextSize::from(15)),
                message: "Warning 2".to_string(),
                severity: Severity::Info,
            },
        ];
        let result = create_test_result(vec![], warnings);
        let messages = warning_messages(&result);

        assert_eq!(messages.len(), 2);
        assert_eq!(messages[0], "Warning 1");
        assert_eq!(messages[1], "Warning 2");
    }

    #[test]
    fn test_error_messages_iter() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);
        let count: usize = error_messages_iter(&result).count();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_find_errors() {
        let errors = vec![
            ParseError::UnexpectedToken {
                span: TextRange::new(TextSize::from(0), TextSize::from(5)),
                expected: vec!["number".to_string()],
            },
            ParseError::InvalidSyntax {
                span: TextRange::new(TextSize::from(10), TextSize::from(15)),
                message: "Invalid".to_string(),
            },
            ParseError::UnexpectedToken {
                span: TextRange::new(TextSize::from(20), TextSize::from(25)),
                expected: vec!["identifier".to_string()],
            },
        ];
        let result = create_test_result(errors, vec![]);

        let unexpected_tokens =
            find_errors(&result, |e| matches!(e, ParseError::UnexpectedToken { .. }));
        assert_eq!(unexpected_tokens.len(), 2);

        let invalid_syntax =
            find_errors(&result, |e| matches!(e, ParseError::InvalidSyntax { .. }));
        assert_eq!(invalid_syntax.len(), 1);
    }

    #[test]
    fn test_has_error_type() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);

        assert!(has_error_type(&result, |e| matches!(
            e,
            ParseError::UnexpectedToken { .. }
        )));
        assert!(!has_error_type(&result, |e| matches!(
            e,
            ParseError::InvalidSyntax { .. }
        )));
    }

    #[test]
    fn test_format_result_success() {
        let mut result = create_test_result(vec![], vec![]);
        result.metrics.nodes_created = 10;
        result.metrics.tokens_consumed = 5;

        let formatted = format_result(&result);
        assert!(formatted.contains("Parse successful"));
        assert!(formatted.contains("10"));
        assert!(formatted.contains('5'));
    }

    #[test]
    fn test_format_result_with_warnings() {
        let warnings = vec![ParseWarning {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            message: "Test warning".to_string(),
            severity: Severity::Warning,
        }];
        let result = create_test_result(vec![], warnings);

        let formatted = format_result(&result);
        assert!(formatted.contains("Warnings"));
        assert!(formatted.contains("Test warning"));
    }

    #[test]
    fn test_format_result_failure() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);

        let formatted = format_result(&result);
        assert!(formatted.contains("Parse failed"));
        assert!(formatted.contains("1 errors"));
    }

    #[test]
    fn test_assert_parse_success() {
        let result = create_test_result(vec![], vec![]);
        assert_parse_success(&result); // Should not panic
    }

    #[test]
    #[should_panic(expected = "Expected parse to succeed")]
    fn test_assert_parse_success_panics() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);
        assert_parse_success(&result); // Should panic
    }

    #[test]
    fn test_assert_error_count() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);
        assert_error_count(&result, 1); // Should not panic
    }

    #[test]
    #[should_panic(expected = "Expected 2 errors")]
    fn test_assert_error_count_panics() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);
        assert_error_count(&result, 2); // Should panic
    }

    #[test]
    fn test_assert_has_error() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);
        assert_has_error(&result, |e| matches!(e, ParseError::UnexpectedToken { .. })); // Should not panic
    }

    #[test]
    #[should_panic(expected = "Expected to find a matching error")]
    fn test_assert_has_error_panics() {
        let errors = vec![ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string()],
        }];
        let result = create_test_result(errors, vec![]);
        assert_has_error(&result, |e| matches!(e, ParseError::InvalidSyntax { .. })); // Should panic
    }

    #[test]
    fn test_parse_error_formatting() {
        let error = ParseError::UnexpectedToken {
            span: TextRange::new(TextSize::from(0), TextSize::from(5)),
            expected: vec!["number".to_string(), "identifier".to_string()],
        };

        let error_str = format!("{error}");
        assert!(!error_str.is_empty());
        assert!(error_str.contains("Unexpected"));
    }
}
