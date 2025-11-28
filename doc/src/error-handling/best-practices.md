# Best Practices

This chapter provides guidelines and common patterns for error handling in Sipha parsers.

## Always Check for Errors

Always check for errors, even if you expect success:

```rust
let result = parser.parse(&tokens, entry_point);

// Always check errors, even if you expect success
if !result.errors.is_empty() {
    // Handle errors appropriately
    return Err(format!("Parsing failed: {} errors", result.errors.len()));
}
```

**Why:** Errors can occur even in valid-looking input due to:
- Grammar ambiguities
- Edge cases
- Recovery attempts
- Incomplete input

## Provide Context in Error Messages

When reporting errors to users, include:

- **Source location**: Line and column numbers
- **Context**: Surrounding code
- **Suggestions**: What was expected or how to fix it

```rust
use sipha::error::ParseError;

fn report_error(error: &ParseError, source: &str) {
    println!("Error: {}", error);
    println!("Location: {:?}", error.span());
    
    // Show context
    if let Some((before, error_span, after)) = error.get_context(source) {
        println!("Context: ...{}[{}]{}...", before, error_span, after);
    }
    
    // Show suggestions
    if let Some(suggestion) = error.did_you_mean("actual_token") {
        println!("Suggestion: {}", suggestion);
    }
}
```

## Handle Warnings Appropriately

Warnings don't prevent parsing, but should be addressed:

```rust
use sipha::error::Severity;

// Warnings don't prevent parsing, but should be addressed
for warning in &result.warnings {
    match warning.severity {
        Severity::Warning => {
            // Log or report warnings
            eprintln!("Warning: {}", warning.message);
        }
        Severity::Info => {
            // Optionally show info messages
            // Useful for development tools
        }
        Severity::Hint => {
            // Hints can be shown in IDE tooltips
            // Less urgent, more suggestions
        }
    }
}
```

## Use Error Recovery Wisely

Error recovery is great for IDEs, but consider disabling it for:

- **Strict validation**: When you want to fail fast
- **Batch processing**: When errors should stop processing
- **Testing**: When you want to catch all errors

```rust
use sipha::backend::ll::{LlParser, LlConfig};

// For strict validation
let strict_config = LlConfig {
    error_recovery: false,  // Disable recovery
    ..Default::default()
};

// For IDE/language server
let ide_config = LlConfig {
    error_recovery: true,  // Enable recovery
    max_recovery_attempts: 100,  // Allow many attempts
    ..Default::default()
};
```

## Leverage Diagnostics

If building an IDE or language server, enable the `diagnostics` feature for:

- Better error messages
- Source code snippets
- Helpful suggestions
- Integration with editor tooling

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["diagnostics"] }
```

## Common Error Patterns

### Pattern 1: Missing Delimiter

```rust
// Input: "if (condition { ... }"
// Error: UnexpectedEof with expected: [")"]

// Handling:
// 1. Report the error with context
// 2. Show what delimiter is missing
// 3. Suggest where to add it
```

### Pattern 2: Unexpected Token

```rust
// Input: "let x = 42 +"
// Error: UnexpectedToken with expected: [number, identifier, "("]

// Handling:
// 1. Show the unexpected token
// 2. List expected alternatives
// 3. Provide "did you mean" suggestions
```

### Pattern 3: Invalid Syntax

```rust
// Input: "function() { return }"
// Error: InvalidSyntax with message: "Missing return value"

// Handling:
// 1. Explain the syntax violation
// 2. Show the problematic code
// 3. Suggest how to fix it
```

## Error Handling Checklist

When implementing error handling:

- [ ] Check `result.errors` after every parse
- [ ] Check `result.warnings` for non-fatal issues
- [ ] Provide context in error messages
- [ ] Use appropriate severity levels for warnings
- [ ] Enable diagnostics for better error reporting
- [ ] Test error recovery behavior
- [ ] Handle lexer errors separately if needed
- [ ] Consider error metrics for debugging

## Performance Considerations

Error handling has minimal performance impact:

- **Error checking**: Negligible overhead
- **Error recovery**: Small overhead, but enables partial results
- **Diagnostics**: Small overhead for formatting
- **Metrics**: Minimal overhead for tracking

## Testing Error Handling

Test your error handling:

```rust
#[test]
fn test_error_handling() {
    let result = parser.parse(&invalid_tokens, entry_point);
    
    // Verify errors are reported
    assert!(!result.errors.is_empty());
    
    // Verify error details
    let error = &result.errors[0];
    assert!(error.span().start() > TextSize::zero());
    
    // Verify recovery (if enabled)
    if config.error_recovery {
        assert!(result.metrics.errors_recovered > 0);
    }
}
```

## Error Handling in Production

For production code:

1. **Log errors**: Use proper logging framework
2. **Monitor metrics**: Track error rates and recovery success
3. **User-friendly messages**: Translate technical errors to user messages
4. **Graceful degradation**: Continue operation when possible
5. **Error reporting**: Collect error data for improvement

## Next Steps

- Review [Error Types](error-types.md) to understand error variants
- See [Working with Errors](working-with-errors.md) for practical patterns
- Learn about [Error Recovery](recovery.md) for recovery strategies
- Explore [Rich Diagnostics](diagnostics.md) for better error reporting

