# Rich Diagnostics

When the `diagnostics` feature is enabled, errors integrate with [`miette`](https://docs.rs/miette) for beautiful error reporting with source code snippets, highlighted locations, and helpful suggestions.

## Enabling Diagnostics

Add the `diagnostics` feature to your `Cargo.toml`:

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["diagnostics"] }
```

This enables:
- Rich error formatting with source code snippets
- Highlighted error locations
- Suggestions and hints
- Integration with editor tooling

## Using Diagnostics

Errors automatically implement the `Diagnostic` trait from miette:

```rust,ignore
#[cfg(feature = "diagnostics")]
use sipha::error::ParseError;

// Errors automatically implement Diagnostic trait
let error: ParseError = /* ... */;

// Print with rich formatting
eprintln!("{:#}", error);

// Errors include:
// - Source code snippets
// - Highlighted error locations
// - Suggestions and hints
// - Related code locations
```

## Example Output

With diagnostics enabled, errors are displayed with rich formatting:

```text
Error: Unexpected token
  ┌─ example.txt:10:15
  │
10 │     let x = 42 +
  │               ^ Unexpected token
  │
  = Expected: identifier, number, or operator
  = Help: Did you mean to add an expression after '+'?
```

## Diagnostic Features

### Source Code Snippets

Diagnostics automatically include source code around the error:

```rust,ignore
let error: ParseError = /* ... */;
eprintln!("{:#}", error);
// Shows:
//   ┌─ file.rs:42:10
//   │
//42 │     let x = invalid
//   │            ^^^^^^^^ Error here
```

### Highlighted Locations

Error locations are highlighted in the source code:

```rust,ignore
// The error span is automatically highlighted
// Multiple related errors can be shown together
```

### Suggestions

Diagnostics can include suggestions:

```rust,ignore
// "Did you mean" suggestions
// Fix suggestions
// Helpful hints
```

### Related Locations

Multiple related errors can be shown together:

```rust,ignore
// Errors at different locations
// Related errors in the same context
// Error chains
```

## Integration with Editors

Diagnostics integrate with editor tooling:

- **Language Servers**: Errors are reported via LSP
- **IDE Integration**: Errors shown in editor
- **Terminal Output**: Beautiful terminal output
- **JSON Output**: Structured error data

## Custom Diagnostic Messages

You can customize diagnostic messages:

```rust,ignore
use sipha::error::ParseError;

// Create errors with custom messages
let error = ParseError::invalid_syntax_with_suggestion(
    span,
    "Invalid expression",
    "Try adding parentheses around the expression"
);
```

## Diagnostic Severity

Diagnostics support different severity levels:

- **Error**: Fatal errors that prevent parsing
- **Warning**: Non-fatal issues (via `ParseWarning`)
- **Info**: Informational messages
- **Hint**: Suggestions for improvement

## Example: Full Diagnostic Output

```rust,ignore
#[cfg(feature = "diagnostics")]
use sipha::error::ParseError;
use miette::Diagnostic;

fn report_with_diagnostics(error: &ParseError, source: &str) {
    // Print with full diagnostic formatting
    eprintln!("{:#}", error);
    
    // Or use miette's reporting features
    // miette::set_hook(/* ... */);
}
```

## Performance Considerations

Diagnostics have minimal performance impact:

- **Compile-time**: Feature is gated, no overhead when disabled
- **Runtime**: Small overhead for formatting
- **Memory**: Source code snippets are included in errors

## When to Use Diagnostics

Use diagnostics for:

- **IDEs and Language Servers**: Better user experience
- **Development Tools**: Clearer error messages
- **Documentation**: Better error examples
- **Debugging**: Easier to understand errors

## Next Steps

- See [Working with Errors](working-with-errors.md) for basic error handling
- Learn about [Error Recovery](recovery.md) for recovery strategies
- Check [Best Practices](best-practices.md) for guidelines

