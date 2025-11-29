# Working with Errors

This chapter covers how to check, report, and extract information from errors in your parsers.

## Working with Parse Results

After parsing, check the `ParseResult` for errors and warnings:

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;
use sipha::syntax::SyntaxNode;
use sipha::error::Severity;

let result = parser.parse(&tokens, entry_point);

// Check for errors
if !result.errors.is_empty() {
    eprintln!("Parsing failed with {} errors:", result.errors.len());
    for error in &result.errors {
        eprintln!("  Error: {}", error);
        eprintln!("    Location: {:?}", error.span());
    }
}

// Check for warnings
if !result.warnings.is_empty() {
    eprintln!("Found {} warnings:", result.warnings.len());
    for warning in &result.warnings {
        eprintln!("  [{}] {} at {:?}", 
            match warning.severity {
                Severity::Warning => "WARNING",
                Severity::Info => "INFO",
                Severity::Hint => "HINT",
            },
            warning.message,
            warning.span
        );
    }
}

// Even with errors, a tree may be available
let root = SyntaxNode::new_root(result.root.clone());
```

## Error Information

### Getting Error Details

`ParseError` provides several methods for extracting information:

```rust,ignore
use sipha::error::ParseError;

// Get the error location
let span = error.span();

// Format expected tokens as a readable string
let expected_str = error.format_expected();
// Returns: "identifier or number" or "identifier, number, or string"

// Get "did you mean" suggestions
if let Some(suggestion) = error.did_you_mean("identifer") {
    println!("{}", suggestion); // "Did you mean 'identifier'?"
}

// Get context around the error
if let Some((before, error_span, after)) = error.get_context(source_code) {
    println!("Context: ...{}[{}]{}...", before, error_span, after);
}

// Format error with full context
let formatted = error.format_with_context(source_code, Some("actual_token"));
println!("{}", formatted);
```

### Example: Comprehensive Error Reporting

```rust,ignore
use sipha::error::ParseError;

fn report_parse_errors(errors: &[ParseError], source: &str) {
    for (i, error) in errors.iter().enumerate() {
        println!("\nError {}: {}", i + 1, error);
        println!("  Location: {:?}", error.span());
        
        // Show context
        if let Some((before, error_span, after)) = error.get_context(source) {
            println!("  Context: ...{}[{}]{}...", before, error_span, after);
        }
        
        // Show expected tokens
        match error {
            ParseError::UnexpectedToken { expected, .. } 
            | ParseError::UnexpectedEof { expected, .. } => {
                if !expected.is_empty() {
                    println!("  Expected: {}", error.format_expected());
                }
            }
            ParseError::InvalidSyntax { message, .. } => {
                println!("  Details: {}", message);
            }
            ParseError::Ambiguity { alternatives, .. } => {
                println!("  Alternatives: {}", alternatives.join(", "));
            }
        }
    }
}
```

## Handling Lexer Errors

Lexer errors occur during tokenization and can be handled separately:

```rust,ignore
use sipha::lexer::LexerBuilder;
use sipha::error::{LexerError, LexerErrorKind};

let lexer = LexerBuilder::new()
    // ... configure lexer
    .build(eof_kind, default_kind)?;

match lexer.tokenize(input) {
    Ok(tokens) => {
        // Tokenization succeeded
    }
    Err(errors) => {
        // Handle lexer errors
        for error in errors {
            eprintln!("Lexer error at {:?}: {}", error.span(), error.kind());
            
            match error.kind() {
                LexerErrorKind::UnexpectedChar { char } => {
                    eprintln!("  Unexpected character: '{}'", char);
                }
                LexerErrorKind::UnterminatedString => {
                    eprintln!("  String literal not closed");
                }
                LexerErrorKind::InvalidEscape { escape } => {
                    eprintln!("  Invalid escape sequence: {}", escape);
                }
                LexerErrorKind::InvalidNumber { reason } => {
                    eprintln!("  Invalid number: {}", reason);
                }
                LexerErrorKind::UnexpectedEof => {
                    eprintln!("  Unexpected end of file");
                }
            }
        }
    }
}
```

Lexer errors can also be converted to parse errors:

```rust,ignore
use sipha::error::{LexerError, ParseError};

let lexer_error: LexerError = /* ... */;
let parse_error: ParseError = lexer_error.into();
```

## Error Metrics

`ParseResult` includes metrics about the parsing process:

```rust,ignore
let result = parser.parse(&tokens, entry_point);

println!("Parsing metrics:");
println!("  Tokens consumed: {}", result.metrics.tokens_consumed);
println!("  Nodes created: {}", result.metrics.nodes_created);
println!("  Errors recovered: {}", result.metrics.errors_recovered);
println!("  Cache hits: {}", result.metrics.cache_hits);
println!("  Parse time: {:?}", result.metrics.parse_time);
```

These metrics are useful for:

- **Performance analysis**: Understanding parse performance
- **Debugging**: Identifying problematic grammar rules
- **Optimization**: Finding bottlenecks in parsing

## Error Handling in Incremental Parsing

Incremental parsing maintains error information across edits:

```rust,ignore
use sipha::incremental::IncrementalParser;

let mut incremental = IncrementalParser::new(parser);

// Initial parse
let result1 = incremental.parse_incremental(/* ... */);

// After edit, errors are updated for affected regions
let result2 = incremental.parse_incremental(/* ... */);

// Errors from previous parse are preserved if not affected by edit
```

## Next Steps

- Learn about [Error Recovery](recovery.md) to understand how errors are handled automatically
- See [Rich Diagnostics](diagnostics.md) for beautiful error reporting
- Check [Best Practices](best-practices.md) for guidelines on error handling
