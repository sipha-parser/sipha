# Error Recovery

Sipha parsers attempt to recover from errors and continue parsing. This is essential for providing good user experience in IDEs and language servers.

## Overview

Error recovery allows parsers to:

- **Continue parsing** despite errors
- **Provide partial results** even when input has errors
- **Maintain parse state** for interactive applications
- **Report multiple errors** in a single pass

## Automatic Recovery

By default, parsers use automatic error recovery:

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};

// Default config includes error recovery
let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)?;

// Parser will attempt to recover from errors
let result = parser.parse(&tokens, entry_point);

// Check how many errors were recovered from
println!("Errors recovered: {}", result.metrics.errors_recovered);
```

Even when errors occur, the parser will:
1. Report the error
2. Attempt to recover
3. Continue parsing
4. Return a parse tree (possibly incomplete)

## Recovery Strategies

Parsers use several strategies to recover:

### 1. Token Skipping

Skip unexpected tokens and continue parsing.

**Example:**
```rust,ignore
// Input: "let x = 42 + + 10"
// Parser skips the second "+" and continues
// Error reported, but parsing continues
```

### 2. Token Insertion

Insert missing tokens (e.g., semicolons, closing delimiters).

**Example:**
```rust,ignore
// Input: "let x = 42 let y = 10"
// Parser may insert a semicolon after "42"
// Error reported, but both statements parsed
```

### 3. Rule Skipping

Skip to next statement or block when current rule fails.

**Example:**
```rust,ignore
// Input: "let x = invalid; let y = 10;"
// Parser skips the first statement and continues with the second
// Error reported for first statement, second statement parsed successfully
```

### 4. Best Effort

Continue parsing despite errors, producing the best possible result.

**Example:**
```rust,ignore
// Input with multiple errors
// Parser attempts to parse as much as possible
// All errors reported, partial tree returned
```

The specific strategy depends on the parser backend and configuration.

## Configuring Recovery

Different backends support different recovery options:

```rust,ignore
use sipha::backend::ll::{LlParser, LlConfig};

let config = LlConfig {
    error_recovery: true,  // Enable error recovery
    max_recovery_attempts: 10,  // Limit recovery attempts
    ..Default::default()
};

let mut parser = LlParser::new(&grammar, config)?;
```

### Recovery Options

**error_recovery** (bool)
- Enable or disable error recovery
- Default: `true`
- When disabled, parser stops at first error

**max_recovery_attempts** (usize)
- Maximum number of recovery attempts per parse
- Default: varies by backend
- Prevents infinite recovery loops

## When to Use Recovery

### Use Recovery For:

- **IDEs and Language Servers**: Need to parse incomplete or erroneous code
- **Interactive Tools**: Users are actively editing code
- **Error Reporting**: Want to find all errors in one pass
- **Partial Results**: Need parse tree even with errors

### Disable Recovery For:

- **Strict Validation**: Want to fail fast on errors
- **Batch Processing**: Errors should stop processing
- **Testing**: Want to catch all errors immediately
- **Performance**: Recovery has overhead

## Recovery Examples

### Example 1: Missing Delimiter

```rust,ignore
// Input: "if (condition { ... }"
// Parser behavior:
// 1. Reports UnexpectedEof error expecting ")"
// 2. May insert ")" and continue
// 3. Parses the block successfully
// Result: Error reported, but parse tree includes the block
```

### Example 2: Unexpected Token

```rust,ignore
// Input: "let x = 42 +"
// Parser behavior:
// 1. Reports UnexpectedToken error
// 2. Skips the "+" or inserts placeholder
// 3. Continues parsing
// Result: Error reported, variable declaration partially parsed
```

### Example 3: Invalid Syntax

```rust,ignore
// Input: "function() { return }"
// Parser behavior:
// 1. Reports InvalidSyntax error
// 2. Marks statement as incomplete
// 3. Continues parsing rest of function
// Result: Error reported, function body partially parsed
```

## Recovery Quality

Recovery quality depends on:

- **Grammar structure**: Well-structured grammars recover better
- **Error location**: Errors at statement boundaries recover better
- **Parser backend**: Different backends have different recovery strategies
- **Configuration**: Recovery settings affect behavior

## Limitations

Error recovery has some limitations:

- **May produce incorrect trees**: Recovered trees may not match intended structure
- **Can mask errors**: Multiple errors may be reduced to fewer reported errors
- **Performance overhead**: Recovery adds processing time
- **Not always possible**: Some errors can't be recovered from

## Best Practices

1. **Always check errors**: Even with recovery, check `result.errors`
2. **Validate recovered trees**: Don't trust recovered parse trees completely
3. **Limit recovery attempts**: Use `max_recovery_attempts` to prevent loops
4. **Consider context**: Recovery works better with more context
5. **Test recovery**: Test your grammar with various error scenarios

## Next Steps

- See [Working with Errors](working-with-errors.md) for how to handle recovered errors
- Learn about [Rich Diagnostics](diagnostics.md) for better error reporting
- Check [Best Practices](best-practices.md) for guidelines on error recovery

