# Error Types

Sipha defines several error types to represent different kinds of problems that can occur during parsing and lexing.

## ParseError

`ParseError` represents errors that occur during parsing. It's an enum with four variants:

```rust,ignore
use sipha::error::ParseError;
use sipha::syntax::{TextRange, TextSize};

// Unexpected token error
let error = ParseError::UnexpectedToken {
    span: TextRange::new(TextSize::from(10), TextSize::from(15)),
    expected: vec!["identifier".to_string(), "number".to_string()],
};

// Invalid syntax error
let error = ParseError::InvalidSyntax {
    span: TextRange::new(TextSize::from(0), TextSize::from(5)),
    message: "Invalid expression syntax".to_string(),
};

// Unexpected end of file
let error = ParseError::UnexpectedEof {
    span: TextRange::new(TextSize::from(100), TextSize::from(100)),
    expected: vec!["}".to_string(), ";".to_string()],
};

// Ambiguity error (GLR parser)
let error = ParseError::Ambiguity {
    span: TextRange::new(TextSize::from(20), TextSize::from(25)),
    alternatives: vec!["expression1".to_string(), "expression2".to_string()],
};
```

### ParseError Variants

#### UnexpectedToken

Occurs when the parser encounters a token that doesn't match any expected production.

- **Most common parse error**
- Contains a list of expected token types
- Parser can often recover by skipping the unexpected token

**Example:**
```rust,ignore
// Input: "let x = 42 +"
// Error: UnexpectedToken with expected: ["number", "identifier", "("]
```

#### InvalidSyntax

Occurs when the input violates grammar rules in a way that can't be attributed to a single unexpected token.

- Contains a descriptive error message
- Used for complex syntax violations
- May require more context to understand

**Example:**
```rust,ignore
// Input: "function() { return }"
// Error: InvalidSyntax with message: "Missing return value"
```

#### UnexpectedEof

Occurs when the parser reaches the end of input but expects more tokens.

- Contains a list of expected tokens
- Common when delimiters are missing (e.g., unclosed parentheses)
- Indicates incomplete input

**Example:**
```rust,ignore
// Input: "if (condition { ... }"
// Error: UnexpectedEof with expected: [")"]
```

#### Ambiguity (GLR backend only)

Occurs when the GLR parser finds multiple valid parse trees.

- Contains a list of alternative interpretations
- Requires disambiguation to resolve
- Only occurs with GLR parser backend

**Example:**
```rust,ignore
// Input: "1 + 2 * 3" (ambiguous without precedence)
// Error: Ambiguity with alternatives: ["(1 + 2) * 3", "1 + (2 * 3)"]
```

## LexerError

`LexerError` represents errors that occur during tokenization (lexing):

```rust,ignore
use sipha::error::{LexerError, LexerErrorKind};
use sipha::syntax::{TextRange, TextSize};

let error = LexerError::new(
    TextRange::new(TextSize::from(5), TextSize::from(6)),
    LexerErrorKind::UnexpectedChar { char: '!' },
);
```

### LexerErrorKind Variants

#### UnexpectedChar

An invalid character was encountered that doesn't match any token pattern.

- Contains the unexpected character
- Usually indicates a typo or unsupported character

**Example:**
```rust,ignore
// Input: "let x = 42#"
// Error: UnexpectedChar { char: '#' }
```

#### UnterminatedString

A string literal was started but never closed.

- Common when quotes are mismatched
- Indicates incomplete string literal

**Example:**
```rust,ignore
// Input: "let s = \"hello world"
// Error: UnterminatedString
```

#### InvalidEscape

An invalid escape sequence was found in a string.

- Contains the invalid escape sequence
- Common with malformed escape sequences

**Example:**
```rust,ignore
// Input: "let s = \"hello\\zworld\""
// Error: InvalidEscape { escape: "\\z" }
```

#### InvalidNumber

A number literal has invalid format.

- Contains a reason explaining the issue
- Common with malformed numeric literals

**Example:**
```rust,ignore
// Input: "let x = 123abc"
// Error: InvalidNumber { reason: "Invalid digit 'a' in number" }
```

#### UnexpectedEof

End of file was reached unexpectedly during tokenization.

- Usually indicates incomplete input
- Common when input is cut off mid-token

**Example:**
```rust,ignore
// Input: "let x = 4" (cut off)
// Error: UnexpectedEof
```

## ParseWarning

`ParseWarning` represents non-fatal issues that don't prevent parsing:

```rust,ignore
use sipha::error::{ParseWarning, Severity};
use sipha::syntax::{TextRange, TextSize};

// Create warnings with different severity levels
let warning = ParseWarning::warning(
    TextRange::new(TextSize::from(0), TextSize::from(10)),
    "Deprecated syntax used".to_string(),
);

let info = ParseWarning::info(
    TextRange::new(TextSize::from(10), TextSize::from(20)),
    "Consider using newer syntax".to_string(),
);

let hint = ParseWarning::hint(
    TextRange::new(TextSize::from(20), TextSize::from(30)),
    "This can be simplified".to_string(),
);
```

### Severity Levels

#### Warning

Important issues that should be addressed.

- Indicates potential problems
- Should be shown to users
- May indicate deprecated or problematic code

#### Info

Informational messages.

- Provides context or suggestions
- Less urgent than warnings
- Can be shown optionally

#### Hint

Suggestions for improvement.

- Optimization opportunities
- Style suggestions
- Can be shown in IDE tooltips

## Converting Between Error Types

Lexer errors can be converted to parse errors:

```rust,ignore
use sipha::error::{LexerError, ParseError};

let lexer_error: LexerError = /* ... */;
let parse_error: ParseError = lexer_error.into();
```

This is useful when you want to handle all errors uniformly.

## Error Methods

All error types provide useful methods:

### ParseError Methods

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

### LexerError Methods

```rust,ignore
use sipha::error::LexerError;

// Get the error location
let span = error.span();

// Get the error kind
let kind = error.kind();
```

## Next Steps

- Learn about [Working with Errors](working-with-errors.md) to see how to use these error types
- See [Error Recovery](recovery.md) to understand how errors are handled
- Check [Best Practices](best-practices.md) for guidelines on error handling

