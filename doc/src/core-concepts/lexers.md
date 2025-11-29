# Lexers

Lexers convert raw text into tokens. Sipha provides a flexible lexer builder API that supports pattern matching, trivia handling, and DFA-based tokenization for performance.

## Overview

The lexer is the first stage of parsing. It takes source text and converts it into a stream of tokens that the parser can consume. Sipha's lexer uses a DFA (Deterministic Finite Automaton) for efficient tokenization.

## Building a Lexer

Use `LexerBuilder` to construct a lexer:

```rust,ignore
use sipha::lexer::{LexerBuilder, Pattern, CharSet};

let lexer = LexerBuilder::new()
    .token(MySyntaxKind::Number, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::digits())),
        min: 1,
        max: None,
    })
    .token(MySyntaxKind::Plus, Pattern::Literal("+".into()))
    .token(MySyntaxKind::Whitespace, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
        min: 1,
        max: None,
    })
    .trivia(MySyntaxKind::Whitespace)
    .build(MySyntaxKind::Eof, MySyntaxKind::Number)
    .expect("Failed to build lexer");
```

## Patterns

Sipha supports several pattern types for matching tokens:

### Literal Patterns

Match exact strings:

```rust,ignore
.token(MySyntaxKind::Plus, Pattern::Literal("+".into()))
.token(MySyntaxKind::LParen, Pattern::Literal("(".into()))
```

### Character Class Patterns

Match character ranges:

```rust,ignore
use sipha::lexer::CharSet;

// Match digits [0-9]
.token(MySyntaxKind::Number, Pattern::CharClass(CharSet::digits()))

// Match letters [a-zA-Z]
.token(MySyntaxKind::Ident, Pattern::CharClass(CharSet::new(vec!['a'..='z', 'A'..='Z'])))

// Match whitespace
.token(MySyntaxKind::Whitespace, Pattern::CharClass(CharSet::whitespace()))
```

### Repeat Patterns

Match repeating patterns:

```rust,ignore
// Match one or more digits
.token(MySyntaxKind::Number, Pattern::Repeat {
    pattern: Box::new(Pattern::CharClass(CharSet::digits())),
    min: 1,
    max: None, // No maximum
})

// Match zero or more whitespace
.token(MySyntaxKind::Whitespace, Pattern::Repeat {
    pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
    min: 0,
    max: None,
})

// Match exactly 3 digits
.token(MySyntaxKind::ThreeDigits, Pattern::Repeat {
    pattern: Box::new(Pattern::CharClass(CharSet::digits())),
    min: 3,
    max: Some(3),
})
```

### Regex Patterns

For complex patterns, use regex:

```rust,ignore
// Match floating point numbers
.token(MySyntaxKind::Float, Pattern::Regex(r"\d+\.\d+".into()))

// Match email addresses
.token(MySyntaxKind::Email, Pattern::Regex(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}".into()))
```

### Any Pattern

Match any single character:

```rust,ignore
.token(MySyntaxKind::AnyChar, Pattern::Any)
```

## Keywords

Keywords are reserved words that should be matched as literals rather than identifiers:

```rust,ignore
let lexer = LexerBuilder::new()
    .token(MySyntaxKind::Ident, Pattern::CharClass(CharSet::new(vec!['a'..='z', 'A'..='Z'])))
    .keyword("if", MySyntaxKind::If)
    .keyword("else", MySyntaxKind::Else)
    .keyword("while", MySyntaxKind::While)
    .build(MySyntaxKind::Eof, MySyntaxKind::Ident)
    .expect("Failed to build lexer");
```

Keywords are matched with higher priority than general patterns, so `"if"` will match as a keyword rather than an identifier.

## Trivia

Trivia are tokens that don't affect parsing but are preserved in the syntax tree (e.g., whitespace, comments):

```rust,ignore
let lexer = LexerBuilder::new()
    .token(MySyntaxKind::Whitespace, Pattern::CharClass(CharSet::whitespace()))
    .token(MySyntaxKind::Comment, Pattern::Regex(r"//.*".into()))
    .trivia(MySyntaxKind::Whitespace)
    .trivia(MySyntaxKind::Comment)
    .build(MySyntaxKind::Eof, MySyntaxKind::Number)
    .expect("Failed to build lexer");
```

The parser will automatically skip trivia during parsing, but they'll still be available in the syntax tree for formatting and IDE features.

## Custom Matchers

For complex tokenization logic, use custom matchers:

```rust,ignore
let lexer = LexerBuilder::new()
    .custom_token(MySyntaxKind::String, |text, pos| {
        // Custom string matching logic
        if text[pos..].starts_with('"') {
            // Find closing quote
            let mut end = pos + 1;
            while end < text.len() && text.as_bytes()[end] != b'"' {
                if text.as_bytes()[end] == b'\\' {
                    end += 2; // Skip escape sequence
                } else {
                    end += 1;
                }
            }
            if end < text.len() {
                Some((end - pos + 1, TokenValue::String(text[pos+1..end].to_string())))
            } else {
                None // Unterminated string
            }
        } else {
            None
        }
    })
    .build(MySyntaxKind::Eof, MySyntaxKind::Number)
    .expect("Failed to build lexer");
```

Custom matchers return `Option<(usize, TokenValue)>` where:
- `usize` is the number of characters matched
- `TokenValue` is the token's value (if any)

## Tokenization

Once the lexer is built, tokenize input:

```rust,ignore
let input = "42 + 10";
let tokens = lexer.tokenize(input)
    .expect("Failed to tokenize input");

for token in &tokens {
    println!("{:?}: {}", token.kind, token.text);
}
```

## DFA Construction

Sipha's lexer uses a DFA (Deterministic Finite Automaton) for efficient tokenization. The DFA is built from NFA (Nondeterministic Finite Automaton) patterns, which are then converted to a DFA for O(n) tokenization performance.

The DFA construction process:

1. **Pattern to NFA**: Each pattern is converted to an NFA
2. **NFA to DFA**: The NFA is converted to a DFA using subset construction
3. **Optimization**: The DFA is optimized for size and performance

## Priority and Ambiguity

When multiple patterns could match the same input, Sipha uses priority to resolve ambiguity:

- **Keywords** have the highest priority (lowest number)
- **Patterns** are prioritized by order of definition (earlier = higher priority)
- **Longest match** wins when priorities are equal

## Error Handling

The lexer returns `LexerError` for invalid input:

```rust,ignore
match lexer.tokenize(input) {
    Ok(tokens) => {
        // Process tokens
    }
    Err(error) => {
        match error.kind {
            LexerErrorKind::UnexpectedChar { .. } => {
                // Handle unexpected character
            }
            LexerErrorKind::InvalidNumber { reason } => {
                // Handle invalid number format
            }
            // ... other error kinds
        }
    }
}
```

## Best Practices

1. **Order patterns carefully**: More specific patterns should come before general ones
2. **Use keywords for reserved words**: This ensures proper matching priority
3. **Mark trivia correctly**: Ensure all trivia kinds are marked with `.trivia()`
4. **Use character classes efficiently**: Prefer character classes over regex when possible
5. **Test edge cases**: Test with empty input, very long input, and invalid input

## Example: Complete Lexer

```rust,ignore
use sipha::lexer::{LexerBuilder, Pattern, CharSet};

let lexer = LexerBuilder::new()
    // Numbers
    .token(MySyntaxKind::Number, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::digits())),
        min: 1,
        max: None,
    })
    // Operators
    .token(MySyntaxKind::Plus, Pattern::Literal("+".into()))
    .token(MySyntaxKind::Minus, Pattern::Literal("-".into()))
    .token(MySyntaxKind::Multiply, Pattern::Literal("*".into()))
    .token(MySyntaxKind::Divide, Pattern::Literal("/".into()))
    // Parentheses
    .token(MySyntaxKind::LParen, Pattern::Literal("(".into()))
    .token(MySyntaxKind::RParen, Pattern::Literal(")".into()))
    // Whitespace (trivia)
    .token(MySyntaxKind::Whitespace, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
        min: 1,
        max: None,
    })
    .trivia(MySyntaxKind::Whitespace)
    .build(MySyntaxKind::Eof, MySyntaxKind::Number)
    .expect("Failed to build lexer");
```

## Next Steps

- Learn about [Grammars](grammars.md) to see how tokens are used in parsing
- Explore [Syntax Trees](syntax-trees/green-red-trees.md) to see how tokens appear in trees
- Check out [Custom Patterns](advanced/custom-patterns.md) for advanced lexer usage

