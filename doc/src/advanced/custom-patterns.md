# Custom Patterns

This chapter shows how to extend lexer patterns with custom matching logic.

## Custom Matchers

For complex tokenization logic, use custom matchers:

```rust
let lexer = LexerBuilder::new()
    .custom_token(MySyntaxKind::String, |text, pos| {
        // Custom string matching
        if text[pos..].starts_with('"') {
            let mut end = pos + 1;
            while end < text.len() && text.as_bytes()[end] != b'"' {
                if text.as_bytes()[end] == b'\\' {
                    end += 2; // Skip escape sequence
                } else {
                    end += 1;
                }
            }
            if end < text.len() {
                let value = text[pos+1..end].to_string();
                Some((end - pos + 1, TokenValue::String(value)))
            } else {
                None // Unterminated string
            }
        } else {
            None
        }
    })
    .build(MySyntaxKind::Eof, MySyntaxKind::Ident)
    .expect("Failed to build lexer");
```

## Custom Matcher Signature

Custom matchers have this signature:

```rust
Fn(&str, usize) -> Option<(usize, TokenValue)>
```

Where:
- **Input**: `&str` is the text, `usize` is the starting position
- **Output**: `Option<(usize, TokenValue)>` where:
  - `usize` is the number of characters matched
  - `TokenValue` is the token's value (if any)

## TokenValue

Token values can be:

```rust
pub enum TokenValue {
    Integer(i64),
    Float(f64),
    String(String),
    None,
}
```

## Examples

### String Literal

```rust
.custom_token(MySyntaxKind::String, |text, pos| {
    if !text[pos..].starts_with('"') {
        return None;
    }
    
    let mut end = pos + 1;
    while end < text.len() {
        match text.as_bytes()[end] {
            b'"' => {
                let value = text[pos+1..end].to_string();
                return Some((end - pos + 1, TokenValue::String(value)));
            }
            b'\\' if end + 1 < text.len() => {
                end += 2; // Skip escape sequence
            }
            _ => end += 1,
        }
    }
    
    None // Unterminated string
})
```

### Floating Point Number

```rust
.custom_token(MySyntaxKind::Float, |text, pos| {
    let mut end = pos;
    let mut has_dot = false;
    
    // Optional sign
    if end < text.len() && (text.as_bytes()[end] == b'+' || text.as_bytes()[end] == b'-') {
        end += 1;
    }
    
    // Digits before dot
    while end < text.len() && text.as_bytes()[end].is_ascii_digit() {
        end += 1;
    }
    
    // Dot
    if end < text.len() && text.as_bytes()[end] == b'.' {
        has_dot = true;
        end += 1;
    }
    
    // Digits after dot
    while end < text.len() && text.as_bytes()[end].is_ascii_digit() {
        end += 1;
    }
    
    if has_dot && end > pos {
        let value = text[pos..end].parse::<f64>().ok()?;
        Some((end - pos, TokenValue::Float(value)))
    } else {
        None
    }
})
```

### Multiline Comment

```rust
.custom_token(MySyntaxKind::Comment, |text, pos| {
    if !text[pos..].starts_with("/*") {
        return None;
    }
    
    let mut end = pos + 2;
    while end + 1 < text.len() {
        if text.as_bytes()[end] == b'*' && text.as_bytes()[end + 1] == b'/' {
            let value = text[pos+2..end].to_string();
            return Some((end - pos + 2, TokenValue::String(value)));
        }
        end += 1;
    }
    
    None // Unterminated comment
})
```

## Best Practices

1. **Return early**: Return `None` quickly if pattern doesn't match
2. **Handle edge cases**: Handle EOF, invalid sequences, etc.
3. **Escape sequences**: Properly handle escape sequences
4. **Performance**: Keep matching logic efficient
5. **Error handling**: Return `None` for invalid input

## Combining with Regular Patterns

Custom matchers work alongside regular patterns:

```rust
let lexer = LexerBuilder::new()
    // Regular patterns
    .token(MySyntaxKind::Number, Pattern::CharClass(CharSet::digits()))
    .token(MySyntaxKind::Plus, Pattern::Literal("+".into()))
    
    // Custom matcher
    .custom_token(MySyntaxKind::String, |text, pos| {
        // Custom logic
    })
    
    .build(MySyntaxKind::Eof, MySyntaxKind::Ident)
    .expect("Failed to build lexer");
```

## Priority

Custom matchers have the same priority as regular patterns based on definition order.

## Next Steps

- See [Lexers](../core-concepts/lexers.md) for basic pattern usage
- Check [Examples](../examples/) for real-world patterns

