# Unicode Support

Sipha provides full Unicode support for identifiers and text when the `unicode` feature is enabled.

## Enabling Unicode

Enable Unicode support in `Cargo.toml`:

```toml
[dependencies]
sipha = { version = "0.5.0", features = ["unicode"] }
```

## Unicode Character Classes

Use Unicode character classes in patterns:

```rust,ignore
use sipha::lexer::{CharSet, Pattern};

// Unicode letters
let unicode_letters = CharSet::unicode_letters();

// Unicode digits
let unicode_digits = CharSet::unicode_digits();

// Unicode identifiers (letters, digits, and connectors)
let unicode_ident = CharSet::unicode_ident_start();
```

## Unicode Identifiers

Match Unicode identifiers:

```rust,ignore
let lexer = LexerBuilder::new()
    .token(MySyntaxKind::Ident, Pattern::CharClass(CharSet::unicode_ident_start()))
    .build(MySyntaxKind::Eof, MySyntaxKind::Ident)
    .expect("Failed to build lexer");
```

## Unicode Normalization

Sipha handles Unicode normalization automatically:

- **NFC**: Normalized Form Canonical Composition
- **NFD**: Normalized Form Canonical Decomposition

## Examples

### Japanese Identifiers

```rust,ignore
let lexer = LexerBuilder::new()
    .token(MySyntaxKind::Ident, Pattern::CharClass(CharSet::unicode_letters()))
    .build(MySyntaxKind::Eof, MySyntaxKind::Ident)
    .expect("Failed to build lexer");

// Can tokenize: 変数名, 関数名, etc.
```

### Emoji in Strings

```rust,ignore
// Emoji are handled correctly in string literals
let input = r#""Hello 👋 World 🌍""#;
let tokens = lexer.tokenize(input).unwrap();
```

## Performance

Unicode support has minimal performance impact:

- **Character classes**: Efficient Unicode property lookups
- **Normalization**: Cached normalization results
- **Identifiers**: Fast Unicode property checks

## Best Practices

1. **Enable when needed**: Only enable `unicode` feature if needed
2. **Use appropriate classes**: Use Unicode character classes for identifiers
3. **Handle normalization**: Be aware of Unicode normalization
4. **Test with Unicode**: Test with various Unicode inputs

## Next Steps

- See [Lexers](../core-concepts/lexers.md) for pattern usage
- Check [Examples](../examples/) for Unicode examples

