# Custom Syntax Kinds

This chapter covers advanced usage of syntax kinds.

## Advanced SyntaxKind Implementation

### Additional Methods

The `SyntaxKind` trait provides optional methods:

```rust
impl SyntaxKind for MySyntaxKind {
    fn is_terminal(self) -> bool {
        // ...
    }
    
    fn is_trivia(self) -> bool {
        // ...
    }
    
    // Optional: Mark keywords
    fn is_keyword(self) -> bool {
        matches!(self, 
            MySyntaxKind::If | 
            MySyntaxKind::While | 
            MySyntaxKind::For
        )
    }
    
    // Optional: Mark literals
    fn is_literal(self) -> bool {
        matches!(self, 
            MySyntaxKind::Number | 
            MySyntaxKind::String | 
            MySyntaxKind::Boolean
        )
    }
}
```

## Syntax Kind Organization

### Grouping Kinds

Organize kinds logically:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MySyntaxKind {
    // Keywords
    If, While, For, Return,
    
    // Operators
    Plus, Minus, Multiply, Divide,
    
    // Punctuation
    LParen, RParen, LBrace, RBrace,
    
    // Literals
    Number, String, Boolean,
    
    // Identifiers
    Ident,
    
    // Trivia
    Whitespace, Comment,
    
    // Non-terminals
    Expr, Stmt, Block,
    
    // Special
    Eof, Error,
}
```

### Using Macros

Use macros to reduce boilerplate:

```rust
macro_rules! define_syntax_kinds {
    (
        keywords: [$($keyword:ident),*],
        operators: [$($op:ident),*],
        // ...
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum MySyntaxKind {
            $($keyword,)*
            $($op,)*
            // ...
        }
        
        impl SyntaxKind for MySyntaxKind {
            fn is_keyword(self) -> bool {
                matches!(self, $(MySyntaxKind::$keyword)|*)
            }
            // ...
        }
    };
}
```

## Syntax Kind Conversion

### Converting to/from Strings

```rust
impl MySyntaxKind {
    pub fn as_str(self) -> &'static str {
        match self {
            MySyntaxKind::If => "if",
            MySyntaxKind::While => "while",
            // ...
        }
    }
    
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "if" => Some(MySyntaxKind::If),
            "while" => Some(MySyntaxKind::While),
            // ...
            _ => None,
        }
    }
}
```

## Best Practices

1. **Use descriptive names**: Choose clear, descriptive names
2. **Group logically**: Organize kinds by category
3. **Document purpose**: Document what each kind represents
4. **Use exhaustiveness**: Use exhaustive matching in implementations
5. **Consider extensions**: Design for future extensions

## Next Steps

- See [Syntax Kinds](../core-concepts/syntax-kinds.md) for basics
- Check [Examples](../examples/) for real-world usage

