# API Overview

This chapter provides an overview of Sipha's key types and traits.

## Core Traits

### SyntaxKind

Trait for syntax kind identifiers:

```rust
pub trait SyntaxKind: Copy + Clone + PartialEq + Eq + Hash + Debug {
    fn is_terminal(self) -> bool;
    fn is_trivia(self) -> bool;
}
```

### Token

Trait for tokens:

```rust
pub trait Token: Clone + PartialEq + Eq + Hash {
    type Kind: SyntaxKind;
    fn kind(&self) -> Self::Kind;
    fn text_len(&self) -> TextSize;
    fn text(&self) -> CompactString;
}
```

### NonTerminal

Trait for non-terminals:

```rust
pub trait NonTerminal: Clone + PartialEq + Eq + Hash {
    fn name(&self) -> &str;
}
```

### ParserBackend

Trait for parser backends:

```rust
pub trait ParserBackend<T, N>: Sized + Send
where
    T: Token,
    N: NonTerminal,
{
    type Config: Default + Clone;
    type Error: std::error::Error + Send + Sync + 'static;
    type State: Send + Sync;
    
    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error>;
    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N>;
    // ... more methods
}
```

## Key Types

### Grammar

Grammar definition:

```rust
pub struct Grammar<T, N> {
    // ...
}
```

### ParseResult

Parse result:

```rust
pub struct ParseResult<T, N> {
    pub root: GreenNode<T::Kind>,
    pub errors: Vec<ParseError<T, N>>,
    pub warnings: Vec<ParseWarning<T, N>>,
    pub metrics: ParseMetrics,
    #[cfg(feature = "backend-glr")]
    pub forest: Option<ParseForest<T::Kind>>,
}
```

### GreenNode

Green tree node:

```rust
pub struct GreenNode<K: SyntaxKind> {
    // ...
}
```

### SyntaxNode

Red tree node:

```rust
pub struct SyntaxNode {
    // ...
}
```

## Builders

### GrammarBuilder

Build grammars:

```rust
pub struct GrammarBuilder<T, N> {
    // ...
}

impl<T, N> GrammarBuilder<T, N> {
    pub fn new() -> Self;
    pub fn entry_point(mut self, entry: N) -> Self;
    pub fn rule(mut self, lhs: N, rhs: Expr<T, N>) -> Self;
    pub fn build(self) -> Result<Grammar<T, N>, GrammarError>;
}
```

### LexerBuilder

Build lexers:

```rust
pub struct LexerBuilder<K: SyntaxKind> {
    // ...
}

impl<K: SyntaxKind> LexerBuilder<K> {
    pub fn new() -> Self;
    pub fn token(mut self, kind: K, pattern: Pattern) -> Self;
    pub fn keyword(mut self, text: &str, kind: K) -> Self;
    pub fn trivia(mut self, kind: K) -> Self;
    pub fn build(self, eof_kind: K, ident_kind: K) -> Result<CompiledLexer<K>, LexerError>;
}
```

### GreenNodeBuilder

Build green trees:

```rust
pub struct GreenNodeBuilder {
    // ...
}

impl GreenNodeBuilder {
    pub fn new() -> Self;
    pub fn start_node(&mut self, kind: K);
    pub fn token(&mut self, kind: K, text: &str) -> Result<(), BuilderError>;
    pub fn finish_node(&mut self) -> Result<(), BuilderError>;
    pub fn finish(self) -> Result<GreenNode<K>, BuilderError>;
}
```

## Incremental Parsing

### IncrementalParser

Incremental parser wrapper:

```rust
pub struct IncrementalParser<P, T, N> {
    // ...
}

impl<P, T, N> IncrementalParser<P, T, N> {
    pub fn new(parser: P) -> Self;
    pub fn parse_incremental(
        &mut self,
        input: &[T],
        old_tree: Option<&GreenNode<T::Kind>>,
        edits: &[TextEdit],
        entry: N,
        grammar: Option<&Grammar<T, N>>,
    ) -> ParseResult<T, N>;
}
```

### TextEdit

Text edit:

```rust
pub struct TextEdit {
    pub range: TextRange,
    pub new_text: String,
}

impl TextEdit {
    pub fn replace(range: TextRange, new_text: impl Into<String>) -> Self;
    pub fn insert(pos: TextSize, text: impl Into<String>) -> Self;
    pub fn delete(range: TextRange) -> Self;
}
```

## Error Types

### ParseError

Parsing error:

```rust
pub struct ParseError<T, N> {
    pub span: TextRange,
    pub message: String,
    pub expected: Vec<T>,
    pub found: Option<T>,
}
```

### LexerError

Lexer error:

```rust
pub struct LexerError {
    pub span: TextRange,
    pub kind: LexerErrorKind,
}
```

## Next Steps

- See [Feature Flags](feature-flags.md) for available features
- Check [FAQ](faq.md) for common questions

