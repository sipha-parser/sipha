# API Overview

This chapter provides an overview of Sipha's key types and traits.

## Core Traits

### SyntaxKind

Trait for syntax kind identifiers:

```rust,ignore
use std::fmt::Debug;
use std::hash::Hash;

pub trait SyntaxKind: Copy + Clone + PartialEq + Eq + Hash + Debug {
    fn is_terminal(self) -> bool;
    fn is_trivia(self) -> bool;
}
```

### Token

Trait for tokens:

```rust,ignore
use std::hash::Hash;
use sipha::syntax::{SyntaxKind, TextSize};
use compact_str::CompactString;

pub trait Token: Clone + PartialEq + Eq + Hash {
    type Kind: SyntaxKind;
    fn kind(&self) -> Self::Kind;
    fn text_len(&self) -> TextSize;
    fn text(&self) -> CompactString;
}
```

### NonTerminal

Trait for non-terminals:

```rust,ignore
use std::hash::Hash;

pub trait NonTerminal: Clone + PartialEq + Eq + Hash {
    fn name(&self) -> &str;
}
```

### ParserBackend

Trait for parser backends:

```rust,ignore
use sipha::grammar::Grammar;

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

```rust,ignore
use std::marker::PhantomData;

pub struct Grammar<T, N> {
    _phantom: PhantomData<(T, N)>,
}
```

### ParseResult

Parse result:

```rust,ignore
use sipha::syntax::GreenNode;

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

```rust,ignore
use sipha::syntax::SyntaxKind;

pub struct GreenNode<K: SyntaxKind> {
    _phantom: std::marker::PhantomData<K>,
}
```

### SyntaxNode

Red tree node:

```rust,ignore
pub struct SyntaxNode {
    // ...
}
```

## Builders

### GrammarBuilder

Build grammars:

```rust,ignore
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

```rust,ignore
# use sipha::syntax::SyntaxKind;
# use sipha::lexer::{LexerBuilder, Pattern, CompiledLexer};
# type LexerError = ();
# type K = ();
pub struct LexerBuilder<K: SyntaxKind> {
    _phantom: std::marker::PhantomData<K>,
}

impl<K: SyntaxKind> LexerBuilder<K> {
    pub fn new() -> Self { todo!() }
    pub fn token(self, kind: K, pattern: Pattern) -> Self { todo!() }
    pub fn keyword(self, text: &str, kind: K) -> Self { todo!() }
    pub fn trivia(self, kind: K) -> Self { todo!() }
    pub fn build(self, eof_kind: K, ident_kind: K) -> Result<CompiledLexer<K>, LexerError> { todo!() }
}
```

### GreenNodeBuilder

Build green trees:

```rust,ignore
# use sipha::syntax::{GreenNode, SyntaxKind};
# type K = ();
# type BuilderError = ();
# struct GreenNodeBuilder;
pub struct GreenNodeBuilder {
    _phantom: std::marker::PhantomData<()>,
}

impl GreenNodeBuilder {
    pub fn new() -> Self { todo!() }
    pub fn start_node(&mut self, kind: K) { todo!() }
    pub fn token(&mut self, kind: K, text: &str) -> Result<(), BuilderError> { todo!() }
    pub fn finish_node(&mut self) -> Result<(), BuilderError> { todo!() }
    pub fn finish(self) -> Result<GreenNode<K>, BuilderError> { todo!() }
}
```

## Incremental Parsing

### IncrementalParser

Incremental parser wrapper:

```rust,ignore
# use sipha::syntax::{GreenNode, SyntaxKind};
# use sipha::grammar::Grammar;
# use sipha::incremental::TextEdit;
# type P = ();
# type T = ();
# type N = ();
# type ParseResult<T, N> = ();
pub struct IncrementalParser<P, T, N> {
    _phantom: std::marker::PhantomData<(P, T, N)>,
}

impl<P, T, N> IncrementalParser<P, T, N> {
    pub fn new(parser: P) -> Self { todo!() }
    pub fn parse_incremental(
        &mut self,
        input: &[T],
        old_tree: Option<&GreenNode<T::Kind>>,
        edits: &[TextEdit],
        entry: N,
        grammar: Option<&Grammar<T, N>>,
    ) -> ParseResult<T, N> { todo!() }
}
```

### TextEdit

Text edit:

```rust,ignore
use sipha::syntax::{TextRange, TextSize};

pub struct TextEdit {
    pub range: TextRange,
    pub new_text: String,
}

impl TextEdit {
    pub fn replace(range: TextRange, new_text: impl Into<String>) -> Self { todo!() }
    pub fn insert(pos: TextSize, text: impl Into<String>) -> Self { todo!() }
    pub fn delete(range: TextRange) -> Self { todo!() }
}
```

## Error Types

### ParseError

Parsing error:

```rust,ignore
use sipha::syntax::TextRange;

pub struct ParseError<T, N> {
    pub span: TextRange,
    pub message: String,
    pub expected: Vec<T>,
    pub found: Option<T>,
}
```

### LexerError

Lexer error:

```rust,ignore
use sipha::syntax::TextRange;

pub struct LexerError {
    pub span: TextRange,
    pub kind: LexerErrorKind,
}
```

## Next Steps

- See [Feature Flags](feature-flags.md) for available features
- Check [FAQ](faq.md) for common questions

