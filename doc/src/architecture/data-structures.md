# Data Structures

This chapter describes the key data structures in Sipha.

## Green Trees

### GreenNode

Immutable, shareable tree node:

```rust,ignore
use sipha::syntax::{SyntaxKind, TextSize};

pub struct GreenNode<K: SyntaxKind> {
    kind: K,
    text_len: TextSize,
    children: GreenChildren<K>,
}
```

### GreenToken

Immutable token:

```rust,ignore
use sipha::syntax::SyntaxKind;
use compact_str::CompactString;

pub struct GreenToken<K: SyntaxKind> {
    kind: K,
    text: CompactString,
}
```

### GreenElement

Tree element (node or token):

```rust,ignore
use std::sync::Arc;
use sipha::syntax::SyntaxKind;

pub enum GreenElement<K: SyntaxKind> {
    Node(Arc<GreenNode<K>>),
    Token(GreenToken<K>),
}
```

## Red Trees

### SyntaxNode

Red tree node for traversal:

```rust,ignore
use std::sync::Arc;

pub struct SyntaxNode {
    green: Arc<GreenNode>,
    parent: Option<Arc<SyntaxNode>>,
    index: usize,
}
```

### SyntaxToken

Red tree token:

```rust,ignore
use std::sync::Arc;

pub struct SyntaxToken {
    green: GreenToken,
    parent: Option<Arc<SyntaxNode>>,
    index: usize,
}
```

## Grammar

### Grammar

Grammar definition:

```rust,ignore
use std::collections::HashMap;
use lasso::Rodeo;

pub struct Grammar<T, N> {
    rules: HashMap<N, Rule<T, N>>,
    entry_point: N,
    interner: Rodeo,
}
```

### Rule

Production rule:

```rust,ignore
pub struct Rule<T, N> {
    pub lhs: N,
    pub rhs: Expr<T, N>,
    pub metadata: RuleMetadata,
}
```

### Expr

Grammar expression:

```rust,ignore
pub enum Expr<T, N> {
    Token(T),
    Rule(N),
    Seq(Vec<Expr<T, N>>),
    Choice(Vec<Expr<T, N>>),
    // ... more variants
}
```

## Incremental Parsing

### IncrementalParser

Incremental parser wrapper:

```rust,ignore
pub struct IncrementalParser<P, T, N> {
    parser: P,
    cache: ParseCache<T::Kind>,
}
```

### IncrementalSession

Incremental parsing session:

```rust,ignore
pub struct IncrementalSession<'a, K> {
    old_tree: Option<&'a GreenNode<K>>,
    edits: &'a [TextEdit],
    affected: AffectedRegion,
    reusable: Vec<ReuseCandidate<K>>,
    cache: Option<&'a ParseCache<K>>,
}
```

### ParseCache

Parse result cache:

```rust,ignore
use std::collections::HashMap;
use std::sync::Arc;
use lasso::Rodeo;

pub struct ParseCache<K> {
    version: usize,
    nodes: HashMap<CacheKey, Arc<GreenNode<K>>>,
    interner: Rodeo,
}
```

## Lexer

### CompiledLexer

Compiled lexer:

```rust,ignore
use std::collections::{HashMap, HashSet};
use compact_str::CompactString;

pub struct CompiledLexer<K> {
    rules: Vec<CompiledRule<K>>,
    dfa: Option<Dfa>,
    keywords: HashMap<CompactString, K>,
    trivia_kinds: HashSet<K>,
}
```

### Token

Lexer token:

```rust,ignore
use compact_str::CompactString;
use sipha::syntax::TextRange;

pub struct Token<K> {
    pub kind: K,
    pub text: CompactString,
    pub range: TextRange,
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

- See [Performance Considerations](performance-considerations.md) for optimization
- Check [Module Structure](module-structure.md) for organization

