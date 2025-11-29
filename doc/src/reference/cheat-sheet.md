# Cheat Sheet

Quick reference for common Sipha patterns and APIs.

## Syntax Kinds

```rust,ignore
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MySyntaxKind {
    // Terminals
    Number, Plus, Minus, Eof,
    // Non-terminals
    Expr, Term,
}

impl SyntaxKind for MySyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr | Self::Term)
    }
    
    fn is_trivia(self) -> bool {
        false // No trivia in this example
    }
}
```

## Lexer Builder

```rust,ignore
# use sipha::syntax::SyntaxKind;
# #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
# enum MySyntaxKind { Number, Plus, Minus, Eof, Whitespace, Expr, Term, }
# impl SyntaxKind for MySyntaxKind {
#     fn is_terminal(self) -> bool { !matches!(self, Self::Expr | Self::Term) }
#     fn is_trivia(self) -> bool { matches!(self, Self::Whitespace) }
# }
use sipha::lexer::{LexerBuilder, Pattern, CharSet};

let lexer = LexerBuilder::new()
    .token(MySyntaxKind::Number, Pattern::Repeat {
        pattern: Box::new(Pattern::CharClass(CharSet::digits())),
        min: 1,
        max: None,
    })
    .token(MySyntaxKind::Plus, Pattern::Literal("+".into()))
    .trivia(MySyntaxKind::Whitespace)
    .build(MySyntaxKind::Eof, MySyntaxKind::Number)?;
```

## Grammar Builder

```rust,ignore
# use sipha::syntax::SyntaxKind;
# #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
# enum MySyntaxKind { Number, Plus, Minus, Eof, Whitespace, Expr, Term, }
# impl SyntaxKind for MySyntaxKind {
#     fn is_terminal(self) -> bool { !matches!(self, Self::Expr | Self::Term) }
#     fn is_trivia(self) -> bool { matches!(self, Self::Whitespace) }
# }
use sipha::grammar::{GrammarBuilder, Expr, NonTerminal};
use sipha::lexer::Token as LexerToken;
use sipha::syntax::{TextRange, TextSize};
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MyNonTerminal { Expr, }
impl NonTerminal for MyNonTerminal {
    fn name(&self) -> &str { match self { MyNonTerminal::Expr => "Expr", } }
}

// Helper to create tokens
fn token(kind: MySyntaxKind, text: &str, offset: u32) -> LexerToken<MySyntaxKind> {
    let len = TextSize::from(u32::try_from(text.len()).unwrap_or(0));
    LexerToken::new(kind, text, TextRange::at(TextSize::from(offset), len))
}

let grammar = GrammarBuilder::new()
    .entry_point(MyNonTerminal::Expr)
    .rule(MyNonTerminal::Expr, Expr::token(token(MySyntaxKind::Number, "1", 0)))
    .build()?;
```

## Grammar Expressions

| Expression | Syntax | Description |
|------------|--------|-------------|
| Token | `Expr::token(token)` | Match a specific token |
| Non-terminal | `Expr::non_terminal(NT::Expr)` | Match a non-terminal |
| Sequence | `Expr::seq(vec![...])` | Match expressions in order |
| Choice | `Expr::choice(vec![...])` | Match one of several alternatives |
| Optional | `Expr::optional(Box::new(...))` | Match zero or one occurrence |
| Repeat | `Expr::repeat(Box::new(...), min, max)` | Match repeated occurrences |
| Empty | `Expr::Empty` | Match nothing (epsilon) |

## Parser Usage

```rust,ignore
# use sipha::grammar::{GrammarBuilder, Grammar, Expr, NonTerminal};
# use sipha::lexer::Token;
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# let grammar: Grammar<MyToken, MyNonTerminal> = todo!();
# let tokens: Vec<Token<MySyntaxKind>> = vec![];
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::backend::ParserBackend;
use sipha::syntax::SyntaxNode;

let config = LlConfig::default();
let mut parser = LlParser::new(&grammar, config)?;
let result = parser.parse(&tokens, MyNonTerminal::Expr);

if !result.errors.is_empty() {
    // Handle errors
}
let root = SyntaxNode::new_root(result.root.clone());
```

## Incremental Parsing

```rust,ignore
# use sipha::backend::ll::LlParser;
# use sipha::grammar::Grammar;
# use sipha::lexer::Token;
# use sipha::syntax::GreenNode;
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# let parser: LlParser = todo!();
# let grammar: Grammar<MyToken, MyNonTerminal> = todo!();
# let tokens: Vec<Token<MySyntaxKind>> = vec![];
# let old_tree: Option<&GreenNode<MySyntaxKind>> = None;
# let entry_point: MyNonTerminal = todo!();
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::syntax::{TextRange, TextSize};

let mut incremental = IncrementalParser::new(parser);
let edits = vec![TextEdit::replace(
    TextRange::new(TextSize::from(0), TextSize::from(2)),
    "new".into(),
)];
let result = incremental.parse_incremental(
    &tokens,
    old_tree,
    &edits,
    entry_point,
    Some(&grammar),
);
```

## Syntax Tree Navigation

```rust,ignore
# use sipha::syntax::{SyntaxNode, GreenNode, SyntaxKind};
# #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
# enum MySyntaxKind { Expr, }
# impl SyntaxKind for MySyntaxKind {
#     fn is_terminal(self) -> bool { false }
#     fn is_trivia(self) -> bool { false }
# }
# let green_node: GreenNode<MySyntaxKind> = todo!();
use sipha::syntax::SyntaxNode;

let root = SyntaxNode::new_root(green_node);

// Children
for child in root.children() {
    println!("{:?}", child.kind());
}

// Descendants
for node in root.descendants() {
    if node.kind() == MySyntaxKind::Expr {
        // Found expression
    }
}

// Parent
if let Some(parent) = root.parent() {
    println!("Parent: {:?}", parent.kind());
}

// Siblings
for sibling in root.next_sibling().into_iter() {
    println!("Sibling: {:?}", sibling.kind());
}
```

## Error Handling

```rust,ignore
# use sipha::backend::ParserBackend;
# type MyToken = ();
# type MyNonTerminal = ();
# struct Parser;
# impl ParserBackend<MyToken, MyNonTerminal> for Parser {
#     type Config = ();
#     type Error = ();
#     type State = ();
#     fn new(_: &(), _: ()) -> Result<Self, ()> { Ok(Parser) }
#     fn parse(&mut self, _: &[MyToken], _: MyNonTerminal) -> sipha::backend::ParseResult<MyToken, MyNonTerminal> { todo!() }
# }
# let mut parser = Parser;
# let tokens: Vec<MyToken> = vec![];
# let result = parser.parse(&tokens, MyNonTerminal::Expr);
// Check for errors
if !result.errors.is_empty() {
    for error in &result.errors {
        eprintln!("Error: {} at {:?}", error.message, error.span);
    }
}

// Check for warnings
for warning in &result.warnings {
    eprintln!("Warning: {}", warning.message);
}
```

## Pattern Matching

| Pattern | Syntax | Example |
|---------|--------|---------|
| Literal | `Pattern::Literal("+".into())` | Match exact string |
| Char Class | `Pattern::CharClass(CharSet::digits())` | Match character class |
| Repeat | `Pattern::Repeat { pattern, min, max }` | Match repetition |
| Regex | `Pattern::Regex(regex)` | Match regex pattern |

## Common Character Sets

```rust,ignore
use sipha::lexer::CharSet;

let _ = CharSet::digits();        // [0-9]
let _ = CharSet::whitespace();    // \s
let _ = CharSet::letters();       // [a-zA-Z]
let _ = CharSet::alphanumeric();  // [a-zA-Z0-9]
let _ = CharSet::hex_digits();    // [0-9a-fA-F]
```

## Backend Selection

| Use Case | Backend | Reason |
|----------|---------|--------|
| Simple grammar | LL | Easy to use, good error messages |
| Left recursion | LR | Natural support |
| Ambiguous grammar | GLR | Handles ambiguity |
| Maximum performance | LR | Efficient table-based parsing |

## Feature Flags

```toml
[dependencies]
sipha = { version = "0.5.0", features = [
    "backend-ll",      # LL(k) parser (default)
    "backend-lr",      # LR parser
    "backend-glr",     # GLR parser (requires backend-lr)
    "diagnostics",     # Rich error diagnostics
    "unicode",         # Unicode support
    "visitor",         # Visitor patterns
    "query",           # XPath-like queries
    "tree-utils",      # Tree utilities
] }
```

## Common Patterns

### Optional Semicolon

```rust,ignore
# use sipha::grammar::{GrammarBuilder, Expr, NonTerminal};
# use sipha::lexer::Token;
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# let semicolon_token: Token<MySyntaxKind> = todo!();
# let mut builder = GrammarBuilder::new();
builder.rule(MyNonTerminal::Stmt, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::Expr),
    Expr::optional(Box::new(Expr::token(semicolon_token))),
]))
```

### Delimited List

```rust,ignore
# use sipha::grammar::{GrammarBuilder, Expr, NonTerminal};
# use sipha::lexer::Token;
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# let open_token: Token<MySyntaxKind> = todo!();
# let close_token: Token<MySyntaxKind> = todo!();
# let mut builder = GrammarBuilder::new();
builder.rule(MyNonTerminal::List, Expr::seq(vec![
    Expr::token(open_token),
    Expr::repeat(
        Box::new(Expr::non_terminal(MyNonTerminal::Item)),
        0,
        None,
    ),
    Expr::token(close_token),
]))
```

### Operator Precedence

```rust,ignore
# use sipha::grammar::{GrammarBuilder, Expr, NonTerminal};
# use sipha::lexer::Token;
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# let multiply_token: Token<MySyntaxKind> = todo!();
# let plus_token: Token<MySyntaxKind> = todo!();
# let mut builder = GrammarBuilder::new();
// Higher precedence first
builder.rule(MyNonTerminal::Expr, Expr::choice(vec![
    Expr::seq(vec![  // * has higher precedence
        Expr::non_terminal(MyNonTerminal::Term),
        Expr::token(multiply_token),
        Expr::non_terminal(MyNonTerminal::Term),
    ]),
    Expr::seq(vec![  // + has lower precedence
        Expr::non_terminal(MyNonTerminal::Expr),
        Expr::token(plus_token),
        Expr::non_terminal(MyNonTerminal::Term),
    ]),
    Expr::non_terminal(MyNonTerminal::Term),
]))
```

## See Also

- [Getting Started](getting-started.md) - Step-by-step tutorial
- [API Documentation](https://docs.rs/sipha) - Full API reference
- [Examples](../examples/) - Complete working examples

