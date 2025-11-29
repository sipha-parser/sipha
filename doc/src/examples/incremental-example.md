# Incremental Parsing Example

This example demonstrates incremental parsing with Sipha.

## Overview

We'll build a simple language server that uses incremental parsing to efficiently update the parse tree as the user edits code.

## Setup

```rust,ignore
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::backend::ll::{LlParser, LlConfig};
use sipha::syntax::{TextRange, TextSize, GreenNode};
use sipha::grammar::Grammar;
use sipha::lexer::CompiledLexer;

// These types would be defined in your actual code
// type MyToken = /* ... */;
// type MyNonTerminal = /* ... */;
// type MySyntaxKind = /* ... */;

struct LanguageServer {
    parser: IncrementalParser<LlParser, MyToken, MyNonTerminal>,
    grammar: Grammar<MyToken, MyNonTerminal>,
    lexer: CompiledLexer<MySyntaxKind>,
    current_tree: Option<GreenNode<MySyntaxKind>>,
}
```

## Initial Parse

```rust,ignore
# use sipha::incremental::{IncrementalParser, TextEdit};
# use sipha::backend::ll::{LlParser, LlConfig};
# use sipha::syntax::{TextRange, TextSize, GreenNode};
# use sipha::grammar::Grammar;
# use sipha::lexer::CompiledLexer;
# struct LanguageServer {
#     parser: IncrementalParser<LlParser, MyToken, MyNonTerminal>,
#     grammar: Grammar<MyToken, MyNonTerminal>,
#     lexer: CompiledLexer<MySyntaxKind>,
#     current_tree: Option<GreenNode<MySyntaxKind>>,
# }
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# fn build_grammar() -> Grammar<MyToken, MyNonTerminal> { todo!() }
# fn build_lexer() -> CompiledLexer<MySyntaxKind> { todo!() }
impl LanguageServer {
    fn new() -> Self {
        let grammar = build_grammar();
        let parser = LlParser::new(&grammar, Default::default()).unwrap();
        let lexer = build_lexer();
        
        Self {
            parser: IncrementalParser::new(parser),
            grammar,
            lexer,
            current_tree: None,
        }
    }
    
    fn initial_parse(&mut self, text: &str) {
        let tokens = self.lexer.tokenize(text).unwrap();
        
        let result = self.parser.parse_incremental(
            &tokens,
            None,              // No old tree
            &[],               // No edits
            MyNonTerminal::Expr,
            Some(&self.grammar), // Populate cache
        );
        
        self.current_tree = Some(result.root.clone());
    }
}
```

## Handling Edits

```rust,ignore
# use sipha::incremental::{IncrementalParser, TextEdit};
# use sipha::backend::ll::{LlParser, LlConfig};
# use sipha::syntax::{TextRange, TextSize, GreenNode};
# use sipha::grammar::Grammar;
# use sipha::lexer::CompiledLexer;
# struct LanguageServer {
#     parser: IncrementalParser<LlParser, MyToken, MyNonTerminal>,
#     grammar: Grammar<MyToken, MyNonTerminal>,
#     lexer: CompiledLexer<MySyntaxKind>,
#     current_tree: Option<GreenNode<MySyntaxKind>>,
# }
# type MyToken = ();
# type MyNonTerminal = ();
# type MySyntaxKind = ();
# impl LanguageServer {
#     fn report_errors(&self, _errors: &[()]) {}
# }
impl LanguageServer {
    fn on_text_changed(&mut self, edits: Vec<TextEdit>, new_text: &str) {
        // Re-tokenize (or update tokens incrementally)
        let tokens = self.lexer.tokenize(new_text).unwrap();
        
        // Incremental parse
        let result = self.parser.parse_incremental(
            &tokens,
            self.current_tree.as_ref(), // Previous tree
            &edits,                     // Text edits
            MyNonTerminal::Expr,
            Some(&self.grammar),        // Update cache
        );
        
        // Update current tree
        self.current_tree = Some(result.root.clone());
        
        // Report errors
        if !result.errors.is_empty() {
            self.report_errors(&result.errors);
        }
    }
}
```

## Creating Edits

```rust,ignore
# use sipha::incremental::TextEdit;
# use sipha::syntax::{TextRange, TextSize};
# struct LanguageServer;
# impl LanguageServer {
#     fn on_text_changed(&mut self, _edits: Vec<TextEdit>, _new_text: &str) {}
# }
# let mut server = LanguageServer;
// User changed "42" to "100" at position 0
let edit = TextEdit::replace(
    TextRange::new(TextSize::from(0), TextSize::from(2)),
    "100".into(),
);

server.on_text_changed(vec![edit], "100 + 10");
```

## Performance

Incremental parsing provides significant performance improvements:

- **Small edits**: 10-100x faster
- **Medium edits**: 5-20x faster
- **Large edits**: 2-5x faster

## Complete Example

See [`examples/incremental_parsing.rs`](../../crates/sipha/examples/incremental_parsing.rs) for the complete working example.

## Next Steps

- See [Incremental Parsing](../incremental-parsing/) for detailed documentation
- Check [Real-World Patterns](real-world.md) for more examples

