# Real-World Patterns

This chapter covers common patterns and idioms for using Sipha in real-world applications.

## Language Server Pattern

### Structure

```rust,ignore
struct LanguageServer {
    parser: IncrementalParser<LlParser, Token, NonTerminal>,
    grammar: Grammar<Token, NonTerminal>,
    lexer: CompiledLexer<SyntaxKind>,
    documents: HashMap<Uri, Document>,
}

struct Document {
    text: String,
    tree: Option<GreenNode<SyntaxKind>>,
    version: i32,
}
```

### Handling Changes

```rust,ignore
impl LanguageServer {
    fn did_change(&mut self, uri: Uri, edits: Vec<TextEdit>, version: i32) {
        let doc = self.documents.get_mut(&uri).unwrap();
        doc.text = apply_edits(&doc.text, &edits);
        doc.version = version;
        
        let tokens = self.lexer.tokenize(&doc.text).unwrap();
        let result = self.parser.parse_incremental(
            &tokens,
            doc.tree.as_ref(),
            &edits,
            MyNonTerminal::Root,
            Some(&self.grammar),
        );
        
        doc.tree = Some(result.root.clone());
        
        // Publish diagnostics
        self.publish_diagnostics(uri, &result.errors);
    }
}
```

## Error Reporting Pattern

### Collecting Errors

```rust,ignore
fn collect_errors(result: &ParseResult) -> Vec<Diagnostic> {
    result.errors
        .iter()
        .map(|error| Diagnostic {
            range: error.span,
            severity: DiagnosticSeverity::Error,
            message: error.message.clone(),
            // ...
        })
        .collect()
}
```

### Reporting to User

```rust,ignore
fn report_errors(errors: &[Diagnostic]) {
    for error in errors {
        eprintln!("Error at {}: {}", error.range, error.message);
    }
}
```

## Tree Traversal Pattern

### Finding Nodes

```rust,ignore
fn find_nodes_by_kind(root: &SyntaxNode, kind: SyntaxKind) -> Vec<SyntaxNode> {
    root.descendants()
        .filter(|n| n.kind() == kind)
        .collect()
}
```

### Collecting Information

```rust,ignore
fn collect_identifiers(root: &SyntaxNode) -> Vec<String> {
    root.descendants()
        .filter(|n| n.kind() == SyntaxKind::Ident)
        .filter_map(|n| n.first_token())
        .map(|t| t.text().to_string())
        .collect()
}
```

## Grammar Extension Pattern

### Adding Rules

```rust,ignore
fn extend_grammar(base: GrammarBuilder) -> GrammarBuilder {
    base
        .rule(MyNonTerminal::NewRule, Expr::token(new_token))
        .rule(MyNonTerminal::AnotherRule, Expr::choice(vec![
            Expr::rule(MyNonTerminal::NewRule),
            Expr::rule(MyNonTerminal::ExistingRule),
        ]))
}
```

## Testing Pattern

### Test Helpers

```rust,ignore
fn parse_test(input: &str) -> ParseResult {
    let lexer = build_test_lexer();
    let grammar = build_test_grammar();
    let mut parser = LlParser::new(&grammar, Default::default()).unwrap();
    
    let tokens = lexer.tokenize(input).unwrap();
    parser.parse(&tokens, MyNonTerminal::Expr)
}

#[test]
fn test_simple_expression() {
    let result = parse_test("1 + 2");
    assert!(result.errors.is_empty());
    // ...
}
```

## Best Practices

1. **Use incremental parsing**: Always use for interactive apps
2. **Cache grammar**: Reuse grammar across parses
3. **Handle errors gracefully**: Don't panic on errors
4. **Test thoroughly**: Test with various inputs
5. **Profile performance**: Profile and optimize hot paths

## Next Steps

- See [Examples](../examples/) for complete examples
- Check [Architecture](../architecture/) for design patterns

