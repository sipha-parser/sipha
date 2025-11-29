# Working with Syntax Trees

This chapter shows how to traverse, query, and work with syntax trees in Sipha.

## Creating a Red Tree

Create a red tree from a parse result:

```rust,ignore
use sipha::syntax::SyntaxNode;

let root = SyntaxNode::new_root(result.root.clone());
```

## Traversal

### Children

Iterate over direct children:

```rust,ignore
for child in root.children() {
    println!("Child: {:?}", child.kind());
}
```

### Descendants

Iterate over all descendants (depth-first):

```rust,ignore
for descendant in root.descendants() {
    println!("Descendant: {:?}", descendant.kind());
}
```

### Ancestors

Iterate over ancestors:

```rust,ignore
if let Some(node) = root.descendants().find(|n| n.kind() == MySyntaxKind::Expr) {
    for ancestor in node.ancestors() {
        println!("Ancestor: {:?}", ancestor.kind());
    }
}
```

### Siblings

Access siblings:

```rust,ignore
if let Some(node) = root.descendants().find(|n| n.kind() == MySyntaxKind::Expr) {
    if let Some(next) = node.next_sibling() {
        println!("Next sibling: {:?}", next.kind());
    }
    if let Some(prev) = node.prev_sibling() {
        println!("Previous sibling: {:?}", prev.kind());
    }
}
```

## Querying

### Find by Kind

Find nodes by syntax kind:

```rust,ignore
let exprs: Vec<_> = root.descendants()
    .filter(|n| n.kind() == MySyntaxKind::Expr)
    .collect();
```

### Find by Position

Find nodes at a specific position:

```rust,ignore
use sipha::syntax::TextSize;

let pos = TextSize::from(10);
if let Some(node) = root.token_at_offset(pos) {
    println!("Token at position {}: {:?}", pos, node.kind());
}
```

### Find Parent

Find the parent of a node:

```rust,ignore
if let Some(parent) = node.parent() {
    println!("Parent: {:?}", parent.kind());
}
```

## Text Access

### Node Text

Get the text covered by a node:

```rust,ignore
let text = node.text();
println!("Node text: {}", text);
```

### Token Text

Get the text of a token:

```rust,ignore
if let Some(token) = node.first_token() {
    println!("Token text: {}", token.text());
}
```

### Text Range

Get the text range of a node:

```rust,ignore
let range = node.text_range();
println!("Range: {:?}", range);
```

## Visitors

Use visitors to traverse trees:

```rust,ignore
#[cfg(feature = "visitor")]
use sipha::syntax::{SyntaxVisitor, SyntaxWalker};

struct MyVisitor;

impl SyntaxVisitor for MyVisitor {
    fn visit_node(&mut self, node: &SyntaxNode) {
        println!("Visiting: {:?}", node.kind());
    }
    
    fn visit_token(&mut self, token: &SyntaxToken) {
        println!("Token: {:?}", token.kind());
    }
}

let mut visitor = MyVisitor;
root.walk(&mut visitor);
```

## Queries

Use XPath-like queries (requires `query` feature):

```rust,ignore
#[cfg(feature = "query")]
use sipha::syntax::{QueryBuilder, XPathQuery};

let query = QueryBuilder::new()
    .descendant(MySyntaxKind::Expr)
    .child(MySyntaxKind::Number)
    .build();

let results: Vec<_> = query.matches(&root).collect();
```

## Pattern Matching

Match tree patterns:

```rust,ignore
// Match specific structure
if let Some(expr) = root.descendants().find(|n| {
    n.kind() == MySyntaxKind::Expr &&
    n.children().any(|c| c.kind() == MySyntaxKind::Number)
}) {
    println!("Found expression with number");
}
```

## Collecting Information

### Count Nodes

Count nodes by kind:

```rust,ignore
let expr_count = root.descendants()
    .filter(|n| n.kind() == MySyntaxKind::Expr)
    .count();
```

### Collect Text

Collect text from nodes:

```rust,ignore
let all_text: String = root.descendants()
    .filter_map(|n| n.first_token())
    .map(|t| t.text().to_string())
    .collect();
```

## Best Practices

1. **Use iterators**: Prefer iterator methods over manual traversal
2. **Cache results**: Cache frequently accessed nodes
3. **Filter early**: Filter nodes as early as possible
4. **Use visitors**: Use visitors for complex traversals
5. **Leverage queries**: Use queries for pattern matching

## Next Steps

- See [Tree Manipulation](manipulation.md) for building and modifying trees
- Check [Examples](../examples/) for real-world usage

