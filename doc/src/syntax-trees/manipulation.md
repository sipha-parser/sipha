# Tree Manipulation

This chapter shows how to build and modify syntax trees in Sipha.

## Building Trees

### GreenNodeBuilder

Use `GreenNodeBuilder` to build green trees:

```rust,ignore
use sipha::syntax::{GreenNodeBuilder, SyntaxKind};

let mut builder = GreenNodeBuilder::new();

// Start a node
builder.start_node(MySyntaxKind::Expr);

// Add tokens
builder.token(MySyntaxKind::Number, "42").unwrap();
builder.token(MySyntaxKind::Plus, "+").unwrap();
builder.token(MySyntaxKind::Number, "10").unwrap();

// Finish the node
let expr_node = builder.finish_node().unwrap();

// Build root
builder.start_node(MySyntaxKind::Root);
builder.finish_node().unwrap();
let root = builder.finish().unwrap();
```

### Nested Structures

Build nested structures:

```rust,ignore
let mut builder = GreenNodeBuilder::new();

builder.start_node(MySyntaxKind::Expr);

// Nested expression
builder.start_node(MySyntaxKind::Term);
builder.token(MySyntaxKind::Number, "1").unwrap();
builder.finish_node().unwrap();

builder.token(MySyntaxKind::Plus, "+").unwrap();

// Another nested expression
builder.start_node(MySyntaxKind::Term);
builder.token(MySyntaxKind::Number, "2").unwrap();
builder.finish_node().unwrap();

builder.finish_node().unwrap();
let root = builder.finish().unwrap();
```

## Modifying Trees

Trees are immutable, so modifications create new trees:

### Replacing Nodes

Replace a node by rebuilding:

```rust,ignore
fn replace_node(old_node: &SyntaxNode, new_kind: MySyntaxKind) -> GreenNode<MySyntaxKind> {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(new_kind);
    
    // Copy children
    for child in old_node.children() {
        match child {
            SyntaxElement::Node(n) => {
                // Recursively replace
                let new_child = replace_node(&n, new_kind);
                // Add new child (simplified - actual API may differ)
            }
            SyntaxElement::Token(t) => {
                builder.token(t.kind(), t.text()).unwrap();
            }
        }
    }
    
    builder.finish_node().unwrap();
    builder.finish().unwrap()
}
```

### Inserting Nodes

Insert nodes by rebuilding:

```rust,ignore
fn insert_child(parent: &SyntaxNode, new_child: GreenNode<MySyntaxKind>, index: usize) -> GreenNode<MySyntaxKind> {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(parent.kind());
    
    // Copy children before index
    for (i, child) in parent.children().enumerate() {
        if i == index {
            // Insert new child
            // (simplified - actual API may differ)
        }
        // Copy existing child
    }
    
    builder.finish_node().unwrap();
    builder.finish().unwrap()
}
```

## Tree Transformation

Transform trees using visitors:

```rust,ignore
struct Transformer;

impl SyntaxVisitor for Transformer {
    fn visit_node(&mut self, node: &SyntaxNode) {
        // Transform node
        if node.kind() == MySyntaxKind::OldKind {
            // Replace with new kind
        }
    }
}
```

## Incremental Updates

For incremental updates, reuse unchanged nodes:

```rust,ignore
use sipha::incremental::IncrementalParser;

// Initial parse
let result1 = parser.parse_incremental(&tokens, None, &[], entry, Some(&grammar));

// After edit, reuse unchanged nodes
let result2 = parser.parse_incremental(
    &new_tokens,
    Some(&result1.root),
    &edits,
    entry,
    Some(&grammar),
);
```

## Best Practices

1. **Use builders**: Use `GreenNodeBuilder` for building trees
2. **Reuse nodes**: Reuse unchanged nodes in incremental updates
3. **Immutable operations**: All tree operations create new trees
4. **Cache results**: Cache frequently accessed nodes
5. **Batch modifications**: Batch multiple modifications together

## Next Steps

- See [Working with Trees](working-with-trees.md) for traversal
- Check [Incremental Parsing](../incremental-parsing/) for efficient updates

