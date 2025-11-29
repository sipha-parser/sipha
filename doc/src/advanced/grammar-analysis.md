# Grammar Analysis

Sipha provides tools for analyzing and optimizing grammars.

## Grammar Validation

Validate grammars before use:

```rust,ignore
use sipha::backend::ll::LlParser;

let errors = LlParser::validate(&grammar);
if !errors.is_empty() {
    for error in errors {
        eprintln!("Grammar error: {:?}", error);
    }
}
```

## Left Recursion Detection

Detect left-recursive rules:

```rust,ignore
use sipha::grammar::analysis::detect_left_recursion;

let left_recursive = detect_left_recursion(&grammar);
for rule in left_recursive {
    eprintln!("Left-recursive rule: {:?}", rule);
}
```

## Reachability Analysis

Find unreachable rules:

```rust,ignore
use sipha::grammar::analysis::find_unreachable_rules;

let unreachable = find_unreachable_rules(&grammar);
for rule in unreachable {
    eprintln!("Unreachable rule: {:?}", rule);
}
```

## FIRST and FOLLOW Sets

Compute FIRST and FOLLOW sets for LL parsing:

```rust,ignore
use sipha::grammar::analysis::compute_first_sets;
use sipha::grammar::analysis::compute_follow_sets;

let first_sets = compute_first_sets(&grammar);
let follow_sets = compute_follow_sets(&grammar);
```

## Conflict Detection

Detect shift/reduce and reduce/reduce conflicts:

```rust,ignore
use sipha::backend::lr::LrParser;

let conflicts = LrParser::detect_conflicts(&grammar);
for conflict in conflicts {
    eprintln!("Conflict: {:?}", conflict);
}
```

## Grammar Optimization

### Factor Common Patterns

Extract common subexpressions:

```rust,ignore
// Before: Duplicated pattern
.rule(MyNonTerminal::Expr, Expr::choice(vec![
    Expr::seq(vec![Expr::non_terminal(MyNonTerminal::Term), Expr::token(plus_token)]),
    Expr::seq(vec![Expr::non_terminal(MyNonTerminal::Term), Expr::token(minus_token)]),
]))

// After: Factored
.rule(MyNonTerminal::Expr, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::Term),
    Expr::choice(vec![
        Expr::token(plus_token),
        Expr::token(minus_token),
    ]),
]))
```

### Eliminate Left Recursion

Transform left-recursive rules:

```rust,ignore
// Before: Left-recursive
.rule(MyNonTerminal::Expr, Expr::choice(vec![
    Expr::seq(vec![
        Expr::non_terminal(MyNonTerminal::Expr),
        Expr::token(plus_token),
        Expr::non_terminal(MyNonTerminal::Term),
    ]),
    Expr::non_terminal(MyNonTerminal::Term),
]))

// After: Right-recursive
.rule(MyNonTerminal::Expr, Expr::seq(vec![
    Expr::non_terminal(MyNonTerminal::Term),
    Expr::opt(Box::new(Expr::seq(vec![
        Expr::token(plus_token),
        Expr::non_terminal(MyNonTerminal::Expr),
    ]))),
]))
```

## Grammar Visualization

Visualize grammar structure (planned feature):

```rust,ignore
// Future API
let dot = grammar.to_dot();
std::fs::write("grammar.dot", dot).unwrap();
```

## Best Practices

1. **Validate early**: Validate grammars before use
2. **Check for conflicts**: Detect and resolve conflicts
3. **Optimize structure**: Factor and simplify grammar
4. **Test thoroughly**: Test with various inputs
5. **Analyze performance**: Profile grammar performance

## Next Steps

- See [Grammars](../core-concepts/grammars.md) for grammar definition
- Check [Performance](performance.md) for optimization tips

