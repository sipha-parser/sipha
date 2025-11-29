# PEG Parser

The PEG (Parsing Expression Grammar) parser uses ordered choice and backtracking with memoization (packrat parsing) for efficient parsing.

## Overview

PEG parsing works by:

1. **Ordered Choice**: Try alternatives in order, first match wins
2. **Backtracking**: If an alternative fails, backtrack and try the next
3. **Memoization**: Cache parse results for O(n) performance (packrat parsing)
4. **Greedy Matching**: Always match as much as possible

## Understanding PEG Grammars

PEG (Parsing Expression Grammar) is a type of formal grammar that describes a language in terms of parsing rules rather than generation rules. Unlike traditional CFG (Context-Free Grammar) parsers, PEG parsers use **ordered choice** semantics, making them deterministic and unambiguous.

### Key Concepts

#### Ordered Choice (`A | B`)

In PEG, the choice operator `|` is **ordered** and **greedy**:

- Alternatives are tried **left to right**
- The **first** matching alternative wins
- Once an alternative matches, **no other alternatives are tried**
- This makes PEG grammars **unambiguous by definition**

**Example**:
```rust,ignore
// Grammar: Expr -> Number | Number
// In CFG: This is ambiguous (both alternatives match "42")
// In PEG: First alternative always wins, second is never tried

Expr::Choice(vec![
    Expr::token(Number("42")),  // This matches first
    Expr::token(Number("99")), // Never tried, even if input is "99"
])
```

#### Greedy Repetition (`A*`, `A+`)

PEG repetition is **greedy** - it matches as much as possible:

- `A*` matches **zero or more** `A`s (as many as possible)
- `A+` matches **one or more** `A`s (as many as possible)
- Repetition always tries to match **more** before trying less

**Example**:
```rust,ignore
// Grammar: Expr -> Number*
// Input: "123"
// Matches: All three digits (greedy)

Expr::Repeat {
    expr: Box::new(Expr::token(Number("1"))),
    min: 0,
    max: None,
}
```

#### Backtracking

When an alternative fails, the parser **backtracks** to try the next alternative:

- Position is restored to where the alternative started
- Previous matches are discarded
- Next alternative is tried from the same position

**Example**:
```rust,ignore
// Grammar: Expr -> "ab" | "ac"
// Input: "ac"
// Process:
//   1. Try "ab" - matches "a", fails on "b"
//   2. Backtrack to start
//   3. Try "ac" - matches successfully
```

## Key Differences from CFG

PEG parsers differ from traditional CFG parsers in fundamental ways:

| Feature | CFG | PEG |
|---------|-----|-----|
| **Choice** | Unordered (all alternatives considered) | Ordered (first match wins) |
| **Ambiguity** | Can be ambiguous | Always unambiguous |
| **Repetition** | May be non-greedy | Always greedy |
| **Left Recursion** | Natural | Requires special handling |
| **Determinism** | May be non-deterministic | Always deterministic |

### Ordered Choice Example

```rust,ignore
// CFG: Expr -> Number | Number
//      This is ambiguous - both alternatives match the same input
//      Parser must use disambiguation rules

// PEG: Expr -> Number | Number  
//      This is unambiguous - first alternative always wins
//      Second alternative is never tried if first matches

Expr::Choice(vec![
    Expr::token(Number("1")),  // Tried first
    Expr::token(Number("2")), // Only tried if first fails
])
```

### Precedence Through Ordering

PEG makes operator precedence **natural** through ordering:

```rust,ignore
// Lower precedence operators tried first
// Higher precedence operators tried later
// This naturally gives * higher precedence than +

Expr::Choice(vec![
    // Addition (lower precedence, tried first)
    Expr::Seq(vec![
        Expr::Rule(Expr),
        Expr::token(Plus),
        Expr::Rule(Term),
    ]),
    // Base case (tried last)
    Expr::Rule(Term),
])

Term::Choice(vec![
    // Multiplication (higher precedence, tried first)
    Expr::Seq(vec![
        Expr::Rule(Term),
        Expr::token(Multiply),
        Expr::Rule(Factor),
    ]),
    // Base case (tried last)
    Expr::Rule(Factor),
])
```

**Input**: `1 + 2 * 3`

**Parse process**:
1. Try `Expr + Term` - matches `1 +`, then tries to match `Term`
2. `Term` tries `Term * Factor` - matches `2 * 3`
3. Result: `1 + (2 * 3)` ✓ (correct precedence)

## Configuration

Configure the PEG parser with `PegConfig`:

```rust,ignore
use sipha::backend::peg::{PegParser, PegConfig};

let config = PegConfig {
    enable_memoization: true,    // Enable packrat parsing
    max_memo_size: 10000,        // Maximum cache size
    error_recovery: true,         // Enable error recovery
    max_errors: 100,             // Maximum errors before giving up
    max_backtrack_depth: 1000,   // Limit backtracking depth
};

let mut parser = PegParser::new(&grammar, config)
    .expect("Failed to create parser");
```

### Memoization (Packrat Parsing)

When `enable_memoization` is true, the parser caches parse results:

- **O(n) Performance**: Linear time parsing for many grammars
- **Memory Trade-off**: Uses more memory for cache
- **Cache Size**: Controlled by `max_memo_size`

Without memoization:
- **Exponential Worst Case**: Can be slow for some grammars
- **Less Memory**: No cache overhead

### Backtracking Depth

The `max_backtrack_depth` parameter limits backtracking:

- **Prevents Infinite Loops**: Stops infinite backtracking
- **Performance**: Lower values may fail on valid inputs
- **Default**: 1000 (usually sufficient)

### Error Recovery

Error recovery strategies:

- **Best-effort parsing**: Continue parsing despite errors
- **Error limits**: Stop after `max_errors` errors
- **Recovery points**: Use `RecoveryPoint` expressions in grammar

## Usage

```rust,ignore
use sipha::backend::peg::{PegParser, PegConfig};
use sipha::backend::ParserBackend;

let config = PegConfig::default();
let mut parser = PegParser::new(&grammar, config)
    .expect("Failed to create parser");

let result = parser.parse(&tokens, MyNonTerminal::Expr);
```

## Writing PEG Grammars

### Basic Patterns

#### Sequences

Sequences match expressions in order:

```rust,ignore
// Match: "a" followed by "b"
Expr::Seq(vec![
    Expr::token(Token::A),
    Expr::token(Token::B),
])
```

#### Optional Elements

Optional expressions match zero or one occurrence:

```rust,ignore
// Match: "a" optionally followed by "b"
Expr::Seq(vec![
    Expr::token(Token::A),
    Expr::Opt(Box::new(Expr::token(Token::B))),
])
```

#### Repetition

Repetition matches zero or more (or one or more) occurrences:

```rust,ignore
// Match: One or more "a"s
Expr::Repeat {
    expr: Box::new(Expr::token(Token::A)),
    min: 1,
    max: None,
}

// Match: Zero to three "a"s
Expr::Repeat {
    expr: Box::new(Expr::token(Token::A)),
    min: 0,
    max: Some(3),
}
```

#### Separated Lists

Separated lists match items separated by a delimiter:

```rust,ignore
// Match: "1, 2, 3" or "1, 2, 3,"
Expr::Separated {
    item: Box::new(Expr::token(Number)),
    separator: Box::new(Expr::token(Comma)),
    min: 1,
    trailing: TrailingSeparator::Allow,
}
```

### Common Patterns

#### Operator Precedence

Express precedence by ordering alternatives (lower precedence first):

```rust,ignore
// Expression with precedence: + < * < ( )
Expr::Choice(vec![
    // Addition (lowest precedence, tried first)
    Expr::Seq(vec![
        Expr::Rule(Expr),
        Expr::token(Plus),
        Expr::Rule(Term),
    ]),
    // Subtraction
    Expr::Seq(vec![
        Expr::Rule(Expr),
        Expr::token(Minus),
        Expr::Rule(Term),
    ]),
    // Base case (tried last)
    Expr::Rule(Term),
])

Term::Choice(vec![
    // Multiplication (higher precedence)
    Expr::Seq(vec![
        Expr::Rule(Term),
        Expr::token(Multiply),
        Expr::Rule(Factor),
    ]),
    // Base case
    Expr::Rule(Factor),
])

Factor::Choice(vec![
    // Parentheses (highest precedence)
    Expr::Delimited {
        open: Box::new(Expr::token(LParen)),
        content: Box::new(Expr::Rule(Expr)),
        close: Box::new(Expr::token(RParen)),
        recover: false,
    },
    // Numbers
    Expr::token(Number),
])
```

#### Left Associativity

Left associativity is natural in PEG (left-recursive rules work correctly):

```rust,ignore
// Left-associative: 1 - 2 - 3 = (1 - 2) - 3
Expr::Choice(vec![
    Expr::Seq(vec![
        Expr::Rule(Expr),  // Left-recursive
        Expr::token(Minus),
        Expr::Rule(Term),
    ]),
    Expr::Rule(Term),
])
```

**Note**: Left recursion is detected and handled, but may require special consideration.

#### Right Associativity

Right associativity requires right-recursive rules:

```rust,ignore
// Right-associative: 2^3^4 = 2^(3^4)
Expr::Choice(vec![
    Expr::Seq(vec![
        Expr::Rule(Term),
        Expr::token(Power),
        Expr::Rule(Expr),  // Right-recursive
    ]),
    Expr::Rule(Term),
])
```

#### Optional Trailing Elements

Handle optional trailing separators:

```rust,ignore
// Match: "a, b, c" or "a, b, c,"
Expr::Separated {
    item: Box::new(Expr::token(Ident)),
    separator: Box::new(Expr::token(Comma)),
    min: 1,
    trailing: TrailingSeparator::Allow,  // or Forbid, or Require
}
```

### Advanced Patterns

#### Lookahead

Positive lookahead checks if expression matches without consuming:

```rust,ignore
// Match "a" only if followed by "b" (but don't consume "b")
Expr::Seq(vec![
    Expr::token(Token::A),
    Expr::Lookahead(Box::new(Expr::token(Token::B))),
])
```

Negative lookahead checks if expression doesn't match:

```rust,ignore
// Match "a" only if NOT followed by "b"
Expr::Seq(vec![
    Expr::token(Token::A),
    Expr::NotLookahead(Box::new(Expr::token(Token::B))),
])
```

#### Error Recovery

Use recovery points to skip to known tokens on error:

```rust,ignore
// On error, skip to semicolon or brace
Expr::RecoveryPoint {
    expr: Box::new(Expr::Rule(Statement)),
    sync_tokens: vec![Semicolon, RBrace].into(),
}
```

#### Delimited Recovery

Recover from missing closing delimiters:

```rust,ignore
// Recover from missing closing parenthesis
Expr::Delimited {
    open: Box::new(Expr::token(LParen)),
    content: Box::new(Expr::Rule(Expr)),
    close: Box::new(Expr::token(RParen)),
    recover: true,  // Skip tokens until closing delimiter found
}
```

## Incremental Parsing

The PEG parser supports incremental parsing with memoization:

```rust,ignore
use sipha::incremental::IncrementalParser;

let mut incremental = IncrementalParser::new(parser);
let result = incremental.parse_incremental(
    &tokens,
    old_tree,
    &edits,
    entry_point,
    Some(&grammar),
);
```

The memoization cache works with incremental parsing:
- **Node Reuse**: Cached nodes are reused when possible
- **Cache Invalidation**: Affected regions invalidate cache entries
- **Performance**: Significant speedup for small edits

## Grammar Requirements and Best Practices

### What PEG Handles Well

PEG parsers excel at:

- **Precedence-based Languages**: Operator precedence is natural through ordering
- **Expression Parsers**: Arithmetic, logical, and comparison expressions
- **Structured Data**: JSON, XML, configuration files
- **Interactive Tools**: Fast parsing with memoization for IDEs and editors
- **Deterministic Parsing**: Always produces one parse tree (no ambiguity)

### Left Recursion

PEG parsers **detect** left recursion and handle it, but with limitations:

```rust,ignore
// Left-recursive rule
Expr -> Expr + Term | Term

// PEG detects this and adds a warning
// The parser will fail on left-recursive rules to prevent infinite loops
```

**Best Practice**: For left-recursive grammars, consider:
- Using right-recursive alternatives where possible
- Restructuring to avoid left recursion
- Using iterative parsing for specific cases

**Example - Avoiding Left Recursion**:
```rust,ignore
// Instead of: Expr -> Expr + Term | Term
// Use iterative approach with repetition:

Expr::Seq(vec![
    Expr::Rule(Term),
    Expr::Repeat {
        expr: Box::new(Expr::Seq(vec![
            Expr::token(Plus),
            Expr::Rule(Term),
        ])),
        min: 0,
        max: None,
    },
])
```

### Ambiguity Resolution

PEG grammars are **always unambiguous** because ordered choice resolves ambiguity:

```rust,ignore
// CFG: Expr -> Number | Number  (ambiguous)
// PEG: Expr -> Number | Number   (unambiguous - first wins)

// If you need different behavior, reorder:
Expr::Choice(vec![
    Expr::token(Number("42")),  // Specific case first
    Expr::token(Number("n")),   // General case second
])
```

**Best Practice**: Order alternatives from **most specific to least specific**.

### Common Pitfalls

#### 1. Order Matters

```rust,ignore
// Wrong: General case first
Expr::Choice(vec![
    Expr::token(Number),        // Matches everything
    Expr::token(Number("42")),  // Never reached
])

// Correct: Specific case first
Expr::Choice(vec![
    Expr::token(Number("42")),  // Specific case first
    Expr::token(Number),        // General case second
])
```

#### 2. Greedy Repetition

```rust,ignore
// This matches as many as possible
Expr::Repeat {
    expr: Box::new(Expr::token(Number)),
    min: 0,
    max: None,
}
// Input: "123" matches all three digits
```

#### 3. Backtracking Cost

Excessive backtracking can be slow. Use lookahead to avoid unnecessary backtracking:

```rust,ignore
// Expensive: Tries "ab", backtracks, tries "ac"
Expr::Choice(vec![
    Expr::Seq(vec![Expr::token(A), Expr::token(B)]),
    Expr::Seq(vec![Expr::token(A), Expr::token(C)]),
])

// Better: Use lookahead to avoid backtracking
Expr::Seq(vec![
    Expr::token(A),
    Expr::Choice(vec![
        Expr::Lookahead(Box::new(Expr::token(B))),
        Expr::token(C),
    ]),
])
```

## Memoization Details

Packrat parsing caches results at each position:

- **Cache Key**: `(rule, position)`
- **Cache Value**: Parse result (success with node, or failure)
- **Cache Size**: Bounded by `max_memo_size`
- **Eviction**: Old entries evicted when cache is full

Benefits:
- **Linear Time**: O(n) for many grammars
- **Node Reuse**: Cached nodes can be reused
- **Incremental**: Works well with incremental parsing

## Ordered Choice Details

Ordered choice (`A | B`) semantics:

1. Try `A`
2. If `A` succeeds, return success (never try `B`)
3. If `A` fails, backtrack and try `B`
4. If both fail, return failure

This differs from CFG where both alternatives are considered.

## Performance

PEG parsing performance:

- **With Memoization**: O(n) time, O(n) space (packrat parsing)
- **Without Memoization**: O(n) to exponential time, O(1) space
- **Incremental**: Fast for small edits (benefits from memoization)
- **Memory**: Cache size depends on grammar and input

## When to Use PEG

### Ideal Use Cases

#### 1. Expression Parsers

PEG excels at parsing expressions with operator precedence:

```rust,ignore
// Natural expression parsing
// Input: "1 + 2 * 3"
// Result: 1 + (2 * 3)  (correct precedence)

// Grammar is intuitive - lower precedence tried first
Expr -> Expr + Term | Term
Term -> Term * Factor | Factor
```

**Why PEG**: Precedence is expressed naturally through ordering, no need for precedence tables.

#### 2. Interactive Tools (IDEs, Editors)

PEG with memoization provides fast incremental parsing:

- **Fast re-parsing**: Memoization caches results
- **Incremental updates**: Only affected regions re-parsed
- **Responsive**: Good performance for real-time editing

**Why PEG**: Memoization makes repeated parsing very fast.

#### 3. Structured Data Formats

PEG handles delimited and separated structures well:

```rust,ignore
// JSON-like structures
Object -> "{" (Pair ("," Pair)*)? "}"
Array -> "[" (Value ("," Value)*)? "]"
```

**Why PEG**: Delimited and separated expressions are first-class.

#### 4. Deterministic Parsing

When you need **guaranteed** unambiguous parsing:

- **No ambiguity**: Ordered choice ensures one parse tree
- **Predictable**: Same input always produces same output
- **No disambiguation needed**: Parser makes choices automatically

**Why PEG**: Unambiguous by design.

### When NOT to Use PEG

#### 1. Truly Ambiguous Grammars

If you need to **track** or **explore** ambiguity:

- **Use GLR instead**: GLR produces parse forests for ambiguous inputs
- **PEG hides ambiguity**: Only one parse tree is produced

#### 2. Very Large Files Without Memoization

Without memoization, some PEG grammars can be exponential:

- **Enable memoization**: Use `enable_memoization: true`
- **Or use LR**: LR parsers are consistently O(n) without memoization

#### 3. Simple LL(1) Grammars

For simple, unambiguous grammars, LL(k) may be simpler:

- **LL is simpler**: Table-based, easier to understand
- **PEG is more flexible**: But adds complexity for simple cases

## Limitations

PEG parser limitations:

- **Exponential Without Memoization**: Can be slow for some grammars
- **Memory Usage**: Memoization requires significant memory
- **Left Recursion**: May require special handling
- **No Ambiguity Tracking**: Ambiguity resolved by ordering (may hide issues)

## Comparison with Other Backends

### PEG vs LL Parser

| Aspect | PEG | LL(k) |
|--------|-----|-------|
| **Approach** | Top-down with backtracking | Top-down predictive |
| **Choice** | Ordered (first match wins) | Based on lookahead |
| **Left Recursion** | Detected, may need handling | Can eliminate |
| **Ambiguity** | Resolved by ordering | Requires lookahead > 1 |
| **Performance** | O(n) with memoization | O(n) always |
| **Memory** | O(n) with memoization | O(grammar size) |
| **Best For** | Precedence, interactive tools | Simple grammars, IDEs |

**Choose PEG when**: You need ordered choice semantics or memoization benefits.

**Choose LL when**: Your grammar is LL(k) compatible and you want simplicity.

### PEG vs LR Parser

| Aspect | PEG | LR |
|--------|-----|-----|
| **Approach** | Top-down with backtracking | Bottom-up shift-reduce |
| **Choice** | Ordered | Based on parse table |
| **Left Recursion** | Detected, may need handling | Natural |
| **Backtracking** | Yes (with limits) | No |
| **Performance** | O(n) with memoization | O(n) always |
| **Error Recovery** | Good | Excellent |
| **Best For** | Precedence, expressions | Batch parsing, languages |

**Choose PEG when**: You need ordered choice or top-down semantics.

**Choose LR when**: You need excellent error recovery or batch processing.

### PEG vs GLR Parser

| Aspect | PEG | GLR |
|--------|-----|-----|
| **Approach** | Top-down deterministic | Bottom-up with ambiguity |
| **Choice** | Ordered (unambiguous) | All alternatives (ambiguous) |
| **Ambiguity** | Resolved by ordering | Tracked in parse forest |
| **Performance** | O(n) with memoization | O(n) to O(n³) |
| **Best For** | Deterministic parsing | Ambiguous grammars |

**Choose PEG when**: You want guaranteed unambiguous parsing.

**Choose GLR when**: Your grammar has inherent ambiguities you need to explore.

## Practical Examples

### Example 1: Arithmetic Expressions

```rust,ignore
// Grammar for: 1 + 2 * 3
let grammar = GrammarBuilder::new()
    .entry_point(Expr)
    .rule(Expr, Expr::Choice(vec![
        // Addition (lowest precedence, tried first)
        Expr::Seq(vec![
            Expr::Rule(Expr),
            Expr::token(Plus),
            Expr::Rule(Term),
        ]),
        // Base case
        Expr::Rule(Term),
    ]))
    .rule(Term, Expr::Choice(vec![
        // Multiplication (higher precedence)
        Expr::Seq(vec![
            Expr::Rule(Term),
            Expr::token(Multiply),
            Expr::Rule(Factor),
        ]),
        // Base case
        Expr::Rule(Factor),
    ]))
    .rule(Factor, Expr::Choice(vec![
        // Parentheses
        Expr::Delimited {
            open: Box::new(Expr::token(LParen)),
            content: Box::new(Expr::Rule(Expr)),
            close: Box::new(Expr::token(RParen)),
            recover: false,
        },
        // Numbers
        Expr::token(Number),
    ]))
    .build()?;
```

### Example 2: JSON-like Lists

```rust,ignore
// Grammar for: [1, 2, 3] or [1, 2, 3,]
let grammar = GrammarBuilder::new()
    .entry_point(Array)
    .rule(Array, Expr::Delimited {
        open: Box::new(Expr::token(LBracket)),
        content: Box::new(Expr::Separated {
            item: Box::new(Expr::Rule(Value)),
            separator: Box::new(Expr::token(Comma)),
            min: 0,
            trailing: TrailingSeparator::Allow,
        }),
        close: Box::new(Expr::token(RBracket)),
        recover: true,
    })
    .build()?;
```

### Example 3: Error Recovery

```rust,ignore
// Grammar with error recovery
let grammar = GrammarBuilder::new()
    .entry_point(Program)
    .rule(Program, Expr::Repeat {
        expr: Box::new(Expr::RecoveryPoint {
            expr: Box::new(Expr::Rule(Statement)),
            sync_tokens: vec![Semicolon, RBrace].into(),
        }),
        min: 0,
        max: None,
    })
    .build()?;
```

## Next Steps

- See [Choosing a Backend](choosing.md) for comparison
- Check [Examples](../examples/peg-example.md) for usage
- Learn about [Incremental Parsing](../incremental-parsing/overview.md)

