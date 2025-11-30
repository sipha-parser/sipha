# Parsing Backends Overview

Sipha supports multiple parsing algorithms via feature flags. Each backend has different characteristics and is suited for different use cases.

## Available Backends

### LL(k) Parser (`backend-ll`)

Top-down predictive parsing with configurable lookahead:

- Supports left-recursion elimination
- Configurable error recovery
- Incremental parsing support
- Good for most grammars

### LR Parser (`backend-lr`)

Bottom-up shift-reduce parsing:

- Efficient for many grammar types
- Good error recovery
- Supports LALR and canonical LR
- Table-based parsing

### GLR Parser (`backend-glr`)

Generalized LR parsing for ambiguous grammars:

- Handles non-deterministic and ambiguous grammars
- Parse forest representation for ambiguity tracking
- Configurable disambiguation strategies
- Incremental parsing support
- Ideal for complex languages like C++

### PEG Parser (`backend-peg`)

Parsing Expression Grammar with ordered choice and memoization:

- Ordered choice semantics (first match wins)
- Packrat parsing with memoization for O(n) performance
- Backtracking with configurable depth limits
- Incremental parsing support
- Ideal for precedence-based languages and interactive tools

### Pratt Parser (`backend-pratt`)

Recursive descent parser with operator precedence parsing:

- Operator precedence parsing for expressions
- Natural handling of operator associativity
- Recursive descent algorithm
- Incremental parsing support
- Ideal for expression-heavy languages and operator precedence parsing

## ParserBackend Trait

All backends implement the `ParserBackend` trait:

```rust,ignore
pub trait ParserBackend<T, N>: Sized + Send
where
    T: Token,
    N: NonTerminal,
{
    type Config: Default + Clone;
    type Error: std::error::Error + Send + Sync + 'static;
    type State: Send + Sync;
    type BackendGrammar: Send + Sync;  // Backend-specific grammar type

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error>;
    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N>;
    fn parse_incremental(...) -> ParseResult<T, N>;
    fn parse_with_session(...) -> ParseResult<T, N>;
    fn validate(grammar: &Grammar<T, N>) -> Vec<GrammarError<T, N>>;
    fn capabilities() -> BackendCapabilities;
    fn state(&self) -> &Self::State;
}
```

## Grammar Transformation

All backends now use a **grammar transformation pipeline** that automatically converts your grammar into a backend-specific format optimized for that parser:

1. **Validation**: The grammar is validated for compatibility
2. **Transformation**: The grammar is transformed into a backend-specific representation (e.g., `LrGrammar`, `LlGrammar`)
3. **Optimization** (optional): The transformed grammar can be optimized for better performance

This transformation happens automatically when you create a parser - no changes to your code are required!

### Enabling Optimization

You can enable grammar optimization via parser configuration:

```rust,ignore
use sipha::backend::traits::OptimizationLevel;

let config = LlConfig {
    optimize: true,
    optimization_level: OptimizationLevel::Basic,
    ..Default::default()
};
let mut parser = LlParser::new(&grammar, config)?;
```

Optimization levels:
- `None`: No optimization (default)
- `Basic`: Basic optimizations (table compression, etc.)
- `Aggressive`: Aggressive optimizations (may take longer but provide better performance)

## Backend Capabilities

Each backend reports its capabilities:

```rust,ignore
pub struct BackendCapabilities {
    pub name: &'static str,
    pub algorithm: Algorithm,
    pub supports_left_recursion: bool,
    pub supports_ambiguity: bool,
    pub supports_incremental: bool,
    pub supports_error_recovery: bool,
    pub max_lookahead: Option<usize>,
}
```

## Choosing a Backend

```mermaid
flowchart TD
    Start[Choose Backend] --> Ambiguous{Grammar<br/>Ambiguous?}
    Ambiguous -->|Yes| GLR[Use GLR Parser]
    Ambiguous -->|No| UseCase{Primary<br/>Use Case?}
    
    UseCase -->|Expression Parser<br/>Precedence-based| PEG1[Use PEG Parser]
    UseCase -->|Interactive Tools<br/>IDEs/Editors| Interactive{Need<br/>Memoization?}
    UseCase -->|Batch Processing| LR[Use LR Parser]
    UseCase -->|Complex Language| GLR2[Use GLR Parser]
    UseCase -->|Simple Grammar| LL[Use LL Parser]
    
    Interactive -->|Yes| PEG2[Use PEG Parser]
    Interactive -->|No| LL2[Use LL Parser]
    
    GLR --> End[Selected Backend]
    GLR2 --> End
    LR --> End
    LL --> End
    LL2 --> End
    PEG1 --> End
    PEG2 --> End
    
    style GLR fill:#ffccbc,color:#000000
    style GLR2 fill:#ffccbc,color:#000000
    style LR fill:#fff9c4,color:#000000
    style LL fill:#c8e6c9,color:#000000
    style LL2 fill:#c8e6c9,color:#000000
    style PEG1 fill:#e1bee7,color:#000000
    style PEG2 fill:#e1bee7,color:#000000
    style End fill:#e1f5ff,color:#000000
```

See [Choosing a Backend](choosing.md) for detailed guidance on selecting the right backend for your use case.

## Next Steps

- Learn about [LL Parser](ll-parser.md)
- Explore [LR Parser](lr-parser.md)
- Check out [GLR Parser](glr-parser.md)
- Discover [PEG Parser](peg-parser.md)
- Try [Pratt Parser](pratt-parser.md) (coming soon)

## See Also

- [Choosing a Backend](choosing.md) - Detailed guidance on backend selection
- [Cheat Sheet](../reference/cheat-sheet.md) - Quick reference for backend usage
- [Glossary](../reference/glossary.md) - Definitions of parsing-related terms

