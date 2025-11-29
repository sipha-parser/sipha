# Adding a Backend

This chapter shows how to implement a new parsing backend for Sipha.

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

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error>;
    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N>;
    fn parse_with_session(...) -> ParseResult<T, N>;
    fn validate(grammar: &Grammar<T, N>) -> Vec<GrammarError<T, N>>;
    fn capabilities() -> BackendCapabilities;
    fn state(&self) -> &Self::State;
}
```

## Implementation Steps

### 1. Define Backend Type

```rust,ignore
pub struct MyParser<T, N>
where
    T: Token,
    N: NonTerminal,
{
    grammar: Grammar<T, N>,
    config: MyConfig,
    state: MyState,
}
```

### 2. Implement ParserBackend

```rust,ignore
impl<T, N> ParserBackend<T, N> for MyParser<T, N>
where
    T: Token,
    N: NonTerminal,
{
    type Config = MyConfig;
    type Error = MyError;
    type State = MyState;

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        // Initialize parser
        Ok(Self {
            grammar: grammar.clone(),
            config,
            state: MyState::new(),
        })
    }

    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        // Implement parsing logic
        // ...
    }

    fn parse_with_session(
        &mut self,
        input: &[T],
        entry: N,
        session: &IncrementalSession<'_, T::Kind>,
    ) -> ParseResult<T, N> {
        // Implement incremental parsing
        // ...
    }

    fn validate(grammar: &Grammar<T, N>) -> Vec<GrammarError<T, N>> {
        // Validate grammar compatibility
        // ...
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "MyParser",
            algorithm: Algorithm::PEG, // Or other
            supports_left_recursion: true,
            supports_ambiguity: false,
            supports_incremental: true,
            supports_error_recovery: true,
            max_lookahead: None,
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}
```

### 3. Implement Parsing Logic

Implement the core parsing algorithm:

```rust,ignore
fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
    let mut builder = GreenNodeBuilder::new();
    let mut errors = Vec::new();
    let mut tokens = input.iter();
    
    // Parse according to algorithm
    // ...
    
    ParseResult {
        root: builder.finish().unwrap(),
        errors,
        warnings: Vec::new(),
        metrics: ParseMetrics::default(),
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: PhantomData,
    }
}
```

### 4. Implement Incremental Parsing

Support incremental parsing:

```rust,ignore
fn parse_with_session(
    &mut self,
    input: &[T],
    entry: N,
    session: &IncrementalSession<'_, T::Kind>,
) -> ParseResult<T, N> {
    // Find reusable nodes
    // Re-parse only affected regions
    // Combine reused and new nodes
    // ...
}
```

### 5. Implement Validation

Validate grammar compatibility:

```rust,ignore
fn validate(grammar: &Grammar<T, N>) -> Vec<GrammarError<T, N>> {
    let mut errors = Vec::new();
    
    // Check for left recursion
    // Check for ambiguity
    // Check for other requirements
    // ...
    
    errors
}
```

## Example: PEG Parser

Here's a simplified example of a PEG parser:

```rust,ignore
pub struct PegParser<T, N> {
    grammar: Grammar<T, N>,
    config: PegConfig,
    state: PegState,
}

impl<T, N> ParserBackend<T, N> for PegParser<T, N>
where
    T: Token,
    N: NonTerminal,
{
    type Config = PegConfig;
    type Error = PegError;
    type State = PegState;

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        Ok(Self {
            grammar: grammar.clone(),
            config,
            state: PegState::new(),
        })
    }

    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        // PEG parsing logic
        // ...
    }

    // ... other methods
}
```

## Best Practices

1. **Follow existing patterns**: Look at LL/LR/GLR implementations
2. **Handle errors gracefully**: Return errors, don't panic
3. **Support incremental**: Implement incremental parsing if possible
4. **Validate grammar**: Check grammar compatibility
5. **Document behavior**: Document algorithm-specific behavior

## Next Steps

- See existing backends for reference:
  - [LL Parser](../../crates/sipha/src/backend/ll/)
  - [LR Parser](../../crates/sipha/src/backend/lr/)
  - [GLR Parser](../../crates/sipha/src/backend/glr/)
- Check [Contributing](contributing.md) for development workflow

