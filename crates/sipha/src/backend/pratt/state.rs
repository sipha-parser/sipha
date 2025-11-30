use crate::grammar::{NonTerminal, Token};

/// Parser state for Pratt parser.
///
/// Pratt parsers use recursive descent with operator precedence, so the state
/// is relatively simple compared to table-driven parsers.
#[derive(Debug)]
pub struct PrattParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Current position in the input
    pub position: usize,
    /// Error count
    pub error_count: usize,
    _phantom: std::marker::PhantomData<(T, N)>,
}

impl<T, N> PrattParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            position: 0,
            error_count: 0,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Reset the state for a new parse
    pub fn reset(&mut self) {
        self.position = 0;
        self.error_count = 0;
    }
}

impl<T, N> Default for PrattParserState<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}
