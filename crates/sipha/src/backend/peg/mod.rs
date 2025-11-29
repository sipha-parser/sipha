mod config;
mod parser;
mod state;

pub use config::PegConfig;
pub use state::PegParserState;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// PEG (Parsing Expression Grammar) parser backend.
///
/// PEG parsers use ordered choice (first match wins) and backtracking with
/// memoization (packrat parsing) for efficient parsing. PEG parsers are
/// particularly well-suited for:
///
/// - Languages with clear precedence rules
/// - Parsing that benefits from ordered choice semantics
/// - Grammars that can benefit from memoization
///
/// # Example
///
/// ```rust,ignore
/// use sipha::backend::peg::{PegParser, PegConfig};
/// use sipha::backend::ParserBackend;
///
/// // Setup grammar and tokens...
/// let config = PegConfig::default();
/// let parser = PegParser::new(&grammar, config)?;
/// let result = parser.parse(&tokens, entry_point);
/// ```
pub struct PegParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    grammar: Grammar<T, N>,
    state: PegParserState<T, N>,
    config: PegConfig,
}

#[derive(Debug, thiserror::Error)]
pub enum PegError {
    #[error("Grammar is not compatible with PEG parsing: {0}")]
    NotPegGrammar(String),

    #[error("Failed to initialize PEG parser: {0}")]
    InitializationFailed(String),
}

impl<T, N> ParserBackend<T, N> for PegParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    type Config = PegConfig;
    type Error = PegError;
    type State = PegParserState<T, N>;

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        // Validate grammar for PEG compatibility
        let errors = Self::validate(grammar);
        if !errors.is_empty() {
            return Err(PegError::NotPegGrammar(
                errors
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        Ok(Self {
            grammar: grammar.clone(),
            state: PegParserState::new(),
            config,
        })
    }

    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        parser::parse(&self.grammar, input, &entry, &self.config, &mut self.state)
    }

    fn parse_with_session(
        &mut self,
        input: &[T],
        entry: N,
        session: &crate::incremental::IncrementalSession<'_, T::Kind>,
    ) -> ParseResult<T, N>
    where
        T: Token,
    {
        if session.edits().is_empty() {
            return self.parse(input, entry);
        }

        // Invalidate cache for affected regions
        // For proper implementation, we'd need to track text positions in cache keys
        // For now, we invalidate the entire cache when there are edits
        self.state.invalidate_cache(self.config.max_memo_size);

        // Use incremental parsing with session support
        parser::parse_with_session(
            &self.grammar,
            input,
            &entry,
            &self.config,
            &mut self.state,
            Some(session),
        )
    }

    fn validate(grammar: &Grammar<T, N>) -> Vec<crate::grammar::GrammarError<T, N>> {
        let mut errors = Vec::new();

        // PEG parsers can handle most grammars, including left recursion
        // (though left recursion in PEG requires special handling)
        // Only check for undefined rules
        for (_lhs, rule) in grammar.rules() {
            // Check if rule references undefined non-terminals
            crate::backend::common::check_undefined_references(grammar, &rule.rhs, &mut errors);
        }

        errors
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "PEG",
            algorithm: Algorithm::PEG,
            supports_left_recursion: true, // PEG can handle left recursion with special techniques
            supports_ambiguity: false,     // PEG uses ordered choice (no ambiguity)
            supports_incremental: true,    // Can support incremental parsing with memoization
            supports_error_recovery: true,
            max_lookahead: None, // PEG doesn't use fixed lookahead
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}

impl<T, N> PegParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
}
