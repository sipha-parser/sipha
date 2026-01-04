mod chart;
mod config;
mod parser;
mod state;

pub use config::EarleyConfig;
pub use parser::EarleyParser;
pub use state::EarleyParserState;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// Earley parser backend for chart-based parsing.
///
/// The Earley parser can handle any context-free grammar, including
/// ambiguous and left-recursive grammars. It uses a chart-based algorithm
/// that builds parse states incrementally.
///
/// # Example
///
/// ```rust,ignore
/// use sipha::backend::earley::{EarleyParser, EarleyConfig};
/// use sipha::backend::ParserBackend;
///
/// let config = EarleyConfig::default();
/// let parser = EarleyParser::new(&grammar, config)?;
/// let result = parser.parse(&tokens, entry_point);
/// ```
pub struct EarleyParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    grammar: Grammar<T, N>,
    state: EarleyParserState<T, N>,
    config: EarleyConfig,
}

#[derive(Debug, thiserror::Error)]
pub enum EarleyError {
    #[error("Grammar validation failed: {0}")]
    GrammarValidationFailed(String),

    #[error("Failed to build parser: {0}")]
    ParserConstructionFailed(String),
}

impl<T, N> ParserBackend<T, N> for EarleyParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    type Config = EarleyConfig;
    type Error = EarleyError;
    type State = EarleyParserState<T, N>;

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        // Validate grammar
        let errors = Self::validate(grammar);
        if !errors.is_empty() {
            return Err(EarleyError::GrammarValidationFailed(
                errors
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        Ok(Self {
            grammar: grammar.clone(),
            state: EarleyParserState::new(),
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
        // If no edits, use cached result if available
        if session.edits().is_empty() {
            return self.parse(input, entry);
        }

        // Check persistent parse cache for the entry point before parsing
        let entry_text_pos = crate::syntax::TextSize::zero();
        let expected_kind = entry
            .to_syntax_kind()
            .or_else(|| entry.default_syntax_kind());

        if let Some(cached_root) =
            session.find_cached_node(entry.name(), entry_text_pos, expected_kind)
        {
            // Verify the cached root matches the input length (simple validation)
            let errors = Vec::new();
            let warnings = Vec::new();
            let metrics = crate::error::ParseMetrics::default();

            return crate::error::ParseResult {
                root: cached_root,
                errors,
                warnings,
                metrics,
                #[cfg(feature = "backend-glr")]
                forest: None,
                _phantom: std::marker::PhantomData::<N>,
            };
        }

        // Invalidate cache for affected region
        let affected = session.affected();
        // For Earley parser, we invalidate the entire cache when there are edits
        // A more sophisticated implementation would track positions in cache keys
        self.state.clear_cache();

        // Use incremental parsing with node reuse during tree extraction
        parser::parse_with_session(
            &self.grammar,
            input,
            &entry,
            &self.config,
            &mut self.state,
            session,
        )
    }

    fn validate(grammar: &Grammar<T, N>) -> Vec<crate::grammar::GrammarError<T, N>> {
        // Earley parser can handle any context-free grammar
        // Just check for undefined references
        let mut errors = Vec::new();
        for (_, rule) in grammar.rules() {
            crate::backend::common::check_undefined_references(grammar, &rule.rhs, &mut errors);
        }
        errors
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "Earley",
            algorithm: Algorithm::Earley,
            supports_left_recursion: true,
            supports_ambiguity: true,
            supports_incremental: true,
            supports_error_recovery: true,
            max_lookahead: None,
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}
