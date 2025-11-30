mod config;
mod grammar;
mod optimizer;
mod parser;
mod recovery;
mod state;
mod transformer;

pub use config::PrattConfig;
pub use grammar::{OperatorInfo, PrattGrammar};
pub use optimizer::PrattOptimizer;
pub use recovery::PrattRecoveryStrategy;
pub use state::PrattParserState;
pub use transformer::PrattTransformer;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// Pratt parser backend for operator precedence parsing.
///
/// Pratt parsers use recursive descent with operator precedence, making them
/// ideal for parsing expressions with operators. They handle operator precedence
/// naturally through precedence levels, avoiding the need for complex grammar
/// transformations.
///
/// # Example
///
/// ```rust,ignore
/// use sipha::backend::pratt::{PrattParser, PrattConfig};
/// use sipha::backend::ParserBackend;
///
/// // Setup grammar with PrattOperator expressions...
/// let config = PrattConfig::default();
/// let parser = PrattParser::new(&grammar, config)?;
/// let result = parser.parse(&tokens, entry_point);
/// ```
pub struct PrattParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    backend_grammar: PrattGrammar<T, N>,
    state: PrattParserState<T, N>,
    config: PrattConfig,
}

#[derive(Debug, thiserror::Error)]
pub enum PrattError {
    #[error("Grammar is not compatible with Pratt parsing: {0}")]
    NotPrattGrammar(String),

    #[error("Failed to initialize Pratt parser: {0}")]
    InitializationFailed(String),
}

impl<T, N> ParserBackend<T, N> for PrattParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    type Config = PrattConfig;
    type Error = PrattError;
    type State = PrattParserState<T, N>;
    type BackendGrammar = PrattGrammar<T, N>;

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        use crate::backend::pipeline::GrammarTransformPipeline;
        use crate::backend::traits::TransformConfig;

        // Transform grammar using the transformation pipeline
        let transform_config = TransformConfig {
            optimize: config.optimize,
            optimization_level: config.optimization_level,
            cache: true,
            backend_options: std::collections::HashMap::new(),
        };

        let backend_grammar = if config.optimize {
            GrammarTransformPipeline::transform_with_optimizer::<
                T,
                N,
                PrattTransformer,
                PrattOptimizer,
            >(grammar, &transform_config, PrattOptimizer)
            .map_err(|e| PrattError::InitializationFailed(e.to_string()))?
        } else {
            GrammarTransformPipeline::transform::<T, N, PrattTransformer>(
                grammar,
                &transform_config,
            )
            .map_err(|e| PrattError::InitializationFailed(e.to_string()))?
        };

        Ok(Self {
            backend_grammar,
            state: PrattParserState::new(),
            config,
        })
    }

    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        parser::parse(
            &self.backend_grammar,
            input,
            &entry,
            &self.config,
            &mut self.state,
        )
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

        // Reset state for incremental parsing
        self.state.reset();

        // Use incremental parsing with session support
        parser::parse_with_session(
            &self.backend_grammar,
            input,
            &entry,
            &self.config,
            &mut self.state,
            Some(session),
        )
    }

    fn validate(grammar: &Grammar<T, N>) -> Vec<crate::grammar::GrammarError<T, N>> {
        let mut errors = Vec::new();

        // Pratt parsers can handle most grammars
        // Check for undefined rules
        for (_lhs, rule) in grammar.rules() {
            // Check if rule references undefined non-terminals
            crate::backend::common::check_undefined_references(grammar, &rule.rhs, &mut errors);
        }

        errors
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "Pratt",
            algorithm: Algorithm::Pratt,
            supports_left_recursion: true, // Pratt handles left recursion via precedence
            supports_ambiguity: false,     // Pratt uses precedence to disambiguate
            supports_incremental: true,    // Can support incremental parsing
            supports_error_recovery: true,
            max_lookahead: Some(1), // Pratt typically uses single-token lookahead
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}

impl<T, N> PrattParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
}
