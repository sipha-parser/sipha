mod config;
mod grammar;
mod optimizer;
mod parser;
mod recovery;
mod state;
mod table;
mod transformer;

pub use config::LrConfig;
pub use grammar::LrGrammar;
pub use optimizer::LrOptimizer;
pub use recovery::LrRecoveryStrategy;
pub use state::LrParserState;
pub use table::{Action, LrParsingTable, Production};
pub use transformer::LrTransformer;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// LR(1) / LALR(1) parser backend for bottom-up shift-reduce parsing.
///
/// LR parsers are powerful and can handle left-recursive grammars. They use
/// a parsing table to efficiently process input. The backend supports both
/// canonical LR(1) and LALR(1) table construction (LALR is the default as it's
/// more practical for most grammars).
///
/// # Example
///
/// ```rust,ignore
/// use sipha::backend::lr::{LrParser, LrConfig};
/// use sipha::backend::ParserBackend;
///
/// // Setup grammar and tokens...
/// let config = LrConfig::default();
/// let parser = LrParser::new(&grammar, config)?;
/// let result = parser.parse(&tokens, entry_point);
/// ```
pub struct LrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    backend_grammar: LrGrammar<T, N>,
    state: LrParserState<T, N>,
    config: LrConfig,
}

#[derive(Debug, thiserror::Error)]
pub enum LrError {
    #[error("Grammar is not LR(1) compatible: {0}")]
    NotLrGrammar(String),

    #[error("Failed to build parsing table: {0}")]
    TableConstructionFailed(String),

    #[error("Grammar contains unsupported constructs: {0}")]
    UnsupportedConstruct(String),
}

impl<T, N> ParserBackend<T, N> for LrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    type Config = LrConfig;
    type Error = LrError;
    type State = LrParserState<T, N>;
    type BackendGrammar = LrGrammar<T, N>;

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        use crate::backend::pipeline::GrammarTransformPipeline;
        use crate::backend::traits::TransformConfig;

        // Transform grammar using the transformation pipeline
        let transform_config = TransformConfig {
            optimize: config.optimize,
            optimization_level: config.optimization_level,
            cache: true,
            backend_options: {
                let mut opts = std::collections::HashMap::new();
                opts.insert("use_lalr".to_string(), config.use_lalr.to_string());
                opts
            },
        };

        let backend_grammar = if config.optimize {
            GrammarTransformPipeline::transform_with_optimizer::<T, N, LrTransformer, LrOptimizer>(
                grammar,
                &transform_config,
                LrOptimizer,
            )
            .map_err(|e| LrError::TableConstructionFailed(e.to_string()))?
        } else {
            GrammarTransformPipeline::transform::<T, N, LrTransformer>(grammar, &transform_config)
                .map_err(|e| LrError::TableConstructionFailed(e.to_string()))?
        };

        Ok(Self {
            backend_grammar,
            state: LrParserState::new(),
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

        self.state.invalidate_cache();

        parser::parse_with_session(
            &self.backend_grammar,
            input,
            session,
            &entry,
            &self.config,
            &mut self.state,
        )
    }

    fn validate(grammar: &Grammar<T, N>) -> Vec<crate::grammar::GrammarError<T, N>> {
        let mut errors = Vec::new();

        // LR parsers can handle left recursion, so we don't check for that
        // But we should check for other issues

        // Check for undefined rules
        for (_lhs, rule) in grammar.rules() {
            // Check if rule references undefined non-terminals
            crate::backend::common::check_undefined_references(grammar, &rule.rhs, &mut errors);
        }

        errors
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "LR(1)/LALR(1)",
            algorithm: Algorithm::LR,
            supports_left_recursion: true, // LR can handle left recursion
            supports_ambiguity: false,
            supports_incremental: true,
            supports_error_recovery: true,
            max_lookahead: Some(1), // LR(1) or LALR(1)
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}

impl<T, N> LrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
}
