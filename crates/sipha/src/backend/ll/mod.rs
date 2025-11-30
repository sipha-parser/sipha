mod config;
mod grammar;
mod optimizer;
mod parser;
mod recovery;
mod table;
mod transformer;

pub use config::LlConfig;
pub use grammar::LlGrammar;
pub use optimizer::LlOptimizer;
pub use parser::LlParserState;
pub use recovery::LlRecoveryStrategy;
pub use transformer::LlTransformer;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// LL(k) parser backend for top-down predictive parsing.
///
/// LL parsers are efficient for many grammar types and support configurable
/// lookahead depth (k). They work well for languages with clear hierarchical
/// structure and are particularly suited for incremental parsing.
///
/// # Example
///
/// ```rust,ignore
/// use sipha::backend::ll::{LlParser, LlConfig};
/// use sipha::backend::ParserBackend;
///
/// // Setup grammar and tokens...
/// let config = LlConfig::default();
/// let parser = LlParser::new(&grammar, config)?;
/// let result = parser.parse(&tokens, entry_point);
/// ```
pub struct LlParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    backend_grammar: LlGrammar<T, N>,
    state: LlParserState<T, N>,
    config: LlConfig,
}

#[derive(Debug, thiserror::Error)]
pub enum LlError {
    #[error("Grammar is not LL(k) compatible: {0}")]
    NotLlGrammar(String),

    #[error("Failed to build parsing table: {0}")]
    TableConstructionFailed(String),
}

impl<T, N> ParserBackend<T, N> for LlParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    type Config = LlConfig;
    type Error = LlError;
    type State = LlParserState<T, N>;
    type BackendGrammar = LlGrammar<T, N>;

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
                opts.insert("lookahead_k".to_string(), config.lookahead.to_string());
                opts
            },
        };

        let backend_grammar = if config.optimize {
            GrammarTransformPipeline::transform_with_optimizer::<T, N, LlTransformer, LlOptimizer>(
                grammar,
                &transform_config,
                LlOptimizer,
            )
            .map_err(|e| LlError::TableConstructionFailed(e.to_string()))?
        } else {
            GrammarTransformPipeline::transform::<T, N, LlTransformer>(grammar, &transform_config)
                .map_err(|e| LlError::TableConstructionFailed(e.to_string()))?
        };

        Ok(Self {
            backend_grammar,
            state: LlParserState::with_cache_settings(config.max_cache_size, config.cache_history),
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

        // Use range-based cache invalidation for better performance
        let affected = session.affected();
        let union_range = affected.union();
        self.state.invalidate_cache_range(union_range);

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
        use crate::grammar::GrammarError;
        let mut errors = Vec::new();

        // Check for left recursion (reuse existing validation)
        // Collect rules into a Vec (Rule is now Clone)
        let rules: Vec<_> = grammar.rules().map(|(_, r)| r.clone()).collect();
        if let Err(e) = crate::grammar::validate_grammar(&rules) {
            match e {
                GrammarError::LeftRecursion(_) | GrammarError::UndefinedRule(_) => errors.push(e),
                _ => {}
            }
        }

        // Check for FIRST/FIRST conflicts in choices
        for (lhs, rule) in grammar.rules() {
            if let crate::grammar::ExtendedExpr::Core(crate::grammar::CoreExpr::Choice(
                alternatives,
            )) = &rule.rhs
            {
                for (i, alt1) in alternatives.iter().enumerate() {
                    let first1 = alt1.first_set(grammar);
                    for (j, alt2) in alternatives.iter().enumerate().skip(i + 1) {
                        let first2 = alt2.first_set(grammar);
                        let conflict: Vec<T> = first1.intersection(&first2).cloned().collect();
                        if !conflict.is_empty() {
                            errors.push(GrammarError::FirstFirstConflict {
                                rule: lhs.clone(),
                                alt1: i,
                                alt2: j,
                                tokens: conflict,
                            });
                        }
                    }
                }
            }
        }

        // Check for FIRST/FOLLOW conflicts (for nullable rules)
        let follow_sets = grammar.compute_follow_sets();
        for (lhs, rule) in grammar.rules() {
            if rule.rhs.is_nullable(grammar) {
                let first = rule.rhs.first_set(grammar);
                if let Some(follow) = follow_sets.get(lhs) {
                    let conflict: Vec<T> = first.intersection(follow).cloned().collect();
                    if !conflict.is_empty() {
                        // This is a FIRST/FOLLOW conflict for nullable rules
                        errors.push(crate::grammar::GrammarError::FirstFollowConflict {
                            rule: lhs.clone(),
                            tokens: conflict,
                        });
                    }
                }
            }
        }

        errors
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "LL(k)",
            algorithm: Algorithm::LL,
            supports_left_recursion: false,
            supports_ambiguity: false,
            supports_incremental: true,
            supports_error_recovery: true,
            max_lookahead: None, // Supports any k (configurable)
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}
