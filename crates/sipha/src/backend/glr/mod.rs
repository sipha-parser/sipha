//! # GLR (Generalized LR) Parser Backend
//!
//! This module implements a GLR parser using Tomita's algorithm to handle
//! non-deterministic and ambiguous grammars by forking the parser stack on conflicts.
//!
//! ## Overview
//!
//! GLR parsing extends LR parsing to handle ambiguous grammars by maintaining
//! multiple parser stacks and forking on conflicts. This allows parsing of
//! complex languages like C++ that have inherent ambiguities.
//!
//! ## Algorithm
//!
//! The implementation uses Tomita's algorithm:
//! 1. Build LR(1) state machine (reuses `backend/lr/table.rs`)
//! 2. Maintain multiple parser stacks
//! 3. On conflicts, fork stacks to explore all possibilities
//! 4. Merge stacks when they converge
//! 5. Return parse forest for ambiguous results

mod disambiguation;
mod forest;
mod grammar;
mod optimizer;
mod parser;
mod recovery;
mod stack;
mod state;
mod transformer;

pub use disambiguation::{
    DisambiguationStrategy, Disambiguator, disambiguate_by_associativity,
    disambiguate_by_precedence, disambiguate_with_custom,
};
pub use forest::{ForestNode, ParseForest};
pub use grammar::GlrGrammar;
pub use optimizer::GlrOptimizer;
pub use recovery::GlrRecoveryStrategy;
pub use stack::GlrStack;
pub use state::GlrParserState;
pub use transformer::GlrTransformer;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// GLR (Generalized LR) parser backend for handling ambiguous grammars.
///
/// The GLR parser extends LR parsing to handle non-deterministic and ambiguous
/// grammars by maintaining multiple parser stacks and forking on conflicts.
/// This makes it ideal for parsing complex languages like C++ that have
/// inherent ambiguities.
///
/// # Example
///
/// ```rust,ignore
/// use sipha::backend::glr::{GlrParser, GlrConfig};
/// use sipha::backend::ParserBackend;
///
/// // Setup grammar and tokens...
/// let config = GlrConfig::default();
/// let parser = GlrParser::new(&grammar, config)?;
/// let result = parser.parse(&tokens, entry_point);
/// ```
pub struct GlrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    backend_grammar: GlrGrammar<T, N>,
    config: GlrConfig,
    state: GlrParserState<T, N>,
}

/// Tuning knobs for the GLR backend.
#[derive(Debug, Clone)]
pub struct GlrConfig {
    /// Maximum number of parallel stacks to maintain
    pub max_stacks: usize,
    /// Whether to return parse forest for ambiguous results
    pub return_forest: bool,
    /// Disambiguation strategy to use
    pub disambiguation: DisambiguationStrategy,
    /// Maximum depth of ambiguity to track (prevents explosion)
    pub max_ambiguity_depth: usize,
    /// Strategy used when pruning stacks beyond `max_stacks`.
    pub pruning_strategy: StackPruningStrategy,
    /// Beam width used during pruning (upper bound on retained stacks).
    pub pruning_beam_width: usize,
    /// Minimum stack count required before attempting parallel merges.
    ///
    /// Parallel merges are only attempted when the `parallel` feature is enabled.
    pub parallel_threshold: usize,

    /// Enable optimizations during transformation
    pub optimize: bool,

    /// Optimization level for grammar transformation
    pub optimization_level: crate::grammar::hint::OptimizationLevel,
}

impl Default for GlrConfig {
    fn default() -> Self {
        Self {
            max_stacks: 1000,
            return_forest: false,
            disambiguation: DisambiguationStrategy::default(),
            max_ambiguity_depth: 10,
            pruning_strategy: StackPruningStrategy::QualityWeighted,
            pruning_beam_width: 512,
            parallel_threshold: 64,
            optimize: false,
            optimization_level: crate::grammar::hint::OptimizationLevel::None,
        }
    }
}

/// Strategy used to rank stacks when pruning the frontier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackPruningStrategy {
    /// Disable priority pruning (fall back to insertion order).
    None,
    /// Prefer stacks with deeper reductions (depth-first bias).
    PreferDeeper,
    /// Prefer stacks that have consumed more tokens (progress-first bias).
    PreferProgress,
    /// Combine depth, progress, and error counts into a weighted score.
    QualityWeighted,
}

#[derive(Debug, thiserror::Error)]
pub enum GlrError {
    #[error("Grammar is not compatible with GLR parsing: {0}")]
    NotGlrGrammar(String),

    #[error("Failed to build parsing table: {0}")]
    TableConstructionFailed(String),
}

impl<T, N> ParserBackend<T, N> for GlrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    type Config = GlrConfig;
    type Error = GlrError;
    type State = GlrParserState<T, N>;
    type BackendGrammar = GlrGrammar<T, N>;

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
                opts.insert("use_lalr".to_string(), "true".to_string());
                opts
            },
        };

        let backend_grammar = if config.optimize {
            GrammarTransformPipeline::transform_with_optimizer::<T, N, GlrTransformer, GlrOptimizer>(
                grammar,
                &transform_config,
                GlrOptimizer,
            )
            .map_err(|e| GlrError::TableConstructionFailed(e.to_string()))?
        } else {
            GrammarTransformPipeline::transform::<T, N, GlrTransformer>(grammar, &transform_config)
                .map_err(|e| GlrError::TableConstructionFailed(e.to_string()))?
        };

        Ok(Self {
            backend_grammar,
            config,
            state: GlrParserState::new(),
        })
    }

    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        #[cfg(feature = "backend-lr")]
        {
            parser::parse(
                &self.backend_grammar,
                input,
                &entry,
                &self.config,
                &mut self.state,
            )
        }
        #[cfg(not(feature = "backend-lr"))]
        {
            unreachable!("GLR requires backend-lr feature")
        }
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
        #[cfg(feature = "backend-lr")]
        {
            if session.edits().is_empty() {
                return self.parse(input, entry);
            }

            parser::parse_with_session(
                &self.backend_grammar,
                input,
                session,
                &entry,
                &self.config,
                &mut self.state,
            )
        }
        #[cfg(not(feature = "backend-lr"))]
        {
            unreachable!("GLR requires backend-lr feature")
        }
    }

    fn validate(grammar: &Grammar<T, N>) -> Vec<crate::grammar::GrammarError<T, N>> {
        // GLR can handle most grammars, including ambiguous ones
        // Only check for undefined rules
        let mut errors = Vec::new();
        for (_lhs, rule) in grammar.rules() {
            // Check if rule references undefined non-terminals
            crate::backend::common::check_undefined_references(grammar, &rule.rhs, &mut errors);
        }
        errors
    }

    fn capabilities() -> BackendCapabilities {
        BackendCapabilities {
            name: "GLR",
            algorithm: Algorithm::GLR,
            supports_left_recursion: true,
            supports_ambiguity: true,   // GLR's main feature
            supports_incremental: true, // Incremental GLR parsing with forest caching
            supports_error_recovery: true,
            max_lookahead: Some(1),
        }
    }

    fn state(&self) -> &Self::State {
        &self.state
    }
}

impl<T, N> GlrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
}
