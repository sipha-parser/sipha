//! GLR Parser Driver Adapter
//!
//! This module provides a `ParserDriver` implementation that wraps the GLR parser.

use crate::backend::glr::disambiguation::DisambiguationStrategy;
use crate::error::ParseResult;
use crate::grammar::{Grammar, NonTerminal, Token};
use std::sync::Arc;

/// Configuration for the GLR driver
#[derive(Debug, Clone, Default)]
pub struct GlrConfig {
    /// Maximum number of active parse stacks
    pub max_stacks: usize,
    /// Whether to collect the parse forest
    pub collect_forest: bool,
    /// Disambiguation configuration
    pub disambiguation: DisambiguationStrategy,
}

/// The state for the GLR parser driver
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct GlrDriverState {
    /// Multiple active stacks (for ambiguous parses)
    stacks: Vec<GlrStackState>,
    /// Current position in the token stream
    position: usize,
}

impl Default for GlrDriverState {
    fn default() -> Self {
        Self {
            stacks: vec![GlrStackState::default()],
            position: 0,
        }
    }
}

/// State of a single GLR stack
#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub struct GlrStackState {
    /// Stack of (`state_id`, `node_index`)
    stack: Vec<(usize, usize)>,
    /// Whether this stack is still active
    active: bool,
}

/// Decision made by the GLR driver
#[derive(Debug, Clone)]
pub enum GlrDecision {
    /// Shift token and go to states (may be multiple for ambiguous grammars)
    Shift { next_states: Vec<usize> },
    /// Reduce by rule on specific stacks
    Reduce {
        rule_index: usize,
        pop_count: usize,
        stack_indices: Vec<usize>,
    },
    /// Split the stack (for ambiguous parses)
    Split { new_stacks: Vec<GlrStackState> },
    /// Merge stacks
    Merge { stack_indices: Vec<usize> },
    /// Accept the parse
    Accept,
    /// Error - all stacks died
    Error,
}

/// GLR Parser Driver implementation
pub struct GlrDriver<T: Token, N: NonTerminal> {
    grammar: Arc<Grammar<T, N>>,
    config: GlrConfig,
}

impl<T: Token, N: NonTerminal> GlrDriver<T, N> {
    /// Create a new GLR driver
    pub fn new(grammar: Arc<Grammar<T, N>>, config: GlrConfig) -> Self {
        Self { grammar, config }
    }

    /// Parse using the GLR algorithm
    ///
    /// This method wraps the existing GLR parser functionality
    #[cfg(feature = "backend-lr")]
    pub fn parse(&self, input: &[T], entry: &N) -> ParseResult<T, N> {
        use crate::backend::glr::{GlrConfig as MainGlrConfig, GlrParserState};

        let main_config = MainGlrConfig {
            max_stacks: self.config.max_stacks,
            return_forest: self.config.collect_forest,
            disambiguation: self.config.disambiguation,
            ..Default::default()
        };

        let lr_config = crate::backend::lr::LrConfig::default();
        let table = crate::backend::lr::LrParsingTable::new(&self.grammar, lr_config.use_lalr)
            .expect("Failed to build LR table");
        let mut state = GlrParserState::new();

        super::parser::parse(
            &self.grammar,
            &table,
            input,
            entry,
            &main_config,
            &mut state,
        )
    }

    /// Parse using the GLR algorithm (stub when backend-lr is disabled)
    ///
    /// Returns a parse result with an error indicating that the backend-lr feature
    /// is required for GLR parsing.
    #[cfg(not(feature = "backend-lr"))]
    pub fn parse(&self, input: &[T], entry: &N) -> ParseResult<T, N> {
        use crate::error::{ParseError, ParseMetrics, ParseWarning};
        use crate::syntax::{GreenNode, TextRange, TextSize};
        
        // Create an error indicating the feature is required
        let error = ParseError::InvalidSyntax {
            span: TextRange::at(TextSize::zero(), TextSize::from(1)),
            message: "GLR backend requires the 'backend-lr' feature to be enabled. \
                      Add 'backend-lr' to your Cargo.toml features.".to_string(),
        };
        
        // Try to get a syntax kind for the error root node
        let error_kind = input
            .first()
            .map(crate::grammar::Token::kind)
            .or_else(|| entry.default_syntax_kind())
            .or_else(|| self.grammar.try_get_fallback_kind::<T::Kind>())
            .unwrap_or_else(|| {
                // If we can't determine a kind, we still need to return something
                // This should rarely happen in practice
                entry.default_syntax_kind().unwrap_or_else(|| {
                    // Last resort: use the first token kind from input if available
                    input.first().map(crate::grammar::Token::kind).unwrap_or_else(|| {
                        // This is a fallback that should never be reached in practice
                        // but we need to satisfy the type system
                        panic!("Cannot create error result: no syntax kind available. \
                                This indicates a configuration error. \
                                Ensure NonTerminal::default_syntax_kind() returns Some.")
                    })
                })
            });
        
        ParseResult {
            root: std::sync::Arc::new(GreenNode::new(error_kind, [], TextSize::zero())),
            errors: vec![error],
            warnings: Vec::<ParseWarning>::new(),
            metrics: ParseMetrics::default(),
            #[cfg(feature = "backend-glr")]
            forest: None,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Get the underlying grammar
    #[must_use]
    pub fn grammar(&self) -> &Grammar<T, N> {
        &self.grammar
    }

    /// Get the configuration
    #[must_use]
    pub const fn config(&self) -> &GlrConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    // Tests would require full grammar and token setup
}
