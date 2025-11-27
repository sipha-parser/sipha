mod config;
mod parser;
mod state;
mod table;

pub use config::LrConfig;
pub use state::LrParserState;
use table::LrParsingTable;

use crate::backend::{Algorithm, BackendCapabilities, ParserBackend};
use crate::error::ParseResult;
use crate::grammar::{Grammar, Token};

/// LR(1) parser backend
pub struct LrParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    grammar: Grammar<T, N>,
    table: LrParsingTable<T, N>,
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

    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        // Validate grammar for LR compatibility
        let errors = Self::validate(grammar);
        if !errors.is_empty() {
            return Err(LrError::NotLrGrammar(
                errors
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        // Build parsing table
        let table = LrParsingTable::new(grammar, config.use_lalr)
            .map_err(LrError::TableConstructionFailed)?;

        Ok(Self {
            grammar: grammar.clone(),
            table,
            state: LrParserState::new(),
            config,
        })
    }

    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        parser::parse(
            &self.grammar,
            &self.table,
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
            &self.grammar,
            &self.table,
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
            Self::check_undefined_references(grammar, &rule.rhs, &mut errors);
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
    /// Check for undefined non-terminal references
    fn check_undefined_references(
        grammar: &Grammar<T, N>,
        expr: &crate::grammar::Expr<T, N>,
        errors: &mut Vec<crate::grammar::GrammarError<T, N>>,
    ) {
        match expr {
            crate::grammar::Expr::Rule(nt) => {
                if grammar.get_rule(nt).is_none() {
                    errors.push(crate::grammar::GrammarError::UndefinedRule(nt.clone()));
                }
            }
            crate::grammar::Expr::Seq(exprs) | crate::grammar::Expr::Choice(exprs) => {
                for e in exprs {
                    Self::check_undefined_references(grammar, e, errors);
                }
            }
            crate::grammar::Expr::Opt(e) | crate::grammar::Expr::Repeat { expr: e, .. } => {
                Self::check_undefined_references(grammar, e, errors);
            }
            crate::grammar::Expr::Separated {
                item, separator, ..
            } => {
                Self::check_undefined_references(grammar, item, errors);
                Self::check_undefined_references(grammar, separator, errors);
            }
            crate::grammar::Expr::Delimited {
                open,
                content,
                close,
                ..
            } => {
                Self::check_undefined_references(grammar, open, errors);
                Self::check_undefined_references(grammar, content, errors);
                Self::check_undefined_references(grammar, close, errors);
            }
            crate::grammar::Expr::Node { expr, .. }
            | crate::grammar::Expr::Label { expr, .. }
            | crate::grammar::Expr::Flatten(expr)
            | crate::grammar::Expr::Prune(expr)
            | crate::grammar::Expr::Lookahead(expr)
            | crate::grammar::Expr::NotLookahead(expr)
            | crate::grammar::Expr::RecoveryPoint { expr, .. } => {
                Self::check_undefined_references(grammar, expr, errors);
            }
            _ => {}
        }
    }
}
