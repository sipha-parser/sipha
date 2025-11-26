mod table;
mod parser;
mod config;

pub use config::LlConfig;
use table::ParsingTable;
pub use parser::LlParserState;

use crate::grammar::{Grammar, Token};
use crate::backend::{ParserBackend, BackendCapabilities, Algorithm};
use crate::error::ParseResult;

/// LL(k) parser backend
pub struct LlParser<T, N>
where
    T: crate::grammar::Token,
    N: crate::grammar::NonTerminal,
{
    grammar: Grammar<T, N>,
    table: ParsingTable<T, N>,
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
    
    fn new(grammar: &Grammar<T, N>, config: Self::Config) -> Result<Self, Self::Error> {
        // Validate grammar for LL compatibility
        let errors = Self::validate(grammar);
        if !errors.is_empty() {
            return Err(LlError::NotLlGrammar(
                errors.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        
        // Build parsing table
        let table = ParsingTable::new(grammar, config.lookahead)
            .map_err(LlError::TableConstructionFailed)?;
        
        Ok(Self {
            grammar: grammar.clone(),
            table,
            state: LlParserState::new(),
            config,
        })
    }
    
    fn parse(&mut self, input: &[T], entry: N) -> ParseResult<T, N> {
        parser::parse(&self.grammar, &self.table, input, &entry, &self.config, &mut self.state)
    }
    
    fn parse_incremental(
        &mut self,
        input: &[T],
        old_tree: Option<&crate::syntax::GreenNode<T::Kind>>,
        edits: &[crate::incremental::TextEdit],
        entry: N,
    ) -> ParseResult<T, N>
    where
        T: Token,
    {
        // Invalidate cache if there are edits
        if !edits.is_empty() {
            self.state.invalidate_cache();
        }
        
        // Incremental parsing with cache invalidation
        // Note: Full node reuse is a future enhancement - basic infrastructure is in place
        parser::parse_incremental(
            &self.grammar,
            &self.table,
            input,
            old_tree,
            edits,
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
            if let crate::grammar::Expr::Choice(alternatives) = &rule.rhs {
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
                        // For now, we'll just note it - could add a specific error type
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

