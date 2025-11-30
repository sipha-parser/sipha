//! Pratt error recovery strategy
//!
//! Implements error recovery for Pratt parsers using:
//! - Token skipping
//! - Synchronization points
//! - Expression-level recovery

use crate::backend::pratt::grammar::PrattGrammar;
use crate::backend::traits::{
    ErrorRecoveryStrategy, RecoveryAction, RecoveryCapabilities, RecoveryContext, RecoveryFailed,
};
use crate::error::ParseError;
use crate::grammar::{NonTerminal, Token};

/// Pratt error recovery strategy
pub struct PrattRecoveryStrategy;

impl<T, N> ErrorRecoveryStrategy<T, N> for PrattRecoveryStrategy
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    fn recover(
        &self,
        error: &ParseError,
        context: &RecoveryContext<T>,
    ) -> Result<RecoveryAction<T>, RecoveryFailed> {
        let _grammar = context
            .parser_state
            .downcast_ref::<PrattGrammar<T, N>>()
            .ok_or_else(|| RecoveryFailed {
                reason: "Parser state does not contain Pratt grammar".to_string(),
            })?;

        match error {
            ParseError::UnexpectedToken { expected, .. } => {
                // Try to skip tokens until we find an expected one
                if !expected.is_empty() {
                    Ok(RecoveryAction::SkipToSyncPoint {
                        tokens: vec![], // Would need to convert expected strings to tokens
                    })
                } else {
                    // Skip current token
                    Ok(RecoveryAction::SkipToken)
                }
            }
            ParseError::UnexpectedEof {
                expected: _expected,
                ..
            } => {
                // Insert expected tokens
                Ok(RecoveryAction::InsertExpected {
                    expected: vec![], // Would need to convert expected strings to tokens
                })
            }
            ParseError::InvalidSyntax { .. } => {
                // Skip current token and continue
                Ok(RecoveryAction::SkipToken)
            }
            ParseError::Ambiguity { .. } => {
                // Pratt parsers shouldn't have ambiguity, but if they do, skip token
                Ok(RecoveryAction::SkipToken)
            }
        }
    }

    fn capabilities(&self) -> RecoveryCapabilities {
        RecoveryCapabilities {
            can_skip_tokens: true,
            can_insert_tokens: true,
            can_skip_to_sync: true,
            can_delete_tokens: false,
            can_insert_expected: true,
        }
    }
}
