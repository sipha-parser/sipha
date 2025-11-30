//! LL error recovery strategy
//!
//! Implements error recovery for LL parsers using:
//! - Token skipping
//! - Token insertion
//! - Panic mode recovery

use crate::backend::ll::grammar::LlGrammar;
use crate::backend::traits::{
    ErrorRecoveryStrategy, RecoveryAction, RecoveryCapabilities, RecoveryContext, RecoveryFailed,
};
use crate::error::ParseError;
use crate::grammar::{NonTerminal, Token};

/// LL error recovery strategy
pub struct LlRecoveryStrategy;

impl<T, N> ErrorRecoveryStrategy<T, N> for LlRecoveryStrategy
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    fn recover(
        &self,
        error: &ParseError,
        context: &RecoveryContext<T>,
    ) -> Result<RecoveryAction<T>, RecoveryFailed> {
        // Try to get the grammar from parser state
        let _grammar = context
            .parser_state
            .downcast_ref::<LlGrammar<T, N>>()
            .ok_or_else(|| RecoveryFailed {
                reason: "Parser state does not contain LL grammar".to_string(),
            })?;

        // Use a simple strategy based on error type
        match error {
            ParseError::UnexpectedToken { expected, .. } => {
                // Try to skip tokens until we find an expected one
                if !expected.is_empty() {
                    Ok(RecoveryAction::SkipToSyncPoint {
                        tokens: vec![], // Would need to convert expected strings to tokens
                    })
                } else {
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
                // Skip to next synchronization point
                Ok(RecoveryAction::SkipToken)
            }
            ParseError::Ambiguity { .. } => {
                // Ambiguity errors can't be recovered from
                Err(RecoveryFailed {
                    reason: "Ambiguity errors cannot be recovered".to_string(),
                })
            }
        }
    }

    fn capabilities(&self) -> RecoveryCapabilities {
        RecoveryCapabilities {
            can_skip_tokens: true,
            can_insert_tokens: true,
            can_skip_to_sync: true,
            can_delete_tokens: false, // LL doesn't support deletion
            can_insert_expected: true,
        }
    }
}
