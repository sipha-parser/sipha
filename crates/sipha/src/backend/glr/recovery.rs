//! GLR error recovery strategy
//!
//! Implements error recovery for GLR parsers using:
//! - Token skipping across multiple stacks
//! - Stack pruning on errors
//! - Recovery point synchronization

use crate::backend::glr::grammar::GlrGrammar;
use crate::backend::traits::{
    ErrorRecoveryStrategy, RecoveryAction, RecoveryCapabilities, RecoveryContext, RecoveryFailed,
};
use crate::error::ParseError;
use crate::grammar::{NonTerminal, Token};

/// GLR error recovery strategy
pub struct GlrRecoveryStrategy;

impl<T, N> ErrorRecoveryStrategy<T, N> for GlrRecoveryStrategy
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
            .downcast_ref::<GlrGrammar<T, N>>()
            .ok_or_else(|| RecoveryFailed {
                reason: "Parser state does not contain GLR grammar".to_string(),
            })?;

        // GLR parsers handle errors differently - they can maintain multiple stacks
        // and prune stacks that hit errors
        match error {
            ParseError::UnexpectedToken { expected, .. } => {
                // Try to skip tokens until we find an expected one
                // GLR can try this on multiple stacks
                if !expected.is_empty() {
                    Ok(RecoveryAction::SkipToSyncPoint {
                        tokens: vec![], // Would need to convert expected strings to tokens
                    })
                } else {
                    // Skip token and let GLR prune stacks naturally
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
                // GLR can try this on multiple stacks
                Ok(RecoveryAction::SkipToken)
            }
            ParseError::Ambiguity { .. } => {
                // GLR handles ambiguity natively, but if we get here,
                // it might be an error in disambiguation
                // Skip token and let GLR handle it
                Ok(RecoveryAction::SkipToken)
            }
        }
    }

    fn capabilities(&self) -> RecoveryCapabilities {
        RecoveryCapabilities {
            can_skip_tokens: true,
            can_insert_tokens: true,
            can_skip_to_sync: true,
            can_delete_tokens: false, // GLR doesn't support deletion
            can_insert_expected: true,
        }
    }
}
