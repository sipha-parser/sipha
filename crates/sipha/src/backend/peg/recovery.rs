//! PEG error recovery strategy
//!
//! Implements error recovery for PEG parsers using:
//! - Token skipping
//! - Backtracking with recovery points
//! - Cut operator for error boundaries

use crate::backend::peg::grammar::PegGrammar;
use crate::backend::traits::{
    ErrorRecoveryStrategy, RecoveryAction, RecoveryCapabilities, RecoveryContext, RecoveryFailed,
};
use crate::error::ParseError;
use crate::grammar::{NonTerminal, Token};

/// PEG error recovery strategy
pub struct PegRecoveryStrategy;

impl<T, N> ErrorRecoveryStrategy<T, N> for PegRecoveryStrategy
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
            .downcast_ref::<PegGrammar<T, N>>()
            .ok_or_else(|| RecoveryFailed {
                reason: "Parser state does not contain PEG grammar".to_string(),
            })?;

        // PEG parsers use backtracking, so recovery is different
        match error {
            ParseError::UnexpectedToken { expected, .. } => {
                // Try to skip tokens until we find an expected one
                if !expected.is_empty() {
                    Ok(RecoveryAction::SkipToSyncPoint {
                        tokens: vec![], // Would need to convert expected strings to tokens
                    })
                } else {
                    // Backtrack and try alternative
                    Ok(RecoveryAction::Backtrack)
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
                // Backtrack to try alternative paths
                Ok(RecoveryAction::Backtrack)
            }
            ParseError::Ambiguity { .. } => {
                // PEG doesn't have ambiguity (ordered choice)
                // This shouldn't happen, but if it does, backtrack
                Ok(RecoveryAction::Backtrack)
            }
        }
    }

    fn capabilities(&self) -> RecoveryCapabilities {
        RecoveryCapabilities {
            can_skip_tokens: true,
            can_insert_tokens: true,
            can_skip_to_sync: true,
            can_delete_tokens: false, // PEG doesn't support deletion
            can_insert_expected: true,
        }
    }
}
