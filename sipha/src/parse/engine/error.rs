use crate::diagnostics::error::ErrorContext;
#[cfg(feature = "miette")]
use crate::diagnostics::grammar_names::GrammarNames;
use crate::types::RuleId;
#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};
#[cfg(feature = "std")]
use std::string::String;

/// Why the VM rejected bytecode as invalid.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BadGraphKind {
    /// `Call` rule id out of range for `rule_entry`.
    RuleEntryOutOfRange { rule: RuleId },
    /// Recovery sync rule id out of range for `rule_entry`.
    SyncRuleEntryOutOfRange { rule: RuleId },
    /// `TokenEnd` without a matching `TokenBegin`.
    TokenStackUnderflow,
    /// `RecoveryResume` without an active `Recover` / `RecoverSync` frame.
    RecoveryResumeWithoutRecoverFrame,
    /// `Return` while a recovery frame is still on the stack (malformed grammar).
    ReturnWithRecoverFrameOnStack,
    /// Unexpected frame while unwinding `Return`.
    UnexpectedFrameDuringReturn,
    /// Reserved frame variant (invalid bytecode).
    ReservedFrame,
    /// Stack underflow when expecting a `Backtrack` frame.
    BacktrackStackUnderflow,
    /// `PartialCommit` with no `Backtrack` frame (malformed grammar).
    PartialCommitWithoutBacktrack,
    /// `PopFlags` without a matching `ContextSave` frame.
    PopFlagsStackUnderflow,
    /// A snapshot mark pointed past the snapshot stack.
    SnapshotMarkOutOfRange { mark: u32 },
    /// A length could not be represented as a `Pos` (byte offset).
    PosOutOfRange { len: usize },
    /// `LiteralSmall` had an invalid length for its inline bytes.
    LiteralSmallLenOutOfRange { len: u8, bytes_len: u8 },
}

impl core::fmt::Display for BadGraphKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::RuleEntryOutOfRange { rule } => {
                write!(f, "rule entry missing for rule id {rule}")
            }
            Self::SyncRuleEntryOutOfRange { rule } => {
                write!(f, "sync rule entry missing for rule id {rule}")
            }
            Self::TokenStackUnderflow => {
                write!(f, "`TokenEnd` without matching `TokenBegin`")
            }
            Self::RecoveryResumeWithoutRecoverFrame => {
                write!(f, "`RecoveryResume` without active recover frame")
            }
            Self::ReturnWithRecoverFrameOnStack => {
                write!(f, "`Return` encountered recovery frame on stack")
            }
            Self::UnexpectedFrameDuringReturn => {
                write!(f, "unexpected frame while unwinding `Return`")
            }
            Self::ReservedFrame => write!(f, "reserved frame in stack"),
            Self::BacktrackStackUnderflow => {
                write!(f, "expected `Backtrack` frame but stack underflowed")
            }
            Self::PartialCommitWithoutBacktrack => {
                write!(f, "`PartialCommit` with no `Backtrack` frame")
            }
            Self::PopFlagsStackUnderflow => {
                write!(f, "`PopFlags` without matching `PushFlags` frame")
            }
            Self::SnapshotMarkOutOfRange { mark } => {
                write!(f, "snapshot mark out of range: {mark}")
            }
            Self::PosOutOfRange { len } => {
                write!(f, "byte length out of range for Pos: {len}")
            }
            Self::LiteralSmallLenOutOfRange { len, bytes_len } => {
                write!(
                    f,
                    "`LiteralSmall` len out of range (len={len}, bytes_len={bytes_len})"
                )
            }
        }
    }
}

/// Parse failure or malformed graph.
#[derive(Clone, Debug)]
pub enum ParseError {
    NoMatch(crate::diagnostics::error::Diagnostic),
    BadGraph(BadGraphKind),
    /// The requested rule name was not found in the parse graph.
    UnknownRuleName(String),
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::NoMatch(d) => write!(f, "{d}"),
            Self::BadGraph(k) => write!(f, "malformed parse graph: {k}"),
            Self::UnknownRuleName(name) => write!(f, "unknown rule name: {name}"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseError {}

/// Result of parsing in multi-error recovery mode: partial output and all collected errors.
///
/// Returned by [`crate::parse::engine::Engine::parse_recovering_multi`] when the grammar uses
/// [`recover_until`](crate::parse::builder::GrammarBuilder::recover_until) and one or more
/// failures occur. `partial` contains tree events and consumed length up to the last
/// recovery point; `errors` holds each parse error in order.
#[derive(Debug)]
pub struct RecoverMultiResult {
    /// Partial parse output (events and consumed length) after collecting errors.
    pub partial: crate::parse::engine::ParseOutput,
    /// All parse errors collected during recovery (in order of occurrence).
    pub errors: Vec<ParseError>,
}

#[cfg(feature = "miette")]
impl ParseError {
    /// If this is a parse failure ([`NoMatch`](Self::NoMatch)), convert it
    /// into a [`miette::Report`] with source and optional literal table for
    /// pretty-printed diagnostics. [`BadGraph`](Self::BadGraph) becomes a simple miette report.
    pub fn to_miette_report(
        &self,
        source: impl Into<String>,
        name: impl Into<String>,
        literals: Option<&crate::parse::insn::LiteralTable<'_>>,
        names: Option<&dyn GrammarNames>,
    ) -> Option<miette::Report> {
        match self {
            Self::NoMatch(diag) => {
                let m = diag.into_miette(source, name, literals, names);
                Some(miette::Report::new(m))
            }
            Self::BadGraph(kind) => {
                let name_str: String = name.into();
                let src_str: String = source.into();
                Some(
                    miette::Report::msg(format!("malformed parse graph: {kind}"))
                        .with_source_code(miette::NamedSource::new(name_str, src_str)),
                )
            }
            Self::UnknownRuleName(rule_name) => {
                let name_str: String = name.into();
                let src_str: String = source.into();
                Some(
                    miette::Report::msg(format!("unknown rule name: {rule_name}"))
                        .with_source_code(miette::NamedSource::new(name_str, src_str)),
                )
            }
        }
    }
}

#[inline]
pub(super) fn no_match(error_ctx: &ErrorContext) -> ParseError {
    ParseError::NoMatch(error_ctx.to_diagnostic())
}

#[cfg(test)]
mod tests {
    use super::BadGraphKind;
    extern crate alloc;
    use alloc::string::ToString;

    #[test]
    fn bad_graph_kind_display_is_actionable() {
        let s = BadGraphKind::RuleEntryOutOfRange { rule: 3 }.to_string();
        assert!(s.contains('3'), "{s}");
        let t = BadGraphKind::TokenStackUnderflow.to_string();
        assert!(!t.is_empty(), "{t}");
    }
}
