use crate::types::{InsnId, Pos, RuleId, SyntaxKind};

#[derive(Clone, Copy)]
pub(super) struct SnapEntry {
    pub(super) word: u32,
    pub(super) val: u64,
}

#[derive(Clone, Copy, Debug)]
pub(super) enum Frame {
    Backtrack {
        alt: u32,
        saved_pos: Pos,
        /// Mark into `events` (legacy captures).
        capture_mark: u32,
        /// Mark into `tree_events` (green/red tree events).
        tree_mark: u32,
        /// Mark into `open_tokens` (in-progress token spans).
        open_tokens_mark: u32,
        /// Mark into `context_stack` (diagnostic "while parsing" context).
        context_mark: u32,
    },
    Return {
        ret_ip: u32,
    },
    #[cfg(feature = "std")]
    MemoReturn {
        ret_ip: u32,
        rule: RuleId,
        start_pos: Pos,
        events_mark: u32,
        tree_mark: u32,
    },
    ContextSave {
        snapshot_mark: u32,
    },
    /// Error recovery: on failure, skip until `sync_rule` matches then continue at resume.
    ///
    /// Marks snapshot the parse state at the start of the `recover_until` body; when the body fails,
    /// the VM truncates captures / tree events / token stack / diagnostic context to these marks
    /// before scanning for the sync rule (see `vm::do_fail`).
    Recover {
        sync_rule: RuleId,
        resume: InsnId,
        capture_mark: u32,
        tree_mark: u32,
        open_tokens_mark: u32,
        context_mark: u32,
    },
    /// Marker when trying the sync rule during recovery; popped by `RecoveryResume`.
    RecoverSync {
        sync_rule: RuleId,
        resume: InsnId,
    },
    /// Defensive: carried over from older designs.
    #[allow(dead_code)]
    _Reserved {
        _kind: SyntaxKind,
    },
}
