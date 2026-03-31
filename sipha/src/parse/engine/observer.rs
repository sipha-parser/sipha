use crate::parse::insn::Insn;
use crate::types::{Pos, RuleId};

/// Optional hooks for observing VM execution (for debugging/tracing/profiling).
///
/// This is intentionally low-level: it receives the concrete instruction and
/// current VM position. Implementations should be lightweight; heavy tracing is
/// best done behind the `trace` feature.
pub trait VmObserver {
    /// Called before executing the instruction at `ip`.
    fn before_insn(&mut self, ip: u32, pos: Pos, insn: Insn);

    /// Called when executing a `Call` to `rule` at `pos`.
    fn on_call(&mut self, _rule: RuleId, _pos: Pos) {}
}
