use crate::types::{InsnId, Pos};

/// Hooks for runtime parse tracing.
///
/// Install a tracer with `Engine::parse_with_tracer` (requires the `trace` feature).
pub trait ParseTracer {
    /// Called when the VM hits a `TracePoint`.
    fn on_trace(&mut self, label: &str, pos: Pos, ip: InsnId);

    /// Called when the VM executes a `Call` (optional).
    fn on_call(&mut self, _rule_name: &str, _pos: Pos) {}

    /// Called when the VM returns from a rule (optional).
    fn on_return(&mut self, _rule_name: &str, _pos: Pos, _succeeded: bool) {}
}

/// Simple tracer that prints to stderr with indentation.
#[cfg(feature = "std")]
pub struct PrintTracer {
    pub indent: usize,
}

#[cfg(feature = "std")]
impl Default for PrintTracer {
    fn default() -> Self {
        Self { indent: 0 }
    }
}

#[cfg(feature = "std")]
impl ParseTracer for PrintTracer {
    fn on_trace(&mut self, label: &str, pos: Pos, _ip: InsnId) {
        eprintln!("{:>width$}@ {pos}: {label}", "", width = self.indent * 2);
    }

    fn on_call(&mut self, rule: &str, pos: Pos) {
        eprintln!(
            "{:>width$}→ {rule} (pos {pos})",
            "",
            width = self.indent * 2
        );
        self.indent += 1;
    }

    fn on_return(&mut self, rule: &str, pos: Pos, ok: bool) {
        self.indent = self.indent.saturating_sub(1);
        let mark = if ok { "✓" } else { "✗" };
        eprintln!(
            "{:>width$}{mark} {rule} (pos {pos})",
            "",
            width = self.indent * 2
        );
    }
}
