use crate::parse::engine::VmObserver;
use crate::parse::insn::{Insn, ParseGraph};
use crate::types::{Pos, RuleId};

#[derive(Clone, Copy, Debug)]
pub struct TraceStep {
    pub ip: u32,
    pub pos: Pos,
    pub insn: Insn,
}

/// Bounded ring-buffer of recent VM steps.
///
/// Enable with the `trace` feature and pass it to `Engine::parse_with_observer`.
pub struct TraceBuffer {
    buf: Vec<TraceStep>,
    cap: usize,
    next: usize,
    full: bool,
}

impl TraceBuffer {
    #[must_use]
    pub fn new(cap: usize) -> Self {
        Self {
            buf: vec![
                TraceStep {
                    ip: 0,
                    pos: 0,
                    insn: Insn::Fail,
                };
                cap.max(1)
            ],
            cap: cap.max(1),
            next: 0,
            full: false,
        }
    }

    pub fn clear(&mut self) {
        self.next = 0;
        self.full = false;
    }

    #[must_use]
    pub fn steps(&self) -> impl Iterator<Item = &TraceStep> {
        let (a, b): (&[TraceStep], &[TraceStep]) = if self.full {
            (&self.buf[self.next..], &self.buf[..self.next])
        } else {
            (&self.buf[..self.next], &[])
        };
        a.iter().chain(b.iter())
    }

    #[must_use]
    pub fn format(&self, graph: &ParseGraph<'_>) -> String {
        let mut out = String::new();
        for s in self.steps() {
            out.push_str(&format!(
                "{:06} pos={} {}\n",
                s.ip,
                s.pos,
                format_insn_one_line(graph, s.insn)
            ));
        }
        out
    }
}

impl VmObserver for TraceBuffer {
    fn before_insn(&mut self, ip: u32, pos: Pos, insn: Insn) {
        let step = TraceStep { ip, pos, insn };
        self.buf[self.next] = step;
        self.next += 1;
        if self.next >= self.cap {
            self.next = 0;
            self.full = true;
        }
    }

    fn on_call(&mut self, _rule: RuleId, _pos: Pos) {}
}

fn format_insn_one_line(graph: &ParseGraph<'_>, insn: Insn) -> String {
    match insn {
        Insn::Byte { byte, .. } => format!("Byte 0x{byte:02X}"),
        Insn::ByteEither { a, b, .. } => format!("ByteEither 0x{a:02X}|0x{b:02X}"),
        Insn::ByteIn3 { a, b, c, .. } => format!("ByteIn3 0x{a:02X}|0x{b:02X}|0x{c:02X}"),
        Insn::ByteRange { lo, hi, .. } => format!("ByteRange 0x{lo:02X}-0x{hi:02X}"),
        Insn::Class { label_id, .. } => graph
            .class_label(label_id)
            .map(|s| format!("Class({s})"))
            .unwrap_or_else(|| format!("Class(label#{label_id})")),
        Insn::ConsumeWhileClass { label_id, min, .. } => graph
            .class_label(label_id)
            .map(|s| format!("ConsumeWhileClass({s}, min={min})"))
            .unwrap_or_else(|| format!("ConsumeWhileClass(label#{label_id}, min={min})")),
        Insn::Literal { lit_id, .. } => format!("Literal#{lit_id}"),
        Insn::LiteralSmall { len, bytes, .. } => {
            let n = len as usize;
            let shown = &bytes[..n.min(bytes.len())];
            format!("LiteralSmall({})", escape_bytes(shown))
        }
        Insn::EndOfInput { .. } => "EndOfInput".to_string(),
        Insn::Fail => "Fail".to_string(),
        Insn::AnyChar { .. } => "AnyChar".to_string(),
        Insn::Char { codepoint, .. } => format!("Char U+{codepoint:04X}"),
        Insn::CharRange { lo, hi, .. } => format!("CharRange U+{lo:04X}-U+{hi:04X}"),
        Insn::Jump { target } => format!("Jump {target}"),
        Insn::Call { rule } => graph
            .rule_name(rule)
            .map(|s| format!("Call {s}"))
            .unwrap_or_else(|| format!("Call#{rule}")),
        Insn::Return => "Return".to_string(),
        Insn::Choice { alt } => format!("Choice alt={alt}"),
        Insn::Commit { target } => format!("Commit {target}"),
        Insn::BackCommit { target } => format!("BackCommit {target}"),
        Insn::NegBackCommit { target } => format!("NegBackCommit {target}"),
        Insn::PartialCommit { target } => format!("PartialCommit {target}"),
        Insn::ByteDispatch { table_id } => format!("ByteDispatch {table_id}"),
        Insn::IfFlag { flag_id, .. } => format!("IfFlag {flag_id}"),
        Insn::IfNotFlag { flag_id, .. } => format!("IfNotFlag {flag_id}"),
        Insn::PushFlags { mask_id } => format!("PushFlags {mask_id}"),
        Insn::PopFlags => "PopFlags".to_string(),
        Insn::CaptureBegin { tag } => format!("CaptureBegin {tag}"),
        Insn::CaptureEnd { tag } => format!("CaptureEnd {tag}"),
        Insn::NodeBegin { kind, field } => match field {
            Some(f) => format!("NodeBegin kind={kind} field={}", graph.field_name(f)),
            None => format!("NodeBegin kind={kind}"),
        },
        Insn::NodeEnd => "NodeEnd".to_string(),
        Insn::TokenBegin { kind, is_trivia } => {
            format!("TokenBegin kind={kind} trivia={is_trivia}")
        }
        Insn::TokenEnd => "TokenEnd".to_string(),
        Insn::RecordExpectedLabel { label_id } => graph
            .expected_label(label_id)
            .map(|s| format!("RecordExpectedLabel({s})"))
            .unwrap_or_else(|| format!("RecordExpectedLabel#{label_id}")),
        Insn::PushDiagnosticContext { label_id } => graph
            .expected_label(label_id)
            .map(|s| format!("PushDiagnosticContext({s})"))
            .unwrap_or_else(|| format!("PushDiagnosticContext#{label_id}")),
        Insn::PopDiagnosticContext => "PopDiagnosticContext".to_string(),
        Insn::SetHint { hint_id } => {
            format!("SetHint({})", graph.strings.resolve(hint_id))
        }
        Insn::TracePoint { label_id } => {
            format!("TracePoint({})", graph.strings.resolve(label_id))
        }
        Insn::RecoverUntil { sync_rule, resume } => {
            format!("RecoverUntil rule#{sync_rule} resume={resume}")
        }
        Insn::RecoveryResume => "RecoveryResume".to_string(),
        Insn::Accept => "Accept".to_string(),
    }
}

fn escape_bytes(bytes: &[u8]) -> String {
    let mut out = String::new();
    out.push('"');
    for &b in bytes {
        match b {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            0x20..=0x7E => out.push(b as char),
            _ => out.push_str(&format!("\\x{b:02X}")),
        }
    }
    out.push('"');
    out
}
