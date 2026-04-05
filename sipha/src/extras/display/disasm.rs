use crate::parse::builder::BuiltGraph;
use crate::parse::insn::Insn;
use crate::parse::string_table::SymbolId;
use crate::types::InsnId;
use std::fmt::Write as _;

/// Disassemble the raw instruction stream with resolved names.
#[must_use]
pub fn disasm(graph: &BuiltGraph) -> String {
    let mut out = String::new();

    // Map rule entry IP -> rule index for headers.
    let mut rule_at_ip: Vec<Option<usize>> = vec![None; graph.insns.len()];
    for (r, &ip) in graph.rule_entry.iter().enumerate() {
        let i = ip as usize;
        if i < rule_at_ip.len() {
            rule_at_ip[i] = Some(r);
        }
    }

    for (ip_usize, insn) in graph.insns.iter().enumerate() {
        if let Some(r) = rule_at_ip[ip_usize] {
            let _ = writeln!(out, "\n; rule {} ({})", graph.rule_name_at(r), r);
        }
        let ip: InsnId = u32::try_from(ip_usize).unwrap_or(u32::MAX);
        let _ = writeln!(out, "{ip:06}: {}", format_insn(graph, ip, *insn));
    }

    out
}

fn format_insn(graph: &BuiltGraph, _ip: InsnId, insn: Insn) -> String {
    match insn {
        Insn::Byte { byte, on_fail } => format!("Byte 0x{byte:02X} on_fail={on_fail}"),
        Insn::ByteEither { a, b, on_fail } => {
            format!("ByteEither 0x{a:02X}|0x{b:02X} on_fail={on_fail}")
        }
        Insn::ByteIn3 { a, b, c, on_fail } => {
            format!("ByteIn3 0x{a:02X}|0x{b:02X}|0x{c:02X} on_fail={on_fail}")
        }
        Insn::ByteRange { lo, hi, on_fail } => {
            format!("ByteRange 0x{lo:02X}-0x{hi:02X} on_fail={on_fail}")
        }
        Insn::Class {
            label_id, on_fail, ..
        } => {
            let label = resolve_label(graph, &graph.class_labels, label_id, "class");
            format!("Class {label:?} on_fail={on_fail}")
        }
        Insn::ConsumeWhileClass {
            label_id,
            min,
            on_fail,
            ..
        } => {
            let label = resolve_label(graph, &graph.class_labels, label_id, "class");
            format!("ConsumeWhileClass {label:?} min={min} on_fail={on_fail}")
        }
        Insn::Literal { lit_id, on_fail } => {
            let lit = get_literal(graph, lit_id);
            format!("Literal#{lit_id} {:?} on_fail={on_fail}", escape_bytes(lit))
        }
        Insn::LiteralSmall {
            len,
            bytes,
            on_fail,
        } => {
            let n = len as usize;
            let lit = &bytes[..n.min(bytes.len())];
            format!("LiteralSmall {:?} on_fail={on_fail}", escape_bytes(lit))
        }
        Insn::EndOfInput { on_fail } => format!("EndOfInput on_fail={on_fail}"),
        Insn::Fail => "Fail".to_string(),
        Insn::AnyChar { on_fail } => format!("AnyChar on_fail={on_fail}"),
        Insn::Char { codepoint, on_fail } => format!("Char U+{codepoint:04X} on_fail={on_fail}"),
        Insn::CharRange { lo, hi, on_fail } => {
            format!("CharRange U+{lo:04X}-U+{hi:04X} on_fail={on_fail}")
        }
        Insn::Jump { target } => format!("Jump {target}"),
        Insn::Call { rule } => {
            let name = graph.rule_name_at(rule as usize);
            format!("Call {name} ({rule})")
        }
        Insn::Return => "Return".to_string(),
        Insn::Choice { alt } => format!("Choice alt={alt}"),
        Insn::Commit { target } => format!("Commit target={target}"),
        Insn::BackCommit { target } => format!("BackCommit target={target}"),
        Insn::NegBackCommit { target } => format!("NegBackCommit target={target}"),
        Insn::PartialCommit { target } => format!("PartialCommit target={target}"),
        Insn::ByteDispatch {
            table_id,
            fallback,
            push_choice_backtrack,
        } => format!(
            "ByteDispatch table_id={table_id} fallback={fallback} push_bt={push_choice_backtrack}"
        ),
        Insn::IfFlag { flag_id, on_fail } => format!("IfFlag {flag_id} on_fail={on_fail}"),
        Insn::IfNotFlag { flag_id, on_fail } => format!("IfNotFlag {flag_id} on_fail={on_fail}"),
        Insn::PushFlags { mask_id } => format!("PushFlags mask_id={mask_id}"),
        Insn::PopFlags => "PopFlags".to_string(),
        Insn::CaptureBegin { tag } => format!("CaptureBegin tag={tag}"),
        Insn::CaptureEnd { tag } => format!("CaptureEnd tag={tag}"),
        Insn::NodeBegin { kind, field } => field.map_or_else(
            || format!("NodeBegin kind={kind}"),
            |f| {
                format!(
                    "NodeBegin kind={kind} field={:?}",
                    graph.strings.resolve(graph.field_names[f as usize])
                )
            },
        ),
        Insn::NodeEnd => "NodeEnd".to_string(),
        Insn::TokenBegin { kind, is_trivia } => {
            format!("TokenBegin kind={kind} trivia={is_trivia}")
        }
        Insn::TokenEnd => "TokenEnd".to_string(),
        Insn::RecordExpectedLabel { label_id } => {
            let label = resolve_label(graph, &graph.expected_labels, label_id, "expected");
            format!("RecordExpectedLabel {label:?}")
        }
        Insn::PushDiagnosticContext { label_id } => {
            let label = resolve_label(graph, &graph.expected_labels, label_id, "expected");
            format!("PushDiagnosticContext {label:?}")
        }
        Insn::PopDiagnosticContext => "PopDiagnosticContext".to_string(),
        Insn::SetHint { hint_id } => {
            let s = graph.strings.resolve(hint_id).to_string();
            format!("SetHint {s:?}")
        }
        Insn::TracePoint { label_id } => {
            let s = graph.strings.resolve(label_id).to_string();
            format!("TracePoint {s:?}")
        }
        Insn::RecoverUntil { sync_rule, resume } => {
            format!("RecoverUntil sync_rule={sync_rule} resume={resume}")
        }
        Insn::RecoveryResume => "RecoveryResume".to_string(),
        Insn::Accept => "Accept".to_string(),
    }
}

fn resolve_label(graph: &BuiltGraph, table: &[SymbolId], label_id: u32, kind: &str) -> String {
    table
        .get(label_id as usize)
        .copied()
        .or_else(|| table.first().copied())
        .map_or_else(
            || format!("{kind}_label#{label_id}"),
            |sym| graph.strings.resolve(sym).to_string(),
        )
}

fn get_literal(graph: &BuiltGraph, id: u32) -> &[u8] {
    let o = &graph.literal_offsets;
    let i = id as usize;
    if i + 1 >= o.len() {
        return &[];
    }
    let start = o[i] as usize;
    let end = o[i + 1] as usize;
    &graph.literal_data[start..end]
}

fn escape_bytes(bytes: &[u8]) -> String {
    let mut out = String::new();
    out.push('"');
    for &b in bytes {
        match b {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            0x20..=0x7E => out.push(b as char),
            _ => {
                let _ = write!(out, "\\x{b:02X}");
            }
        }
    }
    out.push('"');
    out
}
