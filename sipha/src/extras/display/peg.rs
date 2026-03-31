//! PEG notation export for sipha grammars.
//!
//! Walks the instruction stream and reverse-engineers PEG syntax (e.g. `rule <- a / b "lit"`).

use crate::parse::builder::BuiltGraph;
use crate::parse::insn::Insn;
use crate::types::{CharClass, InsnId};
use std::collections::HashSet;
use std::fmt::Write as _;

/// Format the entire grammar as PEG rules, one per line: `rule_name <- expr`.
#[must_use]
pub fn to_peg(graph: &BuiltGraph) -> String {
    let mut out = String::new();
    for r in 0..graph.rule_names.len() {
        let name = graph.rule_name_at(r);
        let start = graph.rule_entry[r];
        let reachable = reachable_insns(graph, start);
        let (expr, _) = parse_sequence_until(graph, start, &reachable, &mut HashSet::new(), None);
        let expr = expr.trim();
        if expr.is_empty() {
            let _ = writeln!(out, "{name} <- (empty)");
        } else {
            let _ = writeln!(out, "{name} <- {expr}");
        }
    }
    out
}

/// All instruction IDs reachable from `start` without following Call. Stops at Return.
fn reachable_insns(graph: &BuiltGraph, start: InsnId) -> HashSet<InsnId> {
    let mut visited = HashSet::new();
    let mut stack = vec![start];
    let n = u32::try_from(graph.insns.len()).unwrap_or(u32::MAX);

    while let Some(ip) = stack.pop() {
        if ip >= n || !visited.insert(ip) {
            continue;
        }
        let insn = graph.insns[ip as usize];
        if matches!(insn, Insn::Return) {
            continue;
        }

        let mut push = |target: InsnId| {
            if target < n {
                stack.push(target);
            }
        };

        match insn {
            Insn::Return | Insn::Accept | Insn::Fail => {}
            Insn::Jump { target } => push(target),
            Insn::Choice { alt } => {
                push(ip + 1);
                push(alt);
            }
            Insn::Commit { target }
            | Insn::BackCommit { target }
            | Insn::PartialCommit { target }
            | Insn::NegBackCommit { target } => {
                push(ip + 1);
                push(target);
            }
            Insn::Byte { on_fail, .. }
            | Insn::ByteEither { on_fail, .. }
            | Insn::ByteIn3 { on_fail, .. }
            | Insn::ByteRange { on_fail, .. }
            | Insn::Class { on_fail, .. }
            | Insn::Literal { on_fail, .. }
            | Insn::LiteralSmall { on_fail, .. }
            | Insn::EndOfInput { on_fail }
            | Insn::AnyChar { on_fail }
            | Insn::Char { on_fail, .. }
            | Insn::CharRange { on_fail, .. }
            | Insn::ConsumeWhileClass { on_fail, .. }
            | Insn::IfFlag { on_fail, .. }
            | Insn::IfNotFlag { on_fail, .. } => {
                push(ip + 1);
                push(on_fail);
            }
            _ => push(ip + 1),
        }
    }
    visited
}

/// Parse a sequence of PEG fragments starting at `ip`, stopping at a control-flow sentinel,
/// `Return`, or the `end_before` bound.
/// Returns the joined fragment and the instruction index after the parsed region.
fn parse_sequence_until(
    graph: &BuiltGraph,
    mut ip: InsnId,
    reachable: &HashSet<InsnId>,
    seen: &mut HashSet<InsnId>,
    end_before: Option<InsnId>,
) -> (String, InsnId) {
    let mut parts = Vec::new();
    let n = u32::try_from(graph.insns.len()).unwrap_or(u32::MAX);

    while ip < n && reachable.contains(&ip) {
        if end_before == Some(ip) {
            break;
        }
        if !seen.insert(ip) {
            break;
        }
        let insn = graph.insns[ip as usize];

        match insn {
            Insn::Return
            | Insn::Commit { .. }
            | Insn::BackCommit { .. }
            | Insn::PartialCommit { .. }
            | Insn::NegBackCommit { .. } => break,
            Insn::Choice { .. } => {
                let (expr, next) = parse_choice(graph, ip, reachable, seen);
                if !expr.is_empty() {
                    parts.push(expr);
                }
                if end_before == Some(next) {
                    return (parts.join(" "), next);
                }
                ip = next;
            }
            Insn::Jump { target } => {
                if end_before == Some(target) {
                    break;
                }
                ip = target;
            }
            Insn::NodeBegin { .. }
            | Insn::NodeEnd
            | Insn::TokenBegin { .. }
            | Insn::TokenEnd
            | Insn::CaptureBegin { .. }
            | Insn::CaptureEnd { .. }
            | Insn::IfFlag { .. }
            | Insn::IfNotFlag { .. }
            | Insn::PushFlags { .. }
            | Insn::PopFlags => {
                ip += 1;
            }
            _ => {
                let (expr, next) = parse_atom(graph, ip);
                if !expr.is_empty() {
                    parts.push(expr);
                }
                if end_before == Some(next) {
                    return (parts.join(" "), next);
                }
                ip = next;
            }
        }
    }

    let s = parts.join(" ");
    (s, ip)
}

/// Parse a `Choice` insn: optional, star, lookahead, negated lookahead, or alternation.
fn parse_choice(
    graph: &BuiltGraph,
    ip: InsnId,
    reachable: &HashSet<InsnId>,
    seen: &mut HashSet<InsnId>,
) -> (String, InsnId) {
    let Insn::Choice { alt } = graph.insns[ip as usize] else {
        return (String::new(), ip + 1);
    };

    // First branch: from ip+1 until we see Commit, PartialCommit, BackCommit, or NegBackCommit.
    let (first, sentinel_ip, sentinel) =
        parse_sequence_until_sentinel(graph, ip + 1, reachable, seen);

    match sentinel {
        Sentinel::Commit(target) => {
            if target == alt {
                let t = first.trim();
                (format!("({t})?"), target)
            } else {
                let (second, _) = parse_sequence_until(graph, alt, reachable, seen, Some(target));
                let ft = first.trim();
                let st = second.trim();
                (format!("({ft}) / ({st})"), target)
            }
        }
        Sentinel::PartialCommit(loop_start, after) => {
            if loop_start == ip + 1 {
                let t = first.trim();
                (format!("({t})*"), after)
            } else {
                (first, after)
            }
        }
        Sentinel::BackCommit(after) => {
            let t = first.trim();
            (format!("&({t})"), after)
        }
        Sentinel::NegBackCommit(after) => {
            let t = first.trim();
            (format!("!({t})"), after)
        }
        Sentinel::None => (first, sentinel_ip),
    }
}

enum Sentinel {
    Commit(InsnId),
    PartialCommit(InsnId, InsnId),
    BackCommit(InsnId),
    NegBackCommit(InsnId),
    None,
}

fn parse_sequence_until_sentinel(
    graph: &BuiltGraph,
    ip: InsnId,
    reachable: &HashSet<InsnId>,
    seen: &mut HashSet<InsnId>,
) -> (String, InsnId, Sentinel) {
    let mut parts = Vec::new();
    let n = u32::try_from(graph.insns.len()).unwrap_or(u32::MAX);
    let mut ip = ip;

    while ip < n && reachable.contains(&ip) {
        let insn = graph.insns[ip as usize];

        match insn {
            Insn::Commit { target } => {
                let s = parts.join(" ");
                return (s, ip, Sentinel::Commit(target));
            }
            Insn::PartialCommit { target } => {
                let s = parts.join(" ");
                return (s, ip, Sentinel::PartialCommit(target, ip + 1));
            }
            Insn::BackCommit { target } => {
                let s = parts.join(" ");
                return (s, ip, Sentinel::BackCommit(target));
            }
            Insn::NegBackCommit { target } => {
                let s = parts.join(" ");
                return (s, ip, Sentinel::NegBackCommit(target));
            }
            Insn::Return => break,
            Insn::Choice { .. } => {
                if !seen.insert(ip) {
                    break;
                }
                let (expr, next) = parse_choice(graph, ip, reachable, seen);
                if !expr.is_empty() {
                    parts.push(expr);
                }
                ip = next;
            }
            Insn::Jump { target } => ip = target,
            Insn::NodeBegin { .. }
            | Insn::NodeEnd
            | Insn::TokenBegin { .. }
            | Insn::TokenEnd
            | Insn::CaptureBegin { .. }
            | Insn::CaptureEnd { .. }
            | Insn::IfFlag { .. }
            | Insn::IfNotFlag { .. }
            | Insn::PushFlags { .. }
            | Insn::PopFlags => {
                ip += 1;
            }
            _ => {
                if !seen.insert(ip) {
                    break;
                }
                let (expr, next) = parse_atom(graph, ip);
                if !expr.is_empty() {
                    parts.push(expr);
                }
                ip = next;
            }
        }
    }

    (parts.join(" "), ip, Sentinel::None)
}

/// Parse a single atom (terminal, call, etc.) and return the fragment plus the next index.
fn parse_atom(graph: &BuiltGraph, ip: InsnId) -> (String, InsnId) {
    let n = u32::try_from(graph.insns.len()).unwrap_or(u32::MAX);
    if ip >= n {
        return (String::new(), ip);
    }

    let insn = graph.insns[ip as usize];
    let next = ip + 1;

    let s = match insn {
        Insn::Byte { byte, .. } => format_byte(byte),
        Insn::ByteEither { a, b, .. } => format!("({}) / ({})", format_byte(a), format_byte(b)),
        Insn::ByteIn3 { a, b, c, .. } => format!(
            "({}) / ({}) / ({})",
            format_byte(a),
            format_byte(b),
            format_byte(c)
        ),
        Insn::ByteRange { lo, hi, .. } => format!("[\\x{lo:02X}-\\x{hi:02X}]"),
        Insn::Class { class, .. } => format_char_class(class),
        Insn::ConsumeWhileClass { class, min, .. } => {
            let atom = format_char_class(class);
            if min == 0 {
                format!("({atom})*")
            } else if min == 1 {
                format!("({atom})+")
            } else {
                format!("({atom}){{{min},}}")
            }
        }
        Insn::Literal { lit_id, .. } => {
            let lit = get_literal(graph, lit_id);
            format!("\"{}\"", escape_peg_literal(lit))
        }
        Insn::LiteralSmall { len, bytes, .. } => {
            let n = len as usize;
            format!("\"{}\"", escape_peg_literal(&bytes[..n.min(bytes.len())]))
        }
        Insn::EndOfInput { .. } => "eoi".to_string(),
        Insn::Fail => "fail".to_string(),
        Insn::AnyChar { .. } => ".".to_string(),
        Insn::Char { codepoint, .. } => format!("\\u{{{codepoint:04X}}}"),
        Insn::CharRange { lo, hi, .. } => format!("[\\u{{{lo:04X}}}-\\u{{{hi:04X}}}]"),
        Insn::Call { rule } => graph.rule_name_at(rule as usize).to_string(),
        Insn::ByteDispatch { .. } => "<byte_dispatch>".to_string(),
        _ => return (String::new(), next),
    };
    (s, next)
}

fn format_byte(byte: u8) -> String {
    if byte == b'"' {
        "\"\\\"\"".to_string()
    } else if byte == b'\\' {
        "\"\\\\\"".to_string()
    } else if byte.is_ascii_graphic() || byte == b' ' {
        format!("\"{}\"", byte as char)
    } else {
        format!("\"\\x{byte:02X}\"")
    }
}

fn format_char_class(class: CharClass) -> String {
    let mut ranges = Vec::new();
    for b in 0u8..=255 {
        if class.contains(b) {
            if ranges.is_empty() || ranges.last().is_none_or(|(_, e)| *e != b.wrapping_sub(1)) {
                ranges.push((b, b));
            } else {
                let last = ranges.last_mut().unwrap();
                last.1 = b;
            }
        }
    }
    if ranges.is_empty() {
        return "[]".to_string();
    }
    let parts: Vec<String> = ranges
        .into_iter()
        .map(|(lo, hi)| {
            if lo == hi {
                format!("\\x{lo:02X}")
            } else {
                format!("\\x{lo:02X}-\\x{hi:02X}")
            }
        })
        .collect();
    format!("[{}]", parts.join(""))
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

fn escape_peg_literal(lit: &[u8]) -> String {
    let mut out = String::with_capacity(lit.len());
    for &b in lit {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b if b.is_ascii_graphic() || b == b' ' => out.push(b as char),
            _ => {
                let _ = write!(out, "\\x{b:02X}");
            }
        }
    }
    out
}
