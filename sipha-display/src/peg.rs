//! PEG notation export for sipha grammars.
//!
//! Walks the instruction stream and reverse-engineers PEG syntax (e.g. `rule <- a / b "lit"`).

use sipha::builder::BuiltGraph;
use sipha::insn::Insn;
use sipha::types::{CharClass, InsnId};
use std::collections::HashSet;

/// Format the entire grammar as PEG rules, one per line: `rule_name <- expr`.
pub fn to_peg(graph: &BuiltGraph) -> String {
    let mut out = String::new();
    for (r, &name) in graph.rule_names.iter().enumerate() {
        let start = graph.rule_entry[r];
        let reachable = reachable_insns(graph, start);
        let (expr, _) = parse_sequence_until(graph, start, &reachable, &mut HashSet::new(), None);
        let expr = expr.trim().to_string();
        if !expr.is_empty() {
            out.push_str(&format!("{} <- {}\n", name, expr));
        } else {
            out.push_str(&format!("{} <- (empty)\n", name));
        }
    }
    out
}

/// All instruction IDs reachable from `start` without following Call. Stops at Return.
fn reachable_insns(graph: &BuiltGraph, start: InsnId) -> HashSet<InsnId> {
    let mut visited = HashSet::new();
    let mut stack = vec![start];
    let n = graph.insns.len() as u32;

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
            Insn::Return | Insn::Accept => {}
            Insn::Fail => {}
            Insn::Jump { target } => push(target),
            Insn::Call { .. } => push(ip + 1),
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
            | Insn::ByteRange { on_fail, .. }
            | Insn::Class { on_fail, .. }
            | Insn::Literal { on_fail, .. }
            | Insn::EndOfInput { on_fail }
            | Insn::AnyChar { on_fail }
            | Insn::Char { on_fail, .. }
            | Insn::CharRange { on_fail, .. } => {
                push(ip + 1);
                push(on_fail);
            }
            Insn::IfFlag { on_fail, .. } | Insn::IfNotFlag { on_fail, .. } => {
                push(ip + 1);
                push(on_fail);
            }
            _ => push(ip + 1),
        }
    }
    visited
}

/// Parse a sequence of expressions from `ip` until we hit a sentinel (Commit, PartialCommit, BackCommit, NegBackCommit), Return, or end_before.
/// Returns (peg string, next_ip after the parsed region).
fn parse_sequence_until(
    graph: &BuiltGraph,
    mut ip: InsnId,
    reachable: &HashSet<InsnId>,
    seen: &mut HashSet<InsnId>,
    end_before: Option<InsnId>,
) -> (String, InsnId) {
    let mut parts = Vec::new();
    let n = graph.insns.len() as u32;

    while ip < n && reachable.contains(&ip) {
        if end_before == Some(ip) {
            break;
        }
        if !seen.insert(ip) {
            break;
        }
        let insn = graph.insns[ip as usize];

        match insn {
            Insn::Return => break,
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
            Insn::Commit { .. } | Insn::BackCommit { .. } | Insn::PartialCommit { .. } | Insn::NegBackCommit { .. } => break,
            Insn::NodeBegin { .. } | Insn::NodeEnd | Insn::TokenBegin { .. } | Insn::TokenEnd => ip += 1,
            Insn::CaptureBegin { .. } | Insn::CaptureEnd { .. } => ip += 1,
            Insn::IfFlag { .. } | Insn::IfNotFlag { .. } | Insn::PushFlags { .. } | Insn::PopFlags => ip += 1,
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

/// Parse a Choice: optional (e?), zero_or_more (e*), lookahead (&e), neg_lookahead (!e), or alternation (e1 / e2).
fn parse_choice(
    graph: &BuiltGraph,
    ip: InsnId,
    reachable: &HashSet<InsnId>,
    seen: &mut HashSet<InsnId>,
) -> (String, InsnId) {
    let alt = match graph.insns[ip as usize] {
        Insn::Choice { alt } => alt,
        _ => return (String::new(), ip + 1),
    };

    // First branch: from ip+1 until we see Commit, PartialCommit, BackCommit, or NegBackCommit.
    let (first, sentinel_ip, sentinel) = parse_sequence_until_sentinel(graph, ip + 1, reachable, seen);

    match sentinel {
        Sentinel::Commit(target) => {
            if target == alt {
                (format!("({})?", first.trim()), target)
            } else {
                let (second, _) = parse_sequence_until(graph, alt, reachable, seen, Some(target));
                (format!("({}) / ({})", first.trim(), second.trim()), target)
            }
        }
        Sentinel::PartialCommit(loop_start, after) => {
            if loop_start == ip + 1 {
                (format!("({})*", first.trim()), after)
            } else {
                (first, after)
            }
        }
        Sentinel::BackCommit(after) => (format!("&({})", first.trim()), after),
        Sentinel::NegBackCommit(after) => (format!("!({})", first.trim()), after),
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
    let n = graph.insns.len() as u32;
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
            Insn::NodeBegin { .. } | Insn::NodeEnd | Insn::TokenBegin { .. } | Insn::TokenEnd => ip += 1,
            Insn::CaptureBegin { .. } | Insn::CaptureEnd { .. } => ip += 1,
            Insn::IfFlag { .. } | Insn::IfNotFlag { .. } | Insn::PushFlags { .. } | Insn::PopFlags => ip += 1,
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

/// Parse a single atom (terminal, call, etc.) and return (peg fragment, next_ip).
fn parse_atom(graph: &BuiltGraph, ip: InsnId) -> (String, InsnId) {
    let n = graph.insns.len() as u32;
    if ip >= n {
        return (String::new(), ip);
    }

    let insn = graph.insns[ip as usize];
    let next = ip + 1;

    let s = match insn {
        Insn::Byte { byte, .. } => format_byte(byte),
        Insn::ByteRange { lo, hi, .. } => format!("[\\x{:02X}-\\x{:02X}]", lo, hi),
        Insn::Class { class, .. } => format_char_class(class),
        Insn::Literal { lit_id, .. } => {
            let lit = get_literal(graph, lit_id);
            format!("\"{}\"", escape_peg_literal(lit))
        }
        Insn::EndOfInput { .. } => "eoi".to_string(),
        Insn::Fail => "fail".to_string(),
        Insn::AnyChar { .. } => ".".to_string(),
        Insn::Char { codepoint, .. } => format!("\\u{{{:04X}}}", codepoint),
        Insn::CharRange { lo, hi, .. } => format!("[\\u{{{:04X}}}-\\u{{{:04X}}}]", lo, hi),
        Insn::Call { rule } => {
            let name = graph.rule_names.get(rule as usize).copied().unwrap_or("?");
            name.to_string()
        }
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
        format!("\"\\x{:02X}\"", byte)
    }
}

fn format_char_class(class: CharClass) -> String {
    let mut ranges = Vec::new();
    for b in 0u8..=255 {
        if class.contains(b) {
            if ranges.is_empty() || ranges.last().map(|(_, e)| *e != b.wrapping_sub(1)).unwrap_or(true) {
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
                format!("\\x{:02X}", lo)
            } else {
                format!("\\x{:02X}-\\x{:02X}", lo, hi)
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
    lit.iter()
        .map(|&b| {
            if b == b'"' {
                "\\\"".to_string()
            } else if b == b'\\' {
                "\\\\".to_string()
            } else if b.is_ascii_graphic() || b == b' ' {
                (b as char).to_string()
            } else {
                format!("\\x{:02X}", b)
            }
        })
        .collect()
}
