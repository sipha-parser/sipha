//! Graph export for sipha grammars: rule dependency graph and control-flow graph (DOT).

use crate::parse::builder::BuiltGraph;
use crate::parse::insn::Insn;
use crate::types::{InsnId, RuleId};
use std::collections::HashSet;
use std::fmt::Write as _;

/// Options for rule-dependency DOT output.
#[derive(Clone, Debug)]
pub struct RuleDepDotOptions {
    /// Only include rules reachable from the start rule (rule index 0). Reduces clutter from dead rules.
    pub reachable_only: bool,
    /// Exclude the named trivia rule (e.g. `"ws"`) from the graph. No effect if the name is not a rule.
    pub exclude_trivia: Option<&'static str>,
    /// Apply node styling: highlight start rule, use different shape/color for token-like rules.
    pub style_rules: bool,
}

impl Default for RuleDepDotOptions {
    fn default() -> Self {
        Self {
            reachable_only: true,
            exclude_trivia: None,
            style_rules: true,
        }
    }
}

/// Rules reachable from `start_rule` by following Call edges.
fn reachable_rules_from(graph: &BuiltGraph, start_rule: usize) -> HashSet<usize> {
    let mut seen = HashSet::new();
    let mut stack = vec![start_rule];
    while let Some(r) = stack.pop() {
        if r >= graph.rule_names.len() || !seen.insert(r) {
            continue;
        }
        let start_ip = graph.rule_entry[r];
        let reachable = reachable_insns(graph, start_ip);
        for &ip in &reachable {
            if let Some(Insn::Call { rule }) = graph.insns.get(ip as usize) {
                let callee = *rule as usize;
                if callee < graph.rule_names.len() {
                    stack.push(callee);
                }
            }
        }
    }
    seen
}

const TERMINAL_NAMES: &[&str] = &[
    "ident",
    "lparen",
    "rparen",
    "lbracket",
    "rbracket",
    "lbrace",
    "rbrace",
    "comma",
    "semicolon",
    "dot",
    "dot_dot",
    "arrow",
    "op_colon",
];

/// Heuristic: rule name looks like a terminal/token rule (lexer-level).
fn is_token_like(name: &str) -> bool {
    if name == "ws" {
        return true;
    }
    if name.ends_with("_lit") || name.ends_with("_kw") || name.starts_with("op_") {
        return true;
    }
    TERMINAL_NAMES.contains(&name)
}

fn escape_dot_id(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Collect all instruction IDs reachable from `start` without following `Call` into other rules.
/// Stops at `Return`. Used to scope a single rule body.
fn reachable_insns(graph: &BuiltGraph, start: InsnId) -> HashSet<InsnId> {
    let mut visited = HashSet::new();
    let mut stack = vec![start];
    let n = u32::try_from(graph.insns.len()).unwrap_or(u32::MAX);

    while let Some(ip) = stack.pop() {
        if ip >= n || !visited.insert(ip) {
            continue;
        }
        let insn = graph.insns[ip as usize];

        let mut push = |target: InsnId| {
            if target < n {
                stack.push(target);
            }
        };

        match insn {
            Insn::Return | Insn::Accept | Insn::Fail => {}
            Insn::Jump { target } => push(target),
            Insn::Call { .. } => {
                push(ip + 1);
            }
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
            Insn::ByteDispatch { .. }
            | Insn::PushFlags { .. }
            | Insn::PopFlags
            | Insn::CaptureBegin { .. }
            | Insn::CaptureEnd { .. }
            | Insn::NodeBegin { .. }
            | Insn::NodeEnd
            | Insn::TokenBegin { .. }
            | Insn::TokenEnd
            | Insn::RecordExpectedLabel { .. }
            | Insn::PushDiagnosticContext { .. }
            | Insn::PopDiagnosticContext
            | Insn::SetHint { .. }
            | Insn::TracePoint { .. }
            | Insn::RecoverUntil { .. }
            | Insn::RecoveryResume => push(ip + 1),
        }
    }
    visited
}

/// Emit a DOT digraph of rule dependencies with default options (reachable-only, styled).
#[must_use]
pub fn to_rule_dep_dot(graph: &BuiltGraph) -> String {
    to_rule_dep_dot_with_options(graph, &RuleDepDotOptions::default())
}

/// Emit a DOT digraph of rule dependencies: node per rule, edge A → B when rule A calls rule B.
/// Edges are deduplicated. With options, can restrict to reachable rules and apply styling.
#[must_use]
pub fn to_rule_dep_dot_with_options(graph: &BuiltGraph, opts: &RuleDepDotOptions) -> String {
    let start_rule = graph
        .rule_names
        .iter()
        .enumerate()
        .find(|&(_, &sym)| graph.strings.resolve(sym) == "start")
        .map_or(0, |(i, _)| i);
    let visible: HashSet<usize> = if opts.reachable_only && !graph.rule_names.is_empty() {
        reachable_rules_from(graph, start_rule)
    } else {
        (0..graph.rule_names.len()).collect()
    };

    let mut out = String::from(
        "digraph {\n\
         \tgraph [rankdir=LR, nodesep=0.5, ranksep=0.8, fontname=\"Helvetica\", fontsize=11, splines=line];\n\
         \tnode [shape=box, fontname=\"Helvetica\", fontsize=10, margin=\"0.25,0.15\", style=\"rounded,filled\", fillcolor=\"#f1f5f9\", color=\"#475569\"];\n\
         \tedge [fontname=\"Helvetica\", fontsize=9, color=\"#6b7280\"];\n\n",
    );

    // Emit node attributes for styling (so every visible node gets a style)
    for &r in &visible {
        let name = graph.rule_name_at(r);
        if opts.exclude_trivia == Some(name) {
            continue;
        }
        let id = escape_dot_id(name);
        if opts.style_rules {
            if r == start_rule {
                let _ = writeln!(
                    out,
                    "\t\"{id}\" [fillcolor=\"#dbeafe\", color=\"#1d4ed8\", penwidth=2];"
                );
            } else if name == "ws" {
                let _ = writeln!(
                    out,
                    "\t\"{id}\" [shape=ellipse, fillcolor=\"#e2e8f0\", color=\"#64748b\"];"
                );
            } else if is_token_like(name) {
                let _ = writeln!(
                    out,
                    "\t\"{id}\" [shape=ellipse, fillcolor=\"#dcfce7\", color=\"#16a34a\"];"
                );
            } else {
                // Structural rules: light gray fill and visible border (readable on white background)
                let _ = writeln!(
                    out,
                    "\t\"{id}\" [fillcolor=\"#f1f5f9\", color=\"#475569\", penwidth=1];"
                );
            }
        }
    }

    // Deduplicated edges, only between visible rules
    let mut edges = HashSet::new();
    for &r in &visible {
        let name = graph.rule_name_at(r);
        if opts.exclude_trivia == Some(name) {
            continue;
        }
        let start_ip = graph.rule_entry[r];
        let reachable = reachable_insns(graph, start_ip);
        for &ip in &reachable {
            if let Some(Insn::Call { rule }) = graph.insns.get(ip as usize) {
                let callee = *rule as usize;
                if callee < graph.rule_names.len() {
                    let to = graph.rule_name_at(callee);
                    if opts.exclude_trivia == Some(to) {
                        continue;
                    }
                    if !visible.contains(&callee) {
                        continue;
                    }
                    if edges.insert((name, to)) {
                        let _ = writeln!(
                            out,
                            "\t\"{}\" -> \"{}\";",
                            escape_dot_id(name),
                            escape_dot_id(to)
                        );
                    }
                }
            }
        }
    }

    out.push_str("}\n");
    out
}

/// Emit a DOT digraph of the control-flow for one rule: nodes are instruction indices, edges are next/jump/choice targets.
#[must_use]
pub fn to_cfg_dot(graph: &BuiltGraph, rule_id: RuleId) -> String {
    let r = rule_id as usize;
    if r >= graph.rule_entry.len() {
        return "digraph {}\n".to_string();
    }

    let start = graph.rule_entry[r];
    let reachable = reachable_insns(graph, start);
    let rule_name = graph.rule_name_at(r);

    let mut out = format!(
        "// CFG for rule \"{rule_name}\"\ndigraph {{\n\
         \tgraph [rankdir=TB, nodesep=0.3, ranksep=0.4, fontname=\"Helvetica\", fontsize=10];\n\
         \tnode [shape=box, fontname=\"Helvetica\", fontsize=9, margin=\"0.2,0.1\", style=rounded];\n\
         \tedge [fontname=\"Helvetica\", fontsize=8];\n\n",
    );

    for &ip in &reachable {
        let insn = &graph.insns[ip as usize];
        let label = insn_label(insn, graph, ip);
        let _ = writeln!(
            out,
            r#"  n{ip} [label="{ip}: {}"];"#,
            escape_dot_label(&label)
        );
    }

    for &ip in &reachable {
        let insn = &graph.insns[ip as usize];
        let targets = insn_successors(insn, ip);
        for t in targets {
            if reachable.contains(&t) {
                let _ = writeln!(out, "  n{ip} -> n{t};");
            }
        }
    }

    let _ = writeln!(out, "}}");
    out
}

fn insn_label(insn: &Insn, graph: &BuiltGraph, _ip: InsnId) -> String {
    match insn {
        Insn::Byte { byte, .. } => format!("byte 0x{byte:02X}"),
        Insn::ByteEither { a, b, .. } => format!("byte 0x{a:02X}|0x{b:02X}"),
        Insn::ByteIn3 { a, b, c, .. } => format!("byte 0x{a:02X}|0x{b:02X}|0x{c:02X}"),
        Insn::ByteRange { lo, hi, .. } => format!("byte_range 0x{lo:02X}-0x{hi:02X}"),
        Insn::Class { label_id, .. } => format!("[class label#{label_id}]"),
        Insn::ConsumeWhileClass { label_id, min, .. } => {
            format!("[consume class label#{label_id} min={min}]")
        }
        Insn::Literal { lit_id, .. } => {
            let lit = get_literal(graph, *lit_id);
            format!("\"{}\"", escape_literal_display(lit))
        }
        Insn::LiteralSmall { len, bytes, .. } => {
            let n = *len as usize;
            format!(
                "\"{}\"",
                escape_literal_display(&bytes[..n.min(bytes.len())])
            )
        }
        Insn::EndOfInput { .. } => "eoi".to_string(),
        Insn::Fail => "fail".to_string(),
        Insn::AnyChar { .. } => ".".to_string(),
        Insn::Char { codepoint, .. } => format!("char U+{codepoint:04X}"),
        Insn::CharRange { lo, hi, .. } => format!("char U+{lo:04X}-U+{hi:04X}"),
        Insn::Jump { target } => format!("jump {target}"),
        Insn::Call { rule } => {
            let name = graph.rule_name_at(*rule as usize);
            format!("call {name}")
        }
        Insn::Return => "return".to_string(),
        Insn::Choice { alt } => format!("choice alt={alt}"),
        Insn::Commit { target } => format!("commit {target}"),
        Insn::BackCommit { target } => format!("back_commit {target}"),
        Insn::PartialCommit { target } => format!("partial_commit {target}"),
        Insn::NegBackCommit { target } => format!("neg_back_commit {target}"),
        Insn::ByteDispatch {
            table_id,
            fallback,
            push_choice_backtrack,
        } => format!("byte_dispatch {table_id} fb={fallback} bt={push_choice_backtrack}"),
        Insn::IfFlag { flag_id, .. } => format!("if_flag {flag_id}"),
        Insn::IfNotFlag { flag_id, .. } => format!("if_not_flag {flag_id}"),
        Insn::PushFlags { mask_id } => format!("push_flags {mask_id}"),
        Insn::PopFlags => "pop_flags".to_string(),
        Insn::CaptureBegin { tag } => format!("capture_begin {tag}"),
        Insn::CaptureEnd { tag } => format!("capture_end {tag}"),
        Insn::NodeBegin { kind, field: _ } => format!("node_begin {kind}"),
        Insn::NodeEnd => "node_end".to_string(),
        Insn::TokenBegin { kind, is_trivia } => {
            format!("token_begin {kind} trivia={is_trivia}")
        }
        Insn::TokenEnd => "token_end".to_string(),
        Insn::RecordExpectedLabel { label_id } => format!("expect_label #{label_id}"),
        Insn::PushDiagnosticContext { label_id } => format!("push_diag_ctx #{label_id}"),
        Insn::PopDiagnosticContext => "pop_diag_ctx".to_string(),
        Insn::SetHint { hint_id } => format!("set_hint #{}", hint_id.0),
        Insn::TracePoint { label_id } => format!("trace_point #{}", label_id.0),
        Insn::RecoverUntil { sync_rule, resume } => {
            format!("recover_until rule#{sync_rule} → {resume}")
        }
        Insn::RecoveryResume => "recovery_resume".to_string(),
        Insn::Accept => "accept".to_string(),
    }
}

fn insn_successors(insn: &Insn, ip: InsnId) -> Vec<InsnId> {
    let mut s = Vec::new();
    match insn {
        Insn::Return | Insn::Accept | Insn::Fail => {}
        Insn::Jump { target } => s.push(*target),
        Insn::Call { .. }
        | Insn::ByteDispatch { .. }
        | Insn::PushFlags { .. }
        | Insn::PopFlags
        | Insn::CaptureBegin { .. }
        | Insn::CaptureEnd { .. }
        | Insn::NodeBegin { .. }
        | Insn::NodeEnd
        | Insn::TokenBegin { .. }
        | Insn::TokenEnd
        | Insn::RecordExpectedLabel { .. }
        | Insn::PushDiagnosticContext { .. }
        | Insn::PopDiagnosticContext
        | Insn::SetHint { .. }
        | Insn::TracePoint { .. }
        | Insn::RecoverUntil { .. }
        | Insn::RecoveryResume => s.push(ip + 1),
        Insn::Choice { alt } => {
            s.push(ip + 1);
            s.push(*alt);
        }
        Insn::Commit { target }
        | Insn::BackCommit { target }
        | Insn::PartialCommit { target }
        | Insn::NegBackCommit { target } => {
            s.push(ip + 1);
            s.push(*target);
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
            s.push(ip + 1);
            s.push(*on_fail);
        }
    }
    s
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

fn escape_literal_display(lit: &[u8]) -> String {
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

fn escape_dot_label(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}
