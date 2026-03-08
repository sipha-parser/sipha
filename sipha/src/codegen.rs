//! # Code Generator
//!
//! Emits a self-contained Rust source file from a [`BuiltGraph`].

use crate::{builder::BuiltGraph, insn::Insn};
use std::fmt::Write;

/// Emit Rust source for the given grammar to `out`.
///
/// # Errors
///
/// Returns [`std::fmt::Result`] if writing to `out` fails.
#[allow(clippy::too_many_lines)]
pub fn emit_rust(graph: &BuiltGraph, out: &mut String) -> std::fmt::Result {
    writeln!(out, "// AUTO-GENERATED — do not edit by hand.")?;
    writeln!(out, "#![allow(clippy::all, dead_code, unused)]")?;
    writeln!(out)?;
    writeln!(
        out,
        "use sipha::insn::{{Insn, LiteralTable, FlagMaskTable, ParseGraph}};"
    )?;
    writeln!(out, "use sipha::types::CharClass;")?;
    writeln!(out, "use sipha::context::FlagMaskWord;")?;
    writeln!(out)?;

    // Literal blob
    writeln!(out, "static LIT_DATA: &[u8] = &[")?;
    write!(out, "    ")?;
    for (i, b) in graph.literal_data.iter().enumerate() {
        write!(out, "0x{b:02X},")?;
        if i % 16 == 15 {
            write!(out, "\n    ")?;
        }
    }
    writeln!(out, "\n];")?;
    writeln!(out, "static LIT_OFFSETS: &[u32] = &[")?;
    write!(out, "    ")?;
    for o in &graph.literal_offsets {
        write!(out, "{o},")?;
    }
    writeln!(out, "\n];")?;
    writeln!(out)?;

    // Flag mask table
    writeln!(out, "static FLAG_MASK_DATA: &[FlagMaskWord] = &[")?;
    for e in &graph.flag_mask_data {
        writeln!(
            out,
            "    FlagMaskWord {{ word: {}, set_bits: {:#018X}u64, clear_bits: {:#018X}u64 }},",
            e.word, e.set_bits, e.clear_bits
        )?;
    }
    writeln!(out, "];")?;
    writeln!(out, "static FLAG_MASK_OFFSETS: &[u32] = &[")?;
    write!(out, "    ")?;
    for o in &graph.flag_mask_offsets {
        write!(out, "{o},")?;
    }
    writeln!(out, "\n];")?;
    writeln!(out)?;

    // Jump tables
    if graph.jump_tables.is_empty() {
        writeln!(out, "static JUMP_TABLES: &[[u32; 256]] = &[];")?;
    } else {
        writeln!(out, "static JUMP_TABLES: &[[u32; 256]] = &[")?;
        for (ti, table) in graph.jump_tables.iter().enumerate() {
            writeln!(out, "    // table {ti}")?;
            writeln!(out, "    [")?;
            for chunk in table.chunks(8) {
                write!(out, "        ")?;
                for &v in chunk {
                    if v == u32::MAX {
                        write!(out, "u32::MAX, ")?;
                    } else {
                        write!(out, "{v}_u32, ")?;
                    }
                }
                writeln!(out)?;
            }
            writeln!(out, "    ],")?;
        }
        writeln!(out, "];")?;
    }
    writeln!(out)?;

    // Rule entry + name tables
    writeln!(out, "static RULE_ENTRY: &[u32] = &[")?;
    for (name, ip) in graph.rule_names.iter().zip(&graph.rule_entry) {
        writeln!(out, "    {ip}, // {name}")?;
    }
    writeln!(out, "];")?;
    writeln!(out, "static RULE_NAMES: &[&str] = &[")?;
    for n in &graph.rule_names {
        writeln!(out, "    \"{n}\",")?;
    }
    writeln!(out, "];")?;
    writeln!(out, "static TAG_NAMES: &[&str] = &[")?;
    for n in &graph.tag_names {
        writeln!(out, "    \"{n}\",")?;
    }
    writeln!(out, "];")?;
    writeln!(out, "static CLASS_LABELS: &[&str] = &[")?;
    for n in &graph.class_labels {
        writeln!(out, "    \"{n}\",")?;
    }
    writeln!(out, "];")?;
    writeln!(out, "static EXPECTED_LABELS: &[&str] = &[")?;
    for n in &graph.expected_labels {
        writeln!(out, "    \"{n}\",")?;
    }
    writeln!(out, "];")?;
    writeln!(out, "static FIELD_NAMES: &[&str] = &[")?;
    for n in &graph.field_names {
        writeln!(out, "    \"{n}\",")?;
    }
    writeln!(out, "];")?;
    writeln!(out)?;

    // Instructions
    writeln!(out, "static INSNS: &[Insn] = &[")?;
    for (i, insn) in graph.insns.iter().enumerate() {
        write!(out, "    /* {i:04} */ ")?;
        emit_insn(out, insn)?;
        writeln!(out, ",")?;
    }
    writeln!(out, "];")?;
    writeln!(out)?;

    // ParseGraph binding
    writeln!(out, "pub static GRAMMAR: ParseGraph = ParseGraph {{")?;
    writeln!(out, "    insns:       INSNS,")?;
    writeln!(out, "    rule_entry:  RULE_ENTRY,")?;
    writeln!(out, "    jump_tables: JUMP_TABLES,")?;
    writeln!(
        out,
        "    literals:    LiteralTable {{ data: LIT_DATA, offsets: LIT_OFFSETS }},"
    )?;
    writeln!(
        out,
        "    flag_masks:  FlagMaskTable {{ data: FLAG_MASK_DATA, offsets: FLAG_MASK_OFFSETS }},"
    )?;
    writeln!(out, "    rule_names:   RULE_NAMES,")?;
    writeln!(out, "    tag_names:    TAG_NAMES,")?;
    writeln!(out, "    class_labels:    CLASS_LABELS,")?;
    writeln!(out, "    expected_labels: EXPECTED_LABELS,")?;
    writeln!(out, "    field_names:     FIELD_NAMES,")?;
    writeln!(out, "}};")?;
    Ok(())
}

fn emit_insn(out: &mut String, insn: &Insn) -> std::fmt::Result {
    match insn {
        Insn::Byte { byte, on_fail } => write!(
            out,
            "Insn::Byte {{ byte: 0x{byte:02X}, on_fail: {on_fail} }}"
        ),
        Insn::ByteRange { lo, hi, on_fail } => write!(
            out,
            "Insn::ByteRange {{ lo: 0x{lo:02X}, hi: 0x{hi:02X}, on_fail: {on_fail} }}"
        ),
        Insn::Class {
            class,
            label_id,
            on_fail,
        } => {
            let [w0, w1, w2, w3] = class.0;
            write!(out, "Insn::Class {{ class: CharClass([{w0:#018X}u64, {w1:#018X}u64, {w2:#018X}u64, {w3:#018X}u64]), label_id: {label_id}, on_fail: {on_fail} }}")
        }
        Insn::Literal { lit_id, on_fail } => write!(
            out,
            "Insn::Literal {{ lit_id: {lit_id}, on_fail: {on_fail} }}"
        ),
        Insn::EndOfInput { on_fail } => write!(out, "Insn::EndOfInput {{ on_fail: {on_fail} }}"),
        Insn::AnyChar { on_fail } => write!(out, "Insn::AnyChar {{ on_fail: {on_fail} }}"),
        Insn::Char { codepoint, on_fail } => write!(
            out,
            "Insn::Char {{ codepoint: 0x{codepoint:06X}_u32, on_fail: {on_fail} }}"
        ),
        Insn::CharRange { lo, hi, on_fail } => write!(
            out,
            "Insn::CharRange {{ lo: 0x{lo:06X}_u32, hi: 0x{hi:06X}_u32, on_fail: {on_fail} }}"
        ),
        Insn::Fail => write!(out, "Insn::Fail"),
        Insn::Jump { target } => write!(out, "Insn::Jump {{ target: {target} }}"),
        Insn::Call { rule } => write!(out, "Insn::Call {{ rule: {rule} }}"),
        Insn::Return => write!(out, "Insn::Return"),
        Insn::Choice { alt } => write!(out, "Insn::Choice {{ alt: {alt} }}"),
        Insn::Commit { target } => write!(out, "Insn::Commit {{ target: {target} }}"),
        Insn::BackCommit { target } => write!(out, "Insn::BackCommit {{ target: {target} }}"),
        Insn::NegBackCommit { target } => write!(out, "Insn::NegBackCommit {{ target: {target} }}"),
        Insn::PartialCommit { target } => write!(out, "Insn::PartialCommit {{ target: {target} }}"),
        Insn::ByteDispatch { table_id } => {
            write!(out, "Insn::ByteDispatch {{ table_id: {table_id} }}")
        }
        Insn::IfFlag { flag_id, on_fail } => write!(
            out,
            "Insn::IfFlag {{ flag_id: {flag_id}, on_fail: {on_fail} }}"
        ),
        Insn::IfNotFlag { flag_id, on_fail } => write!(
            out,
            "Insn::IfNotFlag {{ flag_id: {flag_id}, on_fail: {on_fail} }}"
        ),
        Insn::PushFlags { mask_id } => write!(out, "Insn::PushFlags {{ mask_id: {mask_id} }}"),
        Insn::PopFlags => write!(out, "Insn::PopFlags"),
        Insn::NodeBegin { kind, field } => match field {
            None => write!(out, "Insn::NodeBegin {{ kind: {kind}, field: None }}"),
            Some(f) => write!(out, "Insn::NodeBegin {{ kind: {kind}, field: Some({f}) }}"),
        },
        Insn::NodeEnd => write!(out, "Insn::NodeEnd"),
        Insn::TokenBegin { kind, is_trivia } => write!(
            out,
            "Insn::TokenBegin {{ kind: {kind}, is_trivia: {is_trivia} }}"
        ),
        Insn::TokenEnd => write!(out, "Insn::TokenEnd"),
        Insn::CaptureBegin { tag } => write!(out, "Insn::CaptureBegin {{ tag: {tag} }}"),
        Insn::CaptureEnd { tag } => write!(out, "Insn::CaptureEnd {{ tag: {tag} }}"),
        Insn::RecordExpectedLabel { label_id } => {
            write!(out, "Insn::RecordExpectedLabel {{ label_id: {label_id} }}")
        }
        Insn::RecoverUntil { sync_rule, resume } => write!(
            out,
            "Insn::RecoverUntil {{ sync_rule: {sync_rule}, resume: {resume} }}"
        ),
        Insn::RecoveryResume => write!(out, "Insn::RecoveryResume"),
        Insn::Accept => write!(out, "Insn::Accept"),
    }
}
