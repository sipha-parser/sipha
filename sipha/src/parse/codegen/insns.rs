use crate::parse::{builder::BuiltGraph, insn::Insn};
use proc_macro2::TokenStream;
use quote::quote;

pub(super) fn quote_insns_table(graph: &BuiltGraph) -> TokenStream {
    let insns: Vec<TokenStream> = graph.insns.iter().map(insn_tokens).collect();
    quote! {
        static INSNS: &[Insn] = &[ #( #insns ),* ];
    }
}

#[allow(clippy::too_many_lines)]
fn insn_tokens(insn: &Insn) -> TokenStream {
    match insn {
        Insn::Byte { byte, on_fail } => quote! {
            Insn::Byte { byte: #byte, on_fail: #on_fail }
        },
        Insn::ByteEither { a, b, on_fail } => quote! {
            Insn::ByteEither { a: #a, b: #b, on_fail: #on_fail }
        },
        Insn::ByteIn3 { a, b, c, on_fail } => quote! {
            Insn::ByteIn3 { a: #a, b: #b, c: #c, on_fail: #on_fail }
        },
        Insn::ByteRange { lo, hi, on_fail } => quote! {
            Insn::ByteRange { lo: #lo, hi: #hi, on_fail: #on_fail }
        },
        Insn::Class {
            class,
            label_id,
            on_fail,
        } => {
            let [w0, w1, w2, w3] = class.0;
            quote! {
                Insn::Class {
                    class: CharClass([#w0, #w1, #w2, #w3]),
                    label_id: #label_id,
                    on_fail: #on_fail,
                }
            }
        }
        Insn::Literal { lit_id, on_fail } => quote! {
            Insn::Literal { lit_id: #lit_id, on_fail: #on_fail }
        },
        Insn::LiteralSmall {
            len,
            bytes,
            on_fail,
        } => {
            let [b0, b1, b2, b3, b4, b5, b6, b7] = *bytes;
            quote! {
                Insn::LiteralSmall { len: #len, bytes: [#b0, #b1, #b2, #b3, #b4, #b5, #b6, #b7], on_fail: #on_fail }
            }
        }
        Insn::EndOfInput { on_fail } => quote! {
            Insn::EndOfInput { on_fail: #on_fail }
        },
        Insn::AnyChar { on_fail } => quote! {
            Insn::AnyChar { on_fail: #on_fail }
        },
        Insn::Char { codepoint, on_fail } => quote! {
            Insn::Char { codepoint: #codepoint, on_fail: #on_fail }
        },
        Insn::CharRange { lo, hi, on_fail } => quote! {
            Insn::CharRange { lo: #lo, hi: #hi, on_fail: #on_fail }
        },
        Insn::Fail => quote! { Insn::Fail },
        Insn::Jump { target } => quote! {
            Insn::Jump { target: #target }
        },
        Insn::Call { rule } => quote! {
            Insn::Call { rule: #rule }
        },
        Insn::Return => quote! { Insn::Return },
        Insn::Choice { alt } => quote! {
            Insn::Choice { alt: #alt }
        },
        Insn::Commit { target } => quote! {
            Insn::Commit { target: #target }
        },
        Insn::BackCommit { target } => quote! {
            Insn::BackCommit { target: #target }
        },
        Insn::NegBackCommit { target } => quote! {
            Insn::NegBackCommit { target: #target }
        },
        Insn::PartialCommit { target } => quote! {
            Insn::PartialCommit { target: #target }
        },
        Insn::ByteDispatch {
            table_id,
            fallback,
            push_choice_backtrack,
        } => quote! {
            Insn::ByteDispatch {
                table_id: #table_id,
                fallback: #fallback,
                push_choice_backtrack: #push_choice_backtrack,
            }
        },
        Insn::ConsumeWhileClass {
            class,
            label_id,
            min,
            on_fail,
        } => {
            let [w0, w1, w2, w3] = class.0;
            quote! {
                Insn::ConsumeWhileClass {
                    class: CharClass([#w0, #w1, #w2, #w3]),
                    label_id: #label_id,
                    min: #min,
                    on_fail: #on_fail,
                }
            }
        }
        Insn::IfFlag { flag_id, on_fail } => quote! {
            Insn::IfFlag { flag_id: #flag_id, on_fail: #on_fail }
        },
        Insn::IfNotFlag { flag_id, on_fail } => quote! {
            Insn::IfNotFlag { flag_id: #flag_id, on_fail: #on_fail }
        },
        Insn::PushFlags { mask_id } => quote! {
            Insn::PushFlags { mask_id: #mask_id }
        },
        Insn::PopFlags => quote! { Insn::PopFlags },
        Insn::NodeBegin { kind, field } => {
            let field_ts = field.map_or_else(|| quote! { None }, |f| quote! { Some(#f) });
            quote! {
                Insn::NodeBegin { kind: #kind, field: #field_ts }
            }
        }
        Insn::NodeEnd => quote! { Insn::NodeEnd },
        Insn::TokenBegin { kind, is_trivia } => quote! {
            Insn::TokenBegin { kind: #kind, is_trivia: #is_trivia }
        },
        Insn::TokenEnd => quote! { Insn::TokenEnd },
        Insn::CaptureBegin { tag } => quote! {
            Insn::CaptureBegin { tag: #tag }
        },
        Insn::CaptureEnd { tag } => quote! {
            Insn::CaptureEnd { tag: #tag }
        },
        Insn::RecordExpectedLabel { label_id } => quote! {
            Insn::RecordExpectedLabel { label_id: #label_id }
        },
        Insn::PushDiagnosticContext { label_id } => quote! {
            Insn::PushDiagnosticContext { label_id: #label_id }
        },
        Insn::PopDiagnosticContext => quote! { Insn::PopDiagnosticContext },
        Insn::SetHint { hint_id } => {
            let id = hint_id.0;
            quote! { Insn::SetHint { hint_id: SymbolId(#id) } }
        }
        Insn::TracePoint { label_id } => {
            let id = label_id.0;
            quote! { Insn::TracePoint { label_id: SymbolId(#id) } }
        }
        Insn::RecoverUntil { sync_rule, resume } => quote! {
            Insn::RecoverUntil { sync_rule: #sync_rule, resume: #resume }
        },
        Insn::RecoveryResume => quote! { Insn::RecoveryResume },
        Insn::Accept => quote! { Insn::Accept },
    }
}
