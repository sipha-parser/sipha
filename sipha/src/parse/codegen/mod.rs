//! # Code Generator
//!
//! Emits a self-contained Rust source file from a [`BuiltGraph`].

mod grammar;
mod header;
mod insns;
mod tables;

use crate::parse::builder::BuiltGraph;
use proc_macro2::TokenStream;
use quote::quote;

/// Emit Rust source for the given grammar to `out`.
///
/// # Errors
///
/// Returns [`std::fmt::Result`] if writing to `out` fails.
pub fn emit_rust(graph: &BuiltGraph, out: &mut String) -> std::fmt::Result {
    let tokens = emit_rust_tokens(graph);

    #[cfg(feature = "codegen-pretty")]
    {
        // Parse the generated token stream as a Rust file and pretty-print it.
        // This avoids `TokenStream::to_string()` spacing oddities (notably in attributes).
        if let Ok(file) = syn::parse2::<syn::File>(tokens.clone()) {
            out.push_str(&prettyplease::unparse(&file));
            return Ok(());
        }
    }

    std::fmt::Write::write_fmt(out, format_args!("{tokens}"))
}

fn emit_rust_tokens(graph: &BuiltGraph) -> TokenStream {
    let header = header::quote_rust_header();
    let literals = tables::quote_literal_tables(graph);
    let flag_masks = tables::quote_flag_mask_tables(graph);
    let jump_tables = tables::quote_jump_tables(graph);
    let pool_and_rules = tables::quote_string_pool_and_rule_tables(graph);
    let insns = insns::quote_insns_table(graph);
    let grammar = grammar::quote_grammar_binding();
    quote! {
        #header
        #literals
        #flag_masks
        #jump_tables
        #pool_and_rules
        #insns
        #grammar
    }
}
