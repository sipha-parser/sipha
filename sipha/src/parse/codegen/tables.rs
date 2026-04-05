use crate::parse::builder::BuiltGraph;
use crate::parse::string_table::SymbolId;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(super) fn quote_literal_tables(graph: &BuiltGraph) -> TokenStream {
    let byte_lits: Vec<_> = graph
        .literal_data
        .iter()
        .map(|b| proc_macro2::Literal::u8_unsuffixed(*b))
        .collect();
    let offset_lits: Vec<_> = graph
        .literal_offsets
        .iter()
        .map(|o| proc_macro2::Literal::u32_unsuffixed(*o))
        .collect();
    quote! {
        static LIT_DATA: &[u8] = &[ #( #byte_lits ),* ];
        static LIT_OFFSETS: &[u32] = &[ #( #offset_lits ),* ];
    }
}

pub(super) fn quote_flag_mask_tables(graph: &BuiltGraph) -> TokenStream {
    let words: Vec<u32> = graph.flag_mask_data.iter().map(|e| e.word).collect();
    let set_bits: Vec<u64> = graph.flag_mask_data.iter().map(|e| e.set_bits).collect();
    let clear_bits: Vec<u64> = graph.flag_mask_data.iter().map(|e| e.clear_bits).collect();
    let offset_lits: Vec<_> = graph
        .flag_mask_offsets
        .iter()
        .map(|o| proc_macro2::Literal::u32_unsuffixed(*o))
        .collect();
    quote! {
        static FLAG_MASK_DATA: &[FlagMaskWord] = &[
            #( FlagMaskWord { word: #words, set_bits: #set_bits, clear_bits: #clear_bits } ),*
        ];
        static FLAG_MASK_OFFSETS: &[u32] = &[ #( #offset_lits ),* ];
    }
}

pub(super) fn quote_jump_tables(graph: &BuiltGraph) -> TokenStream {
    if graph.jump_tables.is_empty() {
        return quote! {
            static JUMP_TABLES: &[[u32; 256]] = &[];
        };
    }

    let tables: Vec<TokenStream> = graph
        .jump_tables
        .iter()
        .map(|table| {
            let elems: Vec<TokenStream> = table
                .iter()
                .map(|&v| {
                    if v == u32::MAX {
                        quote! { u32::MAX }
                    } else {
                        quote! { #v }
                    }
                })
                .collect();
            quote! {
                [ #( #elems ),* ]
            }
        })
        .collect();

    quote! {
        static JUMP_TABLES: &[[u32; 256]] = &[
            #( #tables ),*
        ];
    }
}

pub(super) fn quote_string_pool_and_rule_tables(graph: &BuiltGraph) -> TokenStream {
    let pool = graph.strings.pool_strings_for_codegen();
    let str_lits: Vec<TokenStream> = pool
        .iter()
        .map(|s| {
            let lit = proc_macro2::Literal::string(s);
            quote! { #lit }
        })
        .collect();

    let rule_ips = &graph.rule_entry;

    let rule_names = quote_symbol_slice("RULE_NAMES", &graph.rule_names);
    let rule_diagnostic_labels =
        quote_symbol_slice("RULE_DIAGNOSTIC_LABELS", &graph.rule_diagnostic_labels);
    let tag_names = quote_symbol_slice("TAG_NAMES", &graph.tag_names);
    let class_labels = quote_symbol_slice("CLASS_LABELS", &graph.class_labels);
    let expected_labels = quote_symbol_slice("EXPECTED_LABELS", &graph.expected_labels);
    let field_names = quote_symbol_slice("FIELD_NAMES", &graph.field_names);

    quote! {
        static STR_POOL: &[&str] = &[ #( #str_lits ),* ];
        static STRING_TABLE: StringTable = StringTable::from_static_pool(STR_POOL);

        static RULE_ENTRY: &[u32] = &[ #( #rule_ips ),* ];

        #rule_names
        #rule_diagnostic_labels
        #tag_names
        #class_labels
        #expected_labels
        #field_names
    }
}

fn quote_symbol_slice(name: &str, syms: &[SymbolId]) -> TokenStream {
    let name = format_ident!("{}", name);
    let ids: Vec<u32> = syms.iter().map(|s| s.0).collect();
    quote! {
        static #name: &[SymbolId] = &[ #( SymbolId(#ids) ),* ];
    }
}
