use proc_macro2::TokenStream;
use quote::quote;

pub(super) fn quote_rust_header() -> TokenStream {
    quote! {
        // AUTO-GENERATED — do not edit by hand.
        #![allow(dead_code, unused)]

        use sipha::parse::insn::{Insn, LiteralTable, FlagMaskTable, ParseGraph};
        use sipha::parse::string_table::{StringTable, SymbolId};
        use sipha::types::CharClass;
        use sipha::parse::context::FlagMaskWord;
    }
}
