use proc_macro2::TokenStream;
use quote::quote;

pub(super) fn quote_grammar_binding() -> TokenStream {
    quote! {
        pub static GRAMMAR: ParseGraph = ParseGraph {
            insns: INSNS,
            rule_entry: RULE_ENTRY,
            jump_tables: JUMP_TABLES,
            literals: LiteralTable { data: LIT_DATA, offsets: LIT_OFFSETS },
            flag_masks: FlagMaskTable { data: FLAG_MASK_DATA, offsets: FLAG_MASK_OFFSETS },
            strings: &STRING_TABLE,
            rule_names: RULE_NAMES,
            tag_names: TAG_NAMES,
            class_labels: CLASS_LABELS,
            expected_labels: EXPECTED_LABELS,
            field_names: FIELD_NAMES,
        };
    }
}
