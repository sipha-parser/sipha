//! Derive [`LexKind`](sipha::types::LexKind), [`FromLexKind`](sipha::types::FromLexKind),
//! and [`IntoSyntaxKind`](sipha::types::IntoSyntaxKind) for a `#[repr(u16)]` unit enum.
//!
//! Lexical kinds occupy the low `SyntaxKind` range `0 .. COUNT` (contiguous).

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Error, Fields};

pub fn derive_lex_kinds(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => {
            return Error::new_spanned(name, "LexKinds can only be derived for enums")
                .to_compile_error()
                .into();
        }
    };

    let mut unit_variants = Vec::new();
    for v in variants {
        match &v.fields {
            Fields::Unit => {
                unit_variants.push(v);
            }
            _ => {
                return Error::new_spanned(v, "LexKinds only supports unit variants (no fields)")
                    .to_compile_error()
                    .into();
            }
        }
    }

    if unit_variants.is_empty() {
        return Error::new_spanned(name, "LexKinds enum must have at least one variant")
            .to_compile_error()
            .into();
    }

    let variant_idents: Vec<_> = unit_variants.iter().map(|v| &v.ident).collect();
    let n = variant_idents.len();

    quote! {
        impl #impl_generics sipha::types::IntoSyntaxKind for #name #ty_generics #where_clause {
            #[inline]
            fn into_syntax_kind(self) -> sipha::types::SyntaxKind {
                self as sipha::types::SyntaxKind
            }
        }

        impl #impl_generics sipha::types::FromLexKind for #name #ty_generics #where_clause {
            const COUNT: u16 = #n as u16;

            fn try_from_lex(k: sipha::types::SyntaxKind) -> Option<Self> {
                if k < Self::COUNT {
                    Some(unsafe { ::core::mem::transmute::<u16, Self>(k) })
                } else {
                    None
                }
            }
        }

        impl #impl_generics sipha::types::FromSyntaxKind for #name #ty_generics #where_clause {
            #[inline]
            fn from_syntax_kind(k: sipha::types::SyntaxKind) -> Option<Self> {
                <Self as sipha::types::FromLexKind>::try_from_lex(k)
            }
        }
    }
    .into()
}
