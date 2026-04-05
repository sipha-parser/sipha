//! Derive [`IntoSyntaxKind`](sipha::types::IntoSyntaxKind), [`FromSyntaxKind`](sipha::types::FromSyntaxKind),
//! and [`FromRuleKind`](sipha::types::FromRuleKind) for a `#[repr(u16)]` unit enum.
//!
//! Implement [`RuleKind`](sipha::types::RuleKind) yourself for stable display names.
//!
//! Rule (CST node) kinds occupy `SyntaxKind` values starting at the lexical count of the paired
//! `#[sipha(lex = LexEnum)]` type: `base + variant_discriminant`.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Error, Fields, Type, TypePath};

fn parse_lex_type(input: &DeriveInput) -> Result<Type, Error> {
    let mut lex_ty: Option<Type> = None;
    for attr in &input.attrs {
        if !attr.path().is_ident("sipha") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("lex") {
                let v: Type = meta.value()?.parse()?;
                lex_ty = Some(v);
                Ok(())
            } else {
                Ok(())
            }
        })
        .map_err(|e| Error::new(attr.span(), e.to_string()))?;
    }
    lex_ty.ok_or_else(|| {
        Error::new(
            input.span(),
            "missing #[sipha(lex = YourLexEnum)] on RuleKinds enum; \
             node kinds are laid out after lexical kinds",
        )
    })
}

pub fn derive_rule_kinds(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let lex_ty = match parse_lex_type(&input) {
        Ok(t) => t,
        Err(e) => return e.to_compile_error().into(),
    };

    let lex_path = match &lex_ty {
        Type::Path(TypePath { qself: None, path }) => path,
        _ => {
            return Error::new_spanned(&lex_ty, "lex = … must be a simple path (e.g. Lex)")
                .to_compile_error()
                .into();
        }
    };

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => {
            return Error::new_spanned(name, "RuleKinds can only be derived for enums")
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
                return Error::new_spanned(v, "RuleKinds only supports unit variants (no fields)")
                    .to_compile_error()
                    .into();
            }
        }
    }

    if unit_variants.is_empty() {
        return Error::new_spanned(name, "RuleKinds enum must have at least one variant")
            .to_compile_error()
            .into();
    }

    let variant_idents: Vec<_> = unit_variants.iter().map(|v| &v.ident).collect();
    let n = variant_idents.len();

    quote! {
        impl #impl_generics sipha::types::IntoSyntaxKind for #name #ty_generics #where_clause {
            #[inline]
            fn into_syntax_kind(self) -> sipha::types::SyntaxKind {
                <#lex_path as sipha::types::FromLexKind>::COUNT + (self as sipha::types::SyntaxKind)
            }
        }

        impl #impl_generics sipha::types::FromRuleKind for #name #ty_generics #where_clause {
            const BASE: u16 = <#lex_path as sipha::types::FromLexKind>::COUNT;
            const COUNT: u16 = #n as u16;

            fn try_from_rule(k: sipha::types::SyntaxKind) -> Option<Self> {
                let k = k.checked_sub(Self::BASE)?;
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
                <Self as sipha::types::FromRuleKind>::try_from_rule(k)
            }
        }
    }
    .into()
}
