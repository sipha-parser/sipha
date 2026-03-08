//! Derive macro for [`IntoSyntaxKind`] and [`FromSyntaxKind`].
//!
//! [`IntoSyntaxKind`]: https://docs.rs/sipha/latest/sipha/types/trait.IntoSyntaxKind.html
//! [`FromSyntaxKind`]: https://docs.rs/sipha/latest/sipha/types/trait.FromSyntaxKind.html

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Error, Fields};

/// Derives [`IntoSyntaxKind`] and [`FromSyntaxKind`] for an enum of syntax kinds.
///
/// The enum must have [`#[repr(u16)]`](https://doc.rust-lang.org/reference/type-layout.html#repr-attribute)
/// so that variant discriminants are 0, 1, 2, … in definition order. Only unit variants
/// (no fields) are supported.
///
/// # Example
///
/// ```rust,ignore
/// use sipha::SyntaxKinds;
///
/// #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SyntaxKinds)]
/// #[repr(u16)]
/// pub enum Kind {
///     Root,
///     Stmt,
///     TokKeyword,
///     TokIdent,
/// }
///
/// // Then use with the grammar builder and kind_as():
/// // g.node(Kind::Root, |g| { ... });
/// // node.kind_as::<Kind>()
/// ```
pub fn derive_syntax_kinds(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => {
            return Error::new_spanned(name, "SyntaxKinds can only be derived for enums")
                .to_compile_error()
                .into()
        }
    };

    let mut unit_variants = Vec::new();
    for v in variants {
        match &v.fields {
            Fields::Unit => unit_variants.push(v),
            _ => {
                return Error::new_spanned(v, "SyntaxKinds only supports unit variants (no fields)")
                    .to_compile_error()
                    .into()
            }
        }
    }

    if unit_variants.is_empty() {
        return Error::new_spanned(name, "SyntaxKinds enum must have at least one variant")
            .to_compile_error()
            .into();
    }

    let variant_idents: Vec<_> = unit_variants.iter().map(|v| &v.ident).collect();

    let into_impl = quote! {
        impl #impl_generics sipha::types::IntoSyntaxKind for #name #ty_generics #where_clause {
            #[inline]
            fn into_syntax_kind(self) -> sipha::types::SyntaxKind {
                self as sipha::types::SyntaxKind
            }
        }
    };

    let from_impl = quote! {
        impl #impl_generics sipha::types::FromSyntaxKind for #name #ty_generics #where_clause {
            fn from_syntax_kind(k: sipha::types::SyntaxKind) -> Option<Self> {
                const COUNT: u16 = [ #( #name::#variant_idents ),* ].len() as u16;
                if k < COUNT {
                    Some(unsafe { ::core::mem::transmute(k) })
                } else {
                    None
                }
            }
        }
    };

    quote! {
        #into_impl
        #from_impl
    }
    .into()
}
