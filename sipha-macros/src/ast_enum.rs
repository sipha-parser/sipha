use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput, Error, Expr, Fields, Ident,
};

fn parse_kind_expr(attrs: &[Attribute]) -> Result<Expr, Error> {
    for attr in attrs {
        if !attr.path().is_ident("ast") {
            continue;
        }
        let mut kind_expr: Option<Expr> = None;
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("kind") {
                let value = meta.value()?;
                let expr: Expr = value.parse()?;
                kind_expr = Some(expr);
                Ok(())
            } else {
                Err(meta.error("unsupported #[ast(...)] key; expected `kind = ...`"))
            }
        })?;

        return kind_expr
            .ok_or_else(|| Error::new_spanned(attr, "missing `kind = ...` in #[ast(...)]"));
    }

    Err(Error::new(
        proc_macro2::Span::call_site(),
        "missing #[ast(kind = ...)] attribute",
    ))
}

fn to_snake(ident: &Ident) -> Ident {
    // Lightweight PascalCase -> snake_case for `as_foo_bar` style methods.
    // This is not a full unicode case mapping, but is fine for Rust idents.
    let s = ident.to_string();
    let mut out = String::new();
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i != 0 {
                out.push('_');
            }
            for lc in ch.to_lowercase() {
                out.push(lc);
            }
        } else {
            out.push(ch);
        }
    }
    format_ident!("{out}", span = ident.span())
}

pub fn derive_ast_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        Data::Struct(s) => {
            return Error::new(s.struct_token.span, "AstEnum can only be derived for enums")
                .to_compile_error()
                .into()
        }
        Data::Union(u) => {
            return Error::new(u.union_token.span, "AstEnum can only be derived for enums")
                .to_compile_error()
                .into()
        }
    };

    if variants.is_empty() {
        return Error::new_spanned(name, "AstEnum enum must have at least one variant")
            .to_compile_error()
            .into();
    }

    let mut can_cast_arms = Vec::new();
    let mut cast_arms = Vec::new();
    let mut syntax_arms = Vec::new();
    let mut inherent_methods = Vec::new();
    let mut from_impls = Vec::new();

    for v in variants {
        let v_ident = &v.ident;
        let kind_expr = match parse_kind_expr(&v.attrs) {
            Ok(k) => k,
            Err(e) => {
                return Error::new(v.span(), e.to_string())
                    .to_compile_error()
                    .into()
            }
        };

        let inner_ty = match &v.fields {
            Fields::Unnamed(u) if u.unnamed.len() == 1 => &u.unnamed[0].ty,
            Fields::Unnamed(u) => {
                return Error::new(
                    u.span(),
                    "AstEnum only supports tuple variants with exactly one field",
                )
                .to_compile_error()
                .into()
            }
            Fields::Named(n) => {
                return Error::new(
                    n.span(),
                    "AstEnum only supports tuple variants (no named fields)",
                )
                .to_compile_error()
                .into()
            }
            Fields::Unit => {
                return Error::new(v.span(), "AstEnum does not support unit variants")
                    .to_compile_error()
                    .into()
            }
        };

        can_cast_arms.push(quote! {
            k if k == sipha::types::IntoSyntaxKind::into_syntax_kind(#kind_expr) => true
        });

        cast_arms.push(quote! {
            k if k == sipha::types::IntoSyntaxKind::into_syntax_kind(#kind_expr) => {
                <#inner_ty as sipha::tree::ast::AstNode>::cast(node).map(#name::#v_ident)
            }
        });

        syntax_arms.push(quote! { #name::#v_ident(inner) => inner.syntax() });

        let snake = to_snake(v_ident);
        let as_fn = format_ident!("as_{snake}", span = v_ident.span());
        let into_fn = format_ident!("into_{snake}", span = v_ident.span());

        inherent_methods.push(quote! {
            #[inline]
            pub fn #as_fn(&self) -> ::core::option::Option<&#inner_ty> {
                match self {
                    #name::#v_ident(inner) => ::core::option::Option::Some(inner),
                    _ => ::core::option::Option::None,
                }
            }

            #[inline]
            pub fn #into_fn(self) -> ::core::option::Option<#inner_ty> {
                match self {
                    #name::#v_ident(inner) => ::core::option::Option::Some(inner),
                    _ => ::core::option::Option::None,
                }
            }
        });

        from_impls.push(quote! {
            impl #impl_generics ::core::convert::From<#inner_ty> for #name #ty_generics #where_clause {
                #[inline]
                fn from(v: #inner_ty) -> Self {
                    #name::#v_ident(v)
                }
            }
        });
    }

    quote! {
        impl #impl_generics sipha::tree::ast::AstNode for #name #ty_generics #where_clause {
            #[inline]
            fn can_cast(kind: sipha::types::SyntaxKind) -> bool {
                match kind {
                    #( #can_cast_arms, )*
                    _ => false,
                }
            }

            #[inline]
            fn cast(node: sipha::tree::red::SyntaxNode) -> ::core::option::Option<Self> {
                match node.kind() {
                    #( #cast_arms, )*
                    _ => ::core::option::Option::None,
                }
            }

            #[inline]
            fn syntax(&self) -> &sipha::tree::red::SyntaxNode {
                match self {
                    #( #syntax_arms, )*
                }
            }
        }

        impl #impl_generics #name #ty_generics #where_clause {
            #( #inherent_methods )*
        }

        #( #from_impls )*
    }
    .into()
}
