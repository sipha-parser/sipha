use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput, Error, Expr, Fields};

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

pub fn derive_ast_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let kind_expr = match parse_kind_expr(&input.attrs) {
        Ok(k) => k,
        Err(e) => return e.to_compile_error().into(),
    };

    let field_access = match &input.data {
        Data::Struct(s) => match &s.fields {
            Fields::Unnamed(u) if u.unnamed.len() == 1 => quote!(self.0),
            _ => return Error::new(
                s.fields.span(),
                "AstNode can only be derived for tuple structs with exactly one field (SyntaxNode)",
            )
            .to_compile_error()
            .into(),
        },
        Data::Enum(e) => {
            return Error::new(e.enum_token.span, "AstNode can only be derived for structs")
                .to_compile_error()
                .into()
        }
        Data::Union(u) => {
            return Error::new(
                u.union_token.span,
                "AstNode can only be derived for structs",
            )
            .to_compile_error()
            .into()
        }
    };

    quote! {
        impl #impl_generics sipha::tree::ast::AstNode for #name #ty_generics #where_clause {
            #[inline]
            fn can_cast(kind: sipha::types::SyntaxKind) -> bool {
                kind == sipha::types::IntoSyntaxKind::into_syntax_kind(#kind_expr)
            }

            #[inline]
            fn cast(node: sipha::tree::red::SyntaxNode) -> Option<Self> {
                Self::can_cast(node.kind()).then(|| Self(node))
            }

            #[inline]
            fn syntax(&self) -> &sipha::tree::red::SyntaxNode {
                &#field_access
            }
        }
    }
    .into()
}
