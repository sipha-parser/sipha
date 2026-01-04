//! Procedural macros for Sipha
//!
//! This crate provides procedural macros for Sipha:
//! - `AstNode` derive macro for generating typed AST wrappers
//! - `grammar!` macro for declarative grammar definition
//! - `GrammarToken` derive macro for token type generation

mod codegen;
mod parser;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DataEnum, DeriveInput, Expr, Fields, LitStr, Meta, MetaNameValue, parse_macro_input,
};

/// Derive typed AST wrappers for a non-terminal enum
///
/// This macro generates typed wrapper structs for each non-terminal variant,
/// with basic methods for accessing children. In Phase 1, accessors work by
/// child index. Future phases will add label-based accessors.
///
/// # Example
///
/// ```rust,ignore
/// #[derive(AstNode)]
/// enum MyNonTerminal {
///     Expr,
///     Term,
/// }
/// ```
///
/// This will generate wrapper types like `ExprNode`, `TermNode` with methods
/// for accessing children.
#[proc_macro_derive(AstNode, attributes(grammar))]
#[allow(clippy::too_many_lines)]
pub fn derive_ast_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Extract enum variants
    let Data::Enum(DataEnum { variants, .. }) = &input.data else {
        return syn::Error::new(input.ident.span(), "AstNode can only be derived for enums")
            .to_compile_error()
            .into();
    };

    // Generate wrapper structs and implementations for each variant
    let mut wrapper_structs = Vec::new();
    let mut wrapper_impls = Vec::new();

    for variant in variants {
        if !matches!(variant.fields, Fields::Unit) {
            continue; // Skip non-unit variants for now
        }

        let variant_name = &variant.ident;
        let wrapper_name = syn::Ident::new(&format!("{variant_name}Node"), variant_name.span());

        // Extract label attributes (e.g., #[label("lhs", 0)])
        let mut label_accessors = Vec::new();
        for attr in &variant.attrs {
            if attr.path().is_ident("label")
                && let Ok(Meta::NameValue(MetaNameValue {
                    value:
                        Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(lit_str),
                            ..
                        }),
                    ..
                })) = attr.parse_args()
            {
                // Simple label attribute: #[label("lhs")]
                // For now, we'll generate accessors that work by position
                // In future phases, we can enhance this to use actual label information
                let label_name = lit_str.value();
                let method_name = syn::Ident::new(&label_name, lit_str.span());
                label_accessors.push((method_name, label_name));
            }
        }

        // Generate wrapper struct
        wrapper_structs.push(quote! {
            /// Typed AST wrapper for #variant_name
            pub struct #wrapper_name<K: sipha::syntax::SyntaxKind> {
                node: sipha::syntax::SyntaxNode<K>,
            }
        });

        // Generate label-based accessor methods
        let mut label_methods = Vec::new();
        for (idx, (method_name, _label_name)) in label_accessors.iter().enumerate() {
            label_methods.push(quote! {
                /// Get the #_label_name child (by position in sequence)
                pub fn #method_name(&self) -> Option<sipha::syntax::SyntaxNode<K>> {
                    self.nth_child(#idx)
                }
            });
        }

        // Generate wrapper impl with basic accessor methods
        wrapper_impls.push(quote! {
            impl<K: sipha::syntax::SyntaxKind> #wrapper_name<K> {
                /// Create a new wrapper from a syntax node
                pub fn new(node: sipha::syntax::SyntaxNode<K>) -> Self {
                    Self { node }
                }

                /// Get the underlying syntax node
                pub fn syntax_node(&self) -> &sipha::syntax::SyntaxNode<K> {
                    &self.node
                }

                /// Get the kind of this node
                pub fn kind(&self) -> K {
                    self.node.kind()
                }

                /// Get the text range of this node
                pub fn text_range(&self) -> sipha::syntax::TextRange {
                    self.node.text_range()
                }

                /// Get the text content of this node
                pub fn text(&self) -> String {
                    self.node.text()
                }

                /// Get the first child node
                pub fn first_child(&self) -> Option<sipha::syntax::SyntaxNode<K>> {
                    self.node.first_child()
                        .and_then(|elem| match elem {
                            sipha::syntax::SyntaxElement::Node(n) => Some(n),
                            _ => None,
                        })
                }

                /// Get the nth child node (0-indexed)
                pub fn nth_child(&self, index: usize) -> Option<sipha::syntax::SyntaxNode<K>> {
                    self.node.nth_child(index)
                        .and_then(|elem| match elem {
                            sipha::syntax::SyntaxElement::Node(n) => Some(n),
                            _ => None,
                        })
                }

                /// Get all child nodes
                pub fn children(&self) -> impl Iterator<Item = sipha::syntax::SyntaxNode<K>> + '_ {
                    self.node.children()
                        .filter_map(|elem| match elem {
                            sipha::syntax::SyntaxElement::Node(n) => Some(n),
                            _ => None,
                        })
                }

                // Label-based accessors
                #(#label_methods)*
            }

            impl<K: sipha::syntax::SyntaxKind> sipha::syntax::ast::AstNode<K> for #wrapper_name<K> {
                fn syntax_node(&self) -> &sipha::syntax::SyntaxNode<K> {
                    &self.node
                }
            }

            // Generate FromSyntaxNode implementation for this wrapper type
            impl<K: sipha::syntax::SyntaxKind> sipha::syntax::ast::FromSyntaxNode<K> for #wrapper_name<K> {
                type NodeType = Self;

                fn from_syntax_node(node: sipha::syntax::SyntaxNode<K>) -> Option<Self::NodeType> {
                    // Basic implementation - in future phases, we'll add proper kind checking
                    Some(Self::new(node))
                }
            }
        });
    }

    // Generate the code
    let expanded = quote! {
        // Generate wrapper structs
        #(#wrapper_structs)*

        // Generate wrapper implementations (including FromSyntaxNode for each wrapper)
        #(#wrapper_impls)*
    };

    TokenStream::from(expanded)
}

/// The main grammar macro
///
/// This macro allows declarative grammar definition using an EBNF-like syntax.
///
/// # Example
///
/// ```rust,ignore
/// sipha_derive::grammar! {
///     #[entry]
///     Expr = Term ((Plus | Minus) Term)*;
///     
///     Term = Factor ((Star | Slash) Factor)*;
///     
///     Factor = Number
///            | Ident
///            | LParen Expr RParen;
///     
///     // Token patterns
///     #[trivia]
///     @Whitespace = r"\s+";
///     
///     @Number = r"[0-9]+";
///     @Ident = r"[a-zA-Z_][a-zA-Z0-9_]*";
///     @Plus = "+";
///     @Minus = "-";
///     @Star = "*";
///     @Slash = "/";
///     @LParen = "(";
///     @RParen = ")";
/// }
/// ```
#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    let grammar_def = parse_macro_input!(input as parser::GrammarDef);

    match codegen::generate(&grammar_def) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Derive macro for creating token types
///
/// This macro generates `SyntaxKind` trait implementations for enums.
///
/// # Example
///
/// ```rust,ignore
/// #[derive(sipha_derive::GrammarToken)]
/// enum MyToken {
///     #[token(r"[0-9]+")]
///     Number,
///     #[keyword("if")]
///     If,
///     #[trivia]
///     Whitespace,
///     #[eof]
///     Eof,
/// }
/// ```
#[proc_macro_derive(GrammarToken, attributes(token, keyword, trivia, eof))]
pub fn derive_grammar_token(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match derive_token_impl(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn derive_token_impl(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;

    // Get variants from enum
    let variants = match &input.data {
        syn::Data::Enum(data) => &data.variants,
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "GrammarToken can only be derived for enums",
            ));
        }
    };

    // Collect token info
    let mut tokens = Vec::new();
    let mut keywords = Vec::new();
    let mut trivia = Vec::new();
    let mut _eof = None;

    for variant in variants {
        let variant_name = &variant.ident;

        for attr in &variant.attrs {
            if attr.path().is_ident("token") {
                let pattern: LitStr = attr.parse_args()?;
                tokens.push((variant_name.clone(), pattern));
            } else if attr.path().is_ident("keyword") {
                let keyword: LitStr = attr.parse_args()?;
                keywords.push((variant_name.clone(), keyword));
            } else if attr.path().is_ident("trivia") {
                trivia.push(variant_name.clone());
            } else if attr.path().is_ident("eof") {
                _eof = Some(variant_name.clone());
            }
        }
    }

    // Generate SyntaxKind impl
    let is_trivia_arms: Vec<_> = trivia
        .iter()
        .map(|v| {
            quote! { Self::#v => true, }
        })
        .collect();

    let is_terminal_arms: Vec<_> = tokens
        .iter()
        .chain(keywords.iter())
        .map(|(v, _)| {
            quote! { Self::#v => true, }
        })
        .collect();

    let expanded = quote! {
        impl sipha::syntax::SyntaxKind for #name {
            fn is_terminal(self) -> bool {
                match self {
                    #(#is_terminal_arms)*
                    _ => false,
                }
            }

            fn is_trivia(self) -> bool {
                match self {
                    #(#is_trivia_arms)*
                    _ => false,
                }
            }
        }
    };

    Ok(expanded)
}
