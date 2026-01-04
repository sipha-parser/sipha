//! Code generation for grammar DSL
//!
//! Generates Rust code from the parsed grammar AST.

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Result, Error};

use crate::parser::{GrammarDef, GrammarExpr, TokenPattern};

/// Validate grammar definition and provide helpful error messages
fn validate_grammar(grammar: &GrammarDef) -> Result<()> {
    // Check for at least one rule
    if grammar.rules.is_empty() {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            "grammar must have at least one rule\n\
             help: define rules like: `RuleName = expression;`"
        ));
    }

    // Check for entry point
    let has_entry = grammar.rules.iter().any(|r| {
        r.attrs.iter().any(|attr| attr.name == "entry")
    });
    
    if !has_entry {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            "grammar must have an entry point\n\
             help: mark one rule with `#[entry]`, e.g., `#[entry] Expr = ...;`"
        ));
    }

    // Check for duplicate rule names
    let mut rule_names = std::collections::HashSet::new();
    for rule in &grammar.rules {
        if !rule_names.insert(&rule.name) {
            return Err(Error::new_spanned(
                &rule.name,
                format!("duplicate rule name `{}`\n\
                         help: each rule must have a unique name", rule.name)
            ));
        }
    }

    // Check for duplicate token names
    let mut token_names = std::collections::HashSet::new();
    for token in &grammar.tokens {
        if !token_names.insert(&token.name) {
            return Err(Error::new_spanned(
                &token.name,
                format!("duplicate token name `{}`\n\
                         help: each token must have a unique name", token.name)
            ));
        }
    }

    // Validate rule references
    let all_names: std::collections::HashSet<_> = grammar.rules.iter()
        .map(|r| &r.name)
        .chain(grammar.tokens.iter().map(|t| &t.name))
        .collect();

    for rule in &grammar.rules {
        validate_expr(&rule.expr, &all_names, &rule.name)?;
    }

    Ok(())
}

/// Validate a grammar expression
fn validate_expr(
    expr: &GrammarExpr,
    valid_names: &std::collections::HashSet<&syn::Ident>,
    rule_name: &syn::Ident,
) -> Result<()> {
    match expr {
        GrammarExpr::Ref(ident) => {
            if !valid_names.contains(ident) {
                return Err(Error::new_spanned(
                    ident,
                    format!(
                        "undefined reference `{}` in rule `{}`\n\
                         help: make sure `{}` is defined as a rule or token before use",
                        ident, rule_name, ident
                    )
                ));
            }
        }
        GrammarExpr::Seq(items) | GrammarExpr::Choice(items) => {
            for item in items {
                validate_expr(item, valid_names, rule_name)?;
            }
        }
        GrammarExpr::Opt(inner) | GrammarExpr::Star(inner) | GrammarExpr::Plus(inner) | GrammarExpr::Group(inner) => {
            validate_expr(inner, valid_names, rule_name)?;
        }
        GrammarExpr::Literal(_) => {
            // Literals are always valid
        }
    }
    Ok(())
}

/// Generate code from a grammar definition
pub fn generate(grammar: &GrammarDef) -> Result<TokenStream> {
    // Validate grammar before generating code
    validate_grammar(grammar)?;
    
    let syntax_kind_enum = generate_syntax_kind(grammar)?;
    let syntax_kind_impl = generate_syntax_kind_impl(grammar)?;
    let non_terminal_enum = generate_non_terminal(grammar)?;
    let grammar_builder = generate_grammar_builder(grammar)?;
    let lexer_builder = generate_lexer_builder(grammar)?;

    Ok(quote! {
        #syntax_kind_enum
        #syntax_kind_impl
        #non_terminal_enum

        /// Build the grammar from the macro definition
        pub fn build_grammar() -> ::std::result::Result<
            ::sipha::Grammar<::sipha::lexer::Token<SyntaxKind>, NonTerminal>,
            ::sipha::error::GrammarError
        > {
            #grammar_builder
        }

        /// Build the lexer from the macro definition
        pub fn build_lexer() -> ::std::result::Result<
            ::sipha::CompiledLexer<SyntaxKind>,
            ::sipha::error::LexerError
        > {
            #lexer_builder
        }
    })
}

/// Generate the `SyntaxKind` enum
#[allow(clippy::unnecessary_wraps)]
fn generate_syntax_kind(grammar: &GrammarDef) -> Result<TokenStream> {
    let mut variants = Vec::new();

    // Add token variants
    for token in &grammar.tokens {
        let name = &token.name;
        variants.push(quote! { #name, });
    }

    // Add rule variants (non-terminals become syntax kinds too)
    for rule in &grammar.rules {
        let name = &rule.name;
        variants.push(quote! { #name, });
    }

    // Add special variants
    variants.push(quote! { Eof, });
    variants.push(quote! { Error, });

    Ok(quote! {
        /// Syntax kinds generated from the grammar
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum SyntaxKind {
            #(#variants)*
        }
    })
}

/// Generate the `SyntaxKind` trait impl
#[allow(clippy::unnecessary_wraps)]
fn generate_syntax_kind_impl(grammar: &GrammarDef) -> Result<TokenStream> {
    let token_names: Vec<_> = grammar.tokens.iter().map(|t| &t.name).collect();

    // Collect trivia token names
    let trivia_names: Vec<_> = grammar
        .tokens
        .iter()
        .filter(|t| {
            t.attrs
                .iter()
                .any(|attr| attr.name == "trivia" || attr.name.to_string() == "trivia")
        })
        .map(|t| &t.name)
        .collect();

    let trivia_arms: Vec<_> = trivia_names
        .iter()
        .map(|name| quote! { Self::#name => true, })
        .collect();

    Ok(quote! {
        impl ::sipha::syntax::SyntaxKind for SyntaxKind {
            fn is_terminal(self) -> bool {
                match self {
                    #(Self::#token_names => true,)*
                    Self::Eof => true,
                    Self::Error => true,
                    _ => false,
                }
            }

            fn is_trivia(self) -> bool {
                match self {
                    #(#trivia_arms)*
                    _ => false,
                }
            }
        }
    })
}

/// Generate the `NonTerminal` enum
#[allow(clippy::unnecessary_wraps)]
fn generate_non_terminal(grammar: &GrammarDef) -> Result<TokenStream> {
    let variants: Vec<_> = grammar
        .rules
        .iter()
        .map(|r| {
            let name = &r.name;
            quote! { #name, }
        })
        .collect();

    let name_arms: Vec<_> = grammar
        .rules
        .iter()
        .map(|r| {
            let name = &r.name;
            let name_str = name.to_string();
            quote! { Self::#name => #name_str, }
        })
        .collect();

    Ok(quote! {
        /// Non-terminals generated from the grammar
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum NonTerminal {
            #(#variants)*
        }

        impl ::sipha::grammar::NonTerminal for NonTerminal {
            fn name(&self) -> &str {
                match self {
                    #(#name_arms)*
                }
            }
        }
    })
}

/// Generate the grammar builder code
fn generate_grammar_builder(grammar: &GrammarDef) -> Result<TokenStream> {
    let mut rule_additions = Vec::new();
    let mut entry_point = None;

    for rule in &grammar.rules {
        // Check for entry attribute
        for attr in &rule.attrs {
            if attr.name == "entry" {
                entry_point = Some(&rule.name);
            }
        }

        let name = &rule.name;
        let expr_code = generate_expr(&rule.expr)?;

        rule_additions.push(quote! {
            builder = builder.rule(NonTerminal::#name, #expr_code);
        });
    }

    let entry_code = if let Some(entry) = entry_point {
        quote! { builder = builder.entry_point(NonTerminal::#entry); }
    } else if let Some(first_rule) = grammar.rules.first() {
        let name = &first_rule.name;
        quote! { builder = builder.entry_point(NonTerminal::#name); }
    } else {
        quote! {}
    };

    Ok(quote! {
        let mut builder = ::sipha::GrammarBuilder::new();
        #entry_code
        #(#rule_additions)*
        builder.build()
    })
}

/// Generate code for a grammar expression
fn generate_expr(expr: &GrammarExpr) -> Result<TokenStream> {
    match expr {
        GrammarExpr::Ref(ident) => Ok(quote! {
            ::sipha::Expr::rule_ref(NonTerminal::#ident)
        }),
        GrammarExpr::Seq(items) => {
            let item_codes: Vec<_> = items
                .iter()
                .map(generate_expr)
                .collect::<Result<Vec<_>>>()?;
            Ok(quote! {
                ::sipha::Expr::seq(vec![#(#item_codes),*])
            })
        }
        GrammarExpr::Choice(alts) => {
            let alt_codes: Vec<_> = alts.iter().map(generate_expr).collect::<Result<Vec<_>>>()?;
            Ok(quote! {
                ::sipha::Expr::choice(vec![#(#alt_codes),*])
            })
        }
        GrammarExpr::Opt(inner) => {
            let inner_code = generate_expr(inner)?;
            Ok(quote! {
                ::sipha::Expr::opt(#inner_code)
            })
        }
        GrammarExpr::Star(inner) => {
            let inner_code = generate_expr(inner)?;
            Ok(quote! {
                ::sipha::Expr::repeat(#inner_code, 0, None)
            })
        }
        GrammarExpr::Plus(inner) => {
            let inner_code = generate_expr(inner)?;
            Ok(quote! {
                ::sipha::Expr::repeat(#inner_code, 1, None)
            })
        }
        GrammarExpr::Group(inner) => generate_expr(inner),
        GrammarExpr::Literal(lit) => {
            let text = lit.value();
            Ok(quote! {
                ::sipha::Expr::literal(#text)
            })
        }
    }
}

/// Generate the lexer builder code
#[allow(clippy::unnecessary_wraps)]
fn generate_lexer_builder(grammar: &GrammarDef) -> Result<TokenStream> {
    let mut token_additions = Vec::new();

    for token in &grammar.tokens {
        let name = &token.name;
        let pattern_code = match &token.pattern {
            TokenPattern::Literal(lit) => {
                let text = lit.value();
                quote! { ::sipha::lexer::Pattern::Literal(#text.into()) }
            }
            TokenPattern::Regex(lit) => {
                let pattern = lit.value();
                quote! { ::sipha::lexer::Pattern::Regex(#pattern.into()) }
            }
        };

        token_additions.push(quote! {
            builder = builder.token(SyntaxKind::#name, #pattern_code);
        });
    }

    Ok(quote! {
        let mut builder = ::sipha::LexerBuilder::new();
        #(#token_additions)*
        builder.build(SyntaxKind::Eof, SyntaxKind::Error)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::format_ident;

    #[test]
    fn test_generate_expr_ref() {
        let expr = GrammarExpr::Ref(format_ident!("Foo"));
        let code = generate_expr(&expr).unwrap();
        let code_str = code.to_string();
        assert!(code_str.contains("rule_ref"));
    }
}

