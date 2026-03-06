//! Lower IR to GrammarBuilder code (quote).

use crate::ir::*;
use proc_macro2::TokenStream;
use quote::quote;
use syn::LitStr;

/// Emit code that builds the grammar and returns `sipha::BuiltGraph`.
pub fn lower_grammar(grammar: &Grammar) -> TokenStream {
    let start_name = grammar
        .directives
        .iter()
        .find_map(|d| match d {
            Directive::Start(ref id) => Some(id),
            _ => None,
        });

    let trivia = grammar.directives.iter().filter_map(|d| {
        if let Directive::Trivia(ref id) = *d {
            let name_lit = LitStr::new(&id.to_string(), id.span());
            Some(quote! { g.set_trivia_rule(#name_lit); })
        } else {
            None
        }
    });

    // Emit @start rule first so it becomes the grammar entry point (rule 0).
    let rule_order = reorder_rules_for_start(&grammar.rules, start_name);

    let rules = rule_order.iter().map(|r| {
        let name = &r.name;
        let name_lit = LitStr::new(&name.to_string(), name.span());
        let body = lower_expr(&r.body);
        let is_start = start_name.as_ref().map_or(false, |s| s.to_string() == name.to_string());
        let rule_kind = rule_kind(r);

        let body_with_start = if is_start {
            quote! {
                #body
                g.skip();
                g.end_of_input();
                g.accept();
            }
        } else {
            quote! { #body }
        };

        match rule_kind {
            RuleKind::Parser => quote! {
                g.parser_rule(#name_lit, |g| {
                    #body_with_start
                });
            },
            RuleKind::Lexer => quote! {
                g.lexer_rule(#name_lit, |g| {
                    #body_with_start
                });
            },
            RuleKind::Neutral => quote! {
                g.rule(#name_lit, |g| {
                    #body_with_start
                });
            },
        }
    });

    quote! {
        {
            let mut g = sipha::builder::GrammarBuilder::new();
            #(#trivia)*
            #(#rules)*
            g.finish().expect("grammar must be valid")
        }
    }
}

/// Reorder rules so the @start rule is first (sipha uses rule 0 as entry).
fn reorder_rules_for_start<'a>(
    rules: &'a [Rule],
    start_name: Option<&'a syn::Ident>,
) -> Vec<&'a Rule> {
    let start_str = start_name.map(|s| s.to_string());
    let mut ordered: Vec<&Rule> = rules.iter().collect();
    if let Some(ref name) = start_str {
        if let Some(pos) = ordered.iter().position(|r| r.name.to_string() == *name) {
            if pos > 0 {
                let r = ordered.remove(pos);
                ordered.insert(0, r);
            }
        }
    }
    ordered
}

fn rule_kind(rule: &Rule) -> RuleKind {
    if rule.attrs.iter().any(|a| matches!(a, RuleAttr::Parser)) {
        RuleKind::Parser
    } else if rule.attrs.iter().any(|a| matches!(a, RuleAttr::Lexer)) {
        RuleKind::Lexer
    } else {
        RuleKind::Neutral
    }
}

fn lower_expr(node: &ExprNode) -> TokenStream {
    match node {
        ExprNode::Choice(choices) => {
            if choices.len() == 1 {
                return lower_expr(&choices[0]);
            }
            let last_idx = choices.len() - 1;
            let mut alt = lower_expr(&choices[last_idx]);
            for c in choices[..last_idx].iter().rev() {
                let body = lower_expr(c);
                alt = quote! { g.choice(|g| { #body }, |g| { #alt }); };
            }
            alt
        }
        ExprNode::Seq(terms) => {
            let stmts = terms.iter().map(lower_expr);
            quote! { #(#stmts)* }
        }
        ExprNode::Optional(inner) => {
            let body = lower_expr(inner);
            quote! { g.optional(|g| { #body }); }
        }
        ExprNode::ZeroOrMore(inner) => {
            let body = lower_expr(inner);
            quote! { g.zero_or_more(|g| { #body }); }
        }
        ExprNode::OneOrMore(inner) => {
            let body = lower_expr(inner);
            quote! { g.one_or_more(|g| { #body }); }
        }
        ExprNode::Lookahead(inner) => {
            let body = lower_expr(inner);
            quote! { g.lookahead(|g| { #body }); }
        }
        ExprNode::NegLookahead(inner) => {
            let body = lower_expr(inner);
            quote! { g.neg_lookahead(|g| { #body }); }
        }
        ExprNode::Group(inner) => lower_expr(inner),
        ExprNode::Call(ident) => {
            let name = ident.to_string();
            quote! { g.call(#name); }
        }
        ExprNode::Literal(lit) => {
            quote! { g.literal((#lit).as_bytes()); }
        }
        ExprNode::Node { kind, inner } => {
            let body = lower_expr(inner);
            quote! { g.node(#kind, |g| { #body }); }
        }
        ExprNode::Token { kind, inner } => {
            let body = lower_expr(inner);
            quote! { g.token(#kind, |g| { #body }); }
        }
        ExprNode::Trivia { kind, inner } => {
            let body = lower_expr(inner);
            quote! { g.trivia(#kind, |g| { #body }); }
        }
        ExprNode::Capture { tag, inner } => {
            let body = lower_expr(inner);
            quote! { g.capture(#tag, |g| { #body }); }
        }
        ExprNode::NoSkip(inner) => {
            let body = lower_expr(inner);
            quote! { g.no_skip(|g| { #body }); }
        }
    }
}
