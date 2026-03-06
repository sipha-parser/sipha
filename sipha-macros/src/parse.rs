//! Parser for the grammar DSL.

use crate::ir::*;
use syn::parse::{Parse, ParseStream};
use syn::{Attribute, Ident, LitStr, Token};

pub use crate::ir::Grammar;

impl Parse for Grammar {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut directives = Vec::new();
        while input.peek(Token![@]) {
            input.parse::<Token![@]>()?;
            let dir: Ident = input.parse()?;
            let dir_s = dir.to_string();
            if dir_s == "trivia" {
                let name: Ident = input.parse()?;
                input.parse::<Token![;]>()?;
                directives.push(Directive::Trivia(name));
            } else if dir_s == "start" {
                let name: Ident = input.parse()?;
                input.parse::<Token![;]>()?;
                directives.push(Directive::Start(name));
            } else {
                return Err(syn::Error::new_spanned(dir, "expected `trivia` or `start`"));
            }
        }

        let mut rules = Vec::new();
        while !input.is_empty() {
            let attrs = input.call(Attribute::parse_outer)?;
            let name: Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            let body = parse_expr(input)?;
            input.parse::<Token![;]>()?;

            let rule_attrs = attrs
                .iter()
                .filter_map(|a| {
                    a.path().get_ident().and_then(|i| match i.to_string().as_str() {
                        "parser" => Some(RuleAttr::Parser),
                        "lexer" => Some(RuleAttr::Lexer),
                        _ => None,
                    })
                })
                .collect();

            rules.push(Rule {
                attrs: rule_attrs,
                name,
                body,
            });
        }

        Ok(Grammar {
            directives,
            rules,
        })
    }
}

/// Parse a rule body expression (choice is top-level); does not consume `;`.
fn parse_expr(input: ParseStream) -> syn::Result<ExprNode> {
    parse_choice(input)
}

fn parse_choice(input: ParseStream) -> syn::Result<ExprNode> {
    let first = parse_seq(input)?;
    let mut choices = vec![first];
    while input.peek(Token![|]) {
        input.parse::<Token![|]>()?;
        choices.push(parse_seq(input)?);
    }
    Ok(if choices.len() == 1 {
        choices.into_iter().next().unwrap()
    } else {
        ExprNode::Choice(choices)
    })
}

fn parse_seq(input: ParseStream) -> syn::Result<ExprNode> {
    let mut terms = Vec::new();
    loop {
        if input.is_empty() || input.peek(Token![|]) || input.peek(Token![;]) {
            break;
        }
        terms.push(parse_rep(input)?);
    }
    if terms.is_empty() {
        return Err(input.error("expected expression"));
    }
    Ok(if terms.len() == 1 {
        terms.into_iter().next().unwrap()
    } else {
        ExprNode::Seq(terms)
    })
}

fn parse_rep(input: ParseStream) -> syn::Result<ExprNode> {
    let mut node = parse_pre(input)?;
    loop {
        if input.peek(Token![?]) {
            input.parse::<Token![?]>()?;
            node = ExprNode::Optional(Box::new(node));
        } else if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            node = ExprNode::ZeroOrMore(Box::new(node));
        } else if input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
            node = ExprNode::OneOrMore(Box::new(node));
        } else {
            break;
        }
    }
    Ok(node)
}

fn parse_pre(input: ParseStream) -> syn::Result<ExprNode> {
    let mut node = parse_wrapped(input)?;
    loop {
        if input.peek(Token![&]) {
            input.parse::<Token![&]>()?;
            node = ExprNode::Lookahead(Box::new(node));
        } else if input.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            node = ExprNode::NegLookahead(Box::new(node));
        } else {
            break;
        }
    }
    Ok(node)
}

fn parse_wrapped(input: ParseStream) -> syn::Result<ExprNode> {
    let attrs = input.call(Attribute::parse_outer)?;
    let mut node = parse_atom(input)?;
    for attr in attrs.into_iter().rev() {
        node = apply_wrapper_attr(attr, node)?;
    }
    Ok(node)
}

fn apply_wrapper_attr(attr: Attribute, inner: ExprNode) -> syn::Result<ExprNode> {
    let path = attr.path().get_ident().ok_or_else(|| {
        syn::Error::new_spanned(attr.path(), "expected single identifier in attribute")
    })?;
    let s = path.to_string();
    match s.as_str() {
        "node" => {
            let expr = parse_paren_expr(&attr)?;
            Ok(ExprNode::Node {
                kind: expr,
                inner: Box::new(inner),
            })
        }
        "token" => {
            let expr = parse_paren_expr(&attr)?;
            Ok(ExprNode::Token {
                kind: expr,
                inner: Box::new(inner),
            })
        }
        "trivia" => {
            let expr = parse_paren_expr(&attr)?;
            Ok(ExprNode::Trivia {
                kind: expr,
                inner: Box::new(inner),
            })
        }
        "capture" => {
            let expr = parse_paren_expr(&attr)?;
            Ok(ExprNode::Capture {
                tag: expr,
                inner: Box::new(inner),
            })
        }
        "no_skip" => Ok(ExprNode::NoSkip(Box::new(inner))),
        _ => Err(syn::Error::new_spanned(
            path,
            "expected one of: node, token, trivia, capture, no_skip",
        )),
    }
}

fn parse_paren_expr(attr: &Attribute) -> syn::Result<syn::Expr> {
    attr.meta.require_list()?.parse_args::<syn::Expr>()
}

fn parse_atom(input: ParseStream) -> syn::Result<ExprNode> {
    if input.peek(syn::token::Paren) {
        let content;
        syn::parenthesized!(content in input);
        Ok(ExprNode::Group(Box::new(parse_choice(&content)?)))
    } else if input.peek(LitStr) {
        Ok(ExprNode::Literal(input.parse()?))
    } else if input.peek(Ident) {
        Ok(ExprNode::Call(input.parse()?))
    } else {
        Err(input.error("expected identifier, string literal, or parenthesized expression"))
    }
}
