//! Grammar DSL parser
//!
//! Parses the grammar! macro input into an AST.

use syn::{
    Ident, LitStr, Result, Token, bracketed, parenthesized,
    parse::{Parse, ParseStream},
    token,
};

/// A complete grammar definition
#[derive(Debug)]
pub struct GrammarDef {
    pub rules: Vec<RuleDef>,
    pub tokens: Vec<TokenDef>,
}

impl Parse for GrammarDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rules = Vec::new();
        let mut tokens = Vec::new();

        while !input.is_empty() {
            // Check if this is a token definition (starts with @)
            if input.peek(Token![@]) {
                tokens.push(input.parse()?);
            } else {
                rules.push(input.parse()?);
            }
        }

        Ok(GrammarDef { rules, tokens })
    }
}

/// A grammar rule definition
#[derive(Debug)]
pub struct RuleDef {
    pub attrs: Vec<RuleAttr>,
    pub name: Ident,
    pub expr: GrammarExpr,
}

impl Parse for RuleDef {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse attributes
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.parse()?);
        }

        // Parse rule name
        let name: Ident = input.parse()?;

        // Parse = with better error message
        if !input.peek(Token![=]) {
            return Err(input.error(format!(
                "expected `=` after rule name `{name}`\n\
                 help: grammar rules must have the form: RuleName = expression;"
            )));
        }
        input.parse::<Token![=]>()?;

        // Parse expression
        let expr = input.parse()?;

        // Parse ; with better error message
        if !input.peek(Token![;]) {
            return Err(input.error(format!(
                "expected `;` after rule `{name}` expression\n\
                 help: grammar rules must end with a semicolon"
            )));
        }
        input.parse::<Token![;]>()?;

        Ok(RuleDef { attrs, name, expr })
    }
}

/// Rule attributes like #[entry] or #[precedence(level)]
#[derive(Debug)]
pub struct RuleAttr {
    pub name: Ident,
    /// Optional attribute arguments (for future use, e.g., precedence levels)
    #[allow(dead_code)]
    pub args: Option<syn::Meta>,
}

impl Parse for RuleAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![#]>()?;
        let content;
        bracketed!(content in input);

        // Try to parse as meta (supports both simple #[attr] and #[attr(args)])
        let meta: syn::Meta = content.parse()?;
        let name = match &meta {
            syn::Meta::Path(path) => path
                .get_ident()
                .ok_or_else(|| content.error("expected attribute name"))?
                .clone(),
            syn::Meta::NameValue(nv) => nv
                .path
                .get_ident()
                .ok_or_else(|| content.error("expected attribute name"))?
                .clone(),
            syn::Meta::List(list) => list
                .path
                .get_ident()
                .ok_or_else(|| content.error("expected attribute name"))?
                .clone(),
        };

        // Validate known attributes
        let attr_str = name.to_string();
        if !matches!(
            attr_str.as_str(),
            "entry" | "precedence" | "left" | "right" | "nonassoc"
        ) {
            return Err(syn::Error::new_spanned(
                &name,
                format!(
                    "unknown attribute `{attr_str}`\n\
                     help: valid rule attributes are: `entry`, `precedence`, `left`, `right`, `nonassoc`"
                ),
            ));
        }

        Ok(RuleAttr {
            name,
            args: Some(meta),
        })
    }
}

/// A token definition
#[derive(Debug)]
pub struct TokenDef {
    pub attrs: Vec<TokenAttr>,
    pub name: Ident,
    pub pattern: TokenPattern,
}

impl Parse for TokenDef {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse attributes
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.parse()?);
        }

        if !input.peek(Token![@]) {
            return Err(input.error(
                "expected `@` before token name\n\
                 help: token definitions must start with `@`, e.g., `@TokenName = pattern;`",
            ));
        }
        input.parse::<Token![@]>()?;

        let name: Ident = input.parse()?;

        if !input.peek(Token![=]) {
            return Err(input.error(format!(
                "expected `=` after token name `{name}`\n\
                 help: token definitions must have the form: @TokenName = pattern;"
            )));
        }
        input.parse::<Token![=]>()?;

        let pattern = input.parse()?;

        if !input.peek(Token![;]) {
            return Err(input.error(format!(
                "expected `;` after token `{name}` pattern\n\
                 help: token definitions must end with a semicolon"
            )));
        }
        input.parse::<Token![;]>()?;

        Ok(TokenDef {
            attrs,
            name,
            pattern,
        })
    }
}

/// Token attributes like #[trivia]
#[derive(Debug)]
pub struct TokenAttr {
    pub name: Ident,
}

impl Parse for TokenAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![#]>()?;
        let content;
        bracketed!(content in input);
        let name: Ident = content.parse()?;
        Ok(TokenAttr { name })
    }
}

/// A token pattern
#[derive(Debug)]
pub enum TokenPattern {
    /// A literal string pattern
    Literal(LitStr),
    /// A regex pattern (prefixed with r)
    Regex(LitStr),
}

impl Parse for TokenPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let lit: LitStr = input.parse()?;
        let value = lit.value();

        // Check if it's a regex (contains regex metacharacters)
        if value.contains(|c: char| "[](){}*+?|^$\\.".contains(c)) {
            Ok(TokenPattern::Regex(lit))
        } else {
            Ok(TokenPattern::Literal(lit))
        }
    }
}

/// A grammar expression
#[derive(Debug)]
pub enum GrammarExpr {
    /// A reference to a rule or token
    Ref(Ident),
    /// A sequence of expressions
    Seq(Vec<GrammarExpr>),
    /// A choice between alternatives
    Choice(Vec<GrammarExpr>),
    /// Optional (expr?)
    Opt(Box<GrammarExpr>),
    /// Zero or more (expr*)
    Star(Box<GrammarExpr>),
    /// One or more (expr+)
    Plus(Box<GrammarExpr>),
    /// Grouped expression (parenthesized)
    Group(Box<GrammarExpr>),
    /// A literal token
    Literal(LitStr),
}

impl Parse for GrammarExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        parse_choice(input)
    }
}

fn parse_choice(input: ParseStream) -> Result<GrammarExpr> {
    let first = parse_sequence(input)?;

    if input.peek(Token![|]) {
        let mut alternatives = vec![first];

        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            alternatives.push(parse_sequence(input)?);
        }

        Ok(GrammarExpr::Choice(alternatives))
    } else {
        Ok(first)
    }
}

fn parse_sequence(input: ParseStream) -> Result<GrammarExpr> {
    let mut items = Vec::new();

    while !input.is_empty() && !input.peek(Token![|]) && !input.peek(Token![;]) {
        items.push(parse_postfix(input)?);
    }

    if items.len() == 1 {
        Ok(items.pop().unwrap())
    } else if items.is_empty() {
        Err(input.error("expected expression"))
    } else {
        Ok(GrammarExpr::Seq(items))
    }
}

fn parse_postfix(input: ParseStream) -> Result<GrammarExpr> {
    let mut expr = parse_primary(input)?;

    loop {
        if input.peek(Token![?]) {
            input.parse::<Token![?]>()?;
            expr = GrammarExpr::Opt(Box::new(expr));
        } else if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            expr = GrammarExpr::Star(Box::new(expr));
        } else if input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
            expr = GrammarExpr::Plus(Box::new(expr));
        } else {
            break;
        }
    }

    Ok(expr)
}

fn parse_primary(input: ParseStream) -> Result<GrammarExpr> {
    if input.peek(token::Paren) {
        let content;
        parenthesized!(content in input);
        let inner = content.parse()?;
        Ok(GrammarExpr::Group(Box::new(inner)))
    } else if input.peek(LitStr) {
        let lit: LitStr = input.parse()?;
        Ok(GrammarExpr::Literal(lit))
    } else if input.peek(Ident) {
        let ident: Ident = input.parse()?;
        Ok(GrammarExpr::Ref(ident))
    } else {
        Err(input.error(
            "expected identifier, literal string, or parenthesized expression\n\
             help: valid grammar expressions include:\n\
             - Rule or token references: `RuleName` or `TokenName`\n\
             - Literal strings: `\"+\"` or `\"keyword\"`\n\
             - Parenthesized groups: `(Expr | Term)`\n\
             - Repetitions: `Expr*`, `Expr+`, `Expr?`",
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    fn parse_expr(input: &str) -> GrammarExpr {
        syn::parse_str(input).unwrap()
    }

    #[test]
    fn test_parse_ref() {
        let expr = parse_expr("Foo");
        assert!(matches!(expr, GrammarExpr::Ref(_)));
    }

    #[test]
    fn test_parse_choice() {
        let expr = parse_expr("Foo | Bar");
        assert!(matches!(expr, GrammarExpr::Choice(_)));
    }

    #[test]
    fn test_parse_star() {
        let expr = parse_expr("Foo*");
        assert!(matches!(expr, GrammarExpr::Star(_)));
    }

    #[test]
    fn test_parse_plus() {
        let expr = parse_expr("Foo+");
        assert!(matches!(expr, GrammarExpr::Plus(_)));
    }

    #[test]
    fn test_parse_opt() {
        let expr = parse_expr("Foo?");
        assert!(matches!(expr, GrammarExpr::Opt(_)));
    }

    #[test]
    fn test_parse_group() {
        let expr = parse_expr("(Foo | Bar)");
        assert!(matches!(expr, GrammarExpr::Group(_)));
    }
}
