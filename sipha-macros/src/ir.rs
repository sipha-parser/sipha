//! Intermediate representation for the grammar DSL.

use syn::{Expr, Ident, LitStr};

/// Top-level grammar: directives + rules.
#[derive(Debug)]
pub struct Grammar {
    pub directives: Vec<Directive>,
    pub rules: Vec<Rule>,
}

#[derive(Debug)]
pub enum Directive {
    Trivia(Ident),
    Start(Ident),
}

#[derive(Debug)]
pub struct Rule {
    pub attrs: Vec<RuleAttr>,
    pub name: Ident,
    pub body: ExprNode,
}

/// Rule-level attribute: parser, lexer, or neutral.
#[derive(Debug, Clone, Copy)]
pub enum RuleKind {
    Parser,
    Lexer,
    Neutral,
}

#[derive(Debug)]
pub enum RuleAttr {
    Parser,
    Lexer,
}

/// Expression node in the PEG (after parsing; no precedence).
#[derive(Clone, Debug)]
pub enum ExprNode {
    /// Choice: e1 | e2 | ...
    Choice(Vec<Self>),
    /// Sequence: e1 e2 ...
    Seq(Vec<Self>),
    /// e?
    Optional(Box<Self>),
    /// e*
    ZeroOrMore(Box<Self>),
    /// e+
    OneOrMore(Box<Self>),
    /// &e
    Lookahead(Box<Self>),
    /// !e
    NegLookahead(Box<Self>),
    /// ( e )
    Group(Box<Self>),
    /// Call rule by name
    Call(Ident),
    /// Byte literal "foo"
    Literal(LitStr),
    /// `#[node(KIND)]` e or `#[node(KIND)]` `#[field("name")]` e
    Node { kind: Expr, field: Option<LitStr>, inner: Box<Self> },
    /// `#[token(KIND)]` e
    Token { kind: Expr, inner: Box<Self> },
    /// `#[trivia(KIND)]` e
    Trivia { kind: Expr, inner: Box<Self> },
    /// `#[capture(TAG)]` e — legacy capture
    Capture { tag: Expr, inner: Box<Self> },
    /// `#[no_skip]` e
    NoSkip(Box<Self>),
}
