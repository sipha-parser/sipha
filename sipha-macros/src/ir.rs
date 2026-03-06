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
#[derive(Debug)]
pub enum ExprNode {
    /// Choice: e1 | e2 | ...
    Choice(Vec<ExprNode>),
    /// Sequence: e1 e2 ...
    Seq(Vec<ExprNode>),
    /// e?
    Optional(Box<ExprNode>),
    /// e*
    ZeroOrMore(Box<ExprNode>),
    /// e+
    OneOrMore(Box<ExprNode>),
    /// &e
    Lookahead(Box<ExprNode>),
    /// !e
    NegLookahead(Box<ExprNode>),
    /// ( e )
    Group(Box<ExprNode>),
    /// Call rule by name
    Call(Ident),
    /// Byte literal "foo"
    Literal(LitStr),
    /// #[node(KIND)] e
    Node { kind: Expr, inner: Box<ExprNode> },
    /// #[token(KIND)] e
    Token { kind: Expr, inner: Box<ExprNode> },
    /// #[trivia(KIND)] e
    Trivia { kind: Expr, inner: Box<ExprNode> },
    /// #[capture(TAG)] e — legacy capture
    Capture { tag: Expr, inner: Box<ExprNode> },
    /// #[no_skip] e
    NoSkip(Box<ExprNode>),
}
