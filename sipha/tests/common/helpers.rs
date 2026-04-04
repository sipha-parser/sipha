//! S-expression tree dump for property / structure tests (`sexp`).

use sipha::prelude::*;

use super::grammar::K;

fn kind_to_name(k: SyntaxKind) -> Option<&'static str> {
    K::from_syntax_kind(k).map(|k| match k {
        K::Root => "ROOT",
        K::Expr => "EXPR",
        K::BinExpr => "BIN_EXPR",
        K::ParenExpr => "PAREN_EXPR",
        K::Number => "NUMBER",
        K::Plus => "PLUS",
        K::Star => "STAR",
        K::LParen => "LPAREN",
        K::RParen => "RPAREN",
        K::Ws => "WS",
    })
}

pub fn sexp(node: &SyntaxNode) -> String {
    let opts = sipha::tree::sexp::SexpOptions {
        include_trivia: false,
        kind_to_name: Some(kind_to_name),
        max_token_len: None,
    };
    sipha::tree::sexp::syntax_node_to_sexp(node, &opts)
}
