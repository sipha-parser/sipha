//! S-expression tree dump for property / structure tests (`sexp`).

use sipha::prelude::*;

use super::grammar::{Lex, Rule};

fn kind_to_name(k: SyntaxKind) -> Option<&'static str> {
    Lex::from_syntax_kind(k)
        .map(LexKind::display_name)
        .or_else(|| Rule::from_syntax_kind(k).map(RuleKind::display_name))
}

pub fn sexp(node: &SyntaxNode) -> String {
    let opts = sipha::tree::sexp::SexpOptions {
        include_trivia: false,
        kind_to_name: Some(kind_to_name),
        max_token_len: None,
    };
    sipha::tree::sexp::syntax_node_to_sexp(node, &opts)
}
