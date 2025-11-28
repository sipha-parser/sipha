//! Common utilities shared across parser backends

use crate::grammar::{Expr, Grammar, GrammarError, NonTerminal};

/// Check for undefined non-terminal references in a grammar expression.
///
/// Recursively traverses the expression tree and reports any references to
/// non-terminals that are not defined in the grammar.
pub fn check_undefined_references<T, N>(
    grammar: &Grammar<T, N>,
    expr: &Expr<T, N>,
    errors: &mut Vec<GrammarError<T, N>>,
) where
    T: crate::grammar::Token,
    N: NonTerminal,
{
    match expr {
        Expr::Rule(nt) => {
            if grammar.get_rule(nt).is_none() {
                errors.push(GrammarError::UndefinedRule(nt.clone()));
            }
        }
        Expr::Seq(exprs) | Expr::Choice(exprs) => {
            for e in exprs {
                check_undefined_references(grammar, e, errors);
            }
        }
        Expr::Opt(e) | Expr::Repeat { expr: e, .. } => {
            check_undefined_references(grammar, e, errors);
        }
        Expr::Separated {
            item, separator, ..
        } => {
            check_undefined_references(grammar, item, errors);
            check_undefined_references(grammar, separator, errors);
        }
        Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            check_undefined_references(grammar, open, errors);
            check_undefined_references(grammar, content, errors);
            check_undefined_references(grammar, close, errors);
        }
        Expr::Node { expr, .. }
        | Expr::Label { expr, .. }
        | Expr::Flatten(expr)
        | Expr::Prune(expr)
        | Expr::Lookahead(expr)
        | Expr::NotLookahead(expr)
        | Expr::RecoveryPoint { expr, .. } => {
            check_undefined_references(grammar, expr, errors);
        }
        _ => {}
    }
}
