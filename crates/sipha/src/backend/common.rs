//! Common utilities shared across parser backends

use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, GrammarError, NonTerminal};

/// Check for undefined non-terminal references in a grammar expression.
///
/// Recursively traverses the expression tree and reports any references to
/// non-terminals that are not defined in the grammar.
pub fn check_undefined_references<T, N>(
    grammar: &Grammar<T, N>,
    expr: &Expr<T, N>,
    errors: &mut Vec<GrammarError<T, N>>,
) where
    T: crate::grammar::Token + Clone,
    N: NonTerminal + Clone,
{
    match expr {
        ExtendedExpr::Core(CoreExpr::Rule(nt)) => {
            if grammar.get_rule(nt).is_none() {
                errors.push(GrammarError::UndefinedRule(nt.clone()));
            }
        }
        ExtendedExpr::Core(CoreExpr::Seq(exprs) | CoreExpr::Choice(exprs)) => {
            for e in exprs {
                check_undefined_references(grammar, &ExtendedExpr::Core(e.clone()), errors);
            }
        }
        ExtendedExpr::Core(CoreExpr::Opt(e) | CoreExpr::Repeat { expr: e, .. }) => {
            check_undefined_references(grammar, &ExtendedExpr::Core((**e).clone()), errors);
        }
        ExtendedExpr::Core(CoreExpr::Separated {
            item, separator, ..
        }) => {
            check_undefined_references(grammar, &ExtendedExpr::Core((**item).clone()), errors);
            check_undefined_references(grammar, &ExtendedExpr::Core((**separator).clone()), errors);
        }
        ExtendedExpr::Core(CoreExpr::Delimited {
            open,
            content,
            close,
            ..
        }) => {
            check_undefined_references(grammar, &ExtendedExpr::Core((**open).clone()), errors);
            check_undefined_references(grammar, &ExtendedExpr::Core((**content).clone()), errors);
            check_undefined_references(grammar, &ExtendedExpr::Core((**close).clone()), errors);
        }
        ExtendedExpr::Core(
            CoreExpr::Node { expr, .. }
            | CoreExpr::Label { expr, .. }
            | CoreExpr::Flatten(expr)
            | CoreExpr::Prune(expr),
        ) => {
            check_undefined_references(grammar, &ExtendedExpr::Core((**expr).clone()), errors);
        }
        ExtendedExpr::Lookahead(expr)
        | ExtendedExpr::NotLookahead(expr)
        | ExtendedExpr::RecoveryPoint { expr, .. }
        | ExtendedExpr::SemanticPredicate { expr, .. } => {
            check_undefined_references(grammar, expr, errors);
        }
        #[cfg(feature = "backend-peg")]
        ExtendedExpr::Cut(expr) => {
            check_undefined_references(grammar, expr, errors);
        }
        ExtendedExpr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            check_undefined_references(grammar, condition, errors);
            check_undefined_references(grammar, then_expr, errors);
            if let Some(else_expr) = else_expr {
                check_undefined_references(grammar, else_expr, errors);
            }
        }
        // TokenClass and Backreference don't contain rule references
        _ => {}
    }
}
