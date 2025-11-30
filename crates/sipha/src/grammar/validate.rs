use crate::grammar::{CoreExpr, Expr, ExtendedExpr, GrammarError, NonTerminal, Token};
use hashbrown::HashSet;

/// Options that control grammar validation behavior.
#[derive(Debug, Clone, Copy, Default)]
pub struct GrammarValidationOptions {
    /// Skip direct-left-recursion detection when true.
    pub allow_left_recursion: bool,
}

/// Validate a grammar for common issues
///
/// # Errors
///
/// Returns an error if the grammar has validation issues such as left recursion or undefined rules.
pub fn validate_grammar<T, N>(
    rules: &[crate::grammar::Rule<T, N>],
) -> Result<(), GrammarError<T, N>>
where
    T: Token,
    N: NonTerminal,
{
    validate_grammar_with_options(rules, GrammarValidationOptions::default())
}

/// Validate a grammar using custom options.
///
/// # Errors
///
/// Returns `GrammarError::LeftRecursion` if left recursion is detected and not allowed.
pub fn validate_grammar_with_options<T, N>(
    rules: &[crate::grammar::Rule<T, N>],
    options: GrammarValidationOptions,
) -> Result<(), GrammarError<T, N>>
where
    T: Token,
    N: NonTerminal,
{
    // Check for left recursion
    if !options.allow_left_recursion
        && let Some(cycles) = detect_left_recursion(rules)
    {
        return Err(GrammarError::LeftRecursion(cycles));
    }

    // Check for undefined rules
    let defined_rules: HashSet<_> = rules.iter().map(|r| &r.lhs).collect();
    for rule in rules {
        check_undefined_rules(&rule.rhs, &defined_rules)?;
    }

    Ok(())
}

fn detect_left_recursion<T, N>(rules: &[crate::grammar::Rule<T, N>]) -> Option<Vec<Vec<N>>>
where
    T: Clone,
    N: NonTerminal + Clone,
{
    // Simple left recursion detection
    // In a full implementation, this would be more sophisticated
    let mut cycles = Vec::new();

    for rule in rules {
        if is_directly_left_recursive(&rule.rhs, &rule.lhs) {
            cycles.push(vec![rule.lhs.clone()]);
        }
    }

    if cycles.is_empty() {
        None
    } else {
        Some(cycles)
    }
}

fn is_directly_left_recursive<T: Clone, N: PartialEq + Clone>(expr: &Expr<T, N>, lhs: &N) -> bool {
    match expr {
        ExtendedExpr::Core(CoreExpr::Rule(n)) => n == lhs,
        ExtendedExpr::Core(CoreExpr::Seq(exprs)) => exprs
            .first()
            .is_some_and(|e| is_directly_left_recursive(&ExtendedExpr::Core(e.clone()), lhs)),
        ExtendedExpr::Core(CoreExpr::Choice(exprs)) => exprs
            .iter()
            .any(|e| is_directly_left_recursive(&ExtendedExpr::Core(e.clone()), lhs)),
        ExtendedExpr::Core(CoreExpr::Opt(e) | CoreExpr::Repeat { expr: e, .. }) => {
            is_directly_left_recursive(&ExtendedExpr::Core((**e).clone()), lhs)
        }
        #[cfg(feature = "backend-peg")]
        ExtendedExpr::Cut(e) => is_directly_left_recursive(e, lhs),
        ExtendedExpr::SemanticPredicate { expr: e, .. } => is_directly_left_recursive(e, lhs),
        ExtendedExpr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            is_directly_left_recursive(condition, lhs)
                || is_directly_left_recursive(then_expr, lhs)
                || else_expr
                    .as_ref()
                    .is_some_and(|e| is_directly_left_recursive(e, lhs))
        }
        _ => false,
    }
}

fn check_undefined_rules<T, N>(
    expr: &Expr<T, N>,
    defined: &HashSet<&N>,
) -> Result<(), GrammarError<T, N>>
where
    T: Clone,
    N: NonTerminal + Clone,
{
    match expr {
        ExtendedExpr::Core(CoreExpr::Rule(n)) => {
            if !defined.contains(n) {
                return Err(GrammarError::UndefinedRule(n.clone()));
            }
        }
        ExtendedExpr::Core(CoreExpr::Seq(exprs) | CoreExpr::Choice(exprs)) => {
            for e in exprs {
                check_undefined_rules(&ExtendedExpr::Core(e.clone()), defined)?;
            }
        }
        ExtendedExpr::Core(CoreExpr::Opt(e) | CoreExpr::Repeat { expr: e, .. }) => {
            check_undefined_rules(&ExtendedExpr::Core((**e).clone()), defined)?;
        }
        #[cfg(feature = "backend-peg")]
        ExtendedExpr::Cut(e) => {
            check_undefined_rules(e, defined)?;
        }
        ExtendedExpr::SemanticPredicate { expr: e, .. } => {
            check_undefined_rules(e, defined)?;
        }
        ExtendedExpr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            check_undefined_rules(condition, defined)?;
            check_undefined_rules(then_expr, defined)?;
            if let Some(else_expr) = else_expr {
                check_undefined_rules(else_expr, defined)?;
            }
        }
        // TokenClass and Backreference don't contain rule references
        ExtendedExpr::Core(CoreExpr::Separated {
            item, separator, ..
        }) => {
            check_undefined_rules(&ExtendedExpr::Core((**item).clone()), defined)?;
            check_undefined_rules(&ExtendedExpr::Core((**separator).clone()), defined)?;
        }
        ExtendedExpr::Core(CoreExpr::Delimited {
            open,
            content,
            close,
            ..
        }) => {
            check_undefined_rules(&ExtendedExpr::Core((**open).clone()), defined)?;
            check_undefined_rules(&ExtendedExpr::Core((**content).clone()), defined)?;
            check_undefined_rules(&ExtendedExpr::Core((**close).clone()), defined)?;
        }
        ExtendedExpr::Core(
            CoreExpr::Label { expr, .. }
            | CoreExpr::Node { expr, .. }
            | CoreExpr::Flatten(expr)
            | CoreExpr::Prune(expr),
        ) => {
            check_undefined_rules(&ExtendedExpr::Core((**expr).clone()), defined)?;
        }
        ExtendedExpr::Lookahead(expr)
        | ExtendedExpr::NotLookahead(expr)
        | ExtendedExpr::RecoveryPoint { expr, .. } => {
            check_undefined_rules(expr, defined)?;
        }
        _ => {}
    }

    Ok(())
}
