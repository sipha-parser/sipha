use hashbrown::HashSet;
use crate::grammar::{Token, NonTerminal, Expr, GrammarError};

/// Validate a grammar for common issues
///
/// # Errors
///
/// Returns an error if the grammar has validation issues such as left recursion or undefined rules.
pub fn validate_grammar<T, N>(
    rules: &[crate::grammar::Rule<T, N>]
) -> Result<(), GrammarError<T, N>>
where
    T: Token,
    N: NonTerminal,
{
    // Check for left recursion
    if let Some(cycles) = detect_left_recursion(rules) {
        return Err(GrammarError::LeftRecursion(cycles));
    }
    
    // Check for undefined rules
    let defined_rules: HashSet<_> = rules.iter().map(|r| &r.lhs).collect();
    for rule in rules {
        check_undefined_rules(&rule.rhs, &defined_rules)?;
    }
    
    Ok(())
}

fn detect_left_recursion<T, N>(
    rules: &[crate::grammar::Rule<T, N>]
) -> Option<Vec<Vec<N>>>
where
    N: NonTerminal,
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

fn is_directly_left_recursive<T, N: PartialEq>(expr: &Expr<T, N>, lhs: &N) -> bool {
    match expr {
        Expr::Rule(n) => n == lhs,
        Expr::Seq(exprs) => {
            exprs.first().is_some_and(|e| is_directly_left_recursive(e, lhs))
        }
        Expr::Choice(exprs) => {
            exprs.iter().any(|e| is_directly_left_recursive(e, lhs))
        }
        Expr::Opt(e) | Expr::Repeat { expr: e, .. } => {
            is_directly_left_recursive(e, lhs)
        }
        _ => false,
    }
}

fn check_undefined_rules<T, N>(
    expr: &Expr<T, N>,
    defined: &HashSet<&N>,
) -> Result<(), GrammarError<T, N>>
where
    N: NonTerminal,
{
    match expr {
        Expr::Rule(n) => {
            if !defined.contains(n) {
                return Err(GrammarError::UndefinedRule(n.clone()));
            }
        }
        Expr::Seq(exprs) | Expr::Choice(exprs) => {
            for e in exprs {
                check_undefined_rules(e, defined)?;
            }
        }
        Expr::Opt(e) | Expr::Repeat { expr: e, .. } => {
            check_undefined_rules(e, defined)?;
        }
        Expr::Separated { item, separator, .. } => {
            check_undefined_rules(item, defined)?;
            check_undefined_rules(separator, defined)?;
        }
        Expr::Delimited { open, content, close, .. } => {
            check_undefined_rules(open, defined)?;
            check_undefined_rules(content, defined)?;
            check_undefined_rules(close, defined)?;
        }
        Expr::Label { expr, .. }
        | Expr::Node { expr, .. }
        | Expr::Flatten(expr)
        | Expr::Prune(expr)
        | Expr::Lookahead(expr)
        | Expr::NotLookahead(expr)
        | Expr::RecoveryPoint { expr, .. } => {
            check_undefined_rules(expr, defined)?;
        }
        _ => {}
    }
    
    Ok(())
}

