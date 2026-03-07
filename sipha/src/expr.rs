//! # Expression / precedence helpers
//!
//! Optional helpers to build expression grammars with precedence levels
//! without repeating the same left- or right-associative infix pattern.
//!
//! ## Left-associative infix
//!
//! `left_assoc_infix_level` generates a rule: `lower ( op lower )*` with each
//! `op lower` pair wrapped in a node of the given kind. Use for levels like
//! mul/div, add/sub, comparison, etc.
//!
//! ## Right-associative infix
//!
//! `right_assoc_infix_level` generates a rule: `lower op self | lower`. Use for
//! power or assignment-like operators.

use crate::builder::GrammarBuilder;
use crate::types::IntoSyntaxKind;

/// Add a left-associative infix level: `level = lower ( op lower )*`.
///
/// Each occurrence of `op lower` is wrapped in a node with the given `node_kind`.
/// `ops` is the list of rule names to try (e.g. `["op_star", "op_slash"]` for mul/div).
///
/// # Example
///
/// ```ignore
/// // expr_mul = expr_power ( ( * | / | % ) expr_power )*
/// expr::left_assoc_infix_level(
///     g,
///     "expr_mul",
///     "expr_power",
///     &["op_star", "op_slash", "op_percent"],
///     Kind::NodeBinaryExpr,
/// );
/// ```
pub fn left_assoc_infix_level<K: IntoSyntaxKind + Clone + 'static>(
    g: &mut GrammarBuilder,
    level_name: &'static str,
    lower_level_name: &'static str,
    ops: &[&'static str],
    node_kind: K,
) {
    g.parser_rule(level_name, |g| {
        g.call(lower_level_name);
        g.zero_or_more(move |g| {
            g.node(node_kind.clone(), |g| {
                let mut alternatives: Vec<Box<dyn FnOnce(&mut GrammarBuilder)>> = Vec::new();
                for op in ops {
                    let op = *op;
                    let lower = lower_level_name;
                    alternatives.push(Box::new(move |g| {
                        g.call(op);
                        g.call(lower);
                    }));
                }
                g.choices(alternatives);
            });
        });
    });
}

/// Add a right-associative infix level: `level = lower op level | lower`.
///
/// Use for power (`**`) or assignment. The single operator is the rule name in `op_rule`.
///
/// # Example
///
/// ```ignore
/// // expr_power = unary ** expr_power | unary
/// expr::right_assoc_infix_level(g, "expr_power", "unary", "op_power", Kind::NodeBinaryExpr);
/// ```
pub fn right_assoc_infix_level<K: IntoSyntaxKind + Clone + 'static>(
    g: &mut GrammarBuilder,
    level_name: &'static str,
    lower_level_name: &'static str,
    op_rule: &'static str,
    node_kind: K,
) {
    g.parser_rule(level_name, |g| {
        let lower = lower_level_name;
        let op = op_rule;
        let level = level_name;
        let kind = node_kind.clone();
        g.choices(vec![
            Box::new(move |g| {
                g.node(kind, |g| {
                    g.call(lower);
                    g.call(op);
                    g.call(level);
                });
            }),
            Box::new(move |g| {
                g.call(lower_level_name);
            }),
        ]);
    });
}
