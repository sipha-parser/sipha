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

/// Closure type for a single choice arm in the builder.
type BuilderChoice = Box<dyn FnOnce(&mut GrammarBuilder)>;

/// Add a left-associative infix level: `level = lower ( op lower )*`.
///
/// Each occurrence of `op lower` is wrapped in a node with the given `node_kind`.
/// `ops` is the list of rule names to try (e.g. `["op_star", "op_slash"]` for mul/div).
///
/// With `wrapper_kind`, the whole level is wrapped in that node (e.g. `NodeBinaryLevel`).
/// With `rhs_field` and `rhs_wrapper_kind`, the right operand is wrapped in a node
/// labeled with that field so analysis can use `field_by_id` / `field_by_name`.
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
///     &Kind::NodeBinaryExpr,
///     None,
///     None,
///     None,
/// );
/// ```
pub fn left_assoc_infix_level<K: IntoSyntaxKind + Clone + 'static>(
    g: &mut GrammarBuilder,
    level_name: &'static str,
    lower_level_name: &'static str,
    ops: &[&'static str],
    node_kind: &K,
    wrapper_kind: Option<&K>,
    rhs_field: Option<&'static str>,
    rhs_wrapper_kind: Option<&K>,
) {
    g.parser_rule(level_name, |g| {
        let body = |g: &mut GrammarBuilder| {
            g.call(lower_level_name);
            g.zero_or_more({
                let node_kind = node_kind.clone();
                let lower_level_name = lower_level_name;
                let rhs_field = rhs_field;
                let rhs_wrapper_kind = rhs_wrapper_kind.cloned();
                move |g: &mut GrammarBuilder| {
                    g.node(node_kind.clone(), |g| {
                        let mut alternatives: Vec<BuilderChoice> = Vec::new();
                        for op in ops {
                            let op = *op;
                            let lower = lower_level_name;
                            let rhs_field = rhs_field;
                            let rhs_wrapper_kind = rhs_wrapper_kind.clone();
                            alternatives.push(Box::new(move |g| {
                                g.call(op);
                                if let (Some(field), Some(wk)) =
                                    (rhs_field, rhs_wrapper_kind.as_ref())
                                {
                                    g.node_with_field(wk.clone(), field, |g| {
                                        g.call(lower);
                                    });
                                } else {
                                    g.call(lower);
                                }
                            }));
                        }
                        g.choices(alternatives);
                    });
                }
            });
        };
        match wrapper_kind {
            Some(wk) => g.node(wk.clone(), body),
            None => body(g),
        }
    });
}

/// Add a right-associative infix level: `level = lower op level | lower`.
///
/// Use for power (`**`) or assignment. The single operator is the rule name in `op_rule`.
/// With `rhs_field` and `rhs_wrapper_kind`, the recursive `level` operand is wrapped
/// in a node labeled with that field.
///
/// # Example
///
/// ```ignore
/// // expr_power = unary ** expr_power | unary
/// expr::right_assoc_infix_level(g, "expr_power", "unary", "op_power", &Kind::NodeBinaryExpr, None, None);
/// ```
pub fn right_assoc_infix_level<K: IntoSyntaxKind + Clone + 'static>(
    g: &mut GrammarBuilder,
    level_name: &'static str,
    lower_level_name: &'static str,
    op_rule: &'static str,
    node_kind: &K,
    rhs_field: Option<&'static str>,
    rhs_wrapper_kind: Option<&K>,
) {
    g.parser_rule(level_name, |g| {
        let lower = lower_level_name;
        let op = op_rule;
        let level = level_name;
        let kind = node_kind.clone();
        let rhs_field = rhs_field;
        let rhs_wrapper_kind = rhs_wrapper_kind.cloned();
        g.choices(vec![
            Box::new(move |g| {
                g.node(kind.clone(), |g| {
                    g.call(lower);
                    g.call(op);
                    if let (Some(field), Some(wk)) = (rhs_field, rhs_wrapper_kind.as_ref()) {
                        g.node_with_field(wk.clone(), field, |g| {
                            g.call(level);
                        });
                    } else {
                        g.call(level);
                    }
                });
            }),
            Box::new(move |g| {
                g.call(lower_level_name);
            }),
        ]);
    });
}
