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
//!
//! ## Postfix and separated lists
//!
//! [`postfix_chain`] is `operand postfix*`. [`separated_rule_list`] and
//! [`separated1_rule_list`] define parser rules whose body uses
//! [`GrammarBuilder::separated`](crate::parse::builder::GrammarBuilder::separated) /
//! [`separated1`](crate::parse::builder::GrammarBuilder::separated1) with plain [`call`](crate::parse::builder::GrammarBuilder::call)s.

use crate::parse::builder::GrammarBuilder;
use crate::types::RuleKind;

/// Closure type for a single choice arm in the builder.
type BuilderChoice = Box<dyn FnOnce(&mut GrammarBuilder)>;

/// Parameters for [`left_assoc_infix_level`].
pub struct LeftAssocInfixLevel<'a, N: ?Sized> {
    pub level_name: &'static str,
    pub lower_level_name: &'static str,
    pub ops: &'a [&'static str],
    pub node_kind: &'a N,
    pub wrapper_kind: Option<&'a N>,
    pub rhs_field: Option<&'static str>,
    pub rhs_wrapper_kind: Option<&'a N>,
}

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
///     &LeftAssocInfixLevel {
///         level_name: "expr_mul",
///         lower_level_name: "expr_power",
///         ops: &["op_star", "op_slash", "op_percent"],
///         node_kind: &Kind::NodeBinaryExpr,
///         wrapper_kind: None,
///         rhs_field: None,
///         rhs_wrapper_kind: None,
///     },
/// );
/// ```
pub fn left_assoc_infix_level<N: RuleKind + Clone + 'static>(
    g: &mut GrammarBuilder,
    cfg: &LeftAssocInfixLevel<'_, N>,
) {
    let level_name = cfg.level_name;
    let lower_level_name = cfg.lower_level_name;
    let ops = cfg.ops;
    let node_kind = cfg.node_kind;
    let wrapper_kind = cfg.wrapper_kind;
    let rhs_field = cfg.rhs_field;
    let rhs_wrapper_kind = cfg.rhs_wrapper_kind;
    g.parser_rule(level_name, |g| {
        let body = |g: &mut GrammarBuilder| {
            g.call(lower_level_name);
            g.zero_or_more({
                let node_kind = node_kind.clone();
                let rhs_wrapper_kind = rhs_wrapper_kind.cloned();
                move |g: &mut GrammarBuilder| {
                    g.node(node_kind.clone(), |g| {
                        let mut alternatives: Vec<BuilderChoice> = Vec::new();
                        for op in ops {
                            let op = *op;
                            let lower = lower_level_name;
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
pub fn right_assoc_infix_level<N: RuleKind + Clone + 'static>(
    g: &mut GrammarBuilder,
    level_name: &'static str,
    lower_level_name: &'static str,
    op_rule: &'static str,
    node_kind: &N,
    rhs_field: Option<&'static str>,
    rhs_wrapper_kind: Option<&N>,
) {
    g.parser_rule(level_name, |g| {
        let lower = lower_level_name;
        let op = op_rule;
        let level = level_name;
        let kind = node_kind.clone();
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

/// `level = operand postfix*` — postfix/call/index chains inside a [`parser_rule`](crate::parse::builder::GrammarBuilder::parser_rule).
///
/// Example: `postfix_chain(g, "postfix_expr", "primary", "call_suffix")` for `primary call_suffix*`.
pub fn postfix_chain(
    g: &mut GrammarBuilder,
    level_name: &'static str,
    operand_rule: &'static str,
    postfix_rule: &'static str,
) {
    g.parser_rule(level_name, |g| {
        g.call(operand_rule);
        g.zero_or_more(|g| {
            g.call(postfix_rule);
        });
    });
}

/// Parser rule: `(elem_rule (sep_rule elem_rule)*)?` — zero or more `elem_rule` separated by `sep_rule`.
pub fn separated_rule_list(
    g: &mut GrammarBuilder,
    list_name: &'static str,
    elem_rule: &'static str,
    sep_rule: &'static str,
) {
    g.parser_rule(list_name, |g| {
        let elem = elem_rule;
        let sep = sep_rule;
        g.separated(
            |g| {
                g.call(elem);
            },
            |g| {
                g.call(sep);
            },
        );
    });
}

/// Parser rule: `elem_rule (sep_rule elem_rule)*` — one or more elements separated by `sep_rule`.
pub fn separated1_rule_list(
    g: &mut GrammarBuilder,
    list_name: &'static str,
    elem_rule: &'static str,
    sep_rule: &'static str,
) {
    g.parser_rule(list_name, |g| {
        let elem = elem_rule;
        let sep = sep_rule;
        g.separated1(
            |g| {
                g.call(elem);
            },
            |g| {
                g.call(sep);
            },
        );
    });
}
