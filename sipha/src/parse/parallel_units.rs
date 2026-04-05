//! Experimental “parallel units” parsing (feature: `experimental_parallel_units`).
//!
//! This is the practical form of “parallelize a single parse” for PEG grammars:
//! the caller explicitly identifies independent units (byte ranges) and a rule
//! that parses one unit. We parse those units in parallel (one `Engine` per worker),
//! then optionally stitch their trees under a synthetic root.
//!
//! This preserves semantics **within each unit**, but it is not equivalent to a
//! full single-pass parse unless the grammar is context-independent across unit
//! boundaries.

use rayon::prelude::*;
use std::sync::Arc;

use crate::parse::context::ParseContext;
use crate::parse::engine::{Engine, ParseError, ParseOutput};
use crate::parse::insn::ParseGraph;
use crate::tree::green::{GreenElement, GreenNode};
use crate::tree::red::SyntaxNode;
use crate::types::{Pos, RuleId};

/// One parsing unit: a byte span in the full input.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnitRange {
    pub start: Pos,
    pub end: Pos,
}

impl UnitRange {
    #[must_use]
    pub const fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }
}

/// Parse many independent units in parallel.
///
/// The returned vector preserves the input order (`units[i]` → result `i`).
///
/// # Errors
///
/// Each unit yields its own `Result`. A failure parsing one unit does not stop others.
#[must_use]
pub fn parse_units_parallel<'g>(
    graph: &ParseGraph<'g>,
    input: &[u8],
    unit_rule: RuleId,
    units: &[UnitRange],
    context: &ParseContext,
) -> Vec<Result<ParseOutput, ParseError>> {
    units
        .par_iter()
        .map_init(Engine::new, |engine, u| {
            engine.parse_rule_at_with_context(graph, input, unit_rule, u.start, context)
        })
        .collect()
}

/// Stitch successful unit parses into a synthetic green root.
///
/// Each unit output must produce exactly one green root node; failures return `None`.
#[must_use]
pub fn stitch_unit_trees(input: &[u8], outputs: &[ParseOutput]) -> Option<Arc<GreenNode>> {
    let mut children: Vec<GreenElement> = Vec::with_capacity(outputs.len());
    for out in outputs {
        let root = crate::tree::green::build_green_tree(input, &out.tree_events)?;
        children.push(GreenElement::Node(root));
    }
    Some(GreenNode::new(u16::MAX, children))
}

/// Like [`stitch_unit_trees`], but returns a red `SyntaxNode` root.
#[must_use]
pub fn stitch_unit_syntax_root(input: &[u8], outputs: &[ParseOutput]) -> Option<SyntaxNode> {
    stitch_unit_trees(input, outputs).map(SyntaxNode::new_root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::builder::GrammarBuilder;
    use crate::types::{IntoSyntaxKind, LexKind, RuleKind, SyntaxKind};

    #[derive(Clone, Copy)]
    struct TestRule(SyntaxKind);
    impl IntoSyntaxKind for TestRule {
        fn into_syntax_kind(self) -> SyntaxKind {
            self.0
        }
    }
    impl RuleKind for TestRule {
        fn display_name(self) -> &'static str {
            "TEST_RULE"
        }
    }

    #[derive(Clone, Copy)]
    struct TestLex(SyntaxKind);
    impl IntoSyntaxKind for TestLex {
        fn into_syntax_kind(self) -> SyntaxKind {
            self.0
        }
    }
    impl LexKind for TestLex {
        fn display_name(self) -> &'static str {
            "TEST_LEX"
        }
    }

    #[test]
    fn parse_units_parallel_parses_each_unit() {
        // unit := node(2, token(10, byte('a')))
        let mut g = GrammarBuilder::new();
        let unit_rule = g.rule("unit", |g| {
            g.node(TestRule(2), |g| {
                g.token(TestLex(10), |g| {
                    g.byte(b'a');
                });
            });
        });
        g.rule("start", |g| {
            g.call_id(unit_rule);
            g.call_id(unit_rule);
            g.end_of_input();
            g.accept();
        });
        let built = g.finish().unwrap();
        let graph = built.as_graph();

        let input = b"aa";
        let units = [UnitRange::new(0, 1), UnitRange::new(1, 2)];
        let ctx = ParseContext::new();

        let outs = parse_units_parallel(&graph, input, unit_rule, &units, &ctx);
        assert!(outs.iter().all(|r| r.is_ok()));

        let outs_ok: Vec<ParseOutput> = outs.into_iter().map(Result::unwrap).collect();
        let stitched = stitch_unit_trees(input, &outs_ok).expect("stitched");
        assert_eq!(stitched.text_len, 2);
    }
}
