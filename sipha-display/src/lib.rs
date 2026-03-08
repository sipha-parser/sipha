//! # sipha-display
//!
//! Utilities to display a sipha grammar as PEG notation or as a graph (DOT).

pub mod graph;
pub mod peg;

pub use graph::{to_cfg_dot, to_rule_dep_dot, to_rule_dep_dot_with_options, RuleDepDotOptions};
pub use peg::to_peg;

#[cfg(test)]
mod tests {
    use sipha::builder::GrammarBuilder;

    fn minimal_grammar() -> sipha::builder::BuiltGraph {
        let mut g = GrammarBuilder::new();
        g.rule("a", |g| {
            g.literal(b"a");
        });
        g.rule("start", |g| {
            g.choice(
                |g| {
                    g.call("a");
                },
                |g| {
                    g.literal(b"b");
                },
            );
            g.end_of_input();
            g.accept();
        });
        g.finish().unwrap()
    }

    #[test]
    fn test_to_peg() {
        let graph = minimal_grammar();
        let peg = super::to_peg(&graph);
        assert!(peg.contains("start <-"), "PEG should contain start rule");
        assert!(peg.contains("a <-"), "PEG should contain a rule");
        assert!(peg.contains("/"), "PEG should contain choice");
        assert!(peg.contains("\"a\""), "PEG should contain literal a");
        assert!(peg.contains("\"b\""), "PEG should contain literal b");
    }

    #[test]
    fn test_to_rule_dep_dot() {
        let graph = minimal_grammar();
        let dot = super::to_rule_dep_dot(&graph);
        assert!(dot.contains("digraph"), "DOT should be a digraph");
        assert!(dot.contains(r#""start" -> "a""#), "start should call a");
    }

    #[test]
    fn test_to_cfg_dot() {
        let graph = minimal_grammar();
        let dot = super::to_cfg_dot(&graph, 0);
        assert!(dot.contains("digraph"), "CFG DOT should be a digraph");
        assert!(dot.contains("a"), "CFG should mention rule a");
    }
}
