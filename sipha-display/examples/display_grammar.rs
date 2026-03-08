//! Build a small grammar and display it as PEG and as a rule-dependency DOT graph.

use sipha::builder::GrammarBuilder;
use sipha_display::{to_cfg_dot, to_peg, to_rule_dep_dot};

fn main() {
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
    let built = g.finish().expect("grammar build");

    println!("=== PEG ===\n{}\n", to_peg(&built));
    println!(
        "=== Rule dependency graph (DOT) ===\n{}\n",
        to_rule_dep_dot(&built)
    );
    println!(
        "=== CFG for rule 'start' (DOT) ===\n{}\n",
        to_cfg_dot(&built, 1)
    );
}
