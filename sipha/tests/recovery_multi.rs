#![cfg(feature = "std")]

use sipha::prelude::*;

fn build_recovering_grammar() -> BuiltGraph {
    // Keep this grammar intentionally minimal: the VM’s recovery mechanism is
    // exercised without involving tree events, trivia injection, or token/node helpers.
    let mut g = GrammarBuilder::new();

    g.rule("semi", |g| {
        g.literal(b";");
    });

    g.rule("statement", |g| {
        // One lowercase letter.
        g.class(classes::LOWER);
    });

    g.rule("start", |g| {
        g.zero_or_more(|g| {
            g.recover_until("semi", |g| {
                g.call("statement");
            });
            g.optional(|g| {
                g.call("semi");
            });
        });
        g.end_of_input();
        g.accept();
    });

    g.finish().unwrap()
}

#[test]
fn can_collect_multiple_errors_with_recover_until() {
    let built = build_recovering_grammar();
    let graph = built.as_graph();
    let mut engine = Engine::new();

    // Two bad statements: '1' and '2' are not lowercase letters.
    let src = b"a;1;b;2;c;";
    let res = engine.parse_recovering_multi(&graph, src, 8);
    let Err(multi) = res else {
        panic!("expected recovery to collect errors");
    };

    assert!(!multi.errors.is_empty(), "errors: {:#?}", multi.errors);
}
