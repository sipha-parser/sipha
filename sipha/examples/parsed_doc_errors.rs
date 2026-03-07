//! # `ParsedDoc` and better errors
//!
//! Shows the recommended API for compiler/formatter use:
//!
//! 1. Parse; on success build a [`ParsedDoc`] for a single handle to source + tree + line index.
//! 2. Use `doc.root()` for tree walking, `doc.source()`, `doc.offset_to_line_col_1based()`.
//! 3. On parse failure, use the grammar's literal table and a temporary [`LineIndex`]
//!    to format the diagnostic with line/column and source snippet.

use sipha::prelude::*;
use sipha::types::classes;

const NODE_STMT: SyntaxKind = 1;
const TOK_KEYWORD: SyntaxKind = 10;
const TOK_IDENT: SyntaxKind = 11;
const TOK_SEMICOLON: SyntaxKind = 12;
const TOK_WS: SyntaxKind = 20;

fn build_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.begin_rule("root");
    g.node(0, |g| {
        g.trivia(TOK_WS, |g| { g.zero_or_more(|g| { g.class(classes::WHITESPACE); }); });
        g.zero_or_more(|g| { g.call("stmt"); });
    });
    g.end_of_input();
    g.accept();

    g.begin_rule("stmt");
    g.node(NODE_STMT, |g| {
        g.token(TOK_KEYWORD, |g| {
            g.choice(
                |g| { g.literal(b"let"); },
                |g| g.choice(
                    |g| { g.literal(b"const"); },
                    |g| { g.literal(b"var"); },
                ),
            );
        });
        g.trivia(TOK_WS, |g| { g.one_or_more(|g| { g.class(classes::WHITESPACE); }); });
        g.token(TOK_IDENT, |g| { g.call("ident"); });
        g.trivia(TOK_WS, |g| { g.zero_or_more(|g| { g.class(classes::WHITESPACE); }); });
        g.token(TOK_SEMICOLON, |g| { g.byte(b';'); });
        g.trivia(TOK_WS, |g| { g.zero_or_more(|g| { g.class(classes::WHITESPACE); }); });
    });
    g.end_rule();

    g.begin_rule("ident");
    g.char_range('a', 'z');
    g.zero_or_more(|g| {
        g.choice(
            |g| { g.char_range('a', 'z'); },
            |g| { g.char_range('A', 'Z'); },
        );
    });
    g.end_rule();

    g.finish().unwrap()
}

fn main() {
    let built = build_grammar();
    let graph = built.as_graph();
    let literals = Some(&graph.literals);
    let mut engine = Engine::new();

    // ── Success: build ParsedDoc and use it ───────────────────────────────────

    let ok_src = b"let foo;\nconst bar;";
    match engine.parse(&graph, ok_src) {
        Ok(output) => {
            let doc = ParsedDoc::new(ok_src.to_vec(), &output).expect("tree");
            let root = doc.root();
            let stmts = root.find_all_nodes(NODE_STMT);
            println!("Parsed {} statement(s)", stmts.len());
            for (i, stmt) in stmts.iter().enumerate() {
                let (line, col) = doc.offset_to_line_col_1based(stmt.offset());
                println!("  stmt {} at {}:{}", i + 1, line, col);
            }
            // Formatter-style: token groups with trivia
            let first = &stmts[0];
            for g in first.token_groups() {
                println!("  token {:?} at {}..{}", g.token.text(), g.token.text_range().start, g.token.text_range().end);
            }
        }
        Err(ParseError::NoMatch(diag)) => {
            let line_index = LineIndex::new(ok_src);
            let msg = diag.format_with_source(ok_src, &line_index, literals, Some(graph.rule_names), Some(graph.expected_labels));
            println!("{msg}");
        }
        Err(ParseError::BadGraph) => println!("Bad graph"),
    }

    // ── Failure: show better error with snippet ─────────────────────────────────

    let bad_src = b"let foo\n  bar"; // missing semicolon after "foo"
    if let Err(ParseError::NoMatch(diag)) = engine.parse(&graph, bad_src) {
        let line_index = LineIndex::new(bad_src);
        let msg = diag.format_with_source(bad_src, &line_index, literals, Some(graph.rule_names), Some(graph.expected_labels));
        println!("\n--- Error (with line/col and snippet) ---\n{msg}");
    }
}
