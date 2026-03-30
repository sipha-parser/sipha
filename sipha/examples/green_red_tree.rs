//! # Green / Red Tree example
//!
//! Demonstrates the full pipeline:
//!
//! 1. Build a grammar with `g.node()`, `g.token()`, and `g.trivia()`
//! 2. Parse input
//! 3. Call `output.syntax_root(input)` to get the red tree root
//! 4. Navigate the tree: children, tokens, trivia, `token_groups`, `find_node`,
//!    `descendant_semantic_tokens`, `token_at_offset`, `collect_text`
//!
//! Grammar: a tiny statement list
//! ```
//! stmt_list  ←  stmt*
//! stmt       ←  ws* keyword ws+ ident ws* ';' ws*
//! keyword    ←  "let" | "const" | "var"
//! ident      ←  [a-zA-Z_][a-zA-Z0-9_]*
//! ws         ←  [ \t\n\r]+    (trivia)
//! ```

use sipha::prelude::*;
use sipha::types::classes;

// ── SyntaxKind constants ──────────────────────────────────────────────────────

const NODE_STMT_LIST: SyntaxKind = 0;
const NODE_STMT: SyntaxKind = 1;

const TOK_KEYWORD: SyntaxKind = 10;
const TOK_IDENT: SyntaxKind = 11;
const TOK_SEMICOLON: SyntaxKind = 12;
const TOK_WS: SyntaxKind = 20; // trivia kind

// ── Grammar ───────────────────────────────────────────────────────────────────

fn build_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();

    // root: stmt_list EOF
    g.rule("root", |g| {
        g.node(NODE_STMT_LIST, |g| {
            // optional leading whitespace at file start
            g.trivia(TOK_WS, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
            // zero or more statements
            g.zero_or_more(|g| {
                g.call("stmt");
            });
        });
        g.end_of_input();
        g.accept();
    });

    // stmt: keyword ws+ ident ws* ';' trailing_ws
    g.rule("stmt", |g| {
        g.node(NODE_STMT, |g| {
            // keyword token
            g.token(TOK_KEYWORD, |g| {
                g.choice(
                    |g| {
                        g.literal(b"let");
                    },
                    |g| {
                        g.choice(
                            |g| {
                                g.literal(b"const");
                            },
                            |g| {
                                g.literal(b"var");
                            },
                        );
                    },
                );
            });
            // mandatory whitespace between keyword and ident (trivia)
            g.trivia(TOK_WS, |g| {
                g.one_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
            // identifier token
            g.token(TOK_IDENT, |g| {
                g.call("ident");
            });
            // optional whitespace before semicolon
            g.trivia(TOK_WS, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
            // semicolon
            g.token(TOK_SEMICOLON, |g| {
                g.byte(b';');
            });
            // trailing whitespace / blank lines after the statement
            g.trivia(TOK_WS, |g| {
                g.zero_or_more(|g| {
                    g.class(classes::WHITESPACE);
                });
            });
        });
    });

    // ident: [a-zA-Z_][a-zA-Z0-9_]*
    g.rule("ident", |g| {
        g.char_range('a', 'z'); // simplistic: lowercase-only for this demo
        g.zero_or_more(|g| {
            g.choice(
                |g| {
                    g.char_range('a', 'z');
                },
                |g| {
                    g.char_range('A', 'Z');
                },
            );
        });
    });

    g.finish().unwrap()
}

// ── Helpers ───────────────────────────────────────────────────────────────────

const fn kind_name(k: SyntaxKind) -> &'static str {
    match k {
        NODE_STMT_LIST => "StmtList",
        NODE_STMT => "Stmt",
        TOK_KEYWORD => "Keyword",
        TOK_IDENT => "Ident",
        TOK_SEMICOLON => "Semicolon",
        TOK_WS => "Whitespace",
        _ => "?",
    }
}

/// Pretty-print the syntax tree with indentation.
fn print_tree(node: &SyntaxNode, indent: usize) {
    let pad = "  ".repeat(indent);
    let range = node.text_range();
    println!(
        "{}Node({}) {}..{}",
        pad,
        kind_name(node.kind()),
        range.start,
        range.end
    );
    for child in node.children() {
        match child {
            SyntaxElement::Node(n) => print_tree(&n, indent + 1),
            SyntaxElement::Token(t) => {
                let trivia_mark = if t.is_trivia() { " [trivia]" } else { "" };
                println!(
                    "{}  Token({}){}  {:?}  {}..{}",
                    pad,
                    kind_name(t.kind()),
                    trivia_mark,
                    t.text(),
                    t.offset(),
                    t.offset() + t.text_len()
                );
            }
        }
    }
}

fn check(pass: &mut usize, fail: &mut usize, ok: bool, desc: &str) {
    if ok {
        println!("  [✓] {desc}");
        *pass += 1;
    } else {
        println!("  [✗] {desc}");
        *fail += 1;
    }
}

// ── Main ──────────────────────────────────────────────────────────────────────

fn main() {
    let bg = build_grammar();
    let graph = bg.as_graph();
    let mut engine = Engine::new();

    // ── Test 1: Parse a simple statement ─────────────────────────────────────

    let src1 = b"  let foo ;\n";
    let out1 = engine.parse(&graph, src1).expect("parse failed");

    println!(
        "─── Tree for {:?} ───",
        std::str::from_utf8(src1).unwrap().trim()
    );
    let root1 = out1.syntax_root(src1).expect("build_green_tree failed");
    print_tree(&root1, 0);
    println!();

    // ── Test 2: Parse multiple statements ─────────────────────────────────────

    let src2 = b"let foo;\nconst bar ;\nvar baz;\n";
    let out2 = engine.parse(&graph, src2).expect("parse 2 failed");
    let root2 = out2.syntax_root(src2).expect("tree 2 failed");

    println!(
        "─── Tree for {} statements ───",
        root2.find_all_nodes(NODE_STMT).len()
    );
    print_tree(&root2, 0);
    println!();

    // ── Assertions ────────────────────────────────────────────────────────────

    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut c = |ok: bool, desc: &str| check(&mut pass, &mut fail, ok, desc);

    println!("─── Green tree: text storage ───");

    // GreenToken stores its own text — no source buffer needed.
    let stmts = root2.find_all_nodes(NODE_STMT);
    c(stmts.len() == 3, "found 3 Stmt nodes");

    let kw0 = stmts[0].non_trivia_tokens().next().unwrap();
    c(
        kw0.kind() == TOK_KEYWORD,
        "first token of stmt[0] is Keyword",
    );
    c(
        kw0.text() == "let",
        "Keyword text is 'let' (no source buffer needed)",
    );

    let kw1 = stmts[1].non_trivia_tokens().next().unwrap();
    c(kw1.text() == "const", "Keyword text is 'const'");

    let kw2 = stmts[2].non_trivia_tokens().next().unwrap();
    c(kw2.text() == "var", "Keyword text is 'var'");

    // collect_text: reconstruct node text from stored tokens.
    let stmt0_text = stmts[0].collect_text();
    c(
        stmt0_text == "let foo;\n",
        &format!("stmt[0].collect_text() = {stmt0_text:?}"),
    );

    println!();
    println!("─── Red tree: ranges ───");

    // token_at_offset: find the token at a specific byte.
    // "let foo;\n" starts at byte 0; "let" is bytes 0..3.
    let tok_at_0 = root2.token_at_offset(0).unwrap();
    c(
        tok_at_0.kind() == TOK_KEYWORD && tok_at_0.text() == "let",
        "token_at_offset(0) = Keyword 'let'",
    );

    // "foo" starts at byte 4.
    let tok_at_4 = root2.token_at_offset(4).unwrap();
    c(
        tok_at_4.kind() == TOK_IDENT && tok_at_4.text() == "foo",
        "token_at_offset(4) = Ident 'foo'",
    );

    // Trivia token at byte 3 (the space between "let" and "foo").
    let tok_at_3 = root2.token_at_offset(3).unwrap();
    c(
        tok_at_3.is_trivia(),
        "token_at_offset(3) is trivia (whitespace)",
    );

    // semantic_token_at_offset skips trivia.
    let sem = root2.semantic_token_at_offset(3);
    c(
        sem.is_none(),
        "semantic_token_at_offset(3) = None (trivia position)",
    );

    println!();
    println!("─── Trivia: token_groups ───");

    // token_groups on the first stmt.
    let groups = stmts[0].token_groups();
    c(
        groups.len() == 3,
        &format!("stmt[0] has 3 token groups (got {})", groups.len()),
    );

    // Group 0: keyword "let" — no leading trivia (the leading WS is at list level)
    let g0 = &groups[0];
    c(g0.token.kind() == TOK_KEYWORD, "group[0] is Keyword");
    c(g0.leading.is_empty(), "group[0] has no leading trivia");
    c(
        g0.trailing.len() == 1,
        "group[0] has 1 trailing trivia (space)",
    );
    c(
        g0.trailing[0].text() == " ",
        "group[0] trailing trivia text = ' '",
    );

    // Group 1: ident "foo"
    let g1 = &groups[1];
    c(g1.token.kind() == TOK_IDENT, "group[1] is Ident");
    c(g1.leading.is_empty(), "group[1] has no leading trivia");
    c(
        g1.trailing.is_empty(),
        "group[1] has no trailing trivia (no space before ';')",
    );

    // Group 2: semicolon ";"
    let g2 = &groups[2];
    c(g2.token.kind() == TOK_SEMICOLON, "group[2] is Semicolon");
    c(
        g2.trailing.len() == 1,
        "group[2] has trailing newline trivia",
    );
    c(
        g2.trailing[0].text() == "\n",
        "group[2] trailing trivia = '\\n'",
    );

    // full_range covers everything including trivia.
    let stmt0_full = stmts[0].text_range();
    c(
        stmt0_full.start == 0 && stmt0_full.end == 9,
        &format!(
            "stmt[0] range = 0..9, got {}..{}",
            stmt0_full.start, stmt0_full.end
        ),
    );

    println!();
    println!("─── Trivia: leading/trailing at node level ───");

    // root2 (StmtList) has no leading trivia (first child is a Stmt, not trivia).
    let lt = root2.leading_trivia();
    c(lt.is_empty(), "StmtList has no leading trivia");

    // A stmt with leading whitespace (src1 = "  let foo ;\n").
    let stmts1 = root1.find_all_nodes(NODE_STMT);
    let stmt1 = &stmts1[0];
    // The StmtList node wraps the whole thing; the leading whitespace ends up
    // attached to the first token group rather than the parent node.
    let root1_list = root1.find_node(NODE_STMT_LIST).unwrap();
    let list_lt = root1_list.leading_trivia();
    c(
        list_lt.is_empty(),
        &format!(
            "StmtList leading_trivia = {:?}",
            list_lt
                .iter()
                .map(sipha::tree::red::SyntaxToken::text)
                .collect::<Vec<_>>()
        ),
    );

    println!();
    println!("─── Trivia: full_text on TokenWithTrivia ───");

    let stmt1_groups = stmt1.token_groups();
    // keyword group includes the leading indentation and trailing space
    let kw_group = &stmt1_groups[0];
    c(
        kw_group.full_text() == "  let ",
        &format!("kw full_text = {:?}", kw_group.full_text()),
    );

    // semicolon group includes trailing " \n" (space then newline after ';')
    // In src1 = "  let foo ;\n", the token groups retain the indentation.
    let sc_group = stmt1_groups.last().unwrap();
    c(
        sc_group.full_text().ends_with('\n'),
        &format!(
            "semicolon full_text ends with newline: {:?}",
            sc_group.full_text()
        ),
    );

    println!();
    println!("─── descendant_semantic_tokens ───");

    let all_sem: Vec<_> = root2.descendant_semantic_tokens();
    c(
        all_sem.len() == 9, // 3 stmts × (keyword + ident + semicolon)
        &format!("root2 has 9 semantic tokens (got {})", all_sem.len()),
    );

    let idents: Vec<_> = all_sem.iter().filter(|t| t.kind() == TOK_IDENT).collect();
    c(idents.len() == 3, "3 ident tokens");
    c(
        idents[0].text() == "foo" && idents[1].text() == "bar" && idents[2].text() == "baz",
        "idents are foo, bar, baz",
    );

    println!();
    println!("─── Error cases ───");

    // Missing semicolon.
    let bad1 = b"let foo\n";
    c(
        engine.parse(&graph, bad1).is_err(),
        "missing semicolon → parse error",
    );

    // Empty input: valid (zero statements).
    let empty = b"";
    let out_empty = engine
        .parse(&graph, empty)
        .expect("empty input should succeed");
    let root_empty = out_empty.syntax_root(empty).expect("empty tree");
    c(
        root_empty.find_all_nodes(NODE_STMT).is_empty(),
        "empty input → 0 stmts",
    );
    c(
        root_empty.collect_text() == "",
        "empty input → collect_text = ''",
    );

    // Whitespace-only input.
    let ws_only = b"  \n  ";
    let out_ws = engine
        .parse(&graph, ws_only)
        .expect("ws-only should succeed");
    let root_ws = out_ws.syntax_root(ws_only).expect("ws tree");
    c(
        root_ws.find_all_nodes(NODE_STMT).is_empty(),
        "ws-only input → 0 stmts",
    );
    c(
        root_ws.collect_text() == "  \n  ",
        "ws text preserved in tree",
    );

    println!();
    println!("─── Green tree: text is self-contained ───");

    // Build tree, then drop the source buffer — tokens still have their text.
    let owned_src: Vec<u8> = b"let xyz;".to_vec();
    let owned_out = engine.parse(&graph, &owned_src).expect("parse owned");
    let owned_root = owned_out.syntax_root(&owned_src).expect("tree");
    drop(owned_src); // drop the source buffer
                     // The tree's tokens still own their text via Arc<str>.
    let first_kw = owned_root
        .descendant_semantic_tokens()
        .into_iter()
        .find(|t| t.kind() == TOK_KEYWORD)
        .unwrap();
    c(
        first_kw.text() == "let",
        "GreenToken text survives drop of source buffer",
    );

    println!();
    println!(
        "Results: {pass} passed, {fail} failed out of {}.",
        pass + fail
    );
    if fail > 0 {
        std::process::exit(1);
    }
}
