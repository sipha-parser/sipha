#![no_main]

use libfuzzer_sys::fuzz_target;
use once_cell::sync::Lazy;
use sipha::parse::incremental::TextEdit;
use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
enum Lex {
    Number,
    Plus,
    Star,
    LParen,
    RParen,
    Ws,
}

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        match self {
            Lex::Number => "NUMBER",
            Lex::Plus => "PLUS",
            Lex::Star => "STAR",
            Lex::LParen => "LPAREN",
            Lex::RParen => "RPAREN",
            Lex::Ws => "WS",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, RuleKinds)]
#[sipha(lex = Lex)]
#[repr(u16)]
enum Rule {
    Root,
    Expr,
    BinExpr,
    ParenExpr,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::Expr => "EXPR",
            Rule::BinExpr => "BIN_EXPR",
            Rule::ParenExpr => "PAREN_EXPR",
        }
    }
}

fn expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    g.lexer_rule("ws", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| g.class(classes::WHITESPACE));
        });
    });

    g.lexer_rule("number", |g| {
        g.token(Lex::Number, |g| {
            g.one_or_more(|g| g.class(classes::DIGIT));
        });
    });

    g.parser_rule("expr", |g| {
        g.node(Rule::Expr, |g| {
            g.call("mul");
            g.zero_or_more(|g| {
                g.token(Lex::Plus, |g| g.byte(b'+'));
                g.call("mul");
            });
        });
    });

    g.parser_rule("mul", |g| {
        g.node(Rule::BinExpr, |g| {
            g.call("atom");
            g.zero_or_more(|g| {
                g.token(Lex::Star, |g| g.byte(b'*'));
                g.call("atom");
            });
        });
    });

    g.parser_rule("atom", |g| {
        g.choice(|g| g.call("number"), |g| {
            g.node(Rule::ParenExpr, |g| {
                g.token(Lex::LParen, |g| g.byte(b'('));
                g.call("expr");
                g.token(Lex::RParen, |g| g.byte(b')'));
            });
        });
    });

    g.parser_rule("start", |g| {
        g.node(Rule::Root, |g| {
            g.call("expr");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.finish().unwrap()
}

static BUILT: Lazy<BuiltGraph> = Lazy::new(expr_grammar);

fn map_input(data: &[u8]) -> Vec<u8> {
    let mut buf = Vec::with_capacity(data.len());
    for &b in data {
        let mapped = match b % 8 {
            0 => b'0' + (b % 10),
            1 => b'+',
            2 => b'*',
            3 => b'(',
            4 => b')',
            5 => b' ',
            6 => b'\n',
            _ => b'\t',
        };
        buf.push(mapped);
    }
    buf
}

fn map_insert_text(b: u8) -> u8 {
    match b % 6 {
        0 => b'0' + (b % 10),
        1 => b'+',
        2 => b'*',
        3 => b'(',
        4 => b')',
        _ => b' ',
    }
}

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    let graph = BUILT.as_graph();

    // Build an initial valid-ish source and parse it to obtain a baseline syntax tree.
    let mut old_source = map_input(data);
    if old_source.is_empty() {
        old_source.push(b'0');
    }

    let mut engine = Engine::new();
    let old_out = match engine.parse(&graph, &old_source) {
        Ok(o) => o,
        Err(_) => return,
    };
    let Some(old_root) = old_out.syntax_root(&old_source) else {
        return;
    };

    // Derive one non-overlapping edit from the tail of `data`.
    // Keeping it to one edit keeps the target fast and avoids invalid overlapping edits.
    let start = (data[0] as usize) % old_source.len();
    let end = start + ((data[1] as usize) % (old_source.len() - start));
    let insert_len = (data[2] as usize) % 8;
    let mut new_text = Vec::with_capacity(insert_len);
    for i in 0..insert_len {
        new_text.push(map_insert_text(data[3 + (i % (data.len() - 3))]));
    }
    let edit = TextEdit {
        start: start as u32,
        end: end as u32,
        new_text,
    };

    let edits = [edit];
    let new_source = TextEdit::apply_edits(&old_source, &edits);

    // Full parse.
    let full_out = match engine.parse(&graph, &new_source) {
        Ok(o) => o,
        Err(_) => return,
    };
    if full_out.consumed as usize != new_source.len() {
        return;
    }

    // Incremental reparse.
    let mut engine2 = Engine::new();
    let new_root = match sipha::parse::incremental::reparse(&mut engine2, &graph, &old_source, &old_root, &edits) {
        Ok(Some(r)) => r,
        Ok(None) => return,
        Err(_) => return,
    };

    // Basic sanity: incremental parse should also accept full input by construction.
    // We don't do deep tree equality here (too slow for fuzz); tests cover that.
    assert_eq!(new_root.text_len() as usize, new_source.len());
});

