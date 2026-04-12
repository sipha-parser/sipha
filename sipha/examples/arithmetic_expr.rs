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
    Term,
    Factor,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::Expr => "EXPR",
            Rule::Term => "TERM",
            Rule::Factor => "FACTOR",
        }
    }
}

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Root)]
struct Root(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Expr)]
struct Expr(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Term)]
struct Term(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = Rule::Factor)]
struct Factor(SyntaxNode);

#[derive(Debug, Clone, Copy)]
enum Op {
    Push(i64),
    Add,
    Mul,
}

impl Op {
    fn fmt_line(self) -> String {
        match self {
            Self::Push(n) => format!("PUSH {n}"),
            Self::Add => "ADD".to_string(),
            Self::Mul => "MUL".to_string(),
        }
    }
}

fn compile_expr(src: &[u8], expr: &Expr, out: &mut Vec<Op>) -> Result<(), String> {
    let mut it = expr.syntax().children().filter(|e| !e.is_trivia());
    let first = it
        .find_map(|e| e.into_node())
        .ok_or_else(|| "expr: missing first term".to_string())?;
    compile_term(src, &Term(first), out)?;

    while let Some(e) = it.next() {
        match e {
            sipha::tree::red::SyntaxElement::Token(t)
                if t.kind() == Lex::Plus.into_syntax_kind() =>
            {
                let next_term = it
                    .find_map(|e| e.into_node())
                    .ok_or_else(|| "expr: expected term after '+'".to_string())?;
                compile_term(src, &Term(next_term), out)?;
                out.push(Op::Add);
            }
            _ => return Err("expr: unexpected element".to_string()),
        }
    }

    Ok(())
}

fn compile_term(src: &[u8], term: &Term, out: &mut Vec<Op>) -> Result<(), String> {
    let mut it = term.syntax().children().filter(|e| !e.is_trivia());
    let first = it
        .find_map(|e| e.into_node())
        .ok_or_else(|| "term: missing first factor".to_string())?;
    compile_factor(src, &Factor(first), out)?;

    while let Some(e) = it.next() {
        match e {
            sipha::tree::red::SyntaxElement::Token(t)
                if t.kind() == Lex::Star.into_syntax_kind() =>
            {
                let next_factor = it
                    .find_map(|e| e.into_node())
                    .ok_or_else(|| "term: expected factor after '*'".to_string())?;
                compile_factor(src, &Factor(next_factor), out)?;
                out.push(Op::Mul);
            }
            _ => return Err("term: unexpected element".to_string()),
        }
    }

    Ok(())
}

fn compile_factor(src: &[u8], factor: &Factor, out: &mut Vec<Op>) -> Result<(), String> {
    let mut it = factor.syntax().children().filter(|e| !e.is_trivia());
    let first = it.next().ok_or_else(|| "factor: empty".to_string())?;

    match first {
        sipha::tree::red::SyntaxElement::Token(t) if t.kind() == Lex::Number.into_syntax_kind() => {
            let text = t.text();
            let n: i64 = text
                .parse()
                .map_err(|_| format!("number: invalid int literal `{text}`"))?;
            out.push(Op::Push(n));
            Ok(())
        }
        sipha::tree::red::SyntaxElement::Token(t) if t.kind() == Lex::LParen.into_syntax_kind() => {
            let inner_expr = it
                .find_map(|e| e.into_node())
                .ok_or_else(|| "factor: expected expr after '('".to_string())?;
            compile_expr(src, &Expr(inner_expr), out)?;

            let closing = it
                .find_map(|e| e.into_token())
                .ok_or_else(|| "factor: expected ')'".to_string())?;
            if closing.kind() != Lex::RParen.into_syntax_kind() {
                return Err("factor: expected ')'".to_string());
            }
            Ok(())
        }
        _ => Err("factor: expected number or parenthesized expr".to_string()),
    }
}

fn run_vm(ops: &[Op]) -> Result<i64, String> {
    let mut stack: Vec<i64> = Vec::new();
    for &op in ops {
        match op {
            Op::Push(n) => stack.push(n),
            Op::Add => {
                let b = stack
                    .pop()
                    .ok_or_else(|| "vm: stack underflow (add rhs)".to_string())?;
                let a = stack
                    .pop()
                    .ok_or_else(|| "vm: stack underflow (add lhs)".to_string())?;
                stack.push(a + b);
            }
            Op::Mul => {
                let b = stack
                    .pop()
                    .ok_or_else(|| "vm: stack underflow (mul rhs)".to_string())?;
                let a = stack
                    .pop()
                    .ok_or_else(|| "vm: stack underflow (mul lhs)".to_string())?;
                stack.push(a * b);
            }
        }
    }
    match stack.as_slice() {
        [v] => Ok(*v),
        _ => Err("vm: expected exactly one result value".to_string()),
    }
}

fn header(title: &str) {
    println!();
    println!("== {title} ==");
}

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    Lex::from_syntax_kind(k)
        .map(LexKind::display_name)
        .or_else(|| Rule::from_syntax_kind(k).map(RuleKind::display_name))
}

fn grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");
    // This grammar has an intentional rule cycle: expr → term → factor → expr.
    // Cycles are rejected by default unless explicitly enabled.
    g.allow_rule_cycles(true);
    // Emit TracePoint instructions only when explicitly enabled.
    let trace_mode = std::env::var("SIPHA_TRACE").is_ok();
    g.set_trace_mode(trace_mode);

    // The first defined rule is the grammar entrypoint.
    g.parser_rule("start", |g| {
        g.context_rule("start", |g| {
            g.node(Rule::Root, |g| {
                g.call("expr");
                g.skip();
            });
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("ws", |g| {
        g.trivia(Lex::Ws, |g| {
            g.zero_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("number", |g| {
        g.token(Lex::Number, |g| {
            g.one_or_more(|g| {
                g.class_with_label(classes::DIGIT, "digit");
            });
        });
    });

    // expr := term ("+" term)*
    g.parser_rule("expr", |g| {
        g.context_rule("expression", |g| {
            g.trace("enter expr");
            g.node(Rule::Expr, |g| {
                g.call("term");
                g.zero_or_more(|g| {
                    g.token(Lex::Plus, |g| {
                        g.byte(b'+');
                    });
                    g.call("term");
                });
            });
        });
    });

    // term := factor ("*" factor)*
    g.parser_rule("term", |g| {
        g.context_rule("term", |g| {
            g.node(Rule::Term, |g| {
                g.call("factor");
                g.zero_or_more(|g| {
                    g.token(Lex::Star, |g| {
                        g.byte(b'*');
                    });
                    g.call("factor");
                });
            });
        });
    });

    // factor := number | "(" expr ")"
    g.parser_rule("factor", |g| {
        g.context_rule("factor", |g| {
            g.node(Rule::Factor, |g| {
                g.choice(
                    |g| {
                        g.call("number");
                    },
                    |g| {
                        g.token(Lex::LParen, |g| {
                            g.byte(b'(');
                        });
                        g.call("expr");
                        g.hint("did you forget a closing ')' ?");
                        g.token(Lex::RParen, |g| {
                            g.byte(b')');
                        });
                    },
                );
            });
        });
    });

    g.finish().unwrap()
}

fn main() {
    let built = grammar();
    let graph = built.as_graph();

    let src = b"1 + 2 * (3 * 5 + 4)";
    let mut engine = Engine::new();
    #[cfg(feature = "trace")]
    if std::env::var("SIPHA_TRACE").is_ok() {
        let mut tracer = sipha::parse::engine::PrintTracer::default();
        let err = engine
            .parse_with_tracer(&graph, src, &ParseContext::new(), &mut tracer)
            .unwrap_err();
        if let ParseError::NoMatch(d) = err {
            let li = LineIndex::new(src);
            eprintln!(
                "{}",
                d.format_with_source(src, &li, Some(&graph.literals), Some(&graph))
            );
        }
        return;
    }

    let out = engine.parse(&graph, src).unwrap_or_else(|e| {
        if let ParseError::NoMatch(d) = e {
            let li = LineIndex::new(src);
            eprintln!(
                "{}",
                d.format_with_source(src, &li, Some(&graph.literals), Some(&graph))
            );
        } else {
            eprintln!("{e}");
        }
        std::process::exit(1);
    });
    let doc = ParsedDoc::from_slice(src, &out).unwrap();

    // Typed CST: cast root and traverse structure with typed helpers.
    let root: Root = doc.root().ast().unwrap();
    let expr: Expr = root.syntax().child().unwrap();
    let first_term: Term = expr.syntax().child().unwrap();
    let _first_factor: Factor = first_term.syntax().child().unwrap();

    header("input");
    println!("{}", String::from_utf8_lossy(src));

    let mut ops = Vec::new();
    compile_expr(src, &expr, &mut ops).expect("compile to vm ops");

    header("bytecode");
    for (i, op) in ops.iter().copied().enumerate() {
        println!("{i:>3}  {}", op.fmt_line());
    }

    let value = run_vm(&ops).expect("run vm");
    header("result");
    println!("{value}");

    header("cst (tree)");
    println!(
        "{}",
        sipha::tree::tree_display::format_syntax_tree(
            doc.root(),
            &TreeDisplayOptions::default(),
            |k| kind_name(k).unwrap_or("?").to_string()
        )
    );
}
