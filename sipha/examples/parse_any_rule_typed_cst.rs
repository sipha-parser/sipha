use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, sipha::SyntaxKinds)]
#[repr(u16)]
enum K {
    RootExpr,
    RootStmt,
    Expr,
    Number,
    LetKw,
    Ident,
    Eq,
    Ws,
}

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = K::RootExpr)]
pub struct RootExpr(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = K::RootStmt)]
pub struct RootStmt(SyntaxNode);

#[derive(Clone, Debug, sipha::AstNode)]
#[ast(kind = K::Expr)]
pub struct Expr(SyntaxNode);

fn grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.set_trivia_rule("ws");

    // Entry rules (so each can be parsed as a top-level).
    g.parser_rule("expr_root", |g| {
        g.node(K::RootExpr, |g| {
            g.call("expr");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.parser_rule("stmt_root", |g| {
        g.node(K::RootStmt, |g| {
            g.call("stmt");
            g.skip();
        });
        g.end_of_input();
        g.accept();
    });

    g.lexer_rule("ws", |g| {
        g.trivia(K::Ws, |g| {
            g.one_or_more(|g| {
                g.class(classes::WHITESPACE);
            });
        });
    });

    g.lexer_rule("number", |g| {
        g.token(K::Number, |g| {
            g.one_or_more(|g| {
                g.class(classes::DIGIT);
            });
        });
    });

    g.lexer_rule("ident", |g| {
        g.token(K::Ident, |g| {
            g.class(classes::IDENT_START);
            g.zero_or_more(|g| {
                g.class(classes::IDENT_CONT);
            });
        });
    });

    // expr := number
    g.parser_rule("expr", |g| {
        g.node(K::Expr, |g| {
            g.call("number");
        });
    });

    // stmt := "let" ident "=" expr
    g.parser_rule("stmt", |g| {
        g.token(K::LetKw, |g| {
            g.literal(b"let");
        });
        g.call("ident");
        g.token(K::Eq, |g| {
            g.byte(b'=');
        });
        g.call("expr");
    });

    g.finish().unwrap()
}

fn main() {
    let built = grammar();
    let graph = built.as_graph();

    let mut engine = Engine::new();

    // Parse starting from *any* entry rule by name.
    let expr_out = engine
        .parse_rule_named(&graph, b"123", "expr_root")
        .unwrap();
    let expr_root = expr_out.syntax_root(b"123").unwrap();
    let typed_root: RootExpr = expr_root.ast().unwrap();
    let expr: Expr = typed_root.syntax().child().unwrap();
    assert!(expr.syntax().child_nodes().next().is_some());

    let stmt_out = engine
        .parse_rule_named(&graph, b"let x=123", "stmt_root")
        .unwrap();
    let stmt_root = stmt_out.syntax_root(b"let x=123").unwrap();
    let _typed_stmt: RootStmt = stmt_root.ast().unwrap();

    // Unknown rule name becomes a dedicated error.
    let err = engine
        .parse_rule_named(&graph, b"123", "does_not_exist")
        .unwrap_err();
    eprintln!("expected failure: {err}");
}
