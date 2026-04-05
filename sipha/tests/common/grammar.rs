use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
pub enum Lex {
    Number,
    Plus,
    Star,
    LParen,
    RParen,
    Ws,
}

impl LexKind for Lex {
    #[inline]
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
pub enum Rule {
    Root,
    Expr,
    BinExpr,
    ParenExpr,
}

impl RuleKind for Rule {
    #[inline]
    fn display_name(self) -> &'static str {
        match self {
            Rule::Root => "ROOT",
            Rule::Expr => "EXPR",
            Rule::BinExpr => "BIN_EXPR",
            Rule::ParenExpr => "PAREN_EXPR",
        }
    }
}

pub fn expr_grammar() -> BuiltGraph {
    let mut g = GrammarBuilder::new();
    g.allow_rule_cycles(true);
    g.set_trivia_rule("ws");

    g.parser_rule("start", |g| {
        g.node(Rule::Root, |g| {
            g.call("expr");
            g.skip();
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

    g.parser_rule("expr", |g| {
        g.node(Rule::Expr, |g| {
            g.call("mul");
            g.zero_or_more(|g| {
                g.token(Lex::Plus, |g| {
                    g.byte(b'+');
                });
                g.call("mul");
            });
        });
    });

    g.parser_rule("mul", |g| {
        g.node(Rule::BinExpr, |g| {
            g.call("atom");
            g.zero_or_more(|g| {
                g.token(Lex::Star, |g| {
                    g.byte(b'*');
                });
                g.call("atom");
            });
        });
    });

    g.parser_rule("atom", |g| {
        g.choice(
            |g| {
                g.call("number");
            },
            |g| {
                g.node(Rule::ParenExpr, |g| {
                    g.token(Lex::LParen, |g| {
                        g.byte(b'(');
                    });
                    g.call("expr");
                    g.token(Lex::RParen, |g| {
                        g.byte(b')');
                    });
                });
            },
        );
    });

    g.finish().expect("expr grammar should build")
}
