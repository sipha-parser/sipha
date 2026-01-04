//! SQL Parser Example (Simplified)
//!
//! This example demonstrates parsing a simplified SQL SELECT statement.
//! It shows how to handle complex grammar structures with multiple levels
//! of nesting and optional components.
//!
//! Supported SQL subset:
//!   SELECT column1, column2 FROM table WHERE condition
//!
//! Run with: cargo run --example sql_parser

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SqlSyntaxKind {
    // Keywords
    Select,
    From,
    Where,
    // Identifiers and literals
    Ident,
    Number,
    String,
    // Operators
    Eq,      // =
    Ne,      // !=
    Lt,      // <
    Gt,      // >
    And,
    Or,
    // Punctuation
    Comma,
    LParen,
    RParen,
    Whitespace,
    Eof,
    // Non-terminals
    SelectStmt,
    ColumnList,
    TableName,
    WhereClause,
    Condition,
    Expr,
}

impl SyntaxKind for SqlSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(
            self,
            Self::SelectStmt
                | Self::ColumnList
                | Self::TableName
                | Self::WhereClause
                | Self::Condition
                | Self::Expr
        )
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SqlToken {
    kind: SqlSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for SqlToken {
    type Kind = SqlSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(u32::MAX))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum SqlNonTerminal {
    SelectStmt,
    ColumnList,
    TableName,
    WhereClause,
    Condition,
    Expr,
}

impl NonTerminal for SqlNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::SelectStmt => "SelectStmt",
            Self::ColumnList => "ColumnList",
            Self::TableName => "TableName",
            Self::WhereClause => "WhereClause",
            Self::Condition => "Condition",
            Self::Expr => "Expr",
        }
    }
}

fn create_token(kind: SqlSyntaxKind, text: &str) -> SqlToken {
    SqlToken {
        kind,
        text: text.into(),
    }
}

fn build_sql_lexer() -> Result<sipha::lexer::CompiledLexer<SqlSyntaxKind>, Box<dyn std::error::Error>> {
    let lexer = LexerBuilder::new()
        // Keywords (must come before Ident to match first)
        .token(SqlSyntaxKind::Select, Pattern::Literal("SELECT".into()))
        .token(SqlSyntaxKind::From, Pattern::Literal("FROM".into()))
        .token(SqlSyntaxKind::Where, Pattern::Literal("WHERE".into()))
        .token(SqlSyntaxKind::And, Pattern::Literal("AND".into()))
        .token(SqlSyntaxKind::Or, Pattern::Literal("OR".into()))
        // Identifiers
        .token(
            SqlSyntaxKind::Ident,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(
                    CharSet::letters().union(&CharSet::digits()).union(&CharSet::new(['_'])),
                )),
                min: 1,
                max: None,
            },
        )
        // Literals
        .token(
            SqlSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(
            SqlSyntaxKind::String,
            Pattern::Seq(vec![
                Pattern::Literal("\"".into()),
                Pattern::Repeat {
                    pattern: Box::new(Pattern::CharClass(CharSet::all().difference(&CharSet::new(['"'])))),
                    min: 0,
                    max: None,
                },
                Pattern::Literal("\"".into()),
            ]),
        )
        // Operators
        .token(SqlSyntaxKind::Eq, Pattern::Literal("=".into()))
        .token(SqlSyntaxKind::Ne, Pattern::Literal("!=".into()))
        .token(SqlSyntaxKind::Lt, Pattern::Literal("<".into()))
        .token(SqlSyntaxKind::Gt, Pattern::Literal(">".into()))
        // Punctuation
        .token(SqlSyntaxKind::Comma, Pattern::Literal(",".into()))
        .token(SqlSyntaxKind::LParen, Pattern::Literal("(".into()))
        .token(SqlSyntaxKind::RParen, Pattern::Literal(")".into()))
        // Whitespace
        .token(
            SqlSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(SqlSyntaxKind::Whitespace)
        .build(SqlSyntaxKind::Eof, SqlSyntaxKind::Ident)?;

    Ok(lexer)
}

fn build_sql_grammar() -> Result<sipha::grammar::Grammar<SqlToken, SqlNonTerminal>, Box<dyn std::error::Error>> {
    let grammar = GrammarBuilder::new()
        .entry_point(SqlNonTerminal::SelectStmt)
        // SELECT column_list FROM table_name [WHERE condition]
        .rule(
            SqlNonTerminal::SelectStmt,
            Expr::seq([
                Expr::token(create_token(SqlSyntaxKind::Select, "SELECT")),
                Expr::rule(SqlNonTerminal::ColumnList),
                Expr::token(create_token(SqlSyntaxKind::From, "FROM")),
                Expr::rule(SqlNonTerminal::TableName),
                Expr::opt(Box::new(Expr::rule(SqlNonTerminal::WhereClause))),
            ]),
        )
        // column_list = ident (',' ident)*
        .rule(
            SqlNonTerminal::ColumnList,
            Expr::seq([
                Expr::token(create_token(SqlSyntaxKind::Ident, "id")),
                Expr::star(Box::new(Expr::seq([
                    Expr::token(create_token(SqlSyntaxKind::Comma, ",")),
                    Expr::token(create_token(SqlSyntaxKind::Ident, "id")),
                ]))),
            ]),
        )
        // table_name = ident
        .rule(
            SqlNonTerminal::TableName,
            Expr::token(create_token(SqlSyntaxKind::Ident, "table")),
        )
        // WHERE condition
        .rule(
            SqlNonTerminal::WhereClause,
            Expr::seq([
                Expr::token(create_token(SqlSyntaxKind::Where, "WHERE")),
                Expr::rule(SqlNonTerminal::Condition),
            ]),
        )
        // condition = expr op expr [AND|OR condition]
        .rule(
            SqlNonTerminal::Condition,
            Expr::seq([
                Expr::rule(SqlNonTerminal::Expr),
                Expr::choice([
                    Expr::token(create_token(SqlSyntaxKind::Eq, "=")),
                    Expr::token(create_token(SqlSyntaxKind::Ne, "!=")),
                    Expr::token(create_token(SqlSyntaxKind::Lt, "<")),
                    Expr::token(create_token(SqlSyntaxKind::Gt, ">")),
                ]),
                Expr::rule(SqlNonTerminal::Expr),
                Expr::opt(Box::new(Expr::seq([
                    Expr::choice([
                        Expr::token(create_token(SqlSyntaxKind::And, "AND")),
                        Expr::token(create_token(SqlSyntaxKind::Or, "OR")),
                    ]),
                    Expr::rule(SqlNonTerminal::Condition),
                ]))),
            ]),
        )
        // expr = ident | number | string
        .rule(
            SqlNonTerminal::Expr,
            Expr::choice([
                Expr::token(create_token(SqlSyntaxKind::Ident, "id")),
                Expr::token(create_token(SqlSyntaxKind::Number, "1")),
                Expr::token(create_token(SqlSyntaxKind::String, "\"value\"")),
            ]),
        )
        .build()?;

    Ok(grammar)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== SQL Parser Example (Simplified) ===\n");

    let lexer = build_sql_lexer()?;
    let grammar = build_sql_grammar()?;

    let test_queries = vec![
        "SELECT id, name FROM users",
        "SELECT * FROM products WHERE price > 100",
        "SELECT name FROM users WHERE id = 1 AND active = true",
    ];

    for query in test_queries {
        println!("Query: {}", query);
        match lexer.tokenize(query) {
            Ok(tokens) => {
                println!("  Tokens: {}", tokens.len());
                for token in &tokens {
                    if token.kind != SqlSyntaxKind::Eof && token.kind != SqlSyntaxKind::Whitespace {
                        print!("    {:?} ", token.kind);
                    }
                }
                println!();
                println!("  ✓ Tokenized successfully");
            }
            Err(e) => {
                println!("  ✗ Tokenization failed: {:?}", e);
            }
        }
        println!();
    }

    println!("=== Example Complete ===");
    println!("\nNote: This is a simplified SQL parser demonstrating grammar structure.");
    println!("A full SQL parser would need to handle many more constructs.");

    Ok(())
}

