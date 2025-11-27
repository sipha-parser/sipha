//! Example: JSON Grammar Documentation
//!
//! This example demonstrates how to create a JSON grammar with documentation
//! and generate markdown documentation from it.

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token, TrailingSeparator};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum JsonSyntaxKind {
    // Terminals
    String,
    Number,
    True,
    False,
    Null,
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
    Colon,    // :
    Comma,    // ,
    Whitespace,
    Eof,
    // Non-terminals
    Json,
    Value,
    Object,
    Array,
    Pair,
}

impl SyntaxKind for JsonSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(
            self,
            Self::Json | Self::Value | Self::Object | Self::Array | Self::Pair
        )
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

// Use the lexer's Token type which implements grammar::Token
use sipha::lexer::Token as LexerToken;
type JsonToken = LexerToken<JsonSyntaxKind>;

fn create_token(kind: JsonSyntaxKind, text: &str) -> JsonToken {
    use sipha::syntax::TextRange;
    JsonToken::new(
        kind,
        text,
        TextRange::new(
            TextSize::zero(),
            TextSize::from(u32::try_from(text.len()).unwrap_or(0)),
        ),
    )
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum JsonNonTerminal {
    Json,
    Value,
    Object,
    Array,
    Pair,
}

impl NonTerminal for JsonNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Json => "Json",
            Self::Value => "Value",
            Self::Object => "Object",
            Self::Array => "Array",
            Self::Pair => "Pair",
        }
    }
}

#[allow(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Build lexer for JSON
    let lexer = LexerBuilder::new()
        // JSON string: "..." with escape sequences
        .token(
            JsonSyntaxKind::String,
            Pattern::Regex(r#""([^"\\]|\\.)*""#.into()),
        )
        // JSON number: -?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?
        .token(
            JsonSyntaxKind::Number,
            Pattern::Regex(r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?".into()),
        )
        // Keywords
        .keyword("true", JsonSyntaxKind::True)
        .keyword("false", JsonSyntaxKind::False)
        .keyword("null", JsonSyntaxKind::Null)
        // Punctuation
        .token(JsonSyntaxKind::LBrace, Pattern::Literal("{".into()))
        .token(JsonSyntaxKind::RBrace, Pattern::Literal("}".into()))
        .token(JsonSyntaxKind::LBracket, Pattern::Literal("[".into()))
        .token(JsonSyntaxKind::RBracket, Pattern::Literal("]".into()))
        .token(JsonSyntaxKind::Colon, Pattern::Literal(":".into()))
        .token(JsonSyntaxKind::Comma, Pattern::Literal(",".into()))
        // Whitespace
        .token(
            JsonSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(JsonSyntaxKind::Whitespace)
        .build(JsonSyntaxKind::Eof, JsonSyntaxKind::String)?;

    // Test the lexer with some JSON examples
    println!("=== Testing JSON Lexer ===\n");
    let test_cases = vec![
        r#"{"name": "John", "age": 30, "active": true}"#,
        r"[1, 2, 3.14, -42, 1e10]",
        r#"{"nested": {"key": "value"}, "array": [true, false, null]}"#,
    ];

    for test_json in test_cases {
        println!("Input: {test_json}");
        match lexer.tokenize(test_json) {
            Ok(tokens) => {
                println!("  Tokens: {}", tokens.len());
                for token in &tokens {
                    if token.kind != JsonSyntaxKind::Eof {
                        println!("    {:?}: \"{}\"", token.kind, token.text);
                    }
                }
            }
            Err(e) => {
                eprintln!("  Lexer error: {e:?}");
            }
        }
        println!();
    }
    println!("{}", "=".repeat(80));
    println!();

    // Build the JSON grammar with documentation
    // Note: We use representative tokens for the grammar definition
    // The actual tokens from the lexer will match by kind
    let grammar = GrammarBuilder::new()
        .entry_point(JsonNonTerminal::Json)
        // JSON document: a single value
        .rule_with_description(
            JsonNonTerminal::Json,
            Expr::rule(JsonNonTerminal::Value),
            "A JSON document consists of a single value (object, array, string, number, boolean, or null).".to_string(),
        )
        // Value: can be any JSON value type
        .rule_with_description(
            JsonNonTerminal::Value,
            Expr::Choice(vec![
                Expr::rule(JsonNonTerminal::Object),
                Expr::rule(JsonNonTerminal::Array),
                Expr::token(create_token(JsonSyntaxKind::String, "\"string\"")),
                Expr::token(create_token(JsonSyntaxKind::Number, "123")),
                Expr::token(create_token(JsonSyntaxKind::True, "true")),
                Expr::token(create_token(JsonSyntaxKind::False, "false")),
                Expr::token(create_token(JsonSyntaxKind::Null, "null")),
            ]),
            "A JSON value can be an object, array, string, number, boolean (true/false), or null.".to_string(),
        )
        // Object: { "key": value, ... }
        .rule_with_description(
            JsonNonTerminal::Object,
            Expr::Delimited {
                open: Box::new(Expr::token(create_token(JsonSyntaxKind::LBrace, "{"))),
                content: Box::new(Expr::Opt(Box::new(Expr::Separated {
                    item: Box::new(Expr::rule(JsonNonTerminal::Pair)),
                    separator: Box::new(Expr::token(create_token(JsonSyntaxKind::Comma, ","))),
                    min: 0,
                    trailing: TrailingSeparator::Forbid,
                }))),
                close: Box::new(Expr::token(create_token(JsonSyntaxKind::RBrace, "}"))),
                recover: true,
            },
            "A JSON object is a collection of key-value pairs enclosed in curly braces.".to_string(),
        )
        // Array: [ value, ... ]
        .rule_with_description(
            JsonNonTerminal::Array,
            Expr::Delimited {
                open: Box::new(Expr::token(create_token(JsonSyntaxKind::LBracket, "["))),
                content: Box::new(Expr::Opt(Box::new(Expr::Separated {
                    item: Box::new(Expr::rule(JsonNonTerminal::Value)),
                    separator: Box::new(Expr::token(create_token(JsonSyntaxKind::Comma, ","))),
                    min: 0,
                    trailing: TrailingSeparator::Forbid,
                }))),
                close: Box::new(Expr::token(create_token(JsonSyntaxKind::RBracket, "]"))),
                recover: true,
            },
            "A JSON array is an ordered list of values enclosed in square brackets.".to_string(),
        )
        // Pair: "key": value
        .rule_with_description(
            JsonNonTerminal::Pair,
            Expr::Seq(vec![
                Expr::token(create_token(JsonSyntaxKind::String, "\"key\"")),
                Expr::token(create_token(JsonSyntaxKind::Colon, ":")),
                Expr::rule(JsonNonTerminal::Value),
            ]),
            "A key-value pair consists of a string key, a colon, and a value.".to_string(),
        )
        // Token descriptions
        .token_description(
            create_token(JsonSyntaxKind::String, "\"string\""),
            "A JSON string literal, enclosed in double quotes".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::Number, "123"),
            "A JSON number (integer or floating-point)".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::True, "true"),
            "The boolean value true".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::False, "false"),
            "The boolean value false".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::Null, "null"),
            "The null value".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::LBrace, "{"),
            "Left brace, opens an object".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::RBrace, "}"),
            "Right brace, closes an object".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::LBracket, "["),
            "Left bracket, opens an array".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::RBracket, "]"),
            "Right bracket, closes an array".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::Colon, ":"),
            "Colon, separates key from value in a pair".to_string(),
        )
        .token_description(
            create_token(JsonSyntaxKind::Comma, ","),
            "Comma, separates elements in objects and arrays".to_string(),
        )
        .build()?;

    // Generate markdown documentation
    let markdown = grammar.to_markdown(&|token: &JsonToken| format!("{:?}", token.kind()));

    // Write to file
    let output_path = "json_grammar_docs.md";
    fs::write(output_path, &markdown)?;
    println!("✅ Grammar documentation written to: {output_path}");
    println!("\nPreview:");
    println!("{}", "=".repeat(80));

    // Print first 50 lines as preview
    let preview: String = markdown.lines().take(50).collect::<Vec<_>>().join("\n");
    println!("{preview}");
    if markdown.lines().count() > 50 {
        println!(
            "\n... ({} more lines, see {} for full output)",
            markdown.lines().count() - 50,
            output_path
        );
    }

    Ok(())
}
