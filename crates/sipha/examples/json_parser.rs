//! JSON Parser Example
//!
//! This example demonstrates parsing JSON using Sipha's Grammar DSL.
//! It shows how to build a complete parser for a real-world format.

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    Comma,
    Colon,
    Whitespace,
    Eof,
    // Non-terminals
    Value,
    Object,
    Array,
    Pair,
    Pairs,
    Elements,
}

impl SyntaxKind for JsonSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(
            self,
            Self::Value | Self::Object | Self::Array | Self::Pair | Self::Pairs | Self::Elements
        )
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct JsonToken {
    kind: JsonSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for JsonToken {
    type Kind = JsonSyntaxKind;

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
enum JsonNonTerminal {
    Value,
    Object,
    Array,
    Pair,
    Pairs,
    Elements,
}

impl NonTerminal for JsonNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Value => "Value",
            Self::Object => "Object",
            Self::Array => "Array",
            Self::Pair => "Pair",
            Self::Pairs => "Pairs",
            Self::Elements => "Elements",
        }
    }
}

fn create_token(kind: JsonSyntaxKind, text: &str) -> JsonToken {
    JsonToken {
        kind,
        text: text.into(),
    }
}

fn build_json_lexer()
-> Result<sipha::lexer::CompiledLexer<JsonSyntaxKind>, Box<dyn std::error::Error>> {
    Ok(LexerBuilder::new()
        .token(
            JsonSyntaxKind::String,
            Pattern::Regex(r#""([^"\\]|\\.)*""#.into()),
        )
        .token(
            JsonSyntaxKind::Number,
            Pattern::Regex(r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?".into()),
        )
        .token(JsonSyntaxKind::True, Pattern::Literal("true".into()))
        .token(JsonSyntaxKind::False, Pattern::Literal("false".into()))
        .token(JsonSyntaxKind::Null, Pattern::Literal("null".into()))
        .token(JsonSyntaxKind::LBrace, Pattern::Literal("{".into()))
        .token(JsonSyntaxKind::RBrace, Pattern::Literal("}".into()))
        .token(JsonSyntaxKind::LBracket, Pattern::Literal("[".into()))
        .token(JsonSyntaxKind::RBracket, Pattern::Literal("]".into()))
        .token(JsonSyntaxKind::Comma, Pattern::Literal(",".into()))
        .token(JsonSyntaxKind::Colon, Pattern::Literal(":".into()))
        .token(
            JsonSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::whitespace())),
                min: 1,
                max: None,
            },
        )
        .trivia(JsonSyntaxKind::Whitespace)
        .build(JsonSyntaxKind::Eof, JsonSyntaxKind::String)?)
}

fn build_json_grammar()
-> Result<sipha::grammar::Grammar<JsonToken, JsonNonTerminal>, Box<dyn std::error::Error>> {
    // JSON grammar:
    // Value -> String | Number | Object | Array | True | False | Null
    // Object -> { Pairs? }
    // Array -> [ Elements? ]
    // Pairs -> Pair ( , Pair )*
    // Elements -> Value ( , Value )*
    // Pair -> String : Value

    GrammarBuilder::new()
        .entry_point(JsonNonTerminal::Value)
        // Value -> String | Number | Object | Array | True | False | Null
        .rule(
            JsonNonTerminal::Value,
            Expr::Choice(vec![
                Expr::token(create_token(JsonSyntaxKind::String, "")),
                Expr::token(create_token(JsonSyntaxKind::Number, "")),
                Expr::rule(JsonNonTerminal::Object),
                Expr::rule(JsonNonTerminal::Array),
                Expr::token(create_token(JsonSyntaxKind::True, "true")),
                Expr::token(create_token(JsonSyntaxKind::False, "false")),
                Expr::token(create_token(JsonSyntaxKind::Null, "null")),
            ]),
        )
        // Object -> { Pairs? }
        .rule(
            JsonNonTerminal::Object,
            Expr::Seq(vec![
                Expr::token(create_token(JsonSyntaxKind::LBrace, "{")),
                Expr::Opt(Box::new(Expr::rule(JsonNonTerminal::Pairs))),
                Expr::token(create_token(JsonSyntaxKind::RBrace, "}")),
            ]),
        )
        // Array -> [ Elements? ]
        .rule(
            JsonNonTerminal::Array,
            Expr::Seq(vec![
                Expr::token(create_token(JsonSyntaxKind::LBracket, "[")),
                Expr::Opt(Box::new(Expr::rule(JsonNonTerminal::Elements))),
                Expr::token(create_token(JsonSyntaxKind::RBracket, "]")),
            ]),
        )
        // Pairs -> Pair ( , Pair )*
        .rule(
            JsonNonTerminal::Pairs,
            Expr::Seq(vec![
                Expr::rule(JsonNonTerminal::Pair),
                Expr::Repeat {
                    expr: Box::new(Expr::Seq(vec![
                        Expr::token(create_token(JsonSyntaxKind::Comma, ",")),
                        Expr::rule(JsonNonTerminal::Pair),
                    ])),
                    min: 0,
                    max: None,
                    greedy: true,
                },
            ]),
        )
        // Elements -> Value ( , Value )*
        .rule(
            JsonNonTerminal::Elements,
            Expr::Seq(vec![
                Expr::rule(JsonNonTerminal::Value),
                Expr::Repeat {
                    expr: Box::new(Expr::Seq(vec![
                        Expr::token(create_token(JsonSyntaxKind::Comma, ",")),
                        Expr::rule(JsonNonTerminal::Value),
                    ])),
                    min: 0,
                    max: None,
                    greedy: true,
                },
            ]),
        )
        // Pair -> String : Value
        .rule(
            JsonNonTerminal::Pair,
            Expr::Seq(vec![
                Expr::token(create_token(JsonSyntaxKind::String, "")),
                Expr::token(create_token(JsonSyntaxKind::Colon, ":")),
                Expr::rule(JsonNonTerminal::Value),
            ]),
        )
        .build()
        .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== JSON Parser Example ===\n");

    // Build lexer
    println!("1. Building JSON lexer...");
    let lexer = build_json_lexer()?;
    println!("   ✓ Lexer built successfully\n");

    // Build grammar
    println!("2. Building JSON grammar...");
    let grammar = build_json_grammar()?;
    println!("   ✓ Grammar built successfully");
    println!("   Rules: {}", grammar.rules().count());
    println!();

    // Test cases
    let test_cases = vec![
        ("42", "number"),
        (r#""hello""#, "string"),
        ("true", "boolean"),
        ("false", "boolean"),
        ("null", "null"),
        (r#"{"key": "value"}"#, "object"),
        (r#"[1, 2, 3]"#, "array"),
        (
            r#"{"name": "John", "age": 30}"#,
            "object with multiple pairs",
        ),
    ];

    println!("3. Testing JSON parsing:");
    for (input, description) in test_cases {
        println!("   Testing: {} ({})", description, input);
        match lexer.tokenize(input) {
            Ok(tokens) => {
                println!("     ✓ Tokenized ({} tokens)", tokens.len());
                // In a real implementation, you would parse here
                // let result = parser.parse(&tokens, JsonNonTerminal::Value);
            }
            Err(e) => {
                println!("     ✗ Tokenization failed: {:?}", e);
            }
        }
    }

    println!();
    println!("=== Example completed! ===");
    println!();
    println!("Note: This example demonstrates the lexer and grammar setup.");
    println!("To complete the parser, you would:");
    println!("  1. Create a parser backend (LL, LR, GLR, or PEG)");
    println!("  2. Parse the tokens using the grammar");
    println!("  3. Build a syntax tree from the parse result");
    println!("  4. Optionally, build an AST from the syntax tree");

    Ok(())
}
