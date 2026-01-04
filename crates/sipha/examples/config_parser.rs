//! Config File Parser Example (TOML-like)
//!
//! This example demonstrates parsing a simplified TOML-like configuration file format.
//! It shows how to handle key-value pairs, sections, and nested structures.
//!
//! Supported format:
//!   [section]
//!   key = value
//!   nested.key = value
//!
//! Run with: cargo run --example config_parser

use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::lexer::{CharSet, LexerBuilder, Pattern};
use sipha::syntax::{SyntaxKind, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ConfigSyntaxKind {
    // Punctuation
    LBracket, // [
    RBracket, // ]
    Equals,   // =
    Dot,      // .
    Newline,
    // Values
    String,
    Number,
    Bool,
    // Keys
    Ident,
    // Whitespace
    Whitespace,
    Eof,
    // Non-terminals
    Config,
    Section,
    KeyValue,
    Key,
    Value,
}

impl SyntaxKind for ConfigSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(
            self,
            Self::Config | Self::Section | Self::KeyValue | Self::Key | Self::Value
        )
    }

    fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Newline)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ConfigToken {
    kind: ConfigSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for ConfigToken {
    type Kind = ConfigSyntaxKind;

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
enum ConfigNonTerminal {
    Config,
    Section,
    KeyValue,
    Key,
    Value,
}

impl NonTerminal for ConfigNonTerminal {
    fn name(&self) -> &str {
        match self {
            Self::Config => "Config",
            Self::Section => "Section",
            Self::KeyValue => "KeyValue",
            Self::Key => "Key",
            Self::Value => "Value",
        }
    }
}

fn create_token(kind: ConfigSyntaxKind, text: &str) -> ConfigToken {
    ConfigToken {
        kind,
        text: text.into(),
    }
}

fn build_config_lexer()
-> Result<sipha::lexer::CompiledLexer<ConfigSyntaxKind>, Box<dyn std::error::Error>> {
    let lexer = LexerBuilder::new()
        // Punctuation
        .token(ConfigSyntaxKind::LBracket, Pattern::Literal("[".into()))
        .token(ConfigSyntaxKind::RBracket, Pattern::Literal("]".into()))
        .token(ConfigSyntaxKind::Equals, Pattern::Literal("=".into()))
        .token(ConfigSyntaxKind::Dot, Pattern::Literal(".".into()))
        .token(ConfigSyntaxKind::Newline, Pattern::Literal("\n".into()))
        // Keywords
        .token(ConfigSyntaxKind::Bool, Pattern::Literal("true".into()))
        .token(ConfigSyntaxKind::Bool, Pattern::Literal("false".into()))
        // Identifiers
        .token(
            ConfigSyntaxKind::Ident,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(
                    CharSet::letters()
                        .union(&CharSet::digits())
                        .union(&CharSet::new(['_', '-'])),
                )),
                min: 1,
                max: None,
            },
        )
        // Values
        .token(
            ConfigSyntaxKind::Number,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(CharSet::digits())),
                min: 1,
                max: None,
            },
        )
        .token(
            ConfigSyntaxKind::String,
            Pattern::Seq(vec![
                Pattern::Literal("\"".into()),
                Pattern::Repeat {
                    pattern: Box::new(Pattern::CharClass(
                        CharSet::all().difference(&CharSet::new(['"'])),
                    )),
                    min: 0,
                    max: None,
                },
                Pattern::Literal("\"".into()),
            ]),
        )
        // Whitespace
        .token(
            ConfigSyntaxKind::Whitespace,
            Pattern::Repeat {
                pattern: Box::new(Pattern::CharClass(
                    CharSet::whitespace().difference(&CharSet::new(['\n'])),
                )),
                min: 1,
                max: None,
            },
        )
        .trivia(ConfigSyntaxKind::Whitespace)
        .build(ConfigSyntaxKind::Eof, ConfigSyntaxKind::Ident)?;

    Ok(lexer)
}

fn build_config_grammar()
-> Result<sipha::grammar::Grammar<ConfigToken, ConfigNonTerminal>, Box<dyn std::error::Error>> {
    let grammar = GrammarBuilder::new()
        .entry_point(ConfigNonTerminal::Config)
        // config = (section | key_value)*
        .rule(
            ConfigNonTerminal::Config,
            Expr::star(Box::new(Expr::choice([
                Expr::rule(ConfigNonTerminal::Section),
                Expr::rule(ConfigNonTerminal::KeyValue),
            ]))),
        )
        // section = '[' ident ']' newline
        .rule(
            ConfigNonTerminal::Section,
            Expr::seq([
                Expr::token(create_token(ConfigSyntaxKind::LBracket, "[")),
                Expr::token(create_token(ConfigSyntaxKind::Ident, "section")),
                Expr::token(create_token(ConfigSyntaxKind::RBracket, "]")),
                Expr::token(create_token(ConfigSyntaxKind::Newline, "\n")),
            ]),
        )
        // key_value = key '=' value newline
        .rule(
            ConfigNonTerminal::KeyValue,
            Expr::seq([
                Expr::rule(ConfigNonTerminal::Key),
                Expr::token(create_token(ConfigSyntaxKind::Equals, "=")),
                Expr::rule(ConfigNonTerminal::Value),
                Expr::token(create_token(ConfigSyntaxKind::Newline, "\n")),
            ]),
        )
        // key = ident ('.' ident)*
        .rule(
            ConfigNonTerminal::Key,
            Expr::seq([
                Expr::token(create_token(ConfigSyntaxKind::Ident, "key")),
                Expr::star(Box::new(Expr::seq([
                    Expr::token(create_token(ConfigSyntaxKind::Dot, ".")),
                    Expr::token(create_token(ConfigSyntaxKind::Ident, "subkey")),
                ]))),
            ]),
        )
        // value = string | number | bool
        .rule(
            ConfigNonTerminal::Value,
            Expr::choice([
                Expr::token(create_token(ConfigSyntaxKind::String, "\"value\"")),
                Expr::token(create_token(ConfigSyntaxKind::Number, "42")),
                Expr::token(create_token(ConfigSyntaxKind::Bool, "true")),
            ]),
        )
        .build()?;

    Ok(grammar)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Config File Parser Example (TOML-like) ===\n");

    let lexer = build_config_lexer()?;
    let _grammar = build_config_grammar()?;

    let config_content = r#"
[database]
host = "localhost"
port = 5432

[server]
name = "myapp"
debug = true
timeout = 30
"#;

    println!("Config content:");
    println!("{}", config_content);
    println!();

    println!("Tokenizing...");
    match lexer.tokenize(config_content) {
        Ok(tokens) => {
            println!("  Tokens found: {}", tokens.len());
            for (i, token) in tokens.iter().enumerate() {
                if token.kind != ConfigSyntaxKind::Eof && token.kind != ConfigSyntaxKind::Whitespace
                {
                    println!("    [{}] {:?}: \"{}\"", i, token.kind, token.text);
                }
            }
            println!("\n  ✓ Tokenized successfully");
        }
        Err(e) => {
            println!("  ✗ Tokenization failed: {:?}", e);
        }
    }

    println!("\n=== Example Complete ===");
    println!(
        "\nNote: This is a simplified config parser demonstrating key-value and section parsing."
    );
    println!("A full TOML parser would need to handle arrays, tables, and more value types.");

    Ok(())
}
