//! # sipha-macros
//!
//! Procedural macros for defining [sipha] PEG grammars via a DSL.
//!
//! [sipha]: https://docs.rs/sipha

mod ir;
mod lower;
mod parse;
mod syntax_kinds;

use proc_macro::TokenStream;
use syn::parse_macro_input;

/// Expand a PEG grammar DSL into code that builds a sipha `BuiltGraph`.
///
/// ## Directives
///
/// - `@trivia rule_name;` — set the trivia rule (whitespace/comments).
/// - `@start rule_name;` — set the start rule (will get `end_of_input` and `accept`).
///
/// ## Rules
///
/// - `#[parser] name = expr;` — parser rule (auto trivia skip before calls/tokens).
/// - `#[lexer] name = expr;` — lexer rule (no trivia skip).
/// - `name = expr;` — neutral rule (inherits context).
///
/// ## Expression syntax
///
/// - `a | b` — choice (try `a`, then `b` on failure).
/// - `a b` — sequence.
/// - `e?` — optional (zero or one).
/// - `e*` — zero or more.
/// - `e+` — one or more.
/// - `&e` — positive lookahead (match without consuming).
/// - `!e` — negative lookahead.
/// - `( e )` — grouping.
/// - `"literal"` — byte literal.
/// - `rulename` — call another rule.
/// - `#[node(KIND)] e` — wrap in a syntax node.
/// - `#[token(KIND)] e` — wrap in a token.
/// - `#[trivia(KIND)] e` — wrap in a trivia token.
/// - `#[capture(TAG)] e` — wrap in a legacy capture.
#[proc_macro]
pub fn sipha_grammar(input: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(input as parse::Grammar);
    lower::lower_grammar(&grammar).into()
}

/// Derive [`IntoSyntaxKind`] and [`FromSyntaxKind`] for an enum of syntax kinds.
///
/// The enum must have `#[repr(u16)]`. Only unit variants are supported.
#[proc_macro_derive(SyntaxKinds)]
pub fn syntax_kinds_derive(input: TokenStream) -> TokenStream {
    syntax_kinds::derive_syntax_kinds(input)
}
