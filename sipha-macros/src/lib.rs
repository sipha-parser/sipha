//! # sipha-macros
//!
//! Procedural macros for defining [sipha] PEG grammars via a DSL.
//!
//! [sipha]: https://docs.rs/sipha

mod ast_enum;
mod ast_node;
mod ir;
mod lower;
mod lex_kinds;
mod parse;
mod rule_kinds;

use proc_macro::TokenStream;
use syn::parse_macro_input;

/// Expand a PEG grammar DSL into code that builds a sipha `BuiltGraph`.
///
/// The expansion **runs the builder each time the expression executes**. If the same DSL is used
/// in a hot path, build once and reuse — for example store the result in a
/// [`std::sync::OnceLock`] / [`std::sync::LazyLock`] or wrap it in `SharedGrammar` from the `sipha` crate.
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

/// Derive lexical discriminant encoding: [`IntoSyntaxKind`], [`FromSyntaxKind`], [`FromLexKind`].
///
/// The enum must have `#[repr(u16)]`. Only unit variants are supported. Implement
/// [`LexKind`](sipha::types::LexKind) separately (for example `display_name` → `as_str()`).
#[proc_macro_derive(LexKinds)]
pub fn lex_kinds_derive(input: TokenStream) -> TokenStream {
    lex_kinds::derive_lex_kinds(input)
}

/// Derive CST node discriminant encoding after a [`LexKinds`] enum.
///
/// Requires `#[sipha(lex = YourLexEnum)]` on this enum. Implement [`RuleKind`](sipha::types::RuleKind)
/// separately.
#[proc_macro_derive(RuleKinds, attributes(sipha))]
pub fn rule_kinds_derive(input: TokenStream) -> TokenStream {
    rule_kinds::derive_rule_kinds(input)
}

/// Derive `sipha::tree::ast::AstNode` for a tuple-struct wrapper around `SyntaxNode`.
///
/// Requires an attribute of the form `#[ast(kind = Kind::Foo)]` (or any expression that
/// evaluates to something implementing `IntoSyntaxKind`).
#[proc_macro_derive(AstNode, attributes(ast))]
pub fn ast_node_derive(input: TokenStream) -> TokenStream {
    ast_node::derive_ast_node(input)
}

/// Derive `sipha::tree::ast::AstNode` for an enum wrapper around other `AstNode` types.
///
/// Each variant must be a tuple variant with exactly one field (a typed CST wrapper).
/// Each variant requires `#[ast(kind = ...)]` mapping to the underlying syntax kind.
///
/// The derive also generates:
/// - `as_<variant>() -> Option<&Inner>`
/// - `into_<variant>(self) -> Option<Inner>`
/// - `From<Inner> for Enum` for each variant
#[proc_macro_derive(AstEnum, attributes(ast))]
pub fn ast_enum_derive(input: TokenStream) -> TokenStream {
    ast_enum::derive_ast_enum(input)
}
