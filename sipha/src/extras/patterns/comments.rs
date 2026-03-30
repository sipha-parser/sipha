use crate::parse::builder::GrammarBuilder;

use super::ascii::classes as ascii_classes;

/// Line comment with a fixed byte prefix, consuming until newline or end-of-input.
///
/// The newline itself is not consumed.
pub fn line_comment_prefix(g: &mut GrammarBuilder, prefix: &'static [u8]) {
    g.literal(prefix);
    g.zero_or_more(|g| {
        g.class(ascii_classes::NOT_NEWLINE);
    });
}

/// `// ...` (until a newline or end-of-input), not including the newline.
///
/// Useful inside a `lexer_rule` for line comments.
pub fn line_comment_slash_slash(g: &mut GrammarBuilder) {
    line_comment_prefix(g, b"//");
}

/// `# ...` (until a newline or end-of-input), not including the newline.
pub fn line_comment_hash(g: &mut GrammarBuilder) {
    line_comment_prefix(g, b"#");
}

/// Lua-style line comment: `-- ...` (until newline or end-of-input).
pub fn line_comment_lua_double_dash(g: &mut GrammarBuilder) {
    line_comment_prefix(g, b"--");
}

