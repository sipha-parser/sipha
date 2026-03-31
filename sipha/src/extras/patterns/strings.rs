use crate::parse::builder::GrammarBuilder;

use super::ascii::classes as a;
use crate::types::CharClass;

/// A JSON double-quoted string *contents* (no surrounding quotes).
///
/// Matches:
/// - Any byte except `"` and `\\` and ASCII LF
/// - Or a JSON escape sequence (including `\\uXXXX`)
///
/// This is designed for use inside a larger rule like:
/// `g.delimited(|g| g.byte(b'\"'), |g| json_string_contents(g), |g| g.byte(b'\"'))`.
pub fn json_string_contents(g: &mut GrammarBuilder) {
    // This is a byte-level approximation intended for typical UTF-8 input.
    // It does not attempt to fully enforce JSON's "no control chars < 0x20" rule.
    let normal = CharClass::from_bytes(b"\"\\\n").complement();

    g.zero_or_more(|g| {
        g.choice(
            |g| {
                // normal char: not quote, not backslash, not LF
                g.class(normal);
            },
            |g| {
                // escape
                g.byte(b'\\');
                g.choice(
                    |g| {
                        g.class(crate::types::CharClass::from_bytes(b"\"\\/bfnrt"));
                    },
                    |g| {
                        g.byte(b'u');
                        // 4 hex digits
                        g.repeat(4, |g| {
                            g.class(a::HEX_DIGIT);
                        });
                    },
                );
            },
        );
    });
}
