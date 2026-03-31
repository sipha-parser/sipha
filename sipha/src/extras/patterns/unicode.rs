use crate::{parse::builder::GrammarBuilder, types::classes};

/// Match one Unicode whitespace codepoint (per Unicode `White_Space`).
///
/// Includes: ASCII space/tab/newlines, NBSP, OGHAM SPACE MARK, EN QUAD..HAIR SPACE,
/// LINE/PARAGRAPH SEPARATOR, NARROW NBSP, MEDIUM MATHEMATICAL SPACE, IDEOGRAPHIC SPACE,
/// and NEXT LINE (NEL).
///
/// This intentionally does **not** include U+FEFF (BOM), which is deprecated as whitespace.
pub fn whitespace_char(g: &mut GrammarBuilder) {
    crate::choices!(
        g,
        // ASCII whitespace via byte class.
        |g| {
            g.class(classes::WHITESPACE);
        },
        // NEL (U+0085)
        |g| {
            g.literal(b"\xC2\x85");
        },
        // NBSP (U+00A0)
        |g| {
            g.literal(b"\xC2\xA0");
        },
        // OGHAM SPACE MARK (U+1680)
        |g| {
            g.literal(b"\xE1\x9A\x80");
        },
        // EN QUAD..HAIR SPACE (U+2000..U+200A)
        |g| {
            g.literal(b"\xE2\x80\x80");
        }, // 2000
        |g| {
            g.literal(b"\xE2\x80\x81");
        }, // 2001
        |g| {
            g.literal(b"\xE2\x80\x82");
        }, // 2002
        |g| {
            g.literal(b"\xE2\x80\x83");
        }, // 2003
        |g| {
            g.literal(b"\xE2\x80\x84");
        }, // 2004
        |g| {
            g.literal(b"\xE2\x80\x85");
        }, // 2005
        |g| {
            g.literal(b"\xE2\x80\x86");
        }, // 2006
        |g| {
            g.literal(b"\xE2\x80\x87");
        }, // 2007
        |g| {
            g.literal(b"\xE2\x80\x88");
        }, // 2008
        |g| {
            g.literal(b"\xE2\x80\x89");
        }, // 2009
        |g| {
            g.literal(b"\xE2\x80\x8A");
        }, // 200A
        // LINE SEPARATOR / PARAGRAPH SEPARATOR (U+2028 / U+2029)
        |g| {
            g.literal(b"\xE2\x80\xA8");
        },
        |g| {
            g.literal(b"\xE2\x80\xA9");
        },
        // NARROW NO-BREAK SPACE (U+202F)
        |g| {
            g.literal(b"\xE2\x80\xAF");
        },
        // MEDIUM MATHEMATICAL SPACE (U+205F)
        |g| {
            g.literal(b"\xE2\x81\x9F");
        },
        // IDEOGRAPHIC SPACE (U+3000)
        |g| {
            g.literal(b"\xE3\x80\x80");
        },
    );
}

/// Match zero or more Unicode whitespace codepoints.
pub fn whitespace0(g: &mut GrammarBuilder) {
    g.zero_or_more(whitespace_char);
}

/// Match one or more Unicode whitespace codepoints.
pub fn whitespace1(g: &mut GrammarBuilder) {
    whitespace_char(g);
    whitespace0(g);
}

/// Match one Unicode newline sequence/codepoint commonly treated as a line break.
///
/// Matches: `\n`, `\r\n` (as a unit), `\r`, NEL, LS, PS.
pub fn newline(g: &mut GrammarBuilder) {
    crate::choices!(
        g,
        |g| {
            g.literal(b"\r\n");
        },
        |g| {
            g.byte(b'\n');
        },
        |g| {
            g.byte(b'\r');
        },
        |g| {
            g.literal(b"\xC2\x85");
        }, // NEL
        |g| {
            g.literal(b"\xE2\x80\xA8");
        }, // LS
        |g| {
            g.literal(b"\xE2\x80\xA9");
        }, // PS
    );
}
