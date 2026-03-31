use crate::parse::builder::GrammarBuilder;
use crate::types::CharClass;

use super::classes;

#[inline]
fn digits1(g: &mut GrammarBuilder, digit: CharClass, label: &str) {
    g.consume_while_class1_with_label(digit, label);
}

#[inline]
fn digits0_uscore(g: &mut GrammarBuilder, digit: CharClass, label: &str) {
    g.zero_or_more(|g| {
        g.choice(
            |g| {
                g.class_with_label(digit, label);
            },
            |g| {
                g.byte(b'_');
                g.class_with_label(digit, label);
            },
        );
    });
}

#[inline]
fn digits1_uscore(g: &mut GrammarBuilder, digit: CharClass, label: &str) {
    g.class_with_label(digit, label);
    digits0_uscore(g, digit, label);
}

/// ASCII decimal digits: `[0-9]+`.
#[inline]
pub fn dec_digits1(g: &mut GrammarBuilder) {
    digits1(g, classes::DIGIT, "digit");
}

/// ASCII hex digits: `[0-9a-fA-F]+`.
#[inline]
pub fn hex_digits1(g: &mut GrammarBuilder) {
    digits1(g, classes::HEX_DIGIT, "hex digit");
}

/// ASCII decimal digits with `_` separators: `[0-9] ([0-9] | '_' [0-9])*`.
///
/// Guarantees `_` only appears **between** digits.
#[inline]
pub fn dec_digits1_uscore(g: &mut GrammarBuilder) {
    digits1_uscore(g, classes::DIGIT, "digit");
}

/// ASCII hex digits with `_` separators: `[0-9a-fA-F] ([0-9a-fA-F] | '_' [0-9a-fA-F])*`.
#[inline]
pub fn hex_digits1_uscore(g: &mut GrammarBuilder) {
    digits1_uscore(g, classes::HEX_DIGIT, "hex digit");
}

/// ASCII binary digits: `[01]+`.
#[inline]
pub fn bin_digits1(g: &mut GrammarBuilder) {
    digits1(g, classes::BIN_DIGIT, "binary digit");
}

/// ASCII octal digits: `[0-7]+`.
#[inline]
pub fn oct_digits1(g: &mut GrammarBuilder) {
    digits1(g, classes::OCT_DIGIT, "octal digit");
}

/// ASCII binary digits with `_` separators.
#[inline]
pub fn bin_digits1_uscore(g: &mut GrammarBuilder) {
    digits1_uscore(g, classes::BIN_DIGIT, "binary digit");
}

/// ASCII octal digits with `_` separators.
#[inline]
pub fn oct_digits1_uscore(g: &mut GrammarBuilder) {
    digits1_uscore(g, classes::OCT_DIGIT, "octal digit");
}

/// A minimal JSON-style number:
/// `-? (0 | [1-9][0-9]*) ('.' [0-9]+)? ([eE] [+-]? [0-9]+)?`
///
/// This is byte-based (ASCII) and does not accept `_` separators.
pub fn number_json(g: &mut GrammarBuilder) {
    g.optional(|g| {
        g.byte(b'-');
    });

    g.choice(
        |g| {
            g.byte(b'0');
        },
        |g| {
            g.class(CharClass::EMPTY.with_range(b'1', b'9'));
            g.zero_or_more(|g| {
                g.class(classes::DIGIT);
            });
        },
    );

    g.optional(|g| {
        g.byte(b'.');
        dec_digits1(g);
    });

    g.optional(|g| {
        g.byte_either(b'e', b'E');
        g.optional(|g| {
            g.byte_either(b'+', b'-');
        });
        dec_digits1(g);
    });
}

/// Like [`number_json`], but allows `_` separators in the digit runs (non-standard JSON).
///
/// Useful for "code number" lexers.
pub fn number_json_uscore(g: &mut GrammarBuilder) {
    g.optional(|g| {
        g.byte(b'-');
    });

    g.choice(
        |g| {
            g.byte(b'0');
        },
        |g| {
            g.class(CharClass::EMPTY.with_range(b'1', b'9'));
            // Remaining integer digits, allowing `_` between digits.
            digits0_uscore(g, classes::DIGIT, "digit");
        },
    );

    g.optional(|g| {
        g.byte(b'.');
        dec_digits1_uscore(g);
    });

    g.optional(|g| {
        g.byte_either(b'e', b'E');
        g.optional(|g| {
            g.byte_either(b'+', b'-');
        });
        dec_digits1_uscore(g);
    });
}
