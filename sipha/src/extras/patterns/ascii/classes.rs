//! Prebuilt byte character classes for common ASCII-ish grammar work.

use crate::types::CharClass;

pub const UNDERSCORE: CharClass = CharClass::from_byte(b'_');
pub const DASH: CharClass = CharClass::from_byte(b'-');
pub const DOT: CharClass = CharClass::from_byte(b'.');
pub const PLUS: CharClass = CharClass::from_byte(b'+');

pub const SP: CharClass = CharClass::from_byte(b' ');
pub const TAB: CharClass = CharClass::from_byte(b'\t');

pub const LF: CharClass = CharClass::from_byte(b'\n');
pub const CR: CharClass = CharClass::from_byte(b'\r');

/// Horizontal whitespace (space or tab): `[ \\t]`.
pub const HWS: CharClass = SP.union(TAB);

/// ASCII newline bytes (LF or CR): `[\\n\\r]`.
pub const NEWLINE: CharClass = LF.union(CR);

/// ASCII whitespace bytes: `[ \\t\\n\\r]`.
pub const WS: CharClass = HWS.union(NEWLINE);

pub const DIGIT: CharClass = CharClass::EMPTY.with_range(b'0', b'9');
pub const LOWER: CharClass = CharClass::EMPTY.with_range(b'a', b'z');
pub const UPPER: CharClass = CharClass::EMPTY.with_range(b'A', b'Z');
pub const ALPHA: CharClass = LOWER.union(UPPER);
pub const ALNUM: CharClass = ALPHA.union(DIGIT);

/// ASCII identifier start: `[A-Za-z_]`.
pub const IDENT_START: CharClass = ALPHA.union(UNDERSCORE);
/// ASCII identifier continuation: `[A-Za-z0-9_]`.
pub const IDENT_CONT: CharClass = ALNUM.union(UNDERSCORE);

pub const HEX_LOWER: CharClass = CharClass::EMPTY.with_range(b'a', b'f');
pub const HEX_UPPER: CharClass = CharClass::EMPTY.with_range(b'A', b'F');
pub const HEX_DIGIT: CharClass = DIGIT.union(HEX_LOWER).union(HEX_UPPER);

pub const OCT_DIGIT: CharClass = CharClass::EMPTY.with_range(b'0', b'7');
pub const BIN_DIGIT: CharClass = CharClass::from_byte(b'0').union(CharClass::from_byte(b'1'));

/// All bytes except LF (`0x0A`).
pub const NOT_LF: CharClass = CharClass::EMPTY
    .with_range(0, 9)
    .union(CharClass::EMPTY.with_range(11, 255));

/// All bytes except CR (`0x0D`).
pub const NOT_CR: CharClass = CharClass::EMPTY
    .with_range(0, 12)
    .union(CharClass::EMPTY.with_range(14, 255));

/// All bytes except ASCII newline (LF).
pub const NOT_NEWLINE: CharClass = NOT_LF;
