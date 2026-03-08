/// Position in the input byte stream (supports up to 4 GiB files).
pub type Pos = u32;

/// Index into the instruction array.
pub type InsnId = u32;

/// Rule identifier (index into rule table).
pub type RuleId = u16;

/// Capture tag — identifies what kind of capture this is.
pub type Tag = u16;

/// Index into the grammar's `field_names` table; identifies a named child slot (e.g. "lhs", "rhs").
pub type FieldId = u16;

/// Stored discriminant for a syntax kind (used internally in green/red trees and events).
///
/// Grammar authors can use either raw `u16` values or an enum that implements
/// [`IntoSyntaxKind`] and [`FromSyntaxKind`], so you don't need to manage numbering manually.
/// Use the [`SyntaxKinds`](crate::SyntaxKinds) derive macro for an enum with automatic
/// discriminants 0, 1, 2, …
pub type SyntaxKind = u16;

/// Converts a value into the internal [`SyntaxKind`] discriminant.
///
/// Implement this for your enum (e.g. with `#[repr(u16)]`) so you can pass enum variants
/// to [`GrammarBuilder::node`], [`GrammarBuilder::token`], and [`GrammarBuilder::trivia`].
pub trait IntoSyntaxKind {
    fn into_syntax_kind(self) -> SyntaxKind;
}

impl IntoSyntaxKind for SyntaxKind {
    #[inline]
    fn into_syntax_kind(self) -> SyntaxKind {
        self
    }
}

/// Converts a stored [`SyntaxKind`] discriminant back into your type.
///
/// Implement this for your enum so you can use [`kind_as`](crate::red::SyntaxNode::kind_as) on
/// red/green nodes and tokens to get your enum variant instead of a raw `u16`.
pub trait FromSyntaxKind: Sized {
    fn from_syntax_kind(k: SyntaxKind) -> Option<Self>;
}

impl FromSyntaxKind for SyntaxKind {
    #[inline]
    fn from_syntax_kind(k: SyntaxKind) -> Option<Self> {
        Some(k)
    }
}

/// A half-open byte range [start, end) into the input.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    #[must_use]
    #[inline]
    pub const fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    #[must_use]
    #[inline]
    pub const fn len(self) -> usize {
        (self.end - self.start) as usize
    }

    #[must_use]
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    #[must_use]
    #[inline]
    pub fn as_slice(self, input: &[u8]) -> &[u8] {
        &input[self.start as usize..self.end as usize]
    }

    /// Returns true if the byte offset lies inside this span (start inclusive, end exclusive).
    #[must_use]
    #[inline]
    pub const fn contains_offset(self, offset: Pos) -> bool {
        offset >= self.start && offset < self.end
    }

    /// Returns true if `other` is entirely inside this span.
    #[must_use]
    #[inline]
    pub const fn contains_span(self, other: Self) -> bool {
        other.start >= self.start && other.end <= self.end
    }

    /// Merge two spans that touch or overlap into one span covering both.
    /// Returns `None` if the spans are disjoint (no overlap and not adjacent).
    #[must_use]
    #[inline]
    pub fn merge(self, other: Self) -> Option<Self> {
        if self.end >= other.start && other.end >= self.start {
            Some(Self::new(
                self.start.min(other.start),
                self.end.max(other.end),
            ))
        } else {
            None
        }
    }
}

/// Sort spans by start offset; overlapping or adjacent spans can then be merged with [`Span::merge`].
#[inline]
pub fn sort_spans(spans: &mut [Span]) {
    spans.sort_by_key(|s| s.start);
}

/// A 256-bit bitmap for O(1) byte-class membership tests.
///
/// Stored as four `u64` words. Bit `b` lives in word `b >> 6`, position `b & 63`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C, align(32))]
pub struct CharClass(pub [u64; 4]);

impl CharClass {
    pub const EMPTY: Self = Self([0; 4]);
    pub const ANY: Self = Self([u64::MAX; 4]);

    #[must_use]
    #[inline]
    pub const fn from_words(words: [u64; 4]) -> Self {
        Self(words)
    }

    #[must_use]
    #[inline]
    pub const fn contains(self, byte: u8) -> bool {
        let (word, bit) = (byte as usize >> 6, byte as usize & 63);
        (self.0[word] >> bit) & 1 != 0
    }

    #[must_use]
    #[inline]
    pub const fn with_byte(mut self, byte: u8) -> Self {
        let (word, bit) = ((byte >> 6) as usize, (byte & 63) as usize);
        self.0[word] |= 1u64 << bit;
        self
    }

    #[must_use]
    #[inline]
    pub const fn with_range(mut self, lo: u8, hi: u8) -> Self {
        let mut b = lo;
        loop {
            let (word, bit) = ((b >> 6) as usize, (b & 63) as usize);
            self.0[word] |= 1u64 << bit;
            if b == hi {
                break;
            }
            b += 1;
        }
        self
    }

    #[must_use]
    #[inline]
    pub const fn union(self, other: Self) -> Self {
        Self([
            self.0[0] | other.0[0],
            self.0[1] | other.0[1],
            self.0[2] | other.0[2],
            self.0[3] | other.0[3],
        ])
    }

    #[must_use]
    #[inline]
    pub const fn complement(self) -> Self {
        Self([!self.0[0], !self.0[1], !self.0[2], !self.0[3]])
    }

    #[must_use]
    pub const fn from_chars(chars: &[u8]) -> Self {
        let mut cls = Self::EMPTY;
        let mut i = 0;
        while i < chars.len() {
            cls = cls.with_byte(chars[i]);
            i += 1;
        }
        cls
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_len_and_slice() {
        let s = Span::new(1, 4);
        assert_eq!(s.len(), 3);
        assert!(!s.is_empty());
        let input = b"0123456789";
        assert_eq!(s.as_slice(input), b"123");
    }

    #[test]
    fn span_empty() {
        let s = Span::new(2, 2);
        assert_eq!(s.len(), 0);
        assert!(s.is_empty());
    }

    #[test]
    fn char_class_with_byte_and_contains() {
        let c = CharClass::EMPTY.with_byte(b'a').with_byte(b'z');
        assert!(c.contains(b'a'));
        assert!(c.contains(b'z'));
        assert!(!c.contains(b'm'));
    }

    #[test]
    fn char_class_with_range() {
        let c = CharClass::EMPTY.with_range(b'0', b'9');
        for b in b'0'..=b'9' {
            assert!(c.contains(b), "should contain {}", b as char);
        }
        assert!(!c.contains(b'/'));
        assert!(!c.contains(b':'));
    }

    #[test]
    fn char_class_union() {
        let a = CharClass::EMPTY.with_byte(b'a');
        let b = CharClass::EMPTY.with_byte(b'b');
        let u = a.union(b);
        assert!(u.contains(b'a'));
        assert!(u.contains(b'b'));
    }
}

/// Common pre-built character classes (all `const`).
pub mod classes {
    use super::CharClass;
    pub const DIGIT: CharClass = CharClass::EMPTY.with_range(b'0', b'9');
    pub const LOWER: CharClass = CharClass::EMPTY.with_range(b'a', b'z');
    pub const UPPER: CharClass = CharClass::EMPTY.with_range(b'A', b'Z');
    pub const ALPHA: CharClass = LOWER.union(UPPER);
    pub const ALNUM: CharClass = ALPHA.union(DIGIT);
    pub const IDENT_START: CharClass = ALPHA.union(CharClass::EMPTY.with_byte(b'_'));
    pub const IDENT_CONT: CharClass = ALNUM.union(CharClass::EMPTY.with_byte(b'_'));
    pub const WHITESPACE: CharClass = CharClass::from_chars(b" \t\n\r");
    pub const HEX_DIGIT: CharClass = DIGIT
        .union(CharClass::EMPTY.with_range(b'a', b'f'))
        .union(CharClass::EMPTY.with_range(b'A', b'F'));
    pub const PRINTABLE: CharClass = CharClass::EMPTY.with_range(0x20, 0x7E);
    /// All bytes except ASCII newline (`0x0A`). Useful for "rest of line" patterns.
    ///
    /// For a "not newline" constraint in a grammar you can also use
    /// [`neg_lookahead`](crate::builder::GrammarBuilder::neg_lookahead) with `byte(b'\n')`.
    pub const NOT_NEWLINE: CharClass = CharClass::EMPTY
        .with_range(0, 9)
        .union(CharClass::EMPTY.with_range(11, 255));
}

/// Flat capture event used during parsing before tree construction.
/// (Legacy system — use [`TreeEvent`] for the green/red tree.)
#[derive(Clone, Copy, Debug)]
pub enum CaptureEvent {
    Open { tag: Tag, pos: Pos },
    Close { tag: Tag, pos: Pos },
}

/// Events emitted by the VM for the green/red tree builder.
///
/// The grammar author emits these via `g.node()`, `g.token()`, `g.trivia()`.
/// They form a well-nested, complete cover of every annotated span.
/// After a successful parse, pass the event slice to
/// [`crate::green::build_green_tree`].
#[derive(Clone, Copy, Debug)]
pub enum TreeEvent {
    /// Open a syntax node.  Matched by the next balanced [`TreeEvent::NodeClose`].
    /// `field` labels this node as a named child of its parent (for [`crate::red::SyntaxNode::field_by_id`]).
    NodeOpen {
        kind: SyntaxKind,
        field: Option<FieldId>,
        pos: Pos,
    },
    /// Close the innermost open node.
    NodeClose { pos: Pos },
    /// A complete leaf token `[start, end)`.
    ///
    /// `is_trivia = true` for whitespace and comments — they are kept in the
    /// tree but skipped by semantic iterators.
    Token {
        kind: SyntaxKind,
        start: Pos,
        end: Pos,
        is_trivia: bool,
    },
}
