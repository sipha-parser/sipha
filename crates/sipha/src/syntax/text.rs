#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
use std::fmt;

/// Text size in bytes (UTF-8)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct TextSize(u32);

/// Text range representing a span of text
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct TextRange {
    start: TextSize,
    end: TextSize,
}

impl TextSize {
    #[must_use]
    pub const fn from(offset: u32) -> Self {
        Self(offset)
    }

    #[must_use]
    pub const fn into(self) -> u32 {
        self.0
    }

    #[must_use]
    pub const fn zero() -> Self {
        Self(0)
    }
}

impl std::ops::Add<Self> for TextSize {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl std::ops::AddAssign<Self> for TextSize {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl TextRange {
    #[must_use]
    pub const fn new(start: TextSize, end: TextSize) -> Self {
        Self { start, end }
    }

    #[must_use]
    pub const fn at(start: TextSize, len: TextSize) -> Self {
        Self::new(start, TextSize(start.0 + len.0))
    }

    #[must_use]
    pub const fn start(self) -> TextSize {
        self.start
    }

    #[must_use]
    pub const fn end(self) -> TextSize {
        self.end
    }

    #[must_use]
    pub const fn len(self) -> TextSize {
        TextSize(self.end.0 - self.start.0)
    }

    #[must_use]
    pub const fn contains(self, offset: TextSize) -> bool {
        offset.0 >= self.start.0 && offset.0 < self.end.0
    }

    #[must_use]
    pub const fn contains_range(self, other: Self) -> bool {
        other.start.0 >= self.start.0 && other.end.0 <= self.end.0
    }

    #[must_use]
    pub fn intersect(self, other: Self) -> Option<Self> {
        let start = self.start.0.max(other.start.0);
        let end = self.end.0.min(other.end.0);

        if start < end {
            Some(Self::new(TextSize(start), TextSize(end)))
        } else {
            None
        }
    }
}

impl fmt::Display for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start.0, self.end.0)
    }
}

#[cfg(feature = "diagnostics")]
impl From<TextRange> for miette::SourceSpan {
    fn from(range: TextRange) -> Self {
        use miette::SourceOffset;
        Self::new(
            SourceOffset::from(range.start().into() as usize),
            range.len().into() as usize,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_size_from_into() {
        let size = TextSize::from(42);
        assert_eq!(size.into(), 42);
    }

    #[test]
    fn test_text_size_zero() {
        let zero = TextSize::zero();
        assert_eq!(zero.into(), 0);
    }

    #[test]
    fn test_text_size_add() {
        let a = TextSize::from(10);
        let b = TextSize::from(20);
        let sum = a + b;
        assert_eq!(sum.into(), 30);
    }

    #[test]
    fn test_text_size_add_assign() {
        let mut a = TextSize::from(10);
        a += TextSize::from(5);
        assert_eq!(a.into(), 15);
    }

    #[test]
    fn test_text_range_new() {
        let start = TextSize::from(10);
        let end = TextSize::from(20);
        let range = TextRange::new(start, end);
        assert_eq!(range.start(), start);
        assert_eq!(range.end(), end);
    }

    #[test]
    fn test_text_range_at() {
        let start = TextSize::from(10);
        let len = TextSize::from(5);
        let range = TextRange::at(start, len);
        assert_eq!(range.start(), start);
        assert_eq!(range.end(), TextSize::from(15));
    }

    #[test]
    fn test_text_range_len() {
        let range = TextRange::new(TextSize::from(10), TextSize::from(25));
        assert_eq!(range.len(), TextSize::from(15));
    }

    #[test]
    fn test_text_range_contains() {
        let range = TextRange::new(TextSize::from(10), TextSize::from(20));

        assert!(!range.contains(TextSize::from(9))); // Before start
        assert!(range.contains(TextSize::from(10))); // At start
        assert!(range.contains(TextSize::from(15))); // In middle
        assert!(!range.contains(TextSize::from(20))); // At end (exclusive)
        assert!(!range.contains(TextSize::from(21))); // After end
    }

    #[test]
    fn test_text_range_contains_range() {
        let outer = TextRange::new(TextSize::from(10), TextSize::from(30));
        let inner = TextRange::new(TextSize::from(15), TextSize::from(25));
        let overlapping = TextRange::new(TextSize::from(5), TextSize::from(15));
        let outside = TextRange::new(TextSize::from(35), TextSize::from(40));

        assert!(outer.contains_range(inner));
        assert!(!outer.contains_range(overlapping));
        assert!(!outer.contains_range(outside));
        assert!(outer.contains_range(outer)); // Range contains itself
    }

    #[test]
    fn test_text_range_intersect() {
        let range1 = TextRange::new(TextSize::from(10), TextSize::from(20));
        let range2 = TextRange::new(TextSize::from(15), TextSize::from(25));
        let range3 = TextRange::new(TextSize::from(5), TextSize::from(8));

        let intersection = range1.intersect(range2);
        assert!(intersection.is_some());
        let inter = intersection.unwrap();
        assert_eq!(inter.start(), TextSize::from(15));
        assert_eq!(inter.end(), TextSize::from(20));

        assert!(range1.intersect(range3).is_none()); // No overlap
    }

    #[test]
    fn test_text_range_intersect_adjacent() {
        let range1 = TextRange::new(TextSize::from(10), TextSize::from(20));
        let range2 = TextRange::new(TextSize::from(20), TextSize::from(30));

        // Adjacent ranges don't intersect (end is exclusive)
        assert!(range1.intersect(range2).is_none());
    }

    #[test]
    fn test_text_range_display() {
        let range = TextRange::new(TextSize::from(10), TextSize::from(20));
        let display = format!("{range}");
        assert_eq!(display, "10..20");
    }

    #[test]
    fn test_text_range_ordering() {
        let range1 = TextRange::new(TextSize::from(10), TextSize::from(20));
        let range2 = TextRange::new(TextSize::from(10), TextSize::from(20));
        let range3 = TextRange::new(TextSize::from(10), TextSize::from(21));

        assert_eq!(range1, range2);
        assert_ne!(range1, range3);
    }
}
