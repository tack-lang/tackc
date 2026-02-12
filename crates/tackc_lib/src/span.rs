//! Span library based heavily off [`text-span`](https://crates.io/crates/text-span).

use std::{cmp::Ordering, ops::Range};

use serde::{Deserialize, Serialize};

use crate::utils::UnwrapExt;

/// The value used as an index in the [`Span`] type.
pub type SpanValue = u32;

/// The `Span` type represents an area of a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct Span {
    /// The start of the `Span` (Inclusive).
    pub start: SpanValue,
    /// The end of the `Span` (Exclusive).
    pub end: SpanValue,
}

impl Span {
    /// Creates a new `Span`. This span will start and end at the 0th character, making it have a length of zero.
    pub fn new() -> Self {
        Self::new_from(0, 0)
    }

    /// Creates a new `Span` from a pair of start and end indexes.
    ///
    /// # Panics
    /// Panics if start is greater than end, since spans can't have a negative length.
    pub fn new_from(start: SpanValue, end: SpanValue) -> Self {
        assert!(end >= start, "cannot create negative-size span");

        Self { start, end }
    }

    /// Creates a new `Span` pointing to the end of a string.
    ///
    /// # Panics
    /// This function will panic if the input string's length is greater than [`SpanValue::MAX`].
    pub fn eof(string: &str) -> Self {
        assert!(
            string.len() < SpanValue::MAX as usize,
            "Length of `Span::eof` input must be less than `SpanValue::MAX!`"
        );

        Self {
            // Since `string.len() < SpanValue::MAX`, try_into() will return `Ok`.
            start: string.len().try_into().expect_unreachable(), // CHECKED(Chloe)
            end: string.len().try_into().expect_unreachable(),   // CHECKED(Chloe)
        }
    }

    /// Creates a new `Span` pointing to an entire string.
    ///
    /// # Panics
    /// This function will panic if the input string's length is greater than [`SpanValue::MAX`].
    pub fn full(string: &str) -> Self {
        assert!(
            string.len() < SpanValue::MAX as usize,
            "Length of `Span::full` input must be less than `SpanValue::MAX!`"
        );

        Self {
            start: 0,
            // Since `string.len() < SpanValue::MAX`, try_into() will return `Ok`.
            end: string.len().try_into().expect_unreachable(), // CHECKED(Chloe)
        }
    }

    /// Grows the span from the front. This moves the end value up by `amount`.
    pub const fn grow_front(&mut self, amount: SpanValue) {
        self.end += amount;
    }

    /// Returns a span that is grown from the front. This moves the end value up by `amount`.
    #[must_use]
    pub const fn with_grow_front(&self, amount: SpanValue) -> Self {
        let mut new = *self;
        new.end += amount;
        new
    }

    /// Grows the span from the back. This moves the start value back by `amount`.
    ///
    /// # Panics
    /// Panics if the start of the span is less than `amount`, since spans can't have a negative start value.
    pub fn grow_back(&mut self, amount: SpanValue) {
        assert!(
            self.start >= amount,
            "cannot create a span with a negative start value"
        );
        self.start -= amount;
    }

    /// Returns a span that is grown from the back. This moves the start value back by `amount`.
    ///
    /// # Panics
    /// Panics if the start of the span is less than `amount`, since spans can't have a negative start value.
    #[must_use]
    pub fn with_grow_back(&self, amount: SpanValue) -> Self {
        assert!(
            self.start >= amount,
            "cannot create a span with a negative start value"
        );
        let mut new = *self;
        new.start -= amount;
        new
    }

    /// Shrinks the span from the back. This moves the start value up by `amount`.
    ///
    /// # Panics
    /// Panics if the size of the `Span` is less than `amount`, since a `Span`'s size can't be negative.
    pub fn shrink_back(&mut self, amount: SpanValue) {
        assert!(self.len() >= amount, "cannot create negative-size span");
        self.start += amount;
    }

    /// Returns a span that is shrunk from the back. This moves the start value up by `amount`.
    ///
    /// # Panics
    /// Panics if the size of the `Span` is less than `amount`, since a `Span`'s size can't be negative.
    #[must_use]
    pub fn with_shrink_back(&self, amount: SpanValue) -> Self {
        assert!(self.len() >= amount, "cannot create negative-size span");
        let mut new = *self;
        new.start += amount;
        new
    }

    /// Shrinks the span from the front. This moves the end value back by `amount`.
    ///
    /// # Panics
    /// This method will panic if the size of the `Span` is less than `amount`, since a `Span`'s size can't be negative.
    pub fn shrink_front(&mut self, amount: SpanValue) {
        assert!(self.len() >= amount, "cannot create negative-size span");
        self.end -= amount;
    }

    /// Returns a span shrunk from the front. This moves the end value back by `amount`.
    ///
    /// # Panics
    /// This method will panic if the size of the `Span` is less than `amount`, since a `Span`'s size can't be negative.
    #[must_use]
    pub fn with_shrink_front(&self, amount: SpanValue) -> Self {
        assert!(self.len() >= amount, "cannot create negative-size span");
        let mut new = *self;
        new.end -= amount;
        new
    }

    /// Checks if a `Span`'s size is `0`. Returns `true` if `0`, and false if anything else.
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets the length of a `Span`.
    pub const fn len(&self) -> SpanValue {
        self.end - self.start
    }

    /// Resets `self` by changing the start to be the end, plus 1, and changing the end to be the start.
    pub const fn reset(&mut self) {
        self.start = self.end;
    }

    pub const fn fits(&self, string: &str) -> bool {
        (self.end as usize) < string.len()
    }

    /// Applies the span to `string`, with `start` and `end` corresponding to byte indexes.
    ///
    /// # Panics
    /// Panics if `string` is shorter than the end of the span.
    pub fn apply_bytes<'a>(&self, string: &'a str) -> &'a str {
        assert!(
            string.len() >= self.end as usize,
            "string is too short to have the span applied"
        );
        &string[(self.start as usize)..(self.end as usize)]
    }
}

impl From<Span> for Range<SpanValue> {
    fn from(val: Span) -> Self {
        val.start..val.end
    }
}

impl From<Range<SpanValue>> for Span {
    fn from(value: Range<SpanValue>) -> Self {
        Self::new_from(value.start, value.end)
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        dual_order(self.start.cmp(&other.start), self.end.cmp(&other.end))
    }
}

fn dual_order(x: Ordering, y: Ordering) -> Option<Ordering> {
    match (x, y) {
        (x, y) if x == y => Some(x),
        (Ordering::Greater, Ordering::Less) | (Ordering::Less, Ordering::Greater) => None,
        (x, Ordering::Equal) | (Ordering::Equal, x) => Some(x),
        // The arms are exhaustive.
        // If each ordering is equal, first arm.
        // If one is Ordering::Less and the other is Ordering::Greater, second arm.
        // If one ordering is Ordering::Equal, third arm.
        // These are exhaustive.
        _ => unreachable!(), // CHECKED(Chloe)
    }
}
