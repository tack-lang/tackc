//! Span library based heavily off [`text-span`](https://crates.io/crates/text-span).

use std::{cmp::Ordering, ops::Range};

use serde::{Deserialize, Serialize};

use crate::{
    file::{File, FileId},
    utils::UnwrapExt,
};

/// The value used as an index in the [`Span`] type.
pub type SpanValue = u32;

/// The `Span` type represents an area of a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    /// The start of the `Span` (Inclusive).
    pub start: SpanValue,
    /// The end of the `Span` (Exclusive).
    pub end: SpanValue,
    /// The file ID of this span.
    pub file: FileId,
}

impl Span {
    /// Creates a new `Span`. This span will start and end at the 0th character, making it have a length of zero.
    pub fn new(file: &File) -> Self {
        Self::new_from(0, 0, file)
    }

    /// Creates a new `Span` from a pair of start and end indexes, and a file.
    ///
    /// # Panics
    /// Panics if start is greater than end, since spans can't have a negative length.
    pub fn new_from(start: SpanValue, end: SpanValue, file: &File) -> Self {
        assert!(end >= start, "cannot create negative-size span");

        Self {
            start,
            end,
            file: file.id(),
        }
    }

    /// Creates a new `Span` pointing to the end of a file.
    ///
    /// # Panics
    /// This function will panic if the input file's length is greater than [`SpanValue::MAX`].
    pub fn eof(file: &File) -> Self {
        assert!(
            file.len() <= SpanValue::MAX as usize,
            "Length of `Span::eof` input must be less than or equal to `SpanValue::MAX!`"
        );

        Self {
            // Since `file.len() <= SpanValue::MAX`, try_into() will return `Ok`.
            start: file.len().try_into().expect_unreachable(), // CHECKED(Chloe)
            end: file.len().try_into().expect_unreachable(),   // CHECKED(Chloe)
            file: file.id(),
        }
    }

    /// Creates a new `Span` pointing to an entire string.
    ///
    /// # Panics
    /// This function will panic if the input file's length is greater than [`SpanValue::MAX`].
    pub fn full(file: &File) -> Self {
        assert!(
            file.len() <= SpanValue::MAX as usize,
            "Length of `Span::full` input must be less than or equal to `SpanValue::MAX!`"
        );

        Self {
            start: 0,
            // Since `file.len() <= SpanValue::MAX`, try_into() will return `Ok`.
            end: file.len().try_into().expect_unreachable(), // CHECKED(Chloe)
            file: file.id(),
        }
    }

    /// Grows the span from the front. This moves the end value up by `amount`.
    pub const fn grow_front(&mut self, amount: SpanValue) {
        self.end += amount;
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

    /// Shrinks the span from the back. This moves the start value up by `amount`.
    ///
    /// # Panics
    /// Panics if the size of the `Span` is less than `amount`, since a `Span`'s size can't be negative.
    pub fn shrink_back(&mut self, amount: SpanValue) {
        assert!(self.len() >= amount, "cannot create negative-size span");
        self.start += amount;
    }

    /// Shrinks the span from the front. This moves the end value back by `amount`.
    ///
    /// # Panics
    /// This method will panic if the size of the `Span` is less than `amount`, since a `Span`'s size can't be negative.
    pub fn shrink_front(&mut self, amount: SpanValue) {
        assert!(self.len() >= amount, "cannot create negative-size span");
        self.end -= amount;
    }

    /// Moves the span forward. This moves both the start and end value up by `amount`.
    pub const fn move_forward(&mut self, amount: SpanValue) {
        self.start += amount;
        self.end += amount;
    }

    /// Moves the span backward. This moves both the start and end value back by `amount`.
    ///
    /// # Panics
    /// Panics if the start of the span is less than `amount`, since spans can't have a negative start value.
    pub fn move_backward(&mut self, amount: SpanValue) {
        assert!(
            self.start >= amount,
            "cannot create a span with a negative start value"
        );

        self.start -= amount;
        self.end -= amount;
    }

    /// Checks if a `Span`'s size is `0`. Returns `true` if `0`, and false if anything else.
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets the length of a `Span`.
    pub const fn len(&self) -> SpanValue {
        self.end - self.start
    }

    /// Resets `self` by changing the start to be the end.
    pub const fn reset(&mut self) {
        self.start = self.end;
    }

    /// Returns whether the span can be applied to the file.
    pub fn fits(&self, file: &File) -> bool {
        (self.end as usize) <= file.len()
    }

    /// Returns whether the span's file matches the file.
    pub fn matches(&self, file: &File) -> bool {
        self.file == file.id()
    }

    /// Applies the span to `file`, with `start` and `end` corresponding to byte indexes.
    ///
    /// # Panics
    /// Panics if `file` is shorter than the end of the span.
    pub fn apply<'a>(&self, file: &'a File) -> &'a str {
        assert!(
            self.fits(file),
            "file is too short to have the span applied"
        );
        assert!(self.matches(file), "file doesn't match span's file");
        &file[(self.start as usize)..(self.end as usize)]
    }
}

impl From<Span> for Range<SpanValue> {
    fn from(val: Span) -> Self {
        val.start..val.end
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
        (x, Ordering::Equal) | (Ordering::Equal, x) => Some(x),
        (x, y) if x != y => None,
        // The arms are exhaustive.
        // If each ordering is equal, first arm.
        // If one ordering is Ordering::Equal, second arm.
        // If one is Ordering::Less and the other is Ordering::Greater, third arm.
        // These are exhaustive.
        _ => unreachable!(), // CHECKED(Chloe)
    }
}

#[test]
fn dual_order_test() {
    let tests = [
        ((Ordering::Less, Ordering::Less), Some(Ordering::Less)),
        ((Ordering::Less, Ordering::Equal), Some(Ordering::Less)),
        ((Ordering::Less, Ordering::Greater), None),
        ((Ordering::Equal, Ordering::Less), Some(Ordering::Less)),
        ((Ordering::Equal, Ordering::Equal), Some(Ordering::Equal)),
        (
            (Ordering::Equal, Ordering::Greater),
            Some(Ordering::Greater),
        ),
        ((Ordering::Greater, Ordering::Less), None),
        (
            (Ordering::Greater, Ordering::Equal),
            Some(Ordering::Greater),
        ),
        (
            (Ordering::Greater, Ordering::Greater),
            Some(Ordering::Greater),
        ),
    ];

    for ((x, y), correct) in tests {
        assert_eq!(dual_order(x, y), correct);
    }
}

#[test]
fn span_test() {
    use std::path::Path;

    let text =
"Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Pellentesque venenatis, lacus eu viverra ornare, massa est rhoncus turpis, eu pharetra lacus risus eu enim.
Praesent finibus nunc nec nunc ornare ultrices. Aenean volutpat odio magna, vel condimentum sapien finibus ac.
In hac habitasse platea dictumst. Phasellus sodales nibh sit amet.";
    let file = File::new(text, Path::new("lorem_ipsum.txt"));

    let mut span = Span::new(&file);
    span.grow_front(5);
    assert_eq!(span.apply(&file), "Lorem");
    span.reset();
    span.move_forward(1);
    span.grow_front(5);
    assert_eq!(span.apply(&file), "ipsum");
    span.move_backward(6);
    assert_eq!(span.apply(&file), "Lorem");
    span.move_forward(12);
    assert_eq!(span.apply(&file), "dolor");
    span.reset();
    span.move_forward(4);
    assert!(span.is_empty());
    span.grow_back(3);
    assert_eq!(span.apply(&file), "sit");
}
