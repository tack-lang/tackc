use std::array;

#[derive(Debug, Clone)]
pub struct Peekable<I: Iterator, const K: usize> {
    iter: I,
    // None represents "iterator not checked," Some(None) represents "iterator returned None," and Some(Some(T)) represents "iterator returned Some(T)."
    // All Some values will be at beginning, since there can't be holes in the iterator.
    #[allow(clippy::option_option)]
    next: [Option<Option<I::Item>>; K],
}

impl<I: Iterator, const K: usize> Peekable<I, K> {
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            next: array::from_fn(|_| None),
        }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn peek(&mut self, count: usize) -> Option<&I::Item> {
        assert!(count < K);
        self.next
            .iter_mut()
            .take(count + 1)
            .skip_while(|opt| opt.is_some())
            .for_each(|opt| *opt = Some(self.iter.next()));
        self.next[count].as_ref().unwrap().as_ref()
    }
}

impl<I: Iterator, const K: usize> Iterator for Peekable<I, K> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let temp = self.next[0].take();
        for i in 0..(K - 1) {
            if self.next[i + 1].is_none() {
                break;
            }
            self.next[i] = self.next[i + 1].take();
        }
        temp.unwrap_or_else(|| self.iter.next())
    }
}

pub trait IteratorExt: Sized + Iterator {
    fn peekable_tackc<const K: usize>(self) -> Peekable<Self, K> {
        Peekable::new(self)
    }
}

impl<T: Iterator> IteratorExt for T {}

#[test]
fn peekable_test() {
    let mut iter: Peekable<std::ops::Range<i32>, 10> = Peekable::new(0..5);
    assert_eq!(iter.peek(2), Some(&2));
    assert_eq!(iter.next(), Some(0));
    assert_eq!(iter.peek(3), Some(&4));
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    //assert_eq!(iter.peek(10), None);
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), Some(4));
}
