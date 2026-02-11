/// A report mode that ignores errors.
#[derive(Clone, Copy, Default)]
pub struct Skip;
/// A report mode that stops when receiving an error.
#[derive(Clone, Copy, Default)]
pub struct Stop;
/// A report mode that when receiving an errors, consumes the rest of the iterators, reporting any errors.
#[derive(Clone, Copy, Default)]
pub struct Consume;

impl ReportMode for Skip {
    fn on_error<I, T, E, F>(_: &mut I, _: &mut F) -> bool
    where
        I: Iterator<Item = Result<T, E>>,
        F: FnMut(E),
    {
        true
    }
}

impl ReportMode for Stop {
    fn on_error<I, T, E, F>(iter: &mut I, _: &mut F) -> bool
    where
        I: Iterator<Item = Result<T, E>>,
        F: FnMut(E),
    {
        for _ in iter {}
        false
    }
}

impl ReportMode for Consume {
    fn on_error<I, T, E, F>(iter: &mut I, callback: &mut F) -> bool
    where
        I: Iterator<Item = Result<T, E>>,
        F: FnMut(E),
    {
        for i in iter {
            if let Err(e) = i {
                callback(e);
            }
        }
        false
    }
}

/// A trait for "report modes."
/// Report modes can be used in conjunction with the [`Reporter`] iterator in order to control how they act on an error.
pub trait ReportMode: Clone + Copy + Default {
    /// When an error is received, this function will be called.
    fn on_error<I, T, E, F>(iter: &mut I, callback: &mut F) -> bool
    where
        I: Iterator<Item = Result<T, E>>,
        F: FnMut(E);
}

/// The reporter iterator is a complex iterator adapter that can be used for error handling.
pub struct Reporter<I, F, M> {
    iter: I,
    callback: F,
    mode: M,
}

impl<I, T, E, F, M> Reporter<I, F, M>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
    M: ReportMode,
{
    /// Create a new reporter iterator.
    pub const fn new(iter: I, callback: F, mode: M) -> Self {
        Self {
            iter,
            callback,
            mode,
        }
    }
}

impl<I, T, E, F, M> Iterator for Reporter<I, F, M>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
    M: ReportMode,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.iter.next() {
            match item {
                Ok(ok) => return Some(ok),
                Err(e) => {
                    (self.callback)(e);
                    if M::on_error(&mut self.iter, &mut self.callback) {
                    } else {
                        return None;
                    }
                }
            }
        }
        None
    }
}

impl<I, T, E, F, M> Clone for Reporter<I, F, M>
where
    I: Iterator<Item = Result<T, E>> + Clone,
    F: FnMut(E) + Clone,
    M: ReportMode,
{
    fn clone(&self) -> Self {
        Self {
            iter: self.iter.clone(),
            callback: self.callback.clone(),
            mode: self.mode,
        }
    }
}

pub trait IteratorExt: Iterator {
    /// Wrap the `self` iterator with a reporter iterator, using report mode `mode`.
    fn reporter<M, T, E, F: FnMut(E)>(self, callback: F, mode: M) -> Reporter<Self, F, M>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
        M: ReportMode,
    {
        Reporter {
            iter: self,
            callback,
            mode,
        }
    }

    /// Wrap the `self` iterator with a reporter iterator, using report mode [`Skip`].
    fn skip_reporter<T, E, F: FnMut(E)>(self, callback: F) -> Reporter<Self, F, Skip>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, Skip)
    }

    /// Wrap the `self` iterator with a reporter iterator, using report mode [`Stop`].
    fn stop_reporter<T, E, F: FnMut(E)>(self, callback: F) -> Reporter<Self, F, Stop>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, Stop)
    }

    /// Wrap the `self` iterator with a reporter iterator, using report mode [`Consume`].
    fn consume_reporter<T, E, F: FnMut(E)>(self, callback: F) -> Reporter<Self, F, Consume>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, Consume)
    }
}

impl<I: Iterator> IteratorExt for I {}
