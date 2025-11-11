/*#[derive(Clone)]
pub enum ReportMode {
    /// Skip errors and keep going.
    Skip,
    /// Stop after the first error.
    Stop,
    /// Report all errors, ignoring subsequent successes.
    Consume,
}*/

#[derive(Clone, Copy, Default)]
pub struct Skip;
#[derive(Clone, Copy, Default)]
pub struct Stop;
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

pub trait ReportMode: Clone + Copy + Default {
    fn on_error<I, T, E, F>(iter: &mut I, callback: &mut F) -> bool
    where
        I: Iterator<Item = Result<T, E>>,
        F: FnMut(E);
}

pub struct Reporter<I, T, E, F, M>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
    M: ReportMode,
{
    iter: I,
    callback: F,
    mode: M,
}

impl<I, T, E, F, M> Reporter<I, T, E, F, M>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
    M: ReportMode,
{
    pub fn new(iter: I, callback: F, mode: M) -> Reporter<I, T, E, F, M> {
        Reporter {
            iter,
            callback,
            mode,
        }
    }
}

impl<I, T, E, F, M> Iterator for Reporter<I, T, E, F, M>
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

impl<I, T, E, F, M> Clone for Reporter<I, T, E, F, M>
where
    I: Iterator<Item = Result<T, E>> + Clone,
    F: FnMut(E) + Clone,
    M: ReportMode,
{
    fn clone(&self) -> Self {
        Reporter {
            iter: self.iter.clone(),
            callback: self.callback.clone(),
            mode: self.mode,
        }
    }
}

pub trait IteratorExt: Iterator {
    fn reporter<M, T, E, F: FnMut(E)>(self, callback: F, mode: M) -> Reporter<Self, T, E, F, M>
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

    fn skip_reporter<T, E, F: FnMut(E)>(self, callback: F) -> Reporter<Self, T, E, F, Skip>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, Skip)
    }

    fn stop_reporter<T, E, F: FnMut(E)>(self, callback: F) -> Reporter<Self, T, E, F, Stop>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, Stop)
    }

    fn consume_reporter<T, E, F: FnMut(E)>(self, callback: F) -> Reporter<Self, T, E, F, Consume>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, Consume)
    }
}

impl<I: Iterator> IteratorExt for I {}
