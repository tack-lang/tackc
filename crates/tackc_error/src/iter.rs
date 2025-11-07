pub enum ReportMode {
    /// Skip errors and keep going.
    Skip,
    /// Stop after the first error.
    Stop,
    /// Report all errors, ignoring subsequent successes.
    Consume,
}

pub struct Reporter<I, T, E, F>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
{
    iter: I,
    callback: F,
    mode: ReportMode,
    stopped: bool,
}

impl<I, T, E, F> Reporter<I, T, E, F>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
{
    pub fn new(iter: I, callback: F, mode: ReportMode) -> Self {
        Self {
            iter,
            callback,
            mode,
            stopped: false,
        }
    }
}

impl<I, T, E, F> Iterator for Reporter<I, T, E, F>
where
    I: Iterator<Item = Result<T, E>>,
    F: FnMut(E),
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stopped {
            return None;
        }

        for res in self.iter.by_ref() {
            match res {
                Ok(ok) => return Some(ok),
                Err(e) => {
                    (self.callback)(e);
                    match self.mode {
                        ReportMode::Skip => {}
                        ReportMode::Stop => {
                            self.stopped = true;
                            return None;
                        }
                        ReportMode::Consume => {
                            self.stopped = true;
                            // Drain the rest
                            for t in self.iter.by_ref() {
                                if let Err(e) = t {
                                    (self.callback)(e);
                                }
                            }
                            return None;
                        }
                    }
                }
            }
        }
        None
    }
}

pub trait IteratorExt: Iterator {
    fn reporter<T, E, F: FnMut(E)>(self, callback: F, mode: ReportMode) -> impl Iterator<Item = T>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        Reporter::new(self, callback, mode)
    }

    fn skip_reporter<T, E, F: FnMut(E)>(self, callback: F) -> impl Iterator<Item = T>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, ReportMode::Skip)
    }

    fn stop_reporter<T, E, F: FnMut(E)>(self, callback: F) -> impl Iterator<Item = T>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, ReportMode::Stop)
    }

    fn consume_reporter<T, E, F: FnMut(E)>(self, callback: F) -> impl Iterator<Item = T>
    where
        Self: Iterator<Item = Result<T, E>> + Sized,
    {
        self.reporter(callback, ReportMode::Consume)
    }
}

impl<I: Iterator> IteratorExt for I {}
