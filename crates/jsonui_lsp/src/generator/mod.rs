pub(crate) mod unix_epoch;

pub trait Generator<T> {
    /// Generates a new unique ID.
    fn generate(&self) -> T;

    fn with_offset(&self, offset: T) -> T;
}
