use std::ops::Add;
use std::thread::sleep;

use super::Generator;

/// A unique ID generator that generates IDs based on the current epoch time.
///
/// The generator generates IDs based on the current epoch time. This means that the IDs are unique
/// and sequential. The generator is useful when you want to generate unique IDs for your data. This
/// generator is however not available in a `no_std` environment since it relies on the
/// `std::time::SystemTime` to generate the IDs. In no_std environments, you can use the
/// `SimpleGenerator` provided by the `sequential-gen` crate.
///
/// # Examples
///
/// ```rust
/// use sequential_gen::prelude::*;
///
/// let generator = EpochBasedGenerator;
/// let value = generator.generate();
/// let value_2 = generator.generate();
/// assert_ne!(value, value_2);
/// ```
#[derive(Clone, Debug)]
pub struct EpochBasedGenerator;

impl Generator<u128> for EpochBasedGenerator {
    /// Generates a new unique ID based on the current epoch time.
    ///
    /// The ID is generated using the current epoch time. This means that the IDs are unique and
    /// sequential.
    ///
    /// # Returns
    ///
    /// A new unique ID.
    fn generate(&self) -> u128 {
        use std::time::SystemTime;
        sleep(std::time::Duration::from_micros(10));
        // Get the epoch
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    }

    /// Generates a new unique ID based on the current epoch time and adds an offset.
    ///
    /// This is useful when you want to generate a value that is offset by a certain amount.
    ///
    /// # Arguments
    ///
    /// * `offset` - The offset to add to the generated value.
    ///
    /// # Returns
    ///
    /// A new unique ID.
    fn with_offset(&self, offset: u128) -> u128 {
        offset.add(self.generate())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_epoch_based_generator() {
        let generator = EpochBasedGenerator;
        let value = generator.generate();
        let value_2 = generator.generate();
        assert_ne!(value, value_2);
    }

    #[test]
    fn test_epoch_based_generator_with_offset() {
        let generator = EpochBasedGenerator;
        let value = generator.generate();
        let value_2 = generator.with_offset(100);
        assert_ne!(value, value_2);
    }
}
