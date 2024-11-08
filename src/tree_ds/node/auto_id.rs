use super::*;
use crate::generator::{unix_epoch::EpochBasedGenerator, Generator};

pub const GENERATOR: EpochBasedGenerator = EpochBasedGenerator;

impl<Q, T> Node<Q, T>
where
    Q: PartialEq + Eq + Clone + From<u128>,
    T: PartialEq + Eq + Clone,
{
    /// Creates a new node with an auto-generated ID.
    ///
    /// The ID is generated using a sequence generator, meaning that the ID is sequential and unique.
    /// This is useful when you want to create a node without specifying the ID. For a node to be
    /// created with an auto-generated ID, the `Q` type must be of type `AutomatedId`.
    ///
    /// # Arguments
    ///
    /// * `value` - The value to store in the node.
    ///
    /// # Returns
    ///
    /// A new node with an auto-generated ID.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use tree_ds::prelude::*;
    ///
    /// # #[cfg(feature = "auto_id")]
    /// # {
    /// let node = Node::<AutomatedId, &str>::new_with_auto_id(Some("Harry Doe"));
    /// let node_2 = Node::<AutomatedId, &str>::new_with_auto_id(Some("Jane Doe"));
    /// assert_ne!(node.get_node_id(), node_2.get_node_id());
    /// # }
    /// ```
    ///
    /// This is available only when the `auto_id` feature is enabled.
    pub fn new_auto(value: Option<T>) -> Self {
        {
            Self(Arc::new(Mutex::new(_Node {
                node_id: Q::from(GENERATOR.generate()),
                value,
                children: vec![],
                parent: None,
            })))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree_ds::prelude::AutomatedId;

    #[test]
    fn test_new_with_auto_id_async() {
        let node = Node::<AutomatedId, &str>::new_auto(Some("Harry Doe"));
        let node_2 = Node::<AutomatedId, &str>::new_auto(Some("Jane Doe"));
        assert_eq!(node.get_value(), Some("Harry Doe"));
        assert_ne!(node.get_node_id(), node_2.get_node_id());
    }
}
