//! # Tree-DS
//! A simple tree data structure implementation in Rust. It can be used in both `std` and `no_std`
//! environments.
//!
//! The tree data structure is a hierarchical data structure that consists of nodes connected by
//! edges. Each node in the tree can have zero or more children nodes. The tree data structure
//! is used in various applications, such as file systems, computer science, and biology.
//!
//! A note on the choice of return types for the tree operations:
//! - The tree operations return a `Result` type to handle errors that may occur during the operation.
//! - For operations that return a value that may or may not be present, the return type is an `Option`.
//!
//! So for instance when you add a node to the tree, the return type is a `Result<NodeId>` because an
//! error may occur during the operation. When you get a node from the tree, the return type is an
//! `Option<&Node<T, Q>>` because the node may or may not be present in the tree.  
//!
//! ## Usage
//!
//! ```rust
//! use tree_ds::prelude::*;
//!
//! let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
//! let root = tree.add_node(Node::new(1, Some(2)), None).unwrap();
//! let child_1 = tree.add_node(Node::new(2, Some(3)), Some(&root)).unwrap();
//! let child_2 = tree
//!     .add_node(Node::new(3, Some(4)), Some(&child_1))
//!     .unwrap();
//! let child_3 = tree
//!     .add_node(Node::new(4, Some(5)), Some(&child_2))
//!     .unwrap();
//! let sub_tree = tree.get_subtree(&child_2, None);
//! ```
//!
//! ## Nodes
//! A Node is the building blocks of the tree data structure. Each node in the tree can have a value
//! and a unique ID. The value can be of any type that implements the `Eq`, `PartialEq` and `Clone`
//! traits.
//!
//! By default, the tree requires you to provide unique IDs for the nodes. This node Ids can be of
//! any type that implements the `Eq` and `Clone` traits.
//!
//! ```rust
//! use tree_ds::prelude::*;
//!
//! let node = Node::new(1, Some(2));
//! ```
//! However, you can enable the `auto_id` feature to generate IDs automatically. This is useful when
//! you want to create a node without specifying the ID. For a node to be created with an auto-generated
//! ID, the `Q` type must implement the `From<u128>` trait.
//!
//! ```rust, ignore
//! use tree_ds::prelude::*;
//!
//! let node = Node::<AutomatedId, &str>::new_with_auto_id(Some("Harry Doe"));
//! let node_2 = Node::<AutomatedId, &str>::new_with_auto_id(Some("Jane Doe"));
//! assert_ne!(node.get_node_id(), node_2.get_node_id());//!
//! ```
//!
//! ## Traversal
//! The tree supports three traversal strategies:
//! - Pre-order
//! - Post-order
//! - In-order
//!
//! Consider the following tree:
//! ```text
//! Node 1: 2
//! └── Node 2: 3
//!    └── Node 3: 4
//!        └── Node 4: 5
//! ```
//!
//! You can modify nodes during traversal by using the iterator from the returned traversal data.
//!
//! ```rust
//! use tree_ds::prelude::*;
//!
//! # fn main() -> Result<()> {
//! let mut tree = Tree::new(Some("Sample Tree"));
//! let root = tree.add_node(Node::new("Node 1", Some(2)), None)?;
//! let child_1 = tree.add_node(Node::new("Node 2", Some(3)), Some(&root))?;
//! let child_2 = tree.add_node(Node::new("Node 3", Some(4)), Some(&child_1))?;
//! let child_3 = tree.add_node(Node::new("Node 4", Some(5)), Some(&child_2))?;
//!
//! tree.traverse(&root, TraversalStrategy::PreOrder)?
//!   .iter()
//!   .for_each(|node_id| {
//!     let node = tree.get_node_by_id(node_id).unwrap();
//!     let cur_value = node.get_value().unwrap();
//!     node.set_value(Some(cur_value + 1));
//! });
//!
//! # #[cfg(feature = "print_node_id")]
//! # assert_eq!("Sample Tree\n***********\nNode 1: 3\n└── Node 2: 4\n    └── Node 3: 5\n        └── Node 4: 6\n", tree.to_string());
//! # #[cfg(not(feature = "print_node_id"))]
//! # assert_eq!("Sample Tree\n***********\n3\n└── 4\n    └── 5\n        └── 6\n", tree.to_string());
//! # assert_eq!(tree.get_node_by_id(&root).unwrap().get_value().unwrap(), 3);
//! # Ok(())
//! # }
//! ```
//!
//! The newly modified tree will be:
//! ```text
//! Sample Tree
//! ***********
//! Node 1: 3
//! └── Node 2: 4
//!    └── Node 3: 5
//!        └── Node 4: 6
//! ```
//!
//! ## Serialization and Deserialization
//! The tree data structure can be serialized and deserialized using the `serde` feature. By default,
//! the tree serializes all the fields within the nodes. However, you can enable the `compact_serde`
//! feature to serialize only the important node data that is sufficient to properly deserialize the
//! node. This is useful when you want to serialize the tree and store it in a file or a database. The
//! `compact_serde` feature is meant to be used with either the `serde` or the `no_std` feature. Enabling
//! this feature without the `serde` or the `no_std` feature will result in nothing being serialized or
//! deserialized. It should be noted that this feature adds an overhead when deserializing the data since
//! the tree has to be reconstructed from the serialized data.
//!
//!
//! ## `no_std` Environments.
//! This crate can be used in `no_std` environments by enabling the `no_std` feature.
//!
//! ```toml
//! [dependencies]
//! tree-ds = { version = "0.1", features = ["no_std"] }
//! ```
//! The `no_std` feature disables the standard library and uses the `alloc` crate instead. The `alloc`
//! crate provides the necessary types for the tree data structure to work in `no_std` environments.
//! The `no_std` feature is useful when you want to use the tree data structure in embedded systems or
//! environments where the standard library is not available. This feature also supports serialization
//! and deserialization of the tree data structure as well as limited `auto_id` support.
//!
//!
//! ## Cargo Features
//! The following cargo features are also available:
//! - By default the library is synchronous, and you need to manually provide ids for the nodes.
//! - `async`: Enables support for async operations on the tree.
//! - `serde`: Enables serialization and deserialization of the tree.
//! - `auto_id`: Enables auto-generation of node IDs.
//! - `no_std`: Disables the standard library.
//! - `print_node_id`: Enables printing the node ID when printing the tree. It is disabled by default.
//! - `compact-serde`: Enables compact serialization and deserialization of the tree. It is meant to be
//!   used with either the `serde` or the `no_std` features. Enabling this feature without the `serde` or
//!   the `no_std` feature will result in nothing being serialized or deserialized.

mod lib {
    pub use std::collections::HashSet;
    pub use std::rc::Rc;
    pub use std::string::{String, ToString};
    pub use std::sync::Arc;
    pub use std::vec::Vec;
    pub use std::{format, vec};

    pub use self::core::cell::RefCell;
    pub use self::core::clone::Clone;
    pub use self::core::cmp::{Eq, PartialEq};
    pub use self::core::convert::{AsRef, From};
    pub use self::core::default::Default;
    pub use self::core::fmt::{Debug, Display, Error as FmtError, Formatter, Result as FmtResult};
    pub use self::core::hash::{Hash, Hasher};
    pub use self::core::option::Option;
    pub use self::core::result::Result;
    pub use self::core::slice::Iter;

    mod core {
        pub use std::*;
    }
}

mod error;
mod node;
mod tree;

pub mod prelude {
    //! A module to re-export the necessary types for the tree data structure.

    pub use super::node::{Node, Nodes};
    pub use super::tree::{NodeRemovalStrategy, SubTree, TraversalStrategy, Tree};

    /// Defines the default type for the node id.
    ///
    /// The default type for the node id is `u128`.
    pub type AutomatedId = u128;

    /// The error type for this crate.
    pub type Result<T> = super::lib::Result<T, super::error::Error>;
}
