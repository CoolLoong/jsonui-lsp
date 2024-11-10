use super::error::Error::{InvalidOperation, NodeNotFound, RootNodeAlreadyPresent};
use super::lib::*;
use super::node::Nodes;
use super::prelude::{Node, Result};

/// The strategy to use when removing a node from the tree.
///
/// This enum represents the strategy to use when removing a node from the tree. The `RetainChildren`
/// strategy retains the children of the node when the node is removed. The `RemoveNodeAndChildren`
/// strategy removes the node and its children when the node is removed.
#[derive(Clone, Debug, Copy)]
pub enum NodeRemovalStrategy {
    /// Retain the children of the node. This means that the children of the node are attached to the
    /// parent of the node when the node is removed. So the children of the node become children of the
    /// parent of the node.
    RetainChildren,
    /// Remove the node and all subsequent children. This means that the node and its children are
    /// removed from the tree when the node is removed. All the subsequent grand children of the node
    /// are removed from the tree.
    RemoveNodeAndChildren,
}

/// The strategy to use when traversing the tree.
///
/// This enum represents the strategy to use when traversing the tree.
#[allow(clippy::enum_variant_names)]
#[derive(Clone, Debug, Copy)]
pub enum TraversalStrategy {
    /// Traverse the tree in pre-order. This means that the root node is visited first, then the left
    /// child, and then the right child.
    PreOrder,
    /// Traverse the tree in post-order. This means that the left child is visited first, then the right
    /// child, and then the root node.
    PostOrder,
    /// Traverse the tree in in-order. This means that the left child is visited first, then the root
    /// node, and then the right child.
    InOrder,
}

/// A subtree of a tree.
///
/// This struct represents a subtree of a tree. A subtree is a tree that is a part of a larger tree.
pub type SubTree<Q, T> = Tree<Q, T>;

/// A tree data structure.
///
/// This struct represents a tree data structure. A tree is a data structure that consists of nodes
/// connected by edges. Each node has a parent node and zero or more child nodes. The tree has a root
/// node that is the topmost node in the tree. The tree can be used to represent hierarchical data
/// structures such as file systems, organization charts, and family trees. A tree can have any number
/// of nodes and each node can have any number of children. The tree can be traversed in different
/// orders such as pre-order, post-order, and in-order. The tree can be named for easy identification
/// when working with multiple trees or subtrees.
///
/// # Type Parameters
///
/// * `Q` - The type of the node id.
/// * `T` - The type of the node value.
///
/// # Example
///
/// ```rust
/// # use tree_ds::prelude::Tree;
///
/// let tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tree<Q, T>
where
    Q: PartialEq + Eq + Clone,
    T: PartialEq + Eq + Clone,
{
    name:  Option<String>,
    nodes: Nodes<Q, T>,
}

impl<Q, T> Tree<Q, T>
where
    Q: PartialEq + Eq + Clone + Display + Hash + Ord,
    T: PartialEq + Eq + Clone,
{
    /// Create a new tree.
    ///
    /// This method creates a new tree with no nodes.
    ///
    /// # Returns
    ///
    /// A new tree with no nodes.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::Tree;
    ///
    /// let tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    /// ```
    pub fn new(tree_name: Option<&str>) -> Self {
        Self {
            name:  tree_name.map(|x| x.to_string()),
            nodes: Nodes::default(),
        }
    }

    /// Add a node to the tree.
    ///
    /// This method adds a node to the tree. The node is added as a child of the parent node with the
    /// given parent id. If the parent id is `None`, the node is added as a root node. The node id is
    /// used to identify the node and the value is the value of the node. The value can be used to store
    /// any data that you want to associate with the node.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node.
    /// * `value` - The value of the node.
    /// * `parent_id` - The id of the parent node. If `None`, the node is added as a root node.
    ///
    /// # Returns
    ///
    /// The id of the node that was added to the tree. However, if no parent id is provided and the tree
    /// already has a root node, an error is returned.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Tree, Node};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    /// let node_id = tree.add_node(Node::new(1, Some(2)), None);
    ///
    /// assert!(node_id.is_ok());
    /// // This should return an error because the tree already has a root node.
    /// let another_node_id = tree.add_node(Node::new(2, Some(3)), None);
    /// assert!(another_node_id.is_err());
    /// ```
    pub fn add_node(&mut self, node: Node<Q, T>, parent_id: Option<&Q>) -> Result<Q> {
        if let Some(parent_id) = parent_id {
            let parent = self
                .nodes
                .iter()
                .find(|n| &n.get_node_id() == parent_id)
                .ok_or(NodeNotFound(parent_id.to_string()))?;
            parent.add_child(node.clone());
        } else if self.get_root_node().is_some() {
            return Err(RootNodeAlreadyPresent);
        }
        self.nodes.push(node.clone());
        Ok(node.get_node_id())
    }

    /// Get the name of the tree.
    ///
    /// This method gets the name of the tree.
    ///
    /// # Returns
    ///
    /// The name of the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::Tree;
    ///
    /// let tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// assert_eq!(tree.get_name(), Some("Sample Tree"));
    /// ```
    pub fn get_name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    /// Set the name of the tree.
    ///
    /// This method sets the name of the tree.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::Tree;
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    /// tree.rename(Some("New Name"));
    /// assert_eq!(tree.get_name(), Some("New Name"));
    /// ```
    pub fn rename(&mut self, name: Option<&str>) {
        self.name = name.map(|x| x.to_string());
    }

    /// Get a node in the tree.
    ///
    /// This method gets the node with the given node id in the tree.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node.
    ///
    /// # Returns
    ///
    /// The node with the given node id in the tree or `None` if the node is not found.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node = Node::new(1, Some(2));
    /// let node_id = tree.add_node(node.clone(), None).unwrap();
    ///
    /// assert_eq!(tree.get_node_by_id(&node_id), Some(node));
    /// ```
    pub fn get_node_by_id(&self, node_id: &Q) -> Option<Node<Q, T>> {
        self.nodes.iter().find(|n| &n.get_node_id() == node_id).cloned()
    }

    /// Get the root node of the tree.
    ///
    /// This method gets the root node of the tree. The root node is the topmost node in the tree. The
    /// root node has no parent node.
    ///
    /// # Returns
    ///
    /// The root node of the tree or `None` if the tree has no root node.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node = Node::new(1, Some(2));
    /// tree.add_node(node.clone(), None).unwrap();
    ///
    /// assert_eq!(tree.get_root_node(), Some(node));
    /// ```
    pub fn get_root_node(&self) -> Option<Node<Q, T>> {
        self.nodes.iter().find(|n| n.get_parent_id().is_none()).cloned()
    }

    /// Get the height of the node.
    ///
    /// This method gets the height of the node. The height of the node is the number of edges present
    /// in the longest path connecting the node to a leaf node.
    ///
    /// # Returns
    ///
    /// The height of the node. If the node is a leaf node, the height is 0.  This method returns an
    /// error if the node is not found in the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
    ///
    /// assert!(tree.get_node_height(&node_2).is_ok());
    /// assert_eq!(tree.get_node_height(&node_2).unwrap(), 1);
    /// ```
    pub fn get_node_height(&self, node_id: &Q) -> Result<i32> {
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        let children = node.get_children_ids();
        if children.is_empty() {
            return Ok(0);
        }
        let mut height = 0;
        for child in children {
            let child_height = self.get_node_height(&child)?;
            if child_height > height {
                height = child_height;
            }
        }
        Ok(height + 1)
    }

    /// Get the depth of a node in the tree.
    ///
    /// This method gets the depth of a node in the tree. The depth of a node is the length of the path
    /// from the root node to the node. The depth of the node is the number of edges on the path from the
    /// root node to the node.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node.
    ///
    /// # Returns
    ///
    /// The depth of the node in the tree.  This method returns an error if the node is not found in the
    /// tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
    /// let depth_result = tree.get_node_depth(&node_3);
    /// assert!(depth_result.is_ok());
    /// assert_eq!(depth_result.unwrap(), 2);
    /// ```
    pub fn get_node_depth(&self, node_id: &Q) -> Result<i32> {
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        let mut depth = 0;
        let mut parent = node.get_parent_id();
        while let Some(parent_id) = parent {
            depth += 1;
            parent = self
                .get_node_by_id(&parent_id)
                .ok_or(NodeNotFound(parent_id.to_string()))?
                .get_parent_id();
        }
        Ok(depth)
    }

    /// Get the ancestors of a node in the tree.
    ///
    /// This method gets the ancestors of a node in the tree. The ancestors of a node are all the nodes
    /// that are on the path from the root node to the node, not including the node itself.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node.
    ///
    /// # Returns
    ///
    /// The ancestors of the node from closest to furthest.  This method returns an error if the node is
    /// not found in the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
    /// let depth_result = tree.get_ancestor_ids(&node_3);
    /// assert!(depth_result.is_ok());
    /// assert_eq!(depth_result.unwrap(), vec![2, 1]);
    /// ```
    pub fn get_ancestor_ids(&self, node_id: &Q) -> Result<Vec<Q>> {
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        let mut ancestors = vec![];
        let mut parent = node.get_parent_id();
        while let Some(parent_id) = parent {
            ancestors.push(parent_id.clone());
            parent = self
                .get_node_by_id(&parent_id)
                .ok_or(NodeNotFound(parent_id.to_string()))?
                .get_parent_id();
        }
        Ok(ancestors)
    }

    /// Get the height of the tree.
    ///
    /// This method gets the height of the tree. The height of the tree is the length of the longest path
    /// from the root node to a leaf node. The height of the tree is the number of edges on the longest
    /// path from the root node to a leaf node.
    ///
    /// # Returns
    ///
    /// The height of the tree. This method returns an error if the tree has no root node.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree, Result};
    ///
    /// # fn main() -> Result<()> {
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1))?;
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2))?;
    /// let tree_height = tree.get_height();
    /// assert!(tree_height.is_ok());
    /// assert_eq!(tree_height?, 2);
    /// # Ok(())
    /// # }
    /// ```
    pub fn get_height(&self) -> Result<i32> {
        let root = self
            .get_root_node()
            .ok_or(InvalidOperation(String::from("Tree has no root node")))?;
        self.get_node_height(&root.get_node_id())
    }

    /// Get the degree of a node in the tree.
    ///
    /// This method gets the degree of a node in the tree. The degree of a node is the number of children
    /// that the node has.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node.
    ///
    /// # Returns
    ///
    /// The degree of the node in the tree. This method returns an error if the node is not found in the
    /// tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Result, Node, Tree};
    ///
    /// # fn main() -> Result<()> {
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1))?;
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_1))?;
    ///
    /// assert_eq!(tree.get_node_degree(&node_1)?, 2);
    /// assert_eq!(tree.get_node_degree(&node_2)?, 0);
    /// assert_eq!(tree.get_node_degree(&node_3)?, 0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn get_node_degree(&self, node_id: &Q) -> Result<i32> {
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        Ok(node.get_children_ids().len() as i32)
    }

    /// Get the nodes in the tree.
    ///
    /// This method gets the nodes in the tree.
    ///
    /// # Returns
    ///
    /// The nodes in the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node = Node::new(1, Some(2));
    /// tree.add_node(node.clone(), None).unwrap();
    ///
    /// assert_eq!(tree.get_nodes().len(), 1);
    /// ```
    pub fn get_nodes(&self) -> &Nodes<Q, T> {
        self.nodes.as_ref()
    }

    /// Remove a node from the tree.
    ///
    /// This method removes a node from the tree. The node is removed using the given removal strategy.
    /// The removal strategy determines how the node and its children are removed from the tree. The
    /// `RetainChildren` strategy retains the children of the node when the node is removed. The
    /// `RemoveNodeAndChildren` strategy removes the node and its children when the node is removed.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node to remove.
    /// * `strategy` - The strategy to use when removing the node.
    ///
    /// # Returns
    /// An error if the node is not found in the tree or if the node is the root node and the removal
    /// strategy is `RetainChildren`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree, NodeRemovalStrategy, Result};
    ///
    /// # fn main() -> Result<()> {
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1))?;
    /// tree.add_node(Node::new(3, Some(6)), Some(&node_2))?;
    ///
    /// tree.remove_node(&node_2, NodeRemovalStrategy::RetainChildren)?;
    /// assert_eq!(tree.get_nodes().len(), 2);
    /// # Ok(())
    /// # }
    /// ```
    pub fn remove_node(&mut self, node_id: &Q, strategy: NodeRemovalStrategy) -> Result<()> {
        match strategy {
            NodeRemovalStrategy::RetainChildren => {
                let node = self
                    .get_node_by_id(node_id)
                    .ok_or(NodeNotFound(node_id.to_string()))?;
                let parent_node_id = &node.get_parent_id().ok_or(InvalidOperation(String::from(
                    "Cannot remove root node with RetainChildren strategy",
                )))?;
                let parent_node = self
                    .get_node_by_id(parent_node_id)
                    .ok_or(NodeNotFound(parent_node_id.to_string()))?;
                parent_node.remove_child(node.clone());
                let children = node.get_children_ids();
                for child in children {
                    if let Some(child) = self.get_node_by_id(&child) {
                        parent_node.add_child(child);
                    }
                }
                self.nodes.retain(|n| &n.get_node_id() != node_id);
                Ok(())
            }
            NodeRemovalStrategy::RemoveNodeAndChildren => {
                let node = self
                    .get_node_by_id(node_id)
                    .ok_or(NodeNotFound(node_id.to_string()))?;
                let children = node.get_children_ids();
                if let Some(parent_id) = node.get_parent_id() {
                    let parent = self
                        .get_node_by_id(&parent_id)
                        .ok_or(NodeNotFound(parent_id.to_string()))?;
                    parent.remove_child(node.clone());
                }
                self.nodes.retain(|n| &n.get_node_id() != node_id);
                for child in children {
                    let child = self
                        .get_node_by_id(&child)
                        .ok_or(NodeNotFound(child.to_string()))?;
                    node.remove_child(child.clone());
                    self.remove_node(&child.get_node_id(), strategy)?;
                }
                Ok(())
            }
        }
    }

    /// Get a subsection of the tree.
    ///
    /// This method gets a subsection of the tree starting from the node with the given node id. The
    /// subsection is a list of nodes that are descendants of the node with the given node id upto the
    /// given number of descendants. If the number of descendants is `None`, all the descendants of the
    /// node are included in the subsection.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node to get the subsection from.
    /// * `generations` - The number of descendants to include in the subsection. If `None`, all the
    ///   descendants of the node are included in the subsection.
    ///
    /// # Returns
    ///
    /// The subsection of the tree starting from the node with the given node id.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// # fn main() -> tree_ds::prelude::Result<()> {
    /// # let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    ///
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1))?;
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2))?;
    ///
    /// let subsection = tree.get_subtree(&node_2, None)?;
    /// assert_eq!(subsection.get_nodes().len(), 2);
    /// # Ok(())
    /// # }
    /// ```
    pub fn get_subtree(&self, node_id: &Q, generations: Option<i32>) -> Result<SubTree<Q, T>> {
        let mut subsection = Nodes::default();
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        subsection.push(node.clone());
        // Get the subsequent children of the node recursively for the number of generations and add them
        // to the subsection.
        if let Some(generations) = generations {
            let children = node.get_children_ids();
            for current_generation in 0..generations {
                for child in children.clone() {
                    subsection.append(
                        &mut self
                            .get_subtree(&child, Some(current_generation))?
                            .get_nodes()
                            .clone(),
                    );
                }
            }
        } else {
            let children = node.get_children_ids();
            for child in children {
                subsection.append(&mut self.get_subtree(&child, None)?.get_nodes().clone());
            }
        }

        Ok(SubTree {
            name:  Some(node_id.to_string()),
            nodes: subsection,
        })
    }

    /// Get the siblings of a node in the tree.
    ///
    /// This method gets the siblings of a node in the tree. The siblings of a node are the children
    /// that share the same parent as the node.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node to get the siblings of.
    /// * `inclusive` - A flag that indicates whether to include the node in the siblings list.
    ///
    /// # Returns
    ///
    /// The siblings of the node in the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree};
    ///
    /// # fn main() -> tree_ds::prelude::Result<()> {
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1))?;
    /// tree.add_node(Node::new(3, Some(6)), Some(&node_1))?;
    /// tree.add_node(Node::new(4, Some(7)), Some(&node_1))?;
    ///
    /// let siblings = tree.get_sibling_ids(&node_2, false)?;
    /// assert_eq!(siblings.len(), 2);
    ///
    /// let siblings = tree.get_sibling_ids(&node_2, true)?;
    /// assert_eq!(siblings.len(), 3);
    /// # Ok(())
    /// # }
    /// ```
    pub fn get_sibling_ids(&self, node_id: &Q, inclusive: bool) -> Result<Vec<Q>> {
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        if let Some(parent_id) = node.get_parent_id() {
            let parent = self
                .get_node_by_id(&parent_id)
                .ok_or(NodeNotFound(parent_id.to_string()))?;
            if inclusive {
                Ok(parent.get_children_ids().clone())
            } else {
                Ok(parent
                    .get_children_ids()
                    .iter()
                    .filter(|x| *x != node_id)
                    .cloned()
                    .collect())
            }
        } else if inclusive {
            // We need to clone this since Q does not implement Copy.
            Ok(vec![node_id.clone()])
        } else {
            Ok(vec![])
        }
    }

    /// Add a subsection to the tree.
    ///
    /// This method adds a subsection to the tree. The subsection is a list of nodes that are descendants
    /// of the node with the given node id. The subsection is added as children of the node with the
    /// given node id.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The id of the node to add the subsection to.
    /// * `subtree` - The subsection to add to the tree.
    ///
    /// # Returns
    /// This function return an error if:
    /// - The node is not found in the tree.
    /// - The subsection has no root node.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree, SubTree};
    ///
    /// # fn main() -> tree_ds::prelude::Result<()> {
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    /// let node_id = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let mut subtree = SubTree::new(Some("Sample Tree"));
    /// let node_2 = subtree.add_node(Node::new(2, Some(3)), None)?;
    /// subtree.add_node(Node::new(3, Some(6)), Some(&node_2))?;
    /// tree.add_subtree(&node_id, subtree)?;
    /// assert_eq!(tree.get_nodes().len(), 3);
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_subtree(&mut self, node_id: &Q, subtree: SubTree<Q, T>) -> Result<()> {
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        // Get the root node in the subsection and add it as a child of the node.
        let subtree_nodes = subtree.get_nodes();
        let root_node = subtree
            .get_root_node()
            .ok_or(InvalidOperation(String::from("Subtree has no root node.")))?;
        node.add_child(root_node.clone());
        self.nodes.append(&mut subtree_nodes.clone());
        Ok(())
    }

    /// Traverse the subtree from the given node.
    ///
    /// This method traverses the subtree from the given node in the given order.
    ///
    /// # Arguments
    ///
    /// * `order` - The order to traverse the tree.
    /// * `node_id` - The id of the node to start the traversal from.
    ///
    /// # Returns
    ///
    /// The nodes in the tree in the given order. This method returns an error if the node is not found
    /// in the tree.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use tree_ds::prelude::{Node, Tree, TraversalStrategy};
    ///
    /// # fn main() -> tree_ds::prelude::Result<()> {
    /// let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
    /// let node_1 = tree.add_node(Node::new(1, Some(2)), None)?;
    /// let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1))?;
    /// let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2))?;
    ///
    /// let ordered_nodes = tree.traverse(&node_1, TraversalStrategy::PreOrder)?;
    /// # let expected = vec![1, 2, 3];
    /// # assert_eq!(ordered_nodes, expected);
    /// # Ok(())
    /// # }
    /// ```
    pub fn traverse(&self, node_id: &Q, order: TraversalStrategy) -> Result<Vec<Q>> {
        let mut nodes = vec![];
        let node = self
            .get_node_by_id(node_id)
            .ok_or(NodeNotFound(node_id.to_string()))?;
        match &order {
            TraversalStrategy::PreOrder => {
                nodes.push(node_id.clone());
                for child_id in node.get_children_ids().iter() {
                    nodes.append(&mut self.traverse(child_id, order)?);
                }
            }
            TraversalStrategy::PostOrder => {
                for child_id in node.get_children_ids().iter() {
                    nodes.append(&mut self.traverse(child_id, order)?);
                }
                nodes.push(node_id.clone());
            }
            TraversalStrategy::InOrder => {
                for (index, child_id) in node.get_children_ids().iter().enumerate() {
                    if index == 0 {
                        nodes.append(&mut self.traverse(child_id, order)?);
                        if !nodes.contains(child_id) {
                            nodes.push(child_id.clone());
                        }
                        if !nodes.contains(node_id) {
                            nodes.push(node_id.clone());
                        }
                    } else {
                        nodes.push(child_id.clone());
                        nodes.append(&mut self.traverse(child_id, order)?);
                    }
                }
            }
        }
        let mut seen = BfastHashSet::default();
        nodes.retain(|x| seen.insert(x.clone()));
        Ok(nodes)
    }

    /// Print the tree.
    ///
    /// This method prints the tree to the standard output.
    #[doc(hidden)]
    fn print_tree(
        tree: &Tree<Q, T>,
        f: &mut Formatter<'_>,
        node: &Node<Q, T>,
        level: usize,
        mut is_within: (bool, usize),
        is_last_child: bool,
    ) -> Result<()>
    where
        Q: PartialEq + Eq + Clone + Display + Hash,
        T: PartialEq + Eq + Clone + Display + Default,
    {
        for x in 1..level {
            if is_within.0 && x == is_within.1 {
                write!(f, "│   ")?;
            } else {
                write!(f, "    ")?;
            }
        }
        if level > 0 {
            if is_last_child {
                writeln!(f, "└── {}", node)?;
            } else {
                writeln!(f, "├── {}", node)?;
            }
        } else {
            writeln!(f, "{}", node)?;
        }
        let children = node.get_children_ids();
        let children_count = children.len();
        for (index, child) in children.iter().enumerate() {
            let child = tree
                .get_node_by_id(child)
                .ok_or(NodeNotFound(child.to_string()))?;
            let last_item = index == children_count - 1;
            // Check if parent was last child
            let is_parent_last_item = if let Some(parent) = node.get_parent_id() {
                let parent = tree
                    .get_node_by_id(&parent)
                    .ok_or(NodeNotFound(parent.to_string()))?;
                let children_ids = parent.get_children_ids();
                let last_child = children_ids.last();
                last_child.is_some() && last_child.unwrap() == &node.get_node_id()
            } else {
                true
            };
            if !is_within.0 {
                is_within.0 = !is_parent_last_item;
                is_within.1 = level;
            } else {
                is_within.1 = if level > 1 && level <= 3 {
                    level - 1
                } else if level > 3 {
                    level - 2
                } else {
                    level
                };
            }
            Tree::print_tree(tree, f, &child, level + 1, (is_within.0, is_within.1), last_item)?;
        }
        Ok(())
    }
}

impl<Q, T> Default for Tree<Q, T>
where
    Q: PartialEq + Eq + Clone,
    T: PartialEq + Eq + Clone,
{
    /// Create a new tree with no nodes.
    fn default() -> Self {
        Tree {
            name:  None,
            nodes: Nodes::default(),
        }
    }
}

impl<Q, T> Display for Tree<Q, T>
where
    Q: PartialEq + Eq + Clone + Display + Hash + Ord,
    T: PartialEq + Eq + Clone + Display + Default,
{
    /// Print the tree.
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some(name) = &self.name {
            writeln!(f, "{}", name)?;
            writeln!(f, "{}", name.clone().chars().map(|_| "*").collect::<String>())?;
        }
        let node = self.get_root_node().ok_or(FmtError)?;
        Tree::print_tree(self, f, &node, 0, (false, 0), true).map_err(|_| FmtError)?;
        Ok(())
    }
}

impl<Q, T> Drop for Tree<Q, T>
where
    Q: PartialEq + Eq + Clone,
    T: PartialEq + Eq + Clone,
{
    /// Drop the tree.
    #[doc(hidden)]
    fn drop(&mut self) {
        self.nodes.clear();
    }
}

#[cfg(test)]
mod tests {
    use std::hash::DefaultHasher;

    use super::*;

    #[test]
    fn test_tree_new() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        assert_eq!(tree.nodes.len(), 0);
    }

    #[test]
    fn test_tree_add_node() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_id = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        assert_eq!(tree.nodes.len(), 1);
        assert_eq!(node_id, 1);
        let node_id_2 = tree.add_node(Node::new(2, Some(3)), Some(&1)).unwrap();
        assert_eq!(tree.nodes.len(), 2);
        assert_eq!(node_id_2, 2);
        let node_2 = tree.get_node_by_id(&2).unwrap();
        assert_eq!(node_2.get_parent_id().unwrap(), 1);
    }

    #[test]
    fn test_tree_add_more_than_one_root_node() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let result = tree.add_node(Node::new(1, Some(2)), None);
        assert!(result.is_ok());
        let node_id = result.unwrap();
        assert_eq!(tree.nodes.len(), 1);
        assert_eq!(node_id, 1);
        let result = tree.add_node(Node::new(2, Some(3)), None);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), RootNodeAlreadyPresent);
    }

    #[test]
    fn test_tree_get_node() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node = Node::new(1, Some(2));
        tree.add_node(node.clone(), None).unwrap();
        assert_eq!(tree.get_node_by_id(&1), Some(node));
        assert_eq!(tree.get_node_by_id(&2), None);
    }

    #[test]
    fn test_tree_get_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        assert_eq!(tree.get_node_by_id(&1), None);
    }

    #[test]
    fn test_tree_get_nodes() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node = Node::new(1, Some(2));
        tree.add_node(node.clone(), None).unwrap();
        assert_eq!(tree.get_nodes().len(), 1);
    }

    #[test]
    fn test_tree_get_root_node() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node = Node::new(1, Some(2));
        tree.add_node(node.clone(), None).unwrap();
        assert_eq!(tree.get_root_node(), Some(node));
    }

    #[test]
    fn test_tree_get_node_height() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        assert_eq!(tree.get_node_height(&node_1).unwrap(), 2);
        assert_eq!(tree.get_node_height(&node_2).unwrap(), 1);
        assert_eq!(tree.get_node_height(&node_3).unwrap(), 0);
    }

    #[test]
    fn test_tree_get_node_height_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_node_height(&1);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_get_node_depth() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        assert_eq!(tree.get_node_depth(&node_3).unwrap(), 2);
        assert_eq!(tree.get_node_depth(&node_2).unwrap(), 1);
        assert_eq!(tree.get_node_depth(&node_1).unwrap(), 0);
    }

    #[test]
    fn test_tree_get_ancestor_ids() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        let node_4 = tree.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        assert_eq!(tree.get_ancestor_ids(&node_4).unwrap(), vec![2, 1]);
        assert_eq!(tree.get_ancestor_ids(&node_3).unwrap(), vec![2, 1]);
        assert_eq!(tree.get_ancestor_ids(&node_2).unwrap(), vec![1]);
        assert_eq!(tree.get_ancestor_ids(&node_1).unwrap(), Vec::<i32>::new());
    }

    #[test]
    fn test_tree_get_node_ancestor_ids_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_ancestor_ids(&1);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_get_node_depth_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_node_depth(&1);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_get_height() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        assert_eq!(tree.get_height().unwrap(), 2);
    }

    #[test]
    fn test_tree_get_height_no_root_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_height();
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), InvalidOperation("Tree has no root node".to_string()));
    }

    #[test]
    fn test_tree_get_node_degree() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_1)).unwrap();
        assert_eq!(tree.get_node_degree(&node_1).unwrap(), 2);
        assert_eq!(tree.get_node_degree(&node_2).unwrap(), 0);
        assert_eq!(tree.get_node_degree(&node_3).unwrap(), 0);
    }

    #[test]
    fn test_tree_get_node_degree_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_node_degree(&1);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_remove_node() -> Result<()> {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node = Node::new(1, Some(2));
        tree.add_node(node.clone(), None)?;
        let node_2 = Node::new(2, Some(3));
        tree.add_node(node_2.clone(), Some(&1))?;
        let node_3 = Node::new(3, Some(6));
        tree.add_node(node_3.clone(), Some(&2))?;
        tree.remove_node(&2, NodeRemovalStrategy::RetainChildren)?;
        assert_eq!(tree.get_nodes().len(), 2);
        let node_4 = Node::new(4, Some(5));
        let node_5 = Node::new(5, Some(12));
        tree.add_node(node_4.clone(), Some(&3))?;
        tree.add_node(node_5.clone(), Some(&3))?;
        tree.remove_node(&3, NodeRemovalStrategy::RemoveNodeAndChildren)?;
        assert_eq!(tree.get_nodes().len(), 1);
        Ok(())
    }

    #[test]
    fn test_tree_remove_node_no_existent_node() {
        let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
        let result = tree.remove_node(&1, NodeRemovalStrategy::RetainChildren);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_remove_node_no_root_node() {
        let mut tree: Tree<i32, i32> = Tree::new(Some("Sample Tree"));
        tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let result = tree.remove_node(&1, NodeRemovalStrategy::RetainChildren);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            InvalidOperation("Cannot remove root node with RetainChildren strategy".to_string())
        );
    }

    #[test]
    fn test_tree_get_subsection() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node = Node::new(1, Some(2));
        tree.add_node(node.clone(), None).unwrap();
        let node_2 = Node::new(2, Some(3));
        tree.add_node(node_2.clone(), Some(&1)).unwrap();
        let node_3 = Node::new(3, Some(6));
        tree.add_node(node_3.clone(), Some(&2)).unwrap();
        let node_4 = Node::new(4, Some(5));
        tree.add_node(node_4.clone(), Some(&2)).unwrap();
        let node_5 = Node::new(5, Some(6));
        tree.add_node(node_5.clone(), Some(&3)).unwrap();
        let subsection = tree.get_subtree(&2, None).unwrap();
        assert_eq!(subsection.get_name(), Some("2"));
        assert_eq!(subsection.get_nodes().len(), 4);
        let subsection = tree.get_subtree(&2, Some(0)).unwrap();
        assert_eq!(subsection.get_nodes().len(), 1);
        let subsection = tree.get_subtree(&2, Some(1)).unwrap();
        assert_eq!(subsection.get_nodes().len(), 3);
    }

    #[test]
    fn test_tree_get_subsection_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_subtree(&1, None);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn get_siblings() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        tree.add_node(Node::new(3, Some(6)), Some(&node_1)).unwrap();
        tree.add_node(Node::new(4, Some(7)), Some(&node_1)).unwrap();
        let siblings = tree.get_sibling_ids(&node_2, false).unwrap();
        assert_eq!(siblings.len(), 2);
        let siblings = tree.get_sibling_ids(&node_2, true).unwrap();
        assert_eq!(siblings.len(), 3);
    }

    #[test]
    fn test_tree_get_siblings_no_existent_node() {
        let tree = Tree::<u32, u32>::new(Some("Sample Tree"));
        let result = tree.get_sibling_ids(&1, false);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_add_subsection() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_id = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let mut subtree = SubTree::new(Some("Sample Tree"));
        let node_2 = subtree.add_node(Node::new(2, Some(3)), None).unwrap();
        subtree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        tree.add_subtree(&node_id, subtree).unwrap();
        assert_eq!(tree.get_nodes().len(), 3);
    }

    #[test]
    fn test_tree_add_subsection_no_attaching_node() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let mut subtree = SubTree::new(Some("Sample Tree"));
        let node_2 = subtree.add_node(Node::new(2, Some(3)), None).unwrap();
        subtree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        let result = tree.add_subtree(&1, subtree);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("1".to_string()));
    }

    #[test]
    fn test_tree_add_subsection_with_no_root_node() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_id = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let mut subtree = SubTree::new(Some("Sample Tree"));
        let node_2 = Node::new(2, Some(3));
        let result = subtree.add_node(Node::new(3, Some(3)), Some(&node_2.get_node_id()));
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), NodeNotFound("2".to_string()));
        let result = tree.add_subtree(&node_id, subtree);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), InvalidOperation("Subtree has no root node.".to_string()));
    }

    #[test]
    fn test_tree_display() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        tree.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        tree.add_node(Node::new(5, Some(6)), Some(&node_3)).unwrap();
        let expected_str =
            "Sample Tree\n***********\n1: 2\n└── 2: 3\n    ├── 3: 6\n    │   └── 5: 6\n    └── 4: 5\n";
        assert_eq!(tree.to_string(), expected_str);
    }

    #[test]
    fn compare_tree() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        tree.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        tree.add_node(Node::new(5, Some(6)), Some(&node_3)).unwrap();
        let mut tree_2 = Tree::new(Some("Sample Tree"));
        let node_1 = tree_2.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree_2.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree_2.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        tree_2.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        tree_2.add_node(Node::new(5, Some(6)), Some(&node_3)).unwrap();
        assert_eq!(tree, tree_2);
        let tree_3 = Tree::new(Some("Sample Tree"));
        assert_ne!(tree, tree_3);
    }

    #[test]
    fn test_tree_traverse() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_1)).unwrap();
        let node_4 = tree.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        let node_5 = tree.add_node(Node::new(5, Some(6)), Some(&node_2)).unwrap();
        let node_6 = tree.add_node(Node::new(6, Some(7)), Some(&node_3)).unwrap();
        let preorder_nodes = tree.traverse(&node_1, TraversalStrategy::PreOrder).unwrap();
        let expected_preorder = vec![node_1, node_2, node_4, node_5, node_3, node_6];
        assert_eq!(preorder_nodes, expected_preorder);

        let in_order_nodes = tree.traverse(&node_1, TraversalStrategy::InOrder).unwrap();
        let expected_in_order = vec![node_4, node_2, node_5, node_1, node_3, node_6];
        assert_eq!(in_order_nodes, expected_in_order);

        let post_order_nodes = tree.traverse(&node_1, TraversalStrategy::PostOrder).unwrap();
        let expected_post_order = vec![node_4, node_5, node_2, node_6, node_3, node_1];
        assert_eq!(post_order_nodes, expected_post_order);
    }

    #[allow(deprecated)] // This is solely for testing hashing in no_std.
    #[test]
    fn test_hashing() {
        let mut tree = Tree::new(Some("Sample Tree"));
        let node_1 = tree.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        tree.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        tree.add_node(Node::new(5, Some(6)), Some(&node_3)).unwrap();
        let mut tree_2 = Tree::new(Some("Sample Tree"));
        let node_1 = tree_2.add_node(Node::new(1, Some(2)), None).unwrap();
        let node_2 = tree_2.add_node(Node::new(2, Some(3)), Some(&node_1)).unwrap();
        let node_3 = tree_2.add_node(Node::new(3, Some(6)), Some(&node_2)).unwrap();
        tree_2.add_node(Node::new(4, Some(5)), Some(&node_2)).unwrap();
        tree_2.add_node(Node::new(5, Some(6)), Some(&node_3)).unwrap();
        assert_eq!(tree, tree_2);
        let mut hasher = DefaultHasher::new();
        tree.hash(&mut hasher);
        let tree_hash = hasher.finish();
        let mut hasher = DefaultHasher::new();
        tree_2.hash(&mut hasher);
        let tree_2_hash = hasher.finish();
        assert_eq!(tree_hash, tree_2_hash);
    }
}
