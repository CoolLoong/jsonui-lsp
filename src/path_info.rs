use std::cell::RefCell;
use std::collections::VecDeque;

/// A structure to track the path hierarchy of each `Value` in an AST (Abstract Syntax Tree).
///
/// The `PathInfo` structure uses a stack to calculate and store the index of
/// each value within the AST. The stack helps in maintaining the path traversal.
///
/// # Fields
/// - `stack`: A `RefCell` containing a `VecDeque` of `usize`, which represents the stack of index
///   encountered during AST traversal. The stack allows pushing and popping index as you traverse deeper
///   or move back in the tree.
/// - `current`: A `RefCell` containing a `usize` that stores the current `Value` index in the AST.
pub(crate) struct PathInfo {
    stack:   RefCell<VecDeque<usize>>,
    current: RefCell<usize>,
}
impl PathInfo {
    pub(crate) fn new() -> Self {
        PathInfo {
            stack:   RefCell::new(VecDeque::new()),
            current: RefCell::new(0),
        }
    }

    /// Returns the index path of the current `Value` in the AST (Abstract Syntax Tree).
    ///
    /// The AST is represented as a nested array, and this function provides a way to track
    /// the exact location of the current `Value` in this tree structure by returning an
    /// array of indices. Each index in the returned array corresponds to the position of
    /// the `Value` in each level of the tree.
    ///
    /// For example, given an AST structured as follows:
    ///
    /// ```
    /// [[[1, 2, 3], [4, 5]], [6, 7]]
    /// ```
    ///
    /// If the current `Value` is `5`, the function would return `[0, 1, 1]`
    pub(crate) fn gen_path(&self) -> Vec<usize> {
        let stack = self.stack.borrow();
        let mut current = self.current.borrow_mut();
        if !stack.is_empty() {
            let mut result: Vec<usize> = Vec::with_capacity(stack.len() + 1);
            result.extend(stack.iter());
            let path = *current;
            *current += 1;
            result.push(path);
            result
        } else {
            let path = *current;
            *current += 1;
            vec![path]
        }
    }

    /// Exits the current subtree and restores the current index from the `back_index`
    pub(crate) fn pop(&self, back_index: usize) {
        *self.current.borrow_mut() = back_index;
        self.stack.borrow_mut().pop_back();
    }

    /// This function is used to signify traversal deeper into the AST, where a new index of nesting
    /// is entered. The current index is pushed onto the stack, and the `current` index is reset to `0`
    /// to represent the start of a new subtree.
    pub(crate) fn push(&self) -> Vec<usize> {
        let index_value = { *self.current.borrow_mut() };
        let path = self.gen_path();
        let mut current = self.current.borrow_mut();
        let mut stack = self.stack.borrow_mut();
        *current = 0;
        stack.push_back(index_value);
        path
    }
}
