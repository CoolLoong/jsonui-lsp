use super::lib::*;

#[derive(thiserror::Error, Clone, PartialEq, Debug)]
pub enum Error {
    /// The root node is already present in the tree.
    #[error("Root node is already present in the tree.")]
    RootNodeAlreadyPresent,

    /// An invalid operation was performed on the tree.
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),

    /// The node was not found in the tree.
    #[error("Node not found: {0}")]
    NodeNotFound(String),

    /// An error occurred while formatting the output.
    #[error("Formatting error: {0}")]
    FmtError(#[from] FmtError), // Assuming FmtError implements std::error::Error
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_fmt() {
        let err = Error::InvalidOperation("Invalid operation".to_string());
        assert_eq!(r#"Invalid operation: Invalid operation"#, format!("{}", err));
    }

    #[test]
    fn test_error_fmt_root_node_already_present() {
        let err = Error::RootNodeAlreadyPresent;
        assert_eq!("Root node is already present in the tree.", format!("{}", err),);
    }

    #[test]
    fn test_error_from_fmt_error() {
        let err = Error::FmtError(FmtError);
        assert_eq!(Error::from(FmtError), err);
    }
}
