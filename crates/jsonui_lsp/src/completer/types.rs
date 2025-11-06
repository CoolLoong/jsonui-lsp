use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::sync::Arc;

use tower_lsp::lsp_types;

/// Control identifier: (namespace, name, extend(namespace, name))
pub(crate) type ControlIdNoParent = (Arc<str>, Arc<str>);
pub(crate) type ControlId = (Arc<str>, Arc<str>, Option<(Arc<str>, Arc<str>)>);

/// Symbol type for lightweight indexing
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) enum SymbolType {
    Control,
    Variable,
    Color,
}

/// Lightweight symbol reference for indexing
/// Stores (document_hash, control_id, symbol_type, range) instead of full Symbol
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct SymbolRef {
    pub(crate) doc_hash: u64,
    pub(crate) control_id: ControlId,
    pub(crate) symbol_type: SymbolType,
    pub(crate) range: HashRange,
}

/// Color value representation (string or RGB array)
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ColorValue {
    String(String),
    Vec(Vec<f64>),
}
impl Eq for ColorValue {}
impl Hash for ColorValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ColorValue::String(v) => v.hash(state),
            ColorValue::Vec(v) => {
                for &num in v {
                    let bits = num.to_bits();
                    bits.hash(state);
                }
            }
        }
    }
}

/// Metadata about a symbol
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct MetaData {
    pub(crate) is_declare: bool,
}

/// Color symbol
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct Color {
    pub(crate) parent: ControlId,
    pub(crate) range: HashRange,
    pub(crate) value: ColorValue,
}

/// Control symbol
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct Control {
    pub(crate) id: ControlId,
    pub(crate) range: HashRange,
    pub(crate) parent: Option<ControlId>,
}
impl Display for Control {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Control {{ id: {:?}, range: ({:?}), parent: ({:?}) }}",
            self.id, self.range, self.parent
        )
    }
}

/// Variable symbol
#[derive(Debug, PartialEq, Eq, Hash, Clone, Default)]
pub(crate) struct Variable {
    pub(crate) parent: ControlId,
    pub(crate) range: HashRange,
    pub(crate) value: Arc<str>,
}

/// Vanilla control definition from JSON schema
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct VanillaControlDefine {
    pub(crate) name: ControlId,
    pub(crate) type_n: Arc<str>,
    pub(crate) variables: HashSet<Arc<str>>,
}

/// A symbol (control, variable, or color)
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Symbol {
    Control(Control),
    Variable(Variable),
    Color(Color),
}
impl Symbol {
    /// Get the control ID for this symbol
    pub(crate) fn id(&self) -> ControlId {
        match self {
            Symbol::Control(c) => c.id.clone(),
            Symbol::Variable(c) => c.parent.clone(),
            Symbol::Color(c) => c.parent.clone(),
        }
    }

    /// Get the range for this symbol
    pub(crate) fn range(&self) -> HashRange {
        match self {
            Symbol::Control(c) => c.range,
            Symbol::Variable(c) => c.range,
            Symbol::Color(c) => c.range,
        }
    }

    /// Get the symbol type
    pub(crate) fn symbol_type(&self) -> SymbolType {
        match self {
            Symbol::Control(_) => SymbolType::Control,
            Symbol::Variable(_) => SymbolType::Variable,
            Symbol::Color(_) => SymbolType::Color,
        }
    }
}

/// Position in a text document (zero-based line and character offset)
///
/// This is a hashable version of lsp_types::Position for use in sets/maps.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Hash)]
pub struct HashPosition {
    /// Line position in a document (zero-based)
    pub line: u32,
    /// Character offset on a line in a document (zero-based)
    pub character: u32,
}
impl From<HashPosition> for lsp_types::Position {
    fn from(val: HashPosition) -> Self {
        lsp_types::Position {
            line: val.line,
            character: val.character,
        }
    }
}

/// A range in a text document (zero-based start and end positions)
///
/// This is a hashable version of lsp_types::Range for use in sets/maps.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Hash)]
pub struct HashRange {
    /// The range's start position
    pub start: HashPosition,

    /// The range's end position
    pub end: HashPosition,
}
impl HashRange {
    /// Check if this range contains another range
    pub(crate) fn contains(&self, other: &HashRange) -> bool {
        self.start.line <= other.start.line
            && self.end.line >= other.end.line
            && self.start.character <= other.start.character
            && self.end.character >= other.end.character
    }
}
impl PartialOrd for HashRange {
    fn partial_cmp(&self, other: &HashRange) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for HashRange {
    fn cmp(&self, other: &HashRange) -> Ordering {
        let self_size = (self.end.line - self.start.line, self.end.character - self.start.character);
        let other_size =
            (other.end.line - other.start.line, other.end.character - other.start.character);
        self_size.cmp(&other_size)
    }
}
impl From<HashRange> for lsp_types::Range {
    fn from(val: HashRange) -> Self {
        lsp_types::Range {
            start: val.start.into(),
            end: val.end.into(),
        }
    }
}
