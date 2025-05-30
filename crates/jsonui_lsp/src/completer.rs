use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;
use std::vec;

use dashmap::DashMap;
use lasso::Spur;
use log::trace;
use tokio::sync::Mutex as TokioMutex;
use tower_lsp::lsp_types;
use tree_sitter::Node;

use crate::museair::{BfastDashMap, BfastDashSet, BfastHash, BfastHashMap, BfastMultiMap};
use crate::parser::prelude::*;
use crate::towerlsp::*;
use crate::utils::hash_url;
use crate::{Config, StringPool};

// (namespace, name, extend(namespace, name))
pub(crate) type ControlId = (Arc<str>, Arc<str>, Option<(Arc<str>, Arc<str>)>);

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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct MetaData {
    pub(crate) is_declare: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct Color {
    pub(crate) parent: ControlId,
    pub(crate) range:  HashRange,
    pub(crate) value:  ColorValue,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct Control {
    pub(crate) id:     ControlId,
    pub(crate) range:  HashRange,
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Default)]
pub(crate) struct Variable {
    pub(crate) parent: ControlId,
    pub(crate) range:  HashRange,
    pub(crate) value:  Arc<str>,
}
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub(crate) struct VanillaControlDefine {
    pub(crate) name:      ControlId,
    pub(crate) type_n:    Spur,
    pub(crate) variables: HashSet<Spur>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Symbol {
    Control(Control),
    Variable(Variable),
    Color(Color),
}
impl Symbol {
    pub(crate) fn id(&self) -> ControlId {
        match self {
            Symbol::Control(c) => c.id.clone(),
            Symbol::Variable(c) => c.parent.clone(),
            Symbol::Color(c) => c.parent.clone(),
        }
    }

    pub(crate) fn range(&self) -> HashRange {
        match self {
            Symbol::Control(c) => c.range,
            Symbol::Variable(c) => c.range,
            Symbol::Color(c) => c.range,
        }
    }
}

/// Position in a text document expressed as zero-based line and character offset.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Hash)]
pub struct HashPosition {
    /// Line position in a document (zero-based).
    pub line: u32,

    /// Character offset on a line in a document (zero-based).
    pub character: u32,
}
impl Into<lsp_types::Position> for HashPosition {
    fn into(self) -> lsp_types::Position {
        lsp_types::Position {
            line:      self.line,
            character: self.character,
        }
    }
}

/// A range in a text document expressed as (zero-based) start and end positions.
/// A range is comparable to a selection in an editor. Therefore the end position is exclusive.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Hash)]
pub struct HashRange {
    /// The range's start position.
    pub start: HashPosition,

    /// The range's end position.
    pub end: HashPosition,
}
impl HashRange {
    fn contains(&self, other: &HashRange) -> bool {
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

impl Into<lsp_types::Range> for HashRange {
    fn into(self) -> lsp_types::Range {
        lsp_types::Range {
            start: self.start.into(),
            end:   self.end.into(),
        }
    }
}

pub(crate) struct Completer {
    // hash_url -> DocumentParser
    parsers:                BfastDashMap<u64, DocumentParser>,
    // namespace:control_name -> definetion
    vanilla_controls_table: BfastHashMap<(Arc<str>, Arc<str>), VanillaControlDefine>,
    jsonui_define:          BfastHashMap<String, Value>,
    symbol_table:           BfastDashMap<u64, BfastMultiMap<ControlId, Arc<Symbol>>>,
    definitions:            BfastDashSet<Arc<Symbol>>,
    references:             BfastDashSet<Arc<Symbol>>,
    namespace_to_url:       BfastDashMap<Arc<str>, Url>,
}

impl Completer {
    pub(crate) fn new(
        vanilla_controls_table: BfastHashMap<(Arc<str>, Arc<str>), VanillaControlDefine>,
        jsonui_define: BfastHashMap<String, Value>,
    ) -> Self {
        Completer {
            parsers: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
            vanilla_controls_table,
            jsonui_define,
            symbol_table: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
            definitions: BfastDashSet::default(),
            references: BfastDashSet::default(),
            namespace_to_url: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
        }
    }

    pub(crate) async fn complete(
        &self,
        url: Url,
        lang: Arc<TokioMutex<Config>>,
        params: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let url = hash_url(&url);
        let parser_ref = if let Some(parser) = self.parsers.get(&url) {
            parser
        } else {
            trace!("cant find parser for {}!", &url);
            return None;
        };
        let pos = params.text_document_position.position;
        let node = if let Some(node) = parser_ref.get_node_at_position(pos) {
            node
        } else {
            trace!("cant find node for {:?}", &pos);
            trace!("{:?}", *parser_ref);
            return None;
        };
        let quote_pos = Self::get_position_for_quote(&node);
        let parent = parser_ref.get_parent_pair_node(&node);
        // Early return for document nodes
        if parent.kind() == "document" {
            trace!("parent is document node, skip.");
            trace!("{:?}", *parser_ref);
            return None;
        }
        let (lang, append_suffix) = if let Ok(config) = lang.try_lock() {
            (config.lang.clone(), config.append_suffix)
        } else {
            trace!("cant find client lang and settings!");
            return None;
        };
        let char = params.context.as_ref()?.trigger_character.as_ref()?;
        let parents = parser_ref.get_parents(&node);
        let p1 = parents.first()?;
        let (before, after) = parser_ref.get_adjacent_nodes(&node);
        let [n1, n2] = [before.first(), before.get(1)];
        let [n3, n4] = [after.first(), after.get(1)];
        trace!(
            "|n1 {}| |n2 {}| |current {}| |n3 {}| |n4 {}| |p1 {}| |char '{:?}'|",
            n1.map_or("None".to_string(), |f| parser_ref.print_node(f)),
            n2.map_or("None".to_string(), |f| parser_ref.print_node(f)),
            parser_ref.print_node(&node),
            n3.map_or("None".to_string(), |f| parser_ref.print_node(f)),
            n4.map_or("None".to_string(), |f| parser_ref.print_node(f)),
            parser_ref.print_node(p1),
            char
        );
        let completion_type: u8 = match char.as_str() {
            "\"" => {
                if let Some(current_str) = parser_ref.get_string(&node)
                    && current_str == ""
                {
                    if self.is_pair_array(p1) {
                        if self.is_binding(&parser_ref, &node) {
                            if n2.map_or(false, |n| n.kind() == ":") {
                                3 // binding value completion
                            } else {
                                1 // binding type completion
                            }
                        } else {
                            trace!("Error 5");
                            255
                        }
                    } else if n2.map_or(false, |n| n.kind() == ":") {
                        2 // common value completion
                    } else {
                        0 // common type completion
                    }
                } else {
                    trace!("Error 6 current_str is {:?}", parser_ref.get_string(&node));
                    255
                }
            }
            ":" => {
                if self.is_pair_array(p1) {
                    if self.is_binding(&parser_ref, &node) {
                        if n2.map_or(false, |n| self.is_string_node(n)) {
                            3 // binding value completion
                        } else {
                            trace!("Error 4");
                            255
                        }
                    } else {
                        trace!("Error 3");
                        255
                    }
                } else if n2.map_or(false, |n| self.is_string_node(n)) {
                    2 // common value completion
                } else {
                    trace!("Error 2");
                    255
                }
            }
            _ => {
                trace!("Error 1");
                255
            }
        };
        drop(parser_ref);
        if completion_type != 255 {
            trace!("completion_type {}", completion_type);
        }
        match completion_type {
            0 => {
                self.create_common_type_completion(url, pos, quote_pos, lang.clone())
                    .await
            }
            1 => self.create_binding_type_completion(url, pos, quote_pos, lang.clone()),
            2 | 3 => {
                self.create_value_completion(completion_type, url, pos, quote_pos, lang, append_suffix)
            }
            _ => None,
        }
    }

    pub(crate) fn complete_color(&self, url: Url) -> Option<Vec<ColorInformation>> {
        let url = hash_url(&url);
        let symbol_table = self.symbol_table.get(&url)?;
        let colors: Vec<ColorInformation> = symbol_table
            .flat_iter()
            .filter_map(|(_, f)| match f.deref() {
                Symbol::Color(v) => Some(v),
                _ => None,
            })
            .map(|c| Self::create_color_information(c))
            .collect();
        if colors.is_empty() {
            None
        } else {
            Some(colors)
        }
    }

    pub(crate) async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<(GotoDefinitionResponse, bool)> {
        let url = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let hash_url = &hash_url(url);
        let parser_ref = self.parsers.get(&hash_url)?;
        let symbol_table = self.symbol_table.get(hash_url)?;

        let node = parser_ref.get_node_at_position(pos);
        let namespace = parser_ref.namespace();
        if let Some(node) = node {
            let range = Self::node_range(&node);
            let mut containing_symbols: Vec<_> = symbol_table
                .flat_iter()
                .filter(|f| f.0 .0 == namespace)
                .filter_map(|f| {
                    let symbol = f.1;
                    if symbol.range().contains(&range) {
                        Some(symbol)
                    } else {
                        None
                    }
                })
                .collect();
            let symbol = if containing_symbols.is_empty() {
                trace!("cant find symbol for current node.");
                None
            } else {
                containing_symbols.sort_by(|a, b| {
                    let range1 = a.range();
                    let range2 = b.range();
                    range1.cmp(&range2)
                });
                Some(containing_symbols[0])
            }?;
            let symbol = symbol.to_owned();
            let current_np = parser_ref.namespace();
            let symbol_np = symbol.id().2.clone()?.0;
            let is_current_file = current_np == symbol_np;
            drop(parser_ref);
            drop(symbol_table);
            let loc = self.find_definition(symbol.deref()).await?;
            Some((GotoDefinitionResponse::Scalar(loc), is_current_file))
        } else {
            None
        }
    }

    pub(crate) async fn references(&self, params: &ReferenceParams) -> Option<Vec<Location>> {
        let url = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let hash_url = &hash_url(url);
        let parser_ref = self.parsers.get(hash_url)?;
        let symbol_table = self.symbol_table.get(hash_url)?;
        let node = parser_ref.get_node_at_position(pos);
        let namespace = parser_ref.namespace();
        if let Some(node) = node {
            let range = Self::node_range(&node);
            let mut containing_symbols: Vec<_> = symbol_table
                .flat_iter()
                .filter(|f| f.0 .0 == namespace)
                .filter_map(|f| {
                    let symbol = f.1;
                    if symbol.range().contains(&range) {
                        Some(symbol)
                    } else {
                        None
                    }
                })
                .collect();
            let symbol = if containing_symbols.is_empty() {
                None
            } else {
                containing_symbols.sort_by(|a, b| {
                    let range1 = a.range();
                    let range2 = b.range();
                    range1.cmp(&range2)
                });
                Some(containing_symbols[0])
            }?;
            let symbol = symbol.to_owned();
            drop(parser_ref);
            drop(symbol_table);
            return Some(self.find_references(symbol.deref()));
        }
        None
    }

    pub(crate) async fn did_change(&self, url: Url, params: &DidChangeTextDocumentParams) {
        let hash_url = hash_url(&url);
        let parser = self.parsers.get_mut(&hash_url);
        if let Some(mut parser) = parser {
            let changes = &params.content_changes;
            for change in changes {
                parser.edit(change);
            }
            let namespace = parser.namespace();
            if namespace.deref() != "Unknown" {
                self.index_document(&parser);
            }
        }
    }

    pub(crate) async fn did_open(&self, url: &Url, content: &str) {
        let hash_url = hash_url(url);
        let mut new_parser: bool = false;
        let parser = self.parsers.entry(hash_url).or_insert_with(|| {
            new_parser = true;
            DocumentParser::new(hash_url, &content)
        });
        let namespace = parser.namespace();
        if namespace.deref() != "Unknown" {
            if new_parser {
                trace!("Init parser, url({}) hash_url({})", url, hash_url);
            }
            self.namespace_to_url
                .entry(parser.namespace())
                .or_insert(url.clone());
            self.index_document(&parser);
        }
    }

    pub(crate) fn did_close(&self, url: &Url) {
        let hash_url = hash_url(url);
        trace!("Close parser, url({}) hash_url({})", url, hash_url);
        match self.parsers.remove(&hash_url) {
            Some((key, parser)) => {
                if let Some((_, mut symbol_table)) = self.symbol_table.remove(&key) {
                    let definitions = &self.definitions;
                    let references = &self.references;
                    symbol_table.flat_iter().for_each(|f| {
                        definitions.remove(f.1);
                        references.remove(f.1);
                    });
                    symbol_table.clear();
                }
                drop(parser);
            }
            None => {
                trace!("Failed to acquire parser lock for {:?}, skipping cleanup", url);
            }
        }
    }

    pub(crate) async fn did_rename(&self, o_url: Url, new_url: Url) {
        let o_url = hash_url(&o_url);
        let n_url = hash_url(&new_url);
        if let Some((_, mut parser)) = self.parsers.remove(&o_url) {
            let namespace = parser.namespace();
            if namespace.deref() != "Unknown" {
                // update namespace_to_url
                if let Some((k, _)) = self.namespace_to_url.remove(&parser.namespace()) {
                    self.namespace_to_url.insert(k, new_url);
                }
                parser.url = n_url;
                self.parsers.insert(n_url, parser);

                if let Some((_, symbols)) = self.symbol_table.remove(&o_url) {
                    self.symbol_table.insert(n_url, symbols);
                }
            }
        }
    }

    async fn find_definition(&self, symbol: &Symbol) -> Option<Location> {
        match symbol {
            Symbol::Control(c) => {
                let extend = c.id.2.clone()?;
                let url = self.get_url(extend.0.clone())?;
                self.get_or_create_parser(&url, |_| {}).await;

                self.definitions
                    .iter()
                    .filter(|f| matches!(f.deref().deref(), Symbol::Control(_)))
                    .filter_map(|f| {
                        let target = f.id();
                        if target.0 == extend.0 && target.1 == extend.1 {
                            let url = self.get_url(target.0);
                            if let Some(url) = url {
                                Some(Location {
                                    uri:   url,
                                    range: f.range().into(),
                                })
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .last()
            }
            _ => None,
        }
    }

    fn find_references(&self, symbol: &Symbol) -> Vec<Location> {
        match symbol {
            Symbol::Control(c) => {
                let namespace = c.id.0.clone();
                let control_name = c.id.1.clone();
                self.references
                    .iter()
                    .filter(|f| matches!(f.deref().deref(), Symbol::Control(_)))
                    .filter_map(|f| {
                        let target = f.id();
                        if let Some(extend) = target.2 {
                            if extend.0 == namespace && extend.1 == control_name {
                                let url = self.get_url(target.0);
                                if let Some(url) = url {
                                    trace!("{:?} | {:?}", c, extend);
                                    Some(Location {
                                        uri:   url,
                                        range: f.range().into(),
                                    })
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect()
            }
            _ => vec![],
        }
    }

    fn get_url(&self, namespace: Arc<str>) -> Option<Url> {
        let r = self.namespace_to_url.get(&namespace)?;
        Some(r.clone())
    }

    fn get_position_for_quote(node: &Node) -> Position {
        let pos = Self::node_range(node);
        Position {
            line:      pos.start.line,
            character: (pos.start.character + pos.end.character) / 2,
        }
    }

    fn create_color_information(color: &Color) -> ColorInformation {
        match &color.value {
            ColorValue::String(v) => match v.as_str() {
                "white" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   1.0,
                        green: 1.0,
                        blue:  1.0,
                        alpha: 1.0,
                    },
                },
                "silver" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.776,
                        green: 0.776,
                        blue:  0.776,
                        alpha: 1.0,
                    },
                },
                "gray grey" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.333,
                        green: 0.333,
                        blue:  0.333,
                        alpha: 1.0,
                    },
                },
                "black" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.0,
                        green: 0.0,
                        blue:  0.0,
                        alpha: 1.0,
                    },
                },
                "red" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   1.0,
                        green: 0.333,
                        blue:  0.333,
                        alpha: 1.0,
                    },
                },
                "green" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.333,
                        green: 1.0,
                        blue:  0.333,
                        alpha: 1.0,
                    },
                },
                "yellow" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   1.0,
                        green: 1.0,
                        blue:  0.333,
                        alpha: 1.0,
                    },
                },
                "brown" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.706,
                        green: 0.408,
                        blue:  0.302,
                        alpha: 1.0,
                    },
                },
                "cyan" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.0,
                        green: 0.667,
                        blue:  0.667,
                        alpha: 1.0,
                    },
                },
                "blue" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   0.333,
                        green: 0.333,
                        blue:  1.0,
                        alpha: 1.0,
                    },
                },
                "orange" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   1.0,
                        green: 0.667,
                        blue:  0.0,
                        alpha: 1.0,
                    },
                },
                "purple" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   1.0,
                        green: 0.333,
                        blue:  1.0,
                        alpha: 1.0,
                    },
                },
                "nil" | _ => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red:   1.0,
                        green: 1.0,
                        blue:  1.0,
                        alpha: 0.0,
                    },
                },
            },
            ColorValue::Vec(v) => {
                if v.len() == 3 {
                    ColorInformation {
                        range: color.range.into(),
                        color: lsp_types::Color {
                            red:   f32::clamp(v[0] as f32, 0 as f32, 1 as f32),
                            green: f32::clamp(v[1] as f32, 0 as f32, 1 as f32),
                            blue:  f32::clamp(v[2] as f32, 0 as f32, 1 as f32),
                            alpha: 1.0,
                        },
                    }
                } else if v.len() == 4 {
                    ColorInformation {
                        range: color.range.into(),
                        color: lsp_types::Color {
                            red:   f32::clamp(v[0] as f32, 0 as f32, 1 as f32),
                            green: f32::clamp(v[1] as f32, 0 as f32, 1 as f32),
                            blue:  f32::clamp(v[2] as f32, 0 as f32, 1 as f32),
                            alpha: f32::clamp(v[3] as f32, 0 as f32, 1 as f32),
                        },
                    }
                } else {
                    ColorInformation {
                        range: color.range.into(),
                        color: lsp_types::Color {
                            red:   1.0,
                            green: 1.0,
                            blue:  1.0,
                            alpha: 0.0,
                        },
                    }
                }
            }
        }
    }

    fn is_string_node(&self, node: &Node) -> bool {
        matches!(node.kind(), STRING | STRING_CONTENT)
    }

    fn is_pair_array(&self, node: &Node) -> bool {
        let n1 = node.child(1);
        let n2 = node.child(2);
        n1.is_some() && n1.unwrap().kind() == ":" && n2.is_some() && n2.unwrap().kind() == ARRAY
    }

    fn is_binding(&self, parser: &DocumentParser, node: &Node) -> bool {
        let mut current = node.parent();
        while let Some(parent) = current {
            if !matches!(parent.kind(), "ERROR" | "pair") {
                current = Some(parent);
                break;
            }
            current = parent.parent();
        }
        current
            .and_then(|p| Some(p))
            .and_then(|parent| (matches!(parent.kind(), OBJECT)).then(|| parent))
            .and_then(|parent| parent.parent())
            .and_then(|pp| (pp.kind() == ARRAY).then(|| pp))
            .and_then(|pp| pp.parent())
            .and_then(|ppp| (ppp.kind() == "pair").then(|| ppp))
            .and_then(|ppp| ppp.named_child(0))
            .filter(|key| self.is_string_node(key))
            .and_then(|key| parser.string(key))
            .map_or(false, |key| key == "bindings")
    }

    // Helper to extract string values from JSON array
    fn extract_strings(value: Option<&Value>) -> Vec<String> {
        value
            .and_then(|value| Some(to_array_ref(value)))
            .map(|v| v.into_iter().filter_map(|v| Some(to_string(v))).collect())
            .unwrap_or_default()
    }

    // for key completion, there are three types:
    // - common key
    // - specific type key
    // - variables key
    async fn create_common_type_completion<'a>(
        &self,
        url: u64,
        pos: Position,
        quote_pos: Position,
        lang: Arc<str>,
    ) -> Option<Vec<CompletionItem>> {
        let parser_ref = self.parsers.get(&url)?;
        let node = parser_ref.get_node_at_position(pos)?;
        let parent = parser_ref.get_parent_pair_node(&node);
        let common_key = Self::extract_strings(self.jsonui_define.get("common"));
        // if not root node
        let type_key = {
            let key = parent.named_child(0).unwrap();
            let value = parent.named_child(1).unwrap();
            let type_n = parser_ref.field(value, "type");

            let control_id = parser_ref.string(key).unwrap();
            let control_id = split_control_name(control_id, parser_ref.namespace());
            if let Some(control_id) = control_id {
                let type_n = if let Some(type_n) = type_n {
                    Some(parser_ref.string(type_n).unwrap())
                } else {
                    if let Some(control_id) = control_id.2 {
                        drop(parser_ref);
                        self.find_control_type(control_id).await
                    } else {
                        None
                    }
                };
                let type_key = if let Some(type_n) = type_n {
                    Self::extract_strings(self.jsonui_define.get(type_n.as_str()))
                } else {
                    Vec::with_capacity(0)
                };
                type_key
            } else {
                Vec::with_capacity(0)
            }
        };

        let mut result = Vec::with_capacity(type_key.len() + common_key.len());
        let mut order = 0 as usize;
        common_key.into_iter().for_each(|k| {
            result.push(self.create_simple_completion_item(k, lang.clone(), quote_pos, order));
            order += 1;
        });
        type_key.into_iter().for_each(|k| {
            result.push(self.create_simple_completion_item(k, lang.clone(), quote_pos, order));
            order += 1;
        });
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    fn create_binding_type_completion(
        &self,
        url: u64,
        pos: Position,
        quote_pos: Position,
        lang: Arc<str>,
    ) -> Option<Vec<CompletionItem>> {
        let parser_ref = self.parsers.get(&url)?;
        let current = parser_ref.get_node_at_position(pos)?;

        let common_key = Self::extract_strings(self.jsonui_define.get("bindings_properties"));
        // if not root node
        let type_key = {
            let type_n = self
                .find_binding_type(&parser_ref, &current)
                .unwrap_or("global".to_string());
            Self::extract_strings(self.jsonui_define.get(type_n.as_str()))
        };
        drop(parser_ref);

        let mut result = Vec::with_capacity(type_key.len() + common_key.len());
        let mut order = 0 as usize;
        common_key.into_iter().for_each(|k| {
            result.push(self.create_simple_completion_item(k, lang.clone(), quote_pos, order));
            order += 1;
        });
        type_key.into_iter().for_each(|k| {
            result.push(self.create_simple_completion_item(k, lang.clone(), quote_pos, order));
            order += 1;
        });
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    // for value completion, there are two types:
    // - specific type value options
    // - variables reference
    fn create_value_completion(
        &self,
        completion_type: u8,
        url: u64,
        pos: Position,
        quote_pos: Position,
        lang: Arc<str>,
        append_suffix: bool,
    ) -> Option<Vec<CompletionItem>> {
        let parser_ref = self.parsers.get(&url)?;
        let symbol_table = self.symbol_table.get(&url)?;
        let node = parser_ref.get_node_at_position(pos)?;
        let parent = parser_ref.get_parent_pair_node(&node);
        let (before, after) = parser_ref.get_adjacent_nodes(&node);
        let [n1, n2] = [before.first(), before.get(1)];
        let [n3, n4] = [after.first(), after.get(1)];
        let pos = if completion_type == 2 { quote_pos } else { pos };
        // Determine current key
        let current_is_colon = n2.map_or(false, |n| n.kind() != ":");
        let key_node = if current_is_colon { n2 } else { n1 };
        let key = key_node
            .and_then(|n| parser_ref.get_string(n))
            .unwrap_or_default();

        // Get completion values from JSON definition
        let Some(values_def) = self.jsonui_define.get(&key) else {
            return None;
        };

        // Process suffix
        let suffix = if append_suffix {
            n3.filter(|n| n.kind() == ",")
                .is_none()
                .then_some(",")
                .unwrap_or("")
        } else {
            ""
        };

        // Get base values
        let mut values = to_object_ref(values_def)
            .and_then(|obj| obj.get("values"))
            .map(to_array_ref)
            .unwrap_or_default();

        // Add variables
        if let Some(key) = parent.named_child(0) {
            if let Some(control_name) = parser_ref.string(key) {
                let control_id = split_control_name(control_name, parser_ref.namespace());
                if let Some(control_id) = control_id {
                    let mut variables = Vec::new();
                    self.find_variable_key(&mut variables, &symbol_table, &control_id);
                    values.extend(variables.into_iter().map(Value::String));
                }
            }
        }
        drop(parser_ref);
        drop(symbol_table);

        // Process completions
        let completions = values
            .into_iter()
            .enumerate()
            .filter_map(|(i, v)| {
                let (insert_text_str, label, description, format, kind) = match v {
                    Value::Object(ref v) => (
                        v.get("insert_text").and_then(|v| Some(to_string_ref(v))),
                        v.get("label").and_then(|v| Some(to_string_ref(v))),
                        v.get("description")
                            .and_then(|desc| to_object_ref(desc))
                            .and_then(|desc| desc.get(lang.as_ref()).or(desc.get("en-us")))
                            .and_then(|v| Some(to_string_ref(v)))
                            .or(Some("jsonui-support")),
                        v.get("insert_text_format").and_then(|k| {
                            Self::from_number_to_insert_text_format(to_number_ref(k) as u64)
                        }),
                        v.get("kind").and_then(|k| {
                            Self::from_number_to_completion_item_kind(to_number_ref(k) as u64)
                        }),
                    ),
                    Value::String(ref s) => {
                        (Some(s.as_str()), Some(s.as_str()), Some("jsonui-support"), None, None)
                    }
                    _ => return None,
                };

                // Skip specific formats if not after colon
                if !current_is_colon
                    && matches!(format, Some(f) if
                        (f == InsertTextFormat::PLAIN_TEXT && kind.is_some()) ||
                        f == InsertTextFormat::SNIPPET
                    )
                {
                    trace!("Skip specific formats if not after colon");
                    return None;
                }

                // Prepare text edits
                let needs_quotes = current_is_colon && format.is_none();
                let (insert_text, text_edit) = if current_is_colon {
                    let text = insert_text_str.or(label).map(|t| {
                        if needs_quotes {
                            format!(" \"{t}\"{suffix}")
                        } else {
                            format!(" {t}{suffix}")
                        }
                    });
                    (text, None)
                } else if kind.is_none() {
                    let text = insert_text_str.or(label).map(|t| t.to_string());
                    let edit = text.as_ref().map(|t| {
                        CompletionTextEdit::Edit(TextEdit {
                            range:    Range {
                                start: pos,
                                end:   Position {
                                    line:      pos.line,
                                    character: pos.character + 1,
                                },
                            },
                            new_text: format!("{t}\"{suffix}"),
                        })
                    });
                    (text, edit)
                } else {
                    (None, None)
                };
                Some(CompletionItem {
                    label: label.unwrap_or("unknown").to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: description.map(str::to_string),
                        detail:      None,
                    }),
                    kind,
                    insert_text_format: format,
                    insert_text,
                    text_edit,
                    preselect: Some(true),
                    sort_text: Some(Self::number_to_sort_text(i, 4)),
                    ..Default::default()
                })
            })
            .collect::<Vec<_>>();

        (!completions.is_empty()).then_some(completions)
    }

    fn find_binding_type(&self, parser: &DocumentParser, node: &Node) -> Option<String> {
        let mut current = node.parent();
        while let Some(parent) = current {
            if !matches!(parent.kind(), "ERROR" | "pair") {
                current = Some(parent);
                break;
            }
            current = parent.parent();
        }
        let current = current?;
        current
            .named_children(&mut current.walk())
            .filter(|child| child.kind() == "pair")
            .find_map(|pair| {
                pair.named_child(0)
                    .filter(|key| self.is_string_node(key))
                    .and_then(|key| parser.string(key))
                    .and_then(|key| {
                        (key == "binding_type")
                            .then(|| pair.named_child(1).and_then(|val| parser.string(val)))
                    })
                    .flatten()
            })
    }

    async fn find_control_type(&self, control_id: (Arc<str>, Arc<str>)) -> Option<String> {
        let pool = StringPool::global();
        let mut stack = vec![];
        let mut control_id = control_id;
        loop {
            // check type in vanilla_controls_table
            if let Some(v) = self.vanilla_controls_table.get(&control_id) {
                return Some(pool.resolve(&v.type_n).to_string());
            }
            // get all kvs for the control of namespace
            let kvs = self.get_all_kv(&control_id).await?;
            let namespace = control_id.0.clone();
            let control_name = control_id.1.clone();
            // try get type from kvs from control_name directly
            if let Some(type_n) = Self::try_get_type_directly(&kvs, control_name.clone()) {
                return Some(type_n);
            }
            // get type foreach kvs
            match Self::find_type_or_extend(kvs, namespace, control_name) {
                Ok(type_) => return Some(type_),
                Err(Some(extend)) => {
                    stack.push(control_id);
                    control_id = extend;
                    continue;
                }
                Err(None) => {}
            }
            control_id = stack.pop()?;
        }
    }

    async fn get_all_kv(&self, refer: &(Arc<str>, Arc<str>)) -> Option<BfastHashMap<String, Value>> {
        let url = { self.namespace_to_url.get(&refer.0)?.clone() };
        self.get_or_create_parser(&url, |parser| {
            let json_def = parser.hashmap();
            Some(json_def)
        })
        .await?
    }

    async fn get_or_create_parser<F, R>(&self, url: &Url, f: F) -> Option<R>
    where
        F: FnOnce(&DocumentParser) -> R,
    {
        let hash_url = hash_url(url);
        trace!("try get parser url({}) hash_url({})", url, hash_url);
        if let Some(parser) = self.parsers.get(&hash_url) {
            let result = f(parser.value());
            return Some(result);
        }

        trace!("try create parser");
        let path = match url.to_file_path() {
            Ok(p) => p,
            Err(_) => {
                trace!("Failed to convert URL to file path: {}", url);
                trace!("URL scheme: {}, host: {:?}, path: {}", url.scheme(), url.host(), url.path());
                return None;
            }
        };

        let content = match tokio::fs::read_to_string(&path).await {
            Ok(content) => content,
            Err(e) => {
                trace!("Failed to read file at path: {}, error: {}", path.display(), e);
                if !path.exists() {
                    trace!("File does not exist: {}", path.display());
                } else {
                    match tokio::fs::metadata(&path).await {
                        Ok(metadata) => {
                            trace!(
                                "File exists, size: {} bytes, readonly: {}",
                                metadata.len(),
                                metadata.permissions().readonly()
                            );
                        }
                        Err(e) => {
                            trace!("Failed to get metadata for file: {}, error: {}", path.display(), e);
                        }
                    }
                }
                return None;
            }
        };
        self.did_open(url, &content).await;
        if let Some(parser) = self.parsers.get(&hash_url) {
            trace!("create completed");
            let result = f(parser.value());
            return Some(result);
        }

        None
    }

    fn try_get_type_directly(
        json_def: &BfastHashMap<String, Value>,
        control_n: Arc<str>,
    ) -> Option<String> {
        if let Some(Value::Object(props)) = json_def.get(&control_n.to_string()) {
            if let Some(Value::String(type_n)) = props.get("type") {
                return Some(type_n.clone());
            }
        }
        None
    }

    fn find_type_or_extend(
        json_def: BfastHashMap<String, Value>,
        namespace: Arc<str>,
        control_name: Arc<str>,
    ) -> Result<String, Option<(Arc<str>, Arc<str>)>> {
        for (key, value) in json_def {
            let control_id = split_control_name(key, namespace.clone());
            if let Some(control_id) = control_id {
                if control_id.1 != control_name {
                    continue;
                }

                if let Value::Object(props) = value {
                    if let Some(Value::String(type_)) = props.get("type") {
                        return Ok(type_.clone());
                    }
                }

                if let Some(extend) = control_id.2 {
                    return Err(Some(extend));
                }
            }
        }

        Err(None)
    }

    fn number_to_sort_text(num: usize, width: usize) -> String {
        format!("{:0width$}", num, width = width)
    }

    fn from_number_to_insert_text_format(kind: u64) -> Option<InsertTextFormat> {
        match kind {
            1 => Some(InsertTextFormat::PLAIN_TEXT),
            2 => Some(InsertTextFormat::SNIPPET),
            _ => None,
        }
    }

    fn from_number_to_completion_item_kind(kind: u64) -> Option<CompletionItemKind> {
        match kind {
            1 => Some(CompletionItemKind::TEXT),
            2 => Some(CompletionItemKind::METHOD),
            3 => Some(CompletionItemKind::FUNCTION),
            4 => Some(CompletionItemKind::CONSTRUCTOR),
            5 => Some(CompletionItemKind::FIELD),
            6 => Some(CompletionItemKind::VARIABLE),
            7 => Some(CompletionItemKind::CLASS),
            8 => Some(CompletionItemKind::INTERFACE),
            9 => Some(CompletionItemKind::MODULE),
            10 => Some(CompletionItemKind::PROPERTY),
            11 => Some(CompletionItemKind::UNIT),
            12 => Some(CompletionItemKind::VALUE),
            13 => Some(CompletionItemKind::ENUM),
            14 => Some(CompletionItemKind::KEYWORD),
            15 => Some(CompletionItemKind::SNIPPET),
            16 => Some(CompletionItemKind::COLOR),
            17 => Some(CompletionItemKind::FILE),
            18 => Some(CompletionItemKind::REFERENCE),
            19 => Some(CompletionItemKind::FOLDER),
            20 => Some(CompletionItemKind::ENUM_MEMBER),
            21 => Some(CompletionItemKind::CONSTANT),
            22 => Some(CompletionItemKind::STRUCT),
            23 => Some(CompletionItemKind::EVENT),
            24 => Some(CompletionItemKind::OPERATOR),
            25 => Some(CompletionItemKind::TYPE_PARAMETER),
            _ => None,
        }
    }

    fn create_simple_completion_item(
        &self,
        label: String,
        lang: Arc<str>,
        pos: Position,
        order: usize,
    ) -> CompletionItem {
        let define = self.jsonui_define.get(label.as_str());
        let description = if let Some(Value::Object(define)) = define {
            let description = define.get("description");
            if let Some(Value::Object(description)) = description {
                description
                    .get(lang.as_ref())
                    .or(description.get("en-us"))
                    .map(|f| to_string_ref(f))
            } else {
                Some("jsonui-support")
            }
        } else {
            Some("jsonui-support")
        };
        CompletionItem {
            label: label.clone(),
            label_details: Some(CompletionItemLabelDetails {
                description: description.map(|s| s.to_string()),
                ..Default::default()
            }),
            kind: Some(CompletionItemKind::TEXT),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range:    Range {
                    start: Position {
                        line:      pos.line,
                        character: pos.character,
                    },
                    end:   Position {
                        line:      pos.line,
                        character: pos.character + 1,
                    },
                },
                new_text: format!("{}\"", label),
            })),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            sort_text: Some(Self::number_to_sort_text(order, 4)),
            ..Default::default()
        }
    }

    fn find_variable_key(
        &self,
        result: &mut Vec<String>,
        symbol_table: &BfastMultiMap<ControlId, Arc<Symbol>>,
        control_id: &ControlId,
    ) {
        let symbols = symbol_table.get_vec(control_id);
        if let Some(vec) = symbols {
            for i in vec.iter() {
                match i.deref() {
                    Symbol::Control(c) => {
                        if let Some(c) = c.parent.clone() {
                            self.find_variable_key(result, symbol_table, &c);
                        }
                    }
                    Symbol::Variable(c) => {
                        result.push(c.value.to_string());
                    }
                    _ => {}
                }
            }
        }
    }

    fn index_document(&self, parser: &DocumentParser) {
        let metadata = self.build_symbol(parser);
        self.handle_metadata(metadata);
    }

    fn handle_metadata(&self, metadata: Vec<(Arc<Symbol>, MetaData)>) {
        metadata.iter().for_each(|(symbol, metadata)| {
            if metadata.is_declare {
                self.definitions.insert(symbol.clone());
            } else {
                self.references.insert(symbol.clone());
            }
        });
    }

    fn build_symbol(&self, parser: &DocumentParser) -> Vec<(Arc<Symbol>, MetaData)> {
        let url = parser.url;
        let mut symbol_table = self.symbol_table.entry(url).or_insert(BfastMultiMap::default());
        {
            symbol_table.flat_iter().for_each(|(_, v)| {
                self.definitions.remove(v);
                self.references.remove(v);
            });
            symbol_table.clear();
        }

        let root_node = parser.tree.root_node();
        let mut metadata = Vec::new();
        let mut cursor = root_node.walk();
        Self::traverse_node(&mut cursor, &mut symbol_table, &mut metadata, parser);
        metadata
    }

    fn traverse_node(
        cursor: &mut tree_sitter::TreeCursor,
        symbol_table: &mut BfastMultiMap<ControlId, Arc<Symbol>>,
        metadata: &mut Vec<(Arc<Symbol>, MetaData)>,
        parser: &DocumentParser,
    ) {
        let node = cursor.node();
        match node.kind() {
            STRING => {
                if let Some(v) = Self::detect_symbol(&node, parser) {
                    symbol_table.insert(v.0.id(), v.0.clone());
                    metadata.push(v);
                }
            }
            _ => {}
        }
        if cursor.goto_first_child() {
            loop {
                Self::traverse_node(cursor, symbol_table, metadata, parser);
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            cursor.goto_parent();
        }
    }

    fn get_parent_control_name(parser: &DocumentParser, node: &Node) -> Option<ControlId> {
        let parent = parser.get_parent_pair_node(node);
        if parent.kind() == "document" {
            return None;
        }
        parent
            .named_child(0)
            .and_then(|child| parser.get_string(&child))
            .map(|name| split_control_name(name, parser.namespace()))
            .flatten()
    }

    fn detect_symbol(
        node: &tree_sitter::Node,
        parser: &DocumentParser,
    ) -> Option<(Arc<Symbol>, MetaData)> {
        let string_content = parser.get_string(node)?;
        let parent_node = parser.get_parent_pair_node(node);
        let parent = Self::get_parent_control_name(parser, node);

        if Self::is_control_key(node) {
            // for the control node, the first parent its own pair node, so it needs to query again
            let parent_parent = Self::get_parent_control_name(parser, &parent_node);
            let id: ControlId = split_control_name(string_content, parser.namespace())?;
            let range = Self::node_range(&node);
            let symbol = Arc::new(Symbol::Control(Control {
                id: id.clone(),
                range,
                parent: parent_parent.clone(),
            }));
            let metadata = if parent_parent.is_none() {
                MetaData { is_declare: true }
            } else {
                MetaData { is_declare: false }
            };
            return Some((symbol, metadata));
        }

        if string_content.starts_with('$') {
            let pool = StringPool::global();
            let spur = pool.get_or_intern(string_content.as_str());
            let var_name = pool.resolve_to_arc(&spur);
            if let Some(parent) = parent {
                let symbol = Arc::new(Symbol::Variable(Variable {
                    parent,
                    range: Self::node_range(&node),
                    value: var_name,
                }));
                let metadata = node
                    .next_sibling()
                    .map(|f| {
                        if f.kind() == ":" {
                            MetaData { is_declare: true }
                        } else {
                            MetaData { is_declare: false }
                        }
                    })
                    .unwrap_or(MetaData { is_declare: false });
                return Some((symbol, metadata));
            }
        }

        if string_content.eq("color") {
            // get the parent control name for color node
            let parent = parent?;
            let (value, range) = node
                .next_named_sibling()
                .map(|ref sibling| match sibling.kind() {
                    STRING => {
                        let range = Self::node_range(sibling);
                        let v = ColorValue::String(parser.get_string(sibling).unwrap_or("".to_string()));
                        Some((v, range))
                    }
                    ARRAY => {
                        let range = Self::node_range(sibling);
                        let vec = sibling
                            .named_children(&mut sibling.walk())
                            .filter_map(|child| parser.float(child))
                            .collect::<Vec<f64>>();
                        Some((ColorValue::Vec(vec), range))
                    }
                    _ => None,
                })
                .flatten()?;
            let node = Arc::new(Symbol::Color(Color { parent, range, value }));
            let metadata = MetaData { is_declare: false };
            return Some((node, metadata));
        }
        None
    }

    fn is_control_key(node: &tree_sitter::Node) -> bool {
        node.next_sibling()
            .filter(|next| next.kind() == ":")
            .and_then(|colon| colon.next_sibling())
            .filter(|value| value.kind() == "object")
            .and_then(|_| node.parent())
            .map_or(false, |parent| parent.kind() == "pair")
    }

    fn node_range(node: &tree_sitter::Node) -> HashRange {
        HashRange {
            start: HashPosition {
                line:      node.start_position().row as u32,
                character: node.start_position().column as u32,
            },
            end:   HashPosition {
                line:      node.end_position().row as u32,
                character: node.end_position().column as u32,
            },
        }
    }
}

fn split_control_name(name: String, def_namespace: Arc<str>) -> Option<ControlId> {
    let pool = StringPool::global();
    let default_namespace = def_namespace;
    let parts: Vec<&str> = name.split('@').collect();
    match parts.len() {
        2 => {
            let split_result: Vec<&str> = parts[1].split('.').collect();
            Some(match split_result.as_slice() {
                [a, b] => {
                    let namespace = pool.get_or_intern(a);
                    let control_name = pool.get_or_intern(b);
                    let part0 = pool.get_or_intern(parts[0]);
                    (
                        default_namespace,
                        pool.resolve_to_arc(&part0),
                        Some((pool.resolve_to_arc(&namespace), pool.resolve_to_arc(&control_name))),
                    )
                }
                [a] => {
                    let namespace = pool.get_or_intern(a);
                    let part0 = pool.get_or_intern(parts[0]);
                    (
                        default_namespace.clone(),
                        pool.resolve_to_arc(&part0),
                        Some((default_namespace, pool.resolve_to_arc(&namespace))),
                    )
                }
                _ => {
                    let part0 = pool.get_or_intern(parts[0]);
                    (default_namespace, pool.resolve_to_arc(&part0), None)
                }
            })
        }
        1 => {
            let part0 = pool.get_or_intern(parts[0]);
            Some((default_namespace, pool.resolve_to_arc(&part0), None))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_control_name() {
        let default: Arc<str> = Arc::from("achievement");
        let r =
            split_control_name("empty_progress_bar_icon".to_string(), Arc::from("achievement")).unwrap();
        assert_eq!((default.clone(), Arc::from("empty_progress_bar_icon"), None), r);

        let r =
            split_control_name("empty_progress_bar_icon@test.cc".to_string(), Arc::from("achievement"))
                .unwrap();
        assert_eq!(
            (
                default.clone(),
                Arc::from("empty_progress_bar_icon"),
                Some((Arc::from("test"), Arc::from("cc")))
            ),
            r
        );

        let r = split_control_name("empty_progress_bar_icon@cc".to_string(), Arc::from("achievement"))
            .unwrap();
        assert_eq!(
            (
                default.clone(),
                Arc::from("empty_progress_bar_icon"),
                Some((Arc::from("achievement"), Arc::from("cc")))
            ),
            r
        );
    }

    fn create_parser(content: &str) -> DocumentParser {
        DocumentParser::default(content)
    }

    #[test]
    fn test_symbol_table_with_variable() {
        let completer = Completer::new(BfastHashMap::default(), BfastHashMap::default());
        let content = r#"{
            "namespace": "default",
            "test": {
                "key": "$variable"
            }
        }"#;
        let parser = create_parser(content);
        completer.index_document(&parser);

        let symbol_table = completer.symbol_table.get(&0).unwrap();
        let namespace = Arc::from("default");
        let spur = Arc::from("test");
        let control_name = (namespace, spur, None);
        let vec = symbol_table.get_vec(&control_name).unwrap();

        assert_eq!(vec.len(), 2);
        match vec.get(1).unwrap().deref() {
            Symbol::Variable(variable) => assert_eq!(variable.value.as_ref(), "$variable"),
            _ => panic!("Expected Variable symbol"),
        };
    }

    #[test]
    fn test_empty_document() {
        let completer = Completer::new(BfastHashMap::default(), BfastHashMap::default());
        let parser = create_parser("{}");
        completer.index_document(&parser);
        assert!(completer.symbol_table.get(&0).unwrap().is_empty());
    }

    #[test]
    fn test_update_document() {
        let json = r#"{
            "name": "John Doe",
            "age": 30,
            "is_active": true,
            "scores": [95.5, 89.0, 92.3],
            "address": {
                "street": "123 Main St",
                "city": "Anytown"
            }
        }"#;
        let expect = r#"{
            "name": "John Doe",
            "age": 3022,
            "is_active": true,
            "scores": [95.5, 89.0, 92.3],
            "address": {
                "street": "123 Main St",
                "city": "Anytown"
            }
        }"#;
        let mut parser = DocumentParser::default(json);
        let change1 = TextDocumentContentChangeEvent {
            range:        Some(Range {
                start: Position {
                    line:      2,
                    character: 21,
                },
                end:   Position {
                    line:      2,
                    character: 21,
                },
            }),
            text:         "2".to_string(),
            range_length: Some(0),
        };
        let change2 = TextDocumentContentChangeEvent {
            range:        Some(Range {
                start: Position {
                    line:      2,
                    character: 22,
                },
                end:   Position {
                    line:      2,
                    character: 22,
                },
            }),
            text:         "2".to_string(),
            range_length: Some(0),
        };
        let old_parser = parser.clone();
        parser.edit(&change1);
        parser.edit(&change2);
        assert_eq!(expect, parser.rope.to_string());

        let old_parser_tree = format!("{:?}", old_parser);
        assert!(old_parser_tree.contains("Rope length: 253 bytes"));
        assert!(old_parser_tree.contains(r#"\"name\": \"John Doe\""#));
        assert!(old_parser_tree.contains(r#"\"John Doe\""#));
        assert!(old_parser_tree.contains(r#"\"age\": 30"#));

        let new_parser_tree = format!("{:?}", parser);
        assert!(new_parser_tree.contains("Rope length: 255 bytes"));
        assert!(new_parser_tree.contains(r#"\"name\": \"John Doe\""#));
        assert!(new_parser_tree.contains(r#"\"John Doe\""#));
        assert!(new_parser_tree.contains(r#"\"age\": 3022"#));
    }

    #[test]
    fn test_update_document_with_utf8() {
        let json = r#"{
            "name": "李华",
            "age": 30,
            "is_active": true,
            "scores": [95.5, 89.0, 92.3],
            "address": {
                "street": "123 Main St",
                "city": "Anytown"
            }
        }"#;
        let expect = r#"{
            "name": "李华一",
            "age": 30,
            "is_active": true,
            "scores": [95.5, 89.0, 92.3],
            "address": {
                "street": "123 Main St",
                "city": "Anytown"
            }
        }"#;
        let mut parser = DocumentParser::default(json);
        let change1 = TextDocumentContentChangeEvent {
            range:        Some(Range {
                start: Position {
                    line:      1,
                    character: 23,
                },
                end:   Position {
                    line:      1,
                    character: 23,
                },
            }),
            text:         "一".to_string(),
            range_length: Some(0),
        };
        let old_parser = parser.clone();
        parser.edit(&change1);
        assert_eq!(expect, parser.rope.to_string());

        let old_parser_tree = format!("{:?}", old_parser);
        assert!(old_parser_tree.contains("Rope length: 251 bytes"));
        assert!(old_parser_tree.contains(r#"\"name\": \"李华\""#));
        assert!(old_parser_tree.contains(r#"\"age\": 30"#));

        let new_parser_tree = format!("{:?}", parser);
        assert!(new_parser_tree.contains("Rope length: 254 bytes"));
        assert!(new_parser_tree.contains(r#"\"name\": \"李华一\""#));
        assert!(new_parser_tree.contains(r#"\"age\": 30"#));
    }

    #[test]
    fn test_cmp_hash_range() {
        let range1 = HashRange {
            start: HashPosition {
                line:      0,
                character: 10,
            },
            end:   HashPosition {
                line:      0,
                character: 15,
            },
        };
        let range2 = HashRange {
            start: HashPosition {
                line:      0,
                character: 11,
            },
            end:   HashPosition {
                line:      0,
                character: 13,
            },
        };
        let range3 = HashRange {
            start: HashPosition {
                line:      0,
                character: 9,
            },
            end:   HashPosition {
                line:      0,
                character: 13,
            },
        };
        let range4 = HashRange {
            start: HashPosition {
                line:      0,
                character: 9,
            },
            end:   HashPosition {
                line:      0,
                character: 15,
            },
        };
        assert!(range1 > range2);
        assert!(range1 > range3);
        assert!(range1 < range4);
        let mut vec = vec![range1, range2, range3, range4];
        vec.sort();
        assert_eq!(
            format!("{:?}", vec),
            r#"[HashRange { start: HashPosition { line: 0, character: 11 }, end: HashPosition { line: 0, character: 13 } }, HashRange { start: HashPosition { line: 0, character: 9 }, end: HashPosition { line: 0, character: 13 } }, HashRange { start: HashPosition { line: 0, character: 10 }, end: HashPosition { line: 0, character: 15 } }, HashRange { start: HashPosition { line: 0, character: 9 }, end: HashPosition { line: 0, character: 15 } }]"#
        )
    }
}
