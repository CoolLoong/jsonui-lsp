use std::ops::Deref;
use std::sync::Arc;

use tower_lsp::lsp_types;
use tower_lsp::lsp_types::{
    ColorInformation, CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionParams,
    CompletionTextEdit, DidChangeTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse,
    InsertTextFormat, Location, Position, Range, ReferenceParams, TextEdit, Url,
};
use tracing::trace;
use tree_sitter::Node;

use crate::museair::{BfastHashMap, BfastMultiMap};
use crate::parser::DocumentParser;
use crate::parser::prelude::*;
use crate::utils::hash_url;

use super::indexer::SymbolIndexer;
use super::types::*;

pub struct Completer {
    /// Symbol indexing subsystem
    indexer: SymbolIndexer,
    /// Vanilla control definitions (shared via Arc)
    vanilla_controls_table: Arc<BfastHashMap<ControlIdNoParent, VanillaControlDefine>>,
    jsonui_define: BfastHashMap<String, Value>,
}

impl Completer {
    pub fn new(
        vanilla_controls_table: Arc<BfastHashMap<ControlIdNoParent, VanillaControlDefine>>,
        jsonui_define: BfastHashMap<String, Value>,
    ) -> Self {
        let symbol_index = SymbolIndexer::new(vanilla_controls_table.clone());
        Self {
            indexer: symbol_index,
            vanilla_controls_table,
            jsonui_define,
        }
    }

    pub async fn did_open(&self, url: &Url, content: &str) {
        self.indexer.did_open(url, content).await;
    }
    pub fn did_delete(&self, url: &Url) {
        self.indexer.did_delete(url);
    }
    pub async fn did_change(&self, url: Url, params: &DidChangeTextDocumentParams) {
        self.indexer.did_change(url, params).await;
    }
    pub async fn did_rename(&self, o_url: Url, new_url: Url) {
        self.indexer.did_rename(o_url, new_url).await;
    }

    // ========== Completion Methods ==========
    /// Provide completion suggestions
    pub async fn complete(
        &self,
        url: Url,
        config: Arc<crate::config::Config>,
        params: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let url_hash = hash_url(&url);
        let parser_ref = self.indexer.get_parser(&url_hash)?;

        let pos = params.text_document_position.position;
        let node = parser_ref.get_node_at_position(pos)?;
        let quote_pos = Self::get_position_for_quote(&node);
        let parent = parser_ref.get_parent_pair_node(&node);

        // Early return for document nodes
        if parent.kind() == "document" {
            trace!("parent is document node, skip.");
            return None;
        }

        // Configuration is now lock-free via arc-swap
        let lang = &config.lang;
        let append_suffix = config.append_suffix;
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
                self.create_common_type_completion(url_hash, pos, quote_pos, lang)
                    .await
            }
            1 => self.create_binding_type_completion(url_hash, pos, quote_pos, lang),
            2 | 3 => self.create_value_completion(
                completion_type,
                url_hash,
                pos,
                quote_pos,
                lang,
                append_suffix,
            ),
            _ => None,
        }
    }

    /// Provide color information for a document
    pub fn complete_color(&self, url: Url) -> Option<Vec<ColorInformation>> {
        let url_hash = hash_url(&url);
        let symbol_table = self.indexer.get_symbol_table(&url_hash)?;
        let colors: Vec<ColorInformation> = symbol_table
            .flat_iter()
            .filter_map(|(_, f)| match f.deref() {
                Symbol::Color(v) => Some(v),
                _ => None,
            })
            .map(|c| Self::create_color_information(c))
            .collect();
        if colors.is_empty() { None } else { Some(colors) }
    }

    /// Go to definition
    pub async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<(GotoDefinitionResponse, bool)> {
        let url = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let hash_url = hash_url(url);
        let parser_ref = self.indexer.get_parser(&hash_url)?;
        let symbol_table = self.indexer.get_symbol_table(&hash_url)?;

        let node = parser_ref.get_node_at_position(pos);
        let namespace = parser_ref.namespace();
        if let Some(node) = node {
            let range = SymbolIndexer::node_range(&node);
            let mut containing_symbols: Vec<_> = symbol_table
                .flat_iter()
                .filter(|f| f.0.0 == namespace)
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
            let symbol_id = symbol.id();
            let symbol_np = &symbol_id.2.as_ref()?.0;
            let is_current_file = current_np == *symbol_np;
            drop(parser_ref);
            drop(symbol_table);
            let loc = self.find_definition(symbol.deref()).await?;
            Some((GotoDefinitionResponse::Scalar(loc), is_current_file))
        } else {
            None
        }
    }

    /// Find all references
    pub async fn references(&self, params: &ReferenceParams) -> Option<Vec<Location>> {
        let url = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let hash_url = hash_url(url);
        let parser_ref = self.indexer.get_parser(&hash_url)?;
        let symbol_table = self.indexer.get_symbol_table(&hash_url)?;
        let node = parser_ref.get_node_at_position(pos);
        let namespace = parser_ref.namespace();
        if let Some(node) = node {
            let range = SymbolIndexer::node_range(&node);
            let mut containing_symbols: Vec<_> = symbol_table
                .flat_iter()
                .filter(|f| f.0.0 == namespace)
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

    // ========== Helper Methods ==========
    async fn find_definition(&self, symbol: &Symbol) -> Option<Location> {
        match symbol {
            Symbol::Control(c) => {
                let extend = c.id.2.as_ref()?;
                let url = self.indexer.get_url(Arc::clone(&extend.0))?;
                self.indexer.get_or_create_parser(&url, |_| {}).await;

                self.indexer
                    .definitions()
                    .iter()
                    .filter_map(|symbol_ref| {
                        // Check if this is a control symbol matching our target
                        let (namespace, name, _) = &symbol_ref.control_id;
                        if symbol_ref.symbol_type == SymbolType::Control
                            && namespace == &extend.0
                            && name == &extend.1
                        {
                            let url = self.indexer.get_url(Arc::clone(namespace))?;
                            Some(Location {
                                uri: url,
                                range: symbol_ref.range.into(),
                            })
                        } else {
                            None
                        }
                    })
                    .next()
            }
            _ => None,
        }
    }

    fn find_references(&self, symbol: &Symbol) -> Vec<Location> {
        match symbol {
            Symbol::Control(c) => {
                let namespace = &c.id.0;
                let control_name = &c.id.1;
                self.indexer
                    .references()
                    .iter()
                    .filter_map(|symbol_ref| {
                        if symbol_ref.symbol_type != SymbolType::Control {
                            return None;
                        }
                        let extend = symbol_ref.control_id.2.as_ref()?;
                        if &extend.0 == namespace && &extend.1 == control_name {
                            let url = self.indexer.get_url(Arc::clone(&symbol_ref.control_id.0))?;
                            Some(Location {
                                uri: url,
                                range: symbol_ref.range.into(),
                            })
                        } else {
                            None
                        }
                    })
                    .collect()
            }
            _ => vec![],
        }
    }

    fn get_position_for_quote(node: &Node) -> Position {
        let pos = SymbolIndexer::node_range(node);
        Position {
            line: pos.start.line,
            character: (pos.start.character + pos.end.character) / 2,
        }
    }

    fn create_color_information(color: &Color) -> ColorInformation {
        match &color.value {
            ColorValue::String(v) => match v.as_str() {
                "white" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 1.0,
                        green: 1.0,
                        blue: 1.0,
                        alpha: 1.0,
                    },
                },
                "silver" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.776,
                        green: 0.776,
                        blue: 0.776,
                        alpha: 1.0,
                    },
                },
                "gray grey" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.333,
                        green: 0.333,
                        blue: 0.333,
                        alpha: 1.0,
                    },
                },
                "black" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.0,
                        green: 0.0,
                        blue: 0.0,
                        alpha: 1.0,
                    },
                },
                "red" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 1.0,
                        green: 0.333,
                        blue: 0.333,
                        alpha: 1.0,
                    },
                },
                "green" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.333,
                        green: 1.0,
                        blue: 0.333,
                        alpha: 1.0,
                    },
                },
                "yellow" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 1.0,
                        green: 1.0,
                        blue: 0.333,
                        alpha: 1.0,
                    },
                },
                "brown" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.706,
                        green: 0.408,
                        blue: 0.302,
                        alpha: 1.0,
                    },
                },
                "cyan" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.0,
                        green: 0.667,
                        blue: 0.667,
                        alpha: 1.0,
                    },
                },
                "blue" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 0.333,
                        green: 0.333,
                        blue: 1.0,
                        alpha: 1.0,
                    },
                },
                "orange" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 1.0,
                        green: 0.667,
                        blue: 0.0,
                        alpha: 1.0,
                    },
                },
                "purple" => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 1.0,
                        green: 0.333,
                        blue: 1.0,
                        alpha: 1.0,
                    },
                },
                "nil" | _ => ColorInformation {
                    range: color.range.into(),
                    color: lsp_types::Color {
                        red: 1.0,
                        green: 1.0,
                        blue: 1.0,
                        alpha: 0.0,
                    },
                },
            },
            ColorValue::Vec(v) => {
                if v.len() == 3 {
                    ColorInformation {
                        range: color.range.into(),
                        color: lsp_types::Color {
                            red: f32::clamp(v[0] as f32, 0 as f32, 1 as f32),
                            green: f32::clamp(v[1] as f32, 0 as f32, 1 as f32),
                            blue: f32::clamp(v[2] as f32, 0 as f32, 1 as f32),
                            alpha: 1.0,
                        },
                    }
                } else if v.len() == 4 {
                    ColorInformation {
                        range: color.range.into(),
                        color: lsp_types::Color {
                            red: f32::clamp(v[0] as f32, 0 as f32, 1 as f32),
                            green: f32::clamp(v[1] as f32, 0 as f32, 1 as f32),
                            blue: f32::clamp(v[2] as f32, 0 as f32, 1 as f32),
                            alpha: f32::clamp(v[3] as f32, 0 as f32, 1 as f32),
                        },
                    }
                } else {
                    ColorInformation {
                        range: color.range.into(),
                        color: lsp_types::Color {
                            red: 1.0,
                            green: 1.0,
                            blue: 1.0,
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
        matches!(n1.as_ref().map(|n| n.kind()), Some(":"))
            && matches!(n2.as_ref().map(|n| n.kind()), Some(ARRAY))
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

    async fn create_common_type_completion(
        &self,
        url: u64,
        pos: Position,
        quote_pos: Position,
        lang: &Arc<str>,
    ) -> Option<Vec<CompletionItem>> {
        let parser_ref = self.indexer.get_parser(&url)?;
        let node = parser_ref.get_node_at_position(pos)?;
        let parent = parser_ref.get_parent_pair_node(&node);
        let common_key = Self::extract_strings(self.jsonui_define.get("common"));

        // Get type-specific keys
        let type_key = {
            let key = parent.named_child(0)?;
            let value = parent.named_child(1)?;
            let type_n = parser_ref.field(value, "type");

            let control_id = parser_ref.string(key)?;
            let control_id = self
                .indexer
                .split_control_name(control_id, parser_ref.namespace());
            if let Some(control_id) = control_id {
                let type_n = if let Some(type_n) = type_n {
                    parser_ref.string(type_n).map(|s| Arc::from(s.as_str()))
                } else {
                    if let Some(control_id_no_parent) = control_id.2 {
                        drop(parser_ref);
                        self.find_control_type(control_id_no_parent).await
                    } else {
                        None
                    }
                };
                let type_key = if let Some(type_n) = type_n {
                    Self::extract_strings(self.jsonui_define.get(&type_n.to_string()))
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
            result.push(self.create_simple_completion_item(k, lang, quote_pos, order));
            order += 1;
        });
        type_key.into_iter().for_each(|k| {
            result.push(self.create_simple_completion_item(k, lang, quote_pos, order));
            order += 1;
        });
        if result.is_empty() { None } else { Some(result) }
    }

    fn create_binding_type_completion(
        &self,
        url: u64,
        pos: Position,
        quote_pos: Position,
        lang: &Arc<str>,
    ) -> Option<Vec<CompletionItem>> {
        let parser_ref = self.indexer.get_parser(&url)?;
        let current = parser_ref.get_node_at_position(pos)?;

        let common_key = Self::extract_strings(self.jsonui_define.get("bindings_properties"));
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
            result.push(self.create_simple_completion_item(k, lang, quote_pos, order));
            order += 1;
        });
        type_key.into_iter().for_each(|k| {
            result.push(self.create_simple_completion_item(k, lang, quote_pos, order));
            order += 1;
        });
        if result.is_empty() { None } else { Some(result) }
    }

    fn create_value_completion(
        &self,
        completion_type: u8,
        url: u64,
        pos: Position,
        quote_pos: Position,
        lang: &Arc<str>,
        append_suffix: bool,
    ) -> Option<Vec<CompletionItem>> {
        let parser_ref = self.indexer.get_parser(&url)?;
        let symbol_table = self.indexer.get_symbol_table(&url)?;
        let node = parser_ref.get_node_at_position(pos)?;
        let parent = parser_ref.get_parent_pair_node(&node);
        let (before, after) = parser_ref.get_adjacent_nodes(&node);
        let [n1, n2] = [before.first(), before.get(1)];
        let [n3, _n4] = [after.first(), after.get(1)];
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
                let control_id = self
                    .indexer
                    .split_control_name(control_name, parser_ref.namespace());
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
                            range: Range {
                                start: pos,
                                end: Position {
                                    line: pos.line,
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
                        detail: None,
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

    async fn find_control_type(&self, control_id: ControlIdNoParent) -> Option<Arc<str>> {
        let mut stack = vec![];
        let mut control_id = control_id;
        loop {
            // check type in vanilla_controls_table
            if let Some(v) = self.vanilla_controls_table.get(&control_id) {
                return Some(v.type_n.clone());
            }
            // get all kvs for the control of namespace
            let kvs = self.get_all_kv(&control_id).await?;
            let namespace = &control_id.0;
            let control_name = &control_id.1;
            // try get type from kvs from control_name directly
            if let Some(type_n) = Self::try_get_type_directly(&kvs, control_name) {
                return Some(Arc::from(type_n.as_str()));
            }
            // get type foreach kvs
            match self.find_type_or_extend(kvs, namespace, control_name) {
                Ok(type_) => return Some(Arc::from(type_.as_str())),
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
        let url = self.indexer.get_url(Arc::clone(&refer.0))?;
        self.indexer
            .get_or_create_parser(&url, |parser| {
                let json_def = parser.hashmap();
                Some(json_def)
            })
            .await?
    }

    fn try_get_type_directly(
        json_def: &BfastHashMap<String, Value>,
        control_n: &Arc<str>,
    ) -> Option<String> {
        if let Some(Value::Object(props)) = json_def.get(control_n.as_ref()) {
            if let Some(Value::String(type_n)) = props.get("type") {
                return Some(type_n.clone());
            }
        }
        None
    }

    fn find_type_or_extend(
        &self,
        json_def: BfastHashMap<String, Value>,
        namespace: &Arc<str>,
        control_name: &Arc<str>,
    ) -> Result<String, Option<ControlIdNoParent>> {
        for (key, value) in json_def {
            let control_id = self.indexer.split_control_name(key, Arc::clone(namespace));
            if let Some(control_id) = control_id {
                if &control_id.1 != control_name {
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
        lang: &Arc<str>,
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
                range: Range {
                    start: Position {
                        line: pos.line,
                        character: pos.character,
                    },
                    end: Position {
                        line: pos.line,
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
}
