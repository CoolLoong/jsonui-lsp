use std::sync::Arc;

use tracing::trace;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Url};
use tree_sitter::Node;

use crate::museair::{BfastDashMap, BfastDashSet, BfastHashMap, BfastMultiMap};
use crate::parser::prelude::*;
use crate::parser::DocumentParser;
use crate::utils::hash_url;

use super::types::*;

pub struct SymbolIndexer {
    /// Document parsers, keyed by URL hash
    parsers: BfastDashMap<u64, DocumentParser>,
    /// Symbol table per document
    symbol_table: BfastDashMap<u64, BfastMultiMap<ControlId, Arc<Symbol>>>,
    /// Lightweight index of definition symbols (no duplication)
    definitions: BfastDashSet<SymbolRef>,
    /// Lightweight index of reference symbols (no duplication)
    references: BfastDashSet<SymbolRef>,
    /// Mapping from namespace to URL
    namespace_to_url: BfastDashMap<Arc<str>, Url>,
    /// Vanilla controls table for Arc<str> reuse (shared via Arc)
    vanilla_controls_table: Arc<BfastHashMap<(Arc<str>, Arc<str>), VanillaControlDefine>>,
}

impl SymbolIndexer {
    pub fn new(
        vanilla_controls_table: Arc<BfastHashMap<(Arc<str>, Arc<str>), VanillaControlDefine>>,
    ) -> Self {
        Self {
            parsers: BfastDashMap::default(),
            symbol_table: BfastDashMap::default(),
            definitions: BfastDashSet::default(),
            references: BfastDashSet::default(),
            namespace_to_url: BfastDashMap::default(),
            vanilla_controls_table,
        }
    }

    /// Open and index a document
    pub async fn did_open(&self, url: &Url, content: &str) {
        let hash_url = hash_url(url);
        let mut new_parser: bool = false;
        let parser = self.parsers.entry(hash_url).or_insert_with(|| {
            new_parser = true;
            DocumentParser::new(hash_url, content)
        });
        let namespace = parser.namespace();
        if namespace.as_ref() != "Unknown"
            && new_parser {
                trace!("Init parser, url({}) hash_url({})", url, hash_url);
                self.namespace_to_url
                    .entry(parser.namespace())
                    .or_insert(url.clone());
                self.index_document(&parser);
            }
    }

    /// Delete a document and cleanup all its symbols
    ///
    /// This is called when a file is actually deleted from disk (not just closed in editor).
    /// It removes all parsers, symbols, and index entries for the file.
    pub(crate) fn did_close(&self, url: &Url) {
        let hash_url = hash_url(url);

        // Get namespace before removing parser
        if let Some((_, parser)) = self.parsers.remove(&hash_url) {
            let namespace = parser.namespace();

            // Remove all symbols and their index entries
            if let Some((_, symbol_table)) = self.symbol_table.remove(&hash_url) {
                for (_, symbol) in symbol_table.flat_iter() {
                    let symbol_ref = SymbolRef {
                        doc_hash: hash_url,
                        control_id: symbol.id(),
                        symbol_type: symbol.symbol_type(),
                        range: symbol.range(),
                    };
                    self.definitions.remove(&symbol_ref);
                    self.references.remove(&symbol_ref);
                }
            }

            // Clean up namespace mapping if this URL was the mapped one
            if namespace.as_ref() != "Unknown" {
                self.cleanup_namespace(&namespace, url);
            }
        } else {
            trace!("Parser not found for {:?}, skipping cleanup", url);
        }
    }

    /// Handle document changes
    pub(crate) async fn did_change(&self, url: Url, params: &DidChangeTextDocumentParams) {
        let hash_url = hash_url(&url);
        let parser = self.parsers.get_mut(&hash_url);
        if let Some(mut parser) = parser {
            let changes = &params.content_changes;
            for change in changes {
                parser.edit(change);
            }
            let namespace = parser.namespace();
            if namespace.as_ref() != "Unknown" {
                self.index_document(&parser);
            }
        }
    }

    /// Handle file rename
    pub(crate) async fn did_rename(&self, o_url: Url, new_url: Url) {
        let o_hash_url = hash_url(&o_url);
        let n_hash_url = hash_url(&new_url);

        if let Some((_, mut parser)) = self.parsers.remove(&o_hash_url) {
            let namespace = parser.namespace();
            if namespace.as_ref() != "Unknown" {
                // Update namespace mapping
                self.namespace_to_url.insert(namespace, new_url.clone());

                // Update parser with new URL hash
                parser.url = n_hash_url;
                self.parsers.insert(n_hash_url, parser);

                // Move symbol table and update SymbolRefs
                if let Some((_, symbols)) = self.symbol_table.remove(&o_hash_url) {
                    // Update definitions and references with new doc_hash
                    for (_, symbol) in symbols.flat_iter() {
                        let old_ref = SymbolRef {
                            doc_hash: o_hash_url,
                            control_id: symbol.id(),
                            symbol_type: symbol.symbol_type(),
                            range: symbol.range(),
                        };
                        let new_ref = SymbolRef {
                            doc_hash: n_hash_url,
                            control_id: symbol.id(),
                            symbol_type: symbol.symbol_type(),
                            range: symbol.range(),
                        };

                        // Update in definitions
                        if self.definitions.remove(&old_ref).is_some() {
                            self.definitions.insert(new_ref.clone());
                        }

                        // Update in references
                        if self.references.remove(&old_ref).is_some() {
                            self.references.insert(new_ref);
                        }
                    }

                    self.symbol_table.insert(n_hash_url, symbols);
                }
            }
        }
    }

    /// Get URL for a namespace
    pub(crate) fn get_url(&self, namespace: Arc<str>) -> Option<Url> {
        let r = self.namespace_to_url.get(&namespace)?;
        Some(r.clone())
    }

    /// Get parser for a URL hash
    pub(crate) fn get_parser(&self, url_hash: &u64) -> Option<dashmap::mapref::one::Ref<u64, DocumentParser>> {
        self.parsers.get(url_hash)
    }

    /// Get symbol table for a URL hash
    pub(crate) fn get_symbol_table(
        &self,
        url_hash: &u64,
    ) -> Option<dashmap::mapref::one::Ref<u64, BfastMultiMap<ControlId, Arc<Symbol>>>> {
        self.symbol_table.get(url_hash)
    }

    /// Get all definitions (lightweight index)
    pub(crate) fn definitions(&self) -> &BfastDashSet<SymbolRef> {
        &self.definitions
    }

    /// Get all references (lightweight index)
    pub(crate) fn references(&self) -> &BfastDashSet<SymbolRef> {
        &self.references
    }

    /// Resolve a symbol reference to full symbol
    pub(crate) fn resolve_symbol_ref(&self, symbol_ref: &SymbolRef) -> Option<Arc<Symbol>> {
        let symbol_table = self.symbol_table.get(&symbol_ref.doc_hash)?;
        symbol_table
            .get_vec(&symbol_ref.control_id)?
            .iter()
            .find(|s| s.range() == symbol_ref.range)
            .cloned()
    }

    /// Clean up namespace mapping if the URL being closed is the one mapped
    ///
    /// This ensures we don't leak namespace mappings when files are closed.
    /// Uses atomic DashMap operations to avoid race conditions.
    fn cleanup_namespace(&self, namespace: &Arc<str>, url: &Url) {
        // Use entry API for atomic check-and-remove
        if let Some(entry) = self.namespace_to_url.get(namespace)
            && entry.value() == url {
                drop(entry); // Release read lock before removing
                self.namespace_to_url.remove(namespace);
            }
    }

    /// Get or create a parser for a URL
    pub(crate) async fn get_or_create_parser<F, R>(&self, url: &Url, f: F) -> Option<R>
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

    /// Index a document by building its symbol table
    fn index_document(&self, parser: &DocumentParser) {
        let metadata = self.build_symbol(parser);
        self.handle_metadata(metadata);
    }

    /// Handle symbol metadata (definitions vs references)
    fn handle_metadata(&self, metadata: Vec<(Arc<Symbol>, MetaData, u64)>) {
        metadata.iter().for_each(|(symbol, meta, doc_hash)| {
            let symbol_ref = SymbolRef {
                doc_hash: *doc_hash,
                control_id: symbol.id(),
                symbol_type: symbol.symbol_type(),
                range: symbol.range(),
            };
            if meta.is_declare {
                self.definitions.insert(symbol_ref);
            } else {
                self.references.insert(symbol_ref);
            }
        });
    }

    /// Build symbol table for a document
    fn build_symbol(&self, parser: &DocumentParser) -> Vec<(Arc<Symbol>, MetaData, u64)> {
        let url = parser.url;

        // First, remove ALL SymbolRefs for this document from the indexes
        // This is more robust than relying on symbol_table content
        self.cleanup_document_symbols(url);

        // Clear and rebuild the symbol table
        let mut symbol_table = self.symbol_table.entry(url).or_insert(BfastMultiMap::default());
        symbol_table.clear();

        let root_node = parser.tree.root_node();
        let mut metadata = Vec::new();
        let mut cursor = root_node.walk();
        self.traverse_node(&mut cursor, &mut symbol_table, &mut metadata, parser, url);
        metadata
    }

    /// Remove all SymbolRefs for a document from definitions/references indexes
    ///
    /// This ensures no orphaned references remain even if symbol_table is out of sync.
    fn cleanup_document_symbols(&self, doc_hash: u64) {
        // Remove from definitions
        self.definitions.retain(|symbol_ref| symbol_ref.doc_hash != doc_hash);

        // Remove from references
        self.references.retain(|symbol_ref| symbol_ref.doc_hash != doc_hash);
    }

    /// Traverse AST nodes recursively
    fn traverse_node(
        &self,
        cursor: &mut tree_sitter::TreeCursor,
        symbol_table: &mut BfastMultiMap<ControlId, Arc<Symbol>>,
        metadata: &mut Vec<(Arc<Symbol>, MetaData, u64)>,
        parser: &DocumentParser,
        doc_hash: u64,
    ) {
        let node = cursor.node();
        if node.kind() == STRING
            && let Some((symbol, meta)) = self.detect_symbol(&node, parser) {
                symbol_table.insert(symbol.id(), symbol.clone());
                metadata.push((symbol, meta, doc_hash));
            }
        if cursor.goto_first_child() {
            loop {
                self.traverse_node(cursor, symbol_table, metadata, parser, doc_hash);
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            cursor.goto_parent();
        }
    }

    /// Detect symbols from AST nodes
    fn detect_symbol(
        &self,
        node: &tree_sitter::Node,
        parser: &DocumentParser,
    ) -> Option<(Arc<Symbol>, MetaData)> {
        let string_content = parser.get_string(node)?;
        let parent_node = parser.get_parent_pair_node(node);
        let parent = self.get_parent_control_name(parser, node);

        if Self::is_control_key(node) {
            // for the control node, the first parent its own pair node, so it needs to query again
            let parent_parent = self.get_parent_control_name(parser, &parent_node);
            let id: ControlId = self.split_control_name(string_content, parser.namespace())?;
            let range = Self::node_range(node);
            let symbol = Arc::new(Symbol::Control(Control {
                id,
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

        if string_content.starts_with('$')
            && let Some(parent) = parent {
                let symbol = Arc::new(Symbol::Variable(Variable {
                    parent,
                    range: Self::node_range(node),
                    value: Arc::from(string_content.as_str()),
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

        if string_content.eq("color") {
            // get the parent control name for color node
            let parent = parent?;
            let (value, range) = node
                .next_named_sibling()
                .and_then(|ref sibling| match sibling.kind() {
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
                })?;
            let node = Arc::new(Symbol::Color(Color { parent, range, value }));
            let metadata = MetaData { is_declare: false };
            return Some((node, metadata));
        }
        None
    }

    /// Get parent control name for a node
    fn get_parent_control_name(&self, parser: &DocumentParser, node: &Node) -> Option<ControlId> {
        let parent = parser.get_parent_pair_node(node);
        if parent.kind() == "document" {
            return None;
        }
        parent
            .named_child(0)
            .and_then(|child| parser.get_string(&child))
            .and_then(|name| self.split_control_name(name, parser.namespace()))
    }

    /// Check if a node represents a control key
    fn is_control_key(node: &tree_sitter::Node) -> bool {
        node.next_sibling()
            .filter(|next| next.kind() == ":")
            .and_then(|colon| colon.next_sibling())
            .filter(|value| value.kind() == "object")
            .and_then(|_| node.parent())
            .is_some_and(|parent| parent.kind() == "pair")
    }

    /// Convert tree-sitter node to HashRange
    pub(crate) fn node_range(node: &tree_sitter::Node) -> HashRange {
        HashRange {
            start: HashPosition {
                line: node.start_position().row as u32,
                character: node.start_position().column as u32,
            },
            end: HashPosition {
                line: node.end_position().row as u32,
                character: node.end_position().column as u32,
            },
        }
    }

    /// Get or create Arc<str> reusing from vanilla_controls_table
    fn get_or_create_arc(&self, s: &str) -> Arc<str> {
        for (key, _) in self.vanilla_controls_table.iter() {
            if key.0.as_ref() == s {
                return key.0.clone();
            }
            if key.1.as_ref() == s {
                return key.1.clone();
            }
        }
        Arc::from(s)
    }

    /// Split control name into (namespace, name, extend)
    pub(crate) fn split_control_name(&self, name: String, def_namespace: Arc<str>) -> Option<ControlId> {
        let default_namespace = def_namespace;
        let parts: Vec<&str> = name.split('@').collect();
        match parts.len() {
            2 => {
                let split_result: Vec<&str> = parts[1].split('.').collect();
                Some(match split_result.as_slice() {
                    [a, b] => (
                        default_namespace,
                        self.get_or_create_arc(parts[0]),
                        Some((self.get_or_create_arc(a), self.get_or_create_arc(b))),
                    ),
                    [a] => (
                        default_namespace.clone(),
                        self.get_or_create_arc(parts[0]),
                        Some((default_namespace, self.get_or_create_arc(a))),
                    ),
                    _ => (default_namespace, self.get_or_create_arc(parts[0]), None),
                })
            }
            1 => Some((default_namespace, self.get_or_create_arc(parts[0]), None)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_control_name() {
        let index = SymbolIndexer::new(Arc::new(BfastHashMap::default()));
        let default: Arc<str> = Arc::from("achievement");
        let r = index
            .split_control_name("empty_progress_bar_icon".to_string(), Arc::from("achievement"))
            .unwrap();
        assert_eq!((default.clone(), Arc::from("empty_progress_bar_icon"), None), r);

        let r = index
            .split_control_name("empty_progress_bar_icon@test.cc".to_string(), Arc::from("achievement"))
            .unwrap();
        assert_eq!(
            (
                default.clone(),
                Arc::from("empty_progress_bar_icon"),
                Some((Arc::from("test"), Arc::from("cc")))
            ),
            r
        );

        let r = index
            .split_control_name("empty_progress_bar_icon@cc".to_string(), Arc::from("achievement"))
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
        let index = SymbolIndexer::new(Arc::new(BfastHashMap::default()));
        let content = r#"{
            "namespace": "default",
            "test": {
                "key": "$variable"
            }
        }"#;
        let parser = create_parser(content);
        index.index_document(&parser);

        let symbol_table = index.symbol_table.get(&0).unwrap();
        let namespace = Arc::from("default");
        let spur = Arc::from("test");
        let control_name = (namespace, spur, None);
        let vec = symbol_table.get_vec(&control_name).unwrap();

        assert_eq!(vec.len(), 2);
        match vec.get(1).unwrap().as_ref() {
            Symbol::Variable(variable) => assert_eq!(variable.value.as_ref(), "$variable"),
            _ => panic!("Expected Variable symbol"),
        };
    }

    #[test]
    fn test_empty_document() {
        let index = SymbolIndexer::new(Arc::new(BfastHashMap::default()));
        let parser = create_parser("{}");
        index.index_document(&parser);
        assert!(index.symbol_table.get(&0).unwrap().is_empty());
    }
}
