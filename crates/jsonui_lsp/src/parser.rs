use core::fmt;
use std::collections::HashMap;
use std::sync::Arc;

use ropey::Rope;
use tower_lsp::lsp_types::{Position, TextDocumentContentChangeEvent};
use tree_sitter::{InputEdit, Node, Parser, Point, Tree};
use unicode_segmentation::UnicodeSegmentation;

use crate::museair::BfastHashMap;
use crate::StringPool;

pub(crate) const OBJECT: &'static str = "object";
pub(crate) const NUMBER: &'static str = "number";
pub(crate) const STRING: &'static str = "string";
pub(crate) const STRING_CONTENT: &'static str = "string_content";
pub(crate) const TRUE: &'static str = "true";
pub(crate) const FALSE: &'static str = "false";
pub(crate) const NULL: &'static str = "null";
pub(crate) const ARRAY: &'static str = "array";

use std::cell::RefCell;
thread_local! {
    static PARSER: RefCell<Parser> = RefCell::new({
        let mut parser = Parser::new();
        let json = tree_sitter_json::LANGUAGE;
        parser
            .set_language(&json.into())
            .expect("Error loading JSON parser");
        parser
    });
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Null,
}

#[derive(Clone)]
pub struct DocumentParser {
    namespace:       Arc<str>,
    pub(crate) url:  u64,
    pub(crate) rope: Rope,
    pub(crate) tree: Tree,
}

impl fmt::Debug for DocumentParser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DocumentParser")?;
        writeln!(f, "Rope length: {} bytes", self.rope.len_bytes())?;

        writeln!(f, "Parse Tree:")?;
        if let Some(root) = self.root() {
            self.debug_node(f, root, 0)?;
        } else {
            writeln!(f, "[Empty Tree]")?;
        }

        Ok(())
    }
}

impl DocumentParser {
    pub(crate) fn default(text: &str) -> Self {
        DocumentParser::new(0, text)
    }

    pub(crate) fn new(url: u64, text: &str) -> Self {
        let rope = Rope::from_str(text);
        let mut callback = |byte_offset: usize, _: Point| -> &[u8] {
            if byte_offset >= rope.len_bytes() {
                return &[];
            }
            let (chunk, chunk_start_byte, _, _) = rope.chunk_at_byte(byte_offset);
            let local_offset = byte_offset - chunk_start_byte;
            &chunk.as_bytes()[local_offset..]
        };
        let tree = PARSER.with_borrow_mut(|p| p.parse_with_options(&mut callback, None, None).unwrap());
        let object = tree
            .root_node()
            .named_children(&mut tree.root_node().walk())
            .find(|node| node.kind() == "object");
        let namespace = if let Some(object) = object {
            Self::find_namespace_value(object, text)
        } else {
            None
        };
        let namespace = namespace.map_or(Arc::from(""), |f| Arc::from(f.as_str()));
        DocumentParser {
            url,
            namespace,
            rope,
            tree,
        }
    }

    pub(crate) fn namespace(&self) -> Arc<str> {
        self.namespace.clone()
    }

    pub fn root(&self) -> Option<Node> {
        self.tree
            .root_node()
            .named_children(&mut self.tree.root_node().walk())
            .find(|node| node.kind() == "object")
    }

    pub fn get_node_at_position(&self, position: Position) -> Option<Node<'_>> {
        let offset = self.position_to_byte_offset(position)?;

        let root_node = self.tree.root_node();
        self.find_node_at_offset(root_node, offset)
    }

    pub fn get_parents<'a>(&'a self, node: &'a Node<'a>) -> Vec<Node<'a>> {
        let mut parents = Vec::new();
        let mut current = node.parent();
        while let Some(parent) = current {
            if parent.kind() == "pair" && self.is_pair_with_object_or_array(&parent) {
                parents.push(parent);
            }
            current = parent.parent();
        }
        parents.into()
    }

    pub fn get_parent_pair_node<'a>(&'a self, node: &'a Node<'a>) -> Node<'a> {
        let mut current = node.parent();
        while let Some(parent) = current {
            if parent.kind() == "pair" && self.is_pair_with_object(&parent) {
                return parent;
            }
            current = parent.parent();
        }
        self.tree.root_node()
    }

    fn is_pair_with_object(&self, node: &Node) -> bool {
        node.named_children(&mut node.walk())
            .any(|node| node.kind() == "object")
    }

    fn is_pair_with_object_or_array(&self, node: &Node) -> bool {
        node.named_children(&mut node.walk())
            .any(|node| node.kind() == "object" || node.kind() == "array")
    }

    fn flatten_nodes(tree: &Tree) -> Vec<Node> {
        let root_node = tree.root_node();
        let mut nodes = Vec::new();
        Self::traverse_and_collect(root_node, &mut nodes);
        nodes
    }

    fn traverse_and_collect<'a>(node: Node<'a>, nodes: &mut Vec<Node<'a>>) {
        if node.kind() != "\"" {
            nodes.push(node);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::traverse_and_collect(child, nodes);
        }
    }

    pub fn get_adjacent_nodes<'a>(&'a self, node: &'a Node<'a>) -> (Vec<Node<'a>>, Vec<Node<'a>>) {
        let nodes = Self::flatten_nodes(&self.tree);
        nodes
            .iter()
            .enumerate()
            .find(|(_, n)| n.start_byte() == node.start_byte() && n.end_byte() == node.end_byte())
            .map(|(index, _)| {
                let prev = nodes[..index].iter().rev().take(2).rev().cloned().collect();
                let next = nodes[index + 1..].iter().take(2).cloned().collect();
                (prev, next)
            })
            .unwrap_or_else(|| (Vec::new(), Vec::new()))
    }

    pub fn hashmap(&self) -> BfastHashMap<String, Value> {
        let root = self.root();
        let mut result = BfastHashMap::default();
        if root.is_none() {
            return result;
        }
        let root = root.unwrap();

        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            if let Some((key, value)) = self.parse_key_value_pair(child) {
                result.insert(key, value);
            }
        }

        result
    }

    pub fn text(&self, node: Node) -> Option<String> {
        let start_byte = node.start_byte();
        let end_byte = node.end_byte();

        if end_byte > self.rope.len_bytes() {
            return None;
        }

        let start_char = self.rope.byte_to_char(start_byte);
        let end_char = self.rope.byte_to_char(end_byte);

        Some(self.rope.slice(start_char..end_char).to_string())
    }

    pub fn string(&self, node: Node) -> Option<String> {
        match node.kind() {
            STRING => self.text(node).map(|s| {
                let str = &s[1..s.len() - 1];
                unescape_zero_copy::unescape_default(str).map_or(str.to_string(), |f| f.to_string())
            }),
            STRING_CONTENT => self.text(node).map(|s| s.to_string()),
            _ => None,
        }
    }

    pub fn get_string(&self, node: &Node) -> Option<String> {
        match node.kind() {
            STRING => self.get_text(node).map(|s| {
                let str = &s[1..s.len() - 1];
                unescape_zero_copy::unescape_default(str).map_or(str.to_string(), |f| f.to_string())
            }),
            STRING_CONTENT => self.get_text(node).map(|s| s.to_string()),
            _ => None,
        }
    }

    pub fn float(&self, node: Node) -> Option<f64> {
        if node.kind() != "number" {
            return None;
        }
        self.text(node).and_then(|s| s.parse::<f64>().ok())
    }

    pub fn int(&self, node: Node) -> Option<isize> {
        self.float(node).map(|f| f as isize)
    }

    pub fn bool(&self, node: Node) -> Option<bool> {
        match node.kind() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        }
    }

    pub fn field<'a>(&'a self, node: Node<'a>, field_name: &str) -> Option<Node<'a>> {
        if node.kind() != "object" {
            return None;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "pair" {
                if let Some(key_node) = child.child_by_field_name("key") {
                    if let Some(key) = self.string(key_node) {
                        if key == field_name {
                            return child.child_by_field_name("value");
                        }
                    }
                }
            }
        }
        None
    }

    pub fn element<'a>(&'a self, node: Node<'a>, index: usize) -> Option<Node<'a>> {
        if node.kind() != "array" {
            return None;
        }

        let mut count = 0;
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if count == index {
                return Some(child);
            }
            count += 1;
        }
        None
    }

    pub fn get_text(&self, node: &Node) -> Option<String> {
        let start_byte = node.start_byte();
        let end_byte = node.end_byte();

        if end_byte > self.rope.len_bytes() {
            return None;
        }

        let start_char = self.rope.byte_to_char(start_byte);
        let end_char = self.rope.byte_to_char(end_byte);

        Some(self.rope.slice(start_char..end_char).to_string())
    }

    pub fn get_float(&self, node: &Node) -> Option<f64> {
        if node.kind() != "number" {
            return None;
        }
        self.get_text(node).and_then(|s| s.parse::<f64>().ok())
    }

    pub fn get_int(&self, node: &Node) -> Option<isize> {
        self.get_float(node).map(|f| f as isize)
    }

    pub fn get_bool(&self, node: &Node) -> Option<bool> {
        match node.kind() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        }
    }

    pub fn get_field<'a>(&'a self, node: &Node<'a>, field_name: &str) -> Option<Node<'a>> {
        if node.kind() != "object" {
            return None;
        }

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if child.kind() == "pair" {
                if let Some(key_node) = child.child_by_field_name("key") {
                    if let Some(key) = self.string(key_node) {
                        if key == field_name {
                            return child.child_by_field_name("value");
                        }
                    }
                }
            }
        }
        None
    }

    pub fn get_element<'a>(&'a self, node: &Node<'a>, index: usize) -> Option<Node<'a>> {
        if node.kind() != "array" {
            return None;
        }

        let mut count = 0;
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if count == index {
                return Some(child);
            }
            count += 1;
        }
        None
    }

    pub(crate) fn print_node(&self, node: &Node) -> String {
        let text = self.get_text(node).unwrap_or_default();
        let graphemes: Vec<&str> = text.graphemes(true).collect();
        format!(
            "{} {}..{} {:?}",
            node.kind(),
            node.start_position(),
            node.end_position(),
            if graphemes.len() > 30 {
                format!("{}...", graphemes[..27].concat())
            } else {
                text
            }
        )
    }

    pub(crate) fn edit(&mut self, change: &TextDocumentContentChangeEvent) {
        // update tree
        let input = self.create_input_edit(change);
        if let Some(input) = input {
            // update rope
            if let Some(range) = &change.range {
                let start = self.position_to_char(range.start);
                let end = self.position_to_char(range.end);
                if let Some(start) = start
                    && let Some(end) = end
                {
                    let end = usize::max(end, start);
                    self.rope.remove(start..end);
                    self.rope.insert(start, &change.text);
                }
            } else {
                self.rope = Rope::from_str(&change.text);
            }

            self.tree.edit(&input);
            let mut callback = |byte_offset: usize, _: Point| -> &[u8] {
                if byte_offset >= self.rope.len_bytes() {
                    return &[];
                }
                let (chunk, chunk_start_byte, _, _) = self.rope.chunk_at_byte(byte_offset);
                let local_offset = byte_offset - chunk_start_byte;
                &chunk.as_bytes()[local_offset..]
            };
            self.tree = PARSER.with_borrow_mut(|p| {
                p.parse_with_options(&mut callback, Some(&self.tree), None)
                    .unwrap()
            });
            let namespace = self.find_namespace_value_byself();
            if let Some(namespace) = namespace {
                let pool = StringPool::global();
            }
        }
    }

    fn position_to_byte_offset(&self, pos: Position) -> Option<usize> {
        let char_idx = self.position_to_char(pos)?;
        Some(self.rope.char_to_byte(char_idx))
    }

    fn position_to_char(&self, position: Position) -> Option<usize> {
        let line = position.line as usize;
        if line >= self.rope.len_lines() {
            return None;
        }
        let char_in_line = position.character as usize;
        let line_start = self.rope.line_to_char(line);
        let offset = line_start + char_in_line;
        if offset > self.rope.len_chars() {
            return None;
        }
        Some(offset)
    }

    fn utf8_byte_len(text: &str) -> usize {
        text.graphemes(true)
            .flat_map(|g| g.chars())
            .map(|c| c.len_utf8())
            .sum()
    }

    fn create_input_edit(&self, change: &TextDocumentContentChangeEvent) -> Option<InputEdit> {
        match change.range {
            None => {
                let old_len = self.rope.len_bytes();
                let text = change.text.as_str();
                Some(InputEdit {
                    start_byte:       0,
                    old_end_byte:     old_len,
                    new_end_byte:     Self::utf8_byte_len(text),
                    start_position:   Point { row: 0, column: 0 },
                    old_end_position: Self::compute_end_position(None, &self.rope.to_string()),
                    new_end_position: Self::compute_end_position(None, text),
                })
            }
            Some(range) => {
                let start_byte = self.position_to_byte_offset(range.start)?;
                let old_end_byte = self.position_to_byte_offset(range.end)?;
                let text = change.text.as_str();
                Some(InputEdit {
                    start_byte,
                    old_end_byte,
                    new_end_byte: start_byte + Self::utf8_byte_len(text),
                    start_position: Point {
                        row:    range.start.line as usize,
                        column: range.start.character as usize,
                    },
                    old_end_position: Point {
                        row:    range.end.line as usize,
                        column: range.end.character as usize,
                    },
                    new_end_position: Self::compute_end_position(Some(range.start), text),
                })
            }
        }
    }

    fn compute_end_position(start: Option<Position>, text: &str) -> Point {
        let mut row: usize = if start.is_none() {
            0
        } else {
            start.unwrap().line as usize
        };
        let mut column: usize = if start.is_none() {
            0
        } else {
            start.unwrap().character as usize
        };
        let chars = text.graphemes(true);
        for c in chars {
            if c == "\n" {
                row += 1;
                column = 0;
            } else {
                column += 1;
            }
        }
        Point { row, column }
    }

    fn parse_key_value_pair(&self, node: Node) -> Option<(String, Value)> {
        if node.kind() != "pair" {
            return None;
        }

        let key_node = node.child_by_field_name("key")?;
        let value_node = node.child_by_field_name("value")?;

        let key = self.string(key_node)?;
        let value = self.parse_value(value_node)?;

        Some((key, value))
    }

    /// Recursively parse any value node
    fn parse_value(&self, node: Node) -> Option<Value> {
        match node.kind() {
            STRING => self.string(node).map(Value::String),
            NUMBER => self.float(node).map(Value::Number),
            TRUE => Some(Value::Boolean(true)),
            FALSE => Some(Value::Boolean(false)),
            NULL => Some(Value::Null),
            ARRAY => {
                let mut array = Vec::new();
                let mut cursor = node.walk();
                for child in node.named_children(&mut cursor) {
                    if let Some(value) = self.parse_value(child) {
                        array.push(value);
                    }
                }
                Some(Value::Array(array))
            }
            OBJECT => {
                let mut map = HashMap::new();
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if let Some((key, value)) = self.parse_key_value_pair(child) {
                        map.insert(key, value);
                    }
                }
                Some(Value::Object(map))
            }
            _ => None,
        }
    }

    fn find_node_at_offset<'a>(&'a self, node: Node<'a>, offset: usize) -> Option<Node<'a>> {
        if node.start_byte() <= offset && offset <= node.end_byte() && node.kind() != "\"" {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if let Some(found) = self.find_node_at_offset(child, offset) {
                    return Some(found);
                }
            }
            Some(node)
        } else {
            None
        }
    }

    fn debug_node(&self, f: &mut fmt::Formatter<'_>, node: Node, depth: usize) -> fmt::Result {
        let indent = "  ".repeat(depth);

        let text = self.text(node).unwrap_or_default();
        let graphemes: Vec<&str> = text.graphemes(true).collect();
        write!(
            f,
            "{}- {} {}..{} {:?}",
            indent,
            node.kind(),
            node.start_position(),
            node.end_position(),
            if graphemes.len() > 30 {
                format!("{}...", graphemes[..27].concat())
            } else {
                text
            }
        )?;

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            writeln!(f)?;
            self.debug_node(f, child, depth + 1)?;
        }

        Ok(())
    }

    fn find_namespace_value(root_node: Node, source: &str) -> Option<String> {
        let mut cursor = root_node.walk();

        if root_node.kind() != "object" {
            return None;
        }

        for child in root_node.children(&mut cursor) {
            if child.kind() == "pair" {
                let mut pair_cursor = child.walk();
                let mut key_node: Option<Node> = None;
                let mut value_node: Option<Node> = None;

                for pair_child in child.children(&mut pair_cursor) {
                    match pair_child.kind() {
                        "string" if key_node.is_none() => key_node = Some(pair_child),
                        "string" if value_node.is_none() => value_node = Some(pair_child),
                        _ => {}
                    }
                }

                if let (Some(key), Some(value)) = (key_node, value_node) {
                    let key_text = key.utf8_text(source.as_bytes()).ok()?;
                    if key_text.trim_matches('"') == "namespace" {
                        return Some(
                            value
                                .utf8_text(source.as_bytes())
                                .ok()?
                                .trim_matches('"')
                                .to_string(),
                        );
                    }
                }
            }
        }
        None
    }

    fn find_namespace_value_byself(&self) -> Option<String> {
        let root_node = self
            .tree
            .root_node()
            .named_children(&mut self.tree.root_node().walk())
            .find(|node| node.kind() == "object")?;
        let mut cursor = root_node.walk();
        for child in root_node.children(&mut cursor) {
            if child.kind() == "pair" {
                let mut pair_cursor = child.walk();
                let mut key_node: Option<Node> = None;
                let mut value_node: Option<Node> = None;

                for pair_child in child.children(&mut pair_cursor) {
                    match pair_child.kind() {
                        "string" if key_node.is_none() => key_node = Some(pair_child),
                        "string" if value_node.is_none() => value_node = Some(pair_child),
                        _ => {}
                    }
                }

                if let (Some(key), Some(value)) = (key_node, value_node) {
                    let key_text = self.string(key)?;
                    if key_text == "namespace" {
                        return Some(self.string(value)?);
                    }
                }
            }
        }
        None
    }
}

pub(crate) fn to_string(value: Value) -> String {
    if let Value::String(v) = value {
        v
    } else {
        String::new()
    }
}

pub(crate) fn to_string_ref(value: &Value) -> &str {
    if let Value::String(v) = value {
        v.as_str()
    } else {
        ""
    }
}

pub(crate) fn to_object(value: Value) -> HashMap<String, Value> {
    if let Value::Object(v) = value {
        v
    } else {
        HashMap::new()
    }
}

pub(crate) fn to_object_ref(value: &Value) -> Option<&HashMap<String, Value>> {
    if let Value::Object(v) = value {
        Some(v)
    } else {
        None
    }
}
pub(crate) fn to_array(value: Value) -> Vec<Value> {
    if let Value::Array(v) = value {
        v
    } else {
        Vec::new()
    }
}

pub(crate) fn to_array_ref(value: &Value) -> Vec<Value> {
    if let Value::Array(v) = value {
        v.clone()
    } else {
        vec![]
    }
}

pub(crate) fn to_number(value: Value) -> f64 {
    if let Value::Number(v) = value {
        v
    } else {
        0 as f64
    }
}

pub(crate) fn to_number_ref(value: &Value) -> f64 {
    if let Value::Number(v) = value {
        *v
    } else {
        0 as f64
    }
}

#[allow(unused_imports)]
pub(crate) mod prelude {
    pub(crate) use super::{
        to_array, to_array_ref, to_number, to_number_ref, to_object, to_object_ref, to_string,
        to_string_ref, DocumentParser, Value, ARRAY, FALSE, NULL, NUMBER, OBJECT, STRING,
        STRING_CONTENT, TRUE,
    };
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;

    use tower_lsp::lsp_types::Position;

    use super::DocumentParser;
    use crate::museair::BfastHashMap;
    use crate::parser::Value;

    #[test]
    fn test_get_namespace() {
        let n1 = DocumentParser::default("{ // 注释\n\"namespace\"  :  \"test\"  }");
        assert_eq!(Arc::from("test"), n1.namespace);
        let n1 = DocumentParser::default("{\"a\":{\"namespace\":\"nested\"},\"namespace\":\"top\"}");
        assert_eq!(Arc::from("top"), n1.namespace);
    }

    #[test]
    fn test_parse_simple_object() {
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

        let parser = DocumentParser::default(json);
        let root = parser.root().unwrap();

        // Test basic parsing
        assert_eq!(root.kind(), "object");

        // Test object field access
        let name_node = parser.field(root, "name").unwrap();
        assert_eq!(parser.string(name_node), Some("John Doe".to_string()));

        let age_node = parser.field(root, "age").unwrap();
        assert_eq!(parser.float(age_node), Some(30.0));
        assert_eq!(parser.int(age_node), Some(30));

        let is_active_node = parser.field(root, "is_active").unwrap();
        assert_eq!(parser.bool(is_active_node), Some(true));

        // Test array access
        let scores_node = parser.field(root, "scores").unwrap();
        assert_eq!(parser.element(scores_node, 0).and_then(|n| parser.float(n)), Some(95.5));
        assert_eq!(parser.element(scores_node, 1).and_then(|n| parser.float(n)), Some(89.0));

        // Test nested object
        let address_node = parser.field(root, "address").unwrap();
        let street_node = parser.field(address_node, "street").unwrap();
        assert_eq!(parser.string(street_node), Some("123 Main St".to_string()));
    }

    #[test]
    fn test_node_conversions() {
        let json = r#"{
            "string": "test",
            "integer": 42,
            "float": 3.14,
            "true": true,
            "false": false,
            "null": null
        }"#;

        let parser = DocumentParser::default(json);
        let root = parser.root().unwrap();

        // Test string conversion
        let string_node = parser.field(root, "string").unwrap();
        assert_eq!(parser.string(string_node), Some("test".to_string()));
        assert_eq!(parser.float(string_node), None);
        assert_eq!(parser.bool(string_node), None);

        // Test number conversions
        let int_node = parser.field(root, "integer").unwrap();
        assert_eq!(parser.float(int_node), Some(42.0));
        assert_eq!(parser.int(int_node), Some(42));

        let float_node = parser.field(root, "float").unwrap();
        assert_eq!(parser.float(float_node), Some(3.14));
        assert_eq!(parser.int(float_node), Some(3)); // truncation expected

        // Test boolean conversions
        let true_node = parser.field(root, "true").unwrap();
        assert_eq!(parser.bool(true_node), Some(true));
        assert_eq!(parser.string(true_node), None);

        let false_node = parser.field(root, "false").unwrap();
        assert_eq!(parser.bool(false_node), Some(false));

        // Test null
        let null_node = parser.field(root, "null").unwrap();
        assert_eq!(null_node.kind(), "null");
    }

    #[test]
    fn test_array_operations() {
        let json = r#"{
            "empty_array": [],
            "number_array": [1, 2, 3],
            "mixed_array": [1, "two", false, null]
        }"#;

        let parser = DocumentParser::default(json);
        let root = parser.root().unwrap();

        // Test empty array
        let empty_array = parser.field(root, "empty_array").unwrap();
        assert_eq!(parser.element(empty_array, 0), None);

        // Test number array
        let number_array = parser.field(root, "number_array").unwrap();
        assert_eq!(parser.element(number_array, 0).and_then(|n| parser.int(n)), Some(1));
        assert_eq!(parser.element(number_array, 1).and_then(|n| parser.int(n)), Some(2));
        assert_eq!(parser.element(number_array, 2).and_then(|n| parser.int(n)), Some(3));
        assert_eq!(parser.element(number_array, 3), None); // Out of bounds

        // Test mixed array
        let mixed_array = parser.field(root, "mixed_array").unwrap();
        assert_eq!(parser.element(mixed_array, 0).and_then(|n| parser.int(n)), Some(1));
        assert_eq!(
            parser.element(mixed_array, 1).and_then(|n| parser.string(n)),
            Some("two".to_string())
        );
        assert_eq!(parser.element(mixed_array, 2).and_then(|n| parser.bool(n)), Some(false));
        assert_eq!(parser.element(mixed_array, 3).map(|n| n.kind()), Some("null"));
    }

    #[test]
    fn test_hashmap_conversion() {
        let json = r#"{
            "name": "Test",
            "value": 123,
            "nested": {
                "key": "value"
            }
        }"#;

        let parser = DocumentParser::default(json);
        let map = parser.hashmap();

        let mut expected = BfastHashMap::default();
        expected.insert("name".to_string(), Value::String("Test".to_string()));
        expected.insert("value".to_string(), Value::Number(123.0));

        let mut nested = HashMap::new();
        nested.insert("key".to_string(), Value::String("value".to_string()));
        expected.insert("nested".to_string(), Value::Object(nested));

        assert_eq!(map, expected);
    }

    #[test]
    fn test_empty_document() {
        let parser = DocumentParser::default("");
        let root = parser.root();
        assert!(root.is_none());
        assert!(parser.hashmap().is_empty());
    }

    #[test]
    fn test_invalid_access() {
        let json = r#"{"valid": true}"#;
        let parser = DocumentParser::default(json);
        let root = parser.root().unwrap();

        // Access non-existent field
        assert!(parser.field(root, "invalid").is_none());

        // Try conversions on wrong node types
        let valid_node = parser.field(root, "valid").unwrap();
        assert!(parser.string(valid_node).is_none());
        assert!(parser.float(valid_node).is_none());
    }

    #[test]
    fn test_get_node_at_position() {
        let json = r#"
    {
        "user": {
            "name": "Alice",
            "scores": [90, 85.5],
            "friends": [
                { "name": "Bob", "age": 30 }
            ]
        }
    }"#;

        let position = Position {
            line:      5,
            character: 20,
        };
        let parser = DocumentParser::default(json);
        let result = parser.get_node_at_position(position);
        assert!(result.is_some(), "Should find a node at the given position");
        let node = result.unwrap();
        assert_eq!(node.kind(), "string_content");
        assert_eq!(parser.rope.byte_slice(node.byte_range()).to_string(), "friends");
    }

    #[test]
    fn test_get_parents() {
        let json = r#"{
    "namespace": "test",
    "test1": {
        "$var1": "test111",
        "test_array": [
        {
            "test2": {
            
            }
        }
        ]
    }
}"#;
        let position = Position {
            line:      7,
            character: 12,
        };
        let parser = DocumentParser::default(json);
        let result = parser.get_node_at_position(position);
        assert!(result.is_some(), "Should find a node at the given position");
        let node = result.unwrap();
        let result = parser.get_parents(&node);
        assert_eq!(result.len(), 3);
        assert!(parser.print_node(&result[0]).contains("test2"));
        assert!(parser.print_node(&result[1]).contains("test_array"));
        assert!(parser.print_node(&result[2]).contains("test1"));
    }

    #[test]
    fn test_get_parent_pair_node() {
        let json = r#"
    {
        "user@x1.x2": {
            "name": "Alice";
            "controls": [
                {
                    "test2@x3.x4": {
                        "test": 1
                    }
                }
            ]
        }
    }"#;
        let pos1 = Position {
            line:      3,
            character: 16,
        };
        let pos2 = Position {
            line:      7,
            character: 33,
        };
        let parser = DocumentParser::default(json);

        let pos1 = parser.get_node_at_position(pos1);
        assert!(pos1.is_some(), "Should find a node at the given position");
        let pos1 = pos1.unwrap();
        let pos1 = parser.get_parent_pair_node(&pos1);

        let pos2 = parser.get_node_at_position(pos2);
        assert!(pos2.is_some(), "Should find a node at the given position");
        let pos2 = pos2.unwrap();
        let pos2 = parser.get_parent_pair_node(&pos2);

        assert!(parser.print_node(&pos1).contains("user@x1.x2"));
        assert!(parser.print_node(&pos2).contains("test2@x3.x4"));
    }

    #[test]
    fn test_get_parent_pair_node_root() {
        let json = r#"
    {
        "user@x1.x2": {
            "name": "Alice"
        }
    }"#;
        let pos1 = Position {
            line:      2,
            character: 7,
        };
        let parser = DocumentParser::default(json);

        let pos1 = parser.get_node_at_position(pos1);
        assert!(pos1.is_some(), "Should find a node at the given position");
        let pos1 = pos1.unwrap();
        let pos1 = parser.get_parent_pair_node(&pos1);

        assert!(pos1.kind() == "document");
    }

    #[test]
    fn test_get_string() {
        let json = r#"
    {
        "t1": "\"xxx\"",
        "t2": ""
    }"#;
        let parser = DocumentParser::default(json);
        let r = parser.root();
        let t1 = parser.get_field(&r.unwrap(), "t1");
        assert!(parser.string(t1.unwrap()).unwrap().contains(r#"\"xxx\""#));
        let t2 = parser.get_field(&r.unwrap(), "t2");
        assert!(parser.string(t2.unwrap()).unwrap().contains(r#""#));
    }

    #[test]
    fn test_unicode_escapes() {
        assert_eq!(unescape_zero_copy::unescape_default(r"Hello\x0aworld").unwrap(), "Hello\nworld");
        assert_eq!(unescape_zero_copy::unescape_default(r"你好\x0a世界").unwrap(), "你好\n世界");
        assert_eq!(unescape_zero_copy::unescape_default(r"\u00A0").unwrap(), "\u{00A0}");
        assert_eq!(unescape_zero_copy::unescape_default(r"\u2764").unwrap(), "❤");
        assert_eq!(unescape_zero_copy::unescape_default("\u{1F600}").unwrap(), "😀");
        assert_eq!(unescape_zero_copy::unescape_default("\u{1F468}\u{200D}\u{1F466}").unwrap(), "👨‍👦");
        assert!(unescape_zero_copy::unescape_default(r"\u12").is_err());
        assert!(unescape_zero_copy::unescape_default(r"\u{110000}").is_err());
    }
}
