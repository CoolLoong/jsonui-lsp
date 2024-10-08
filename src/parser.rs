use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::fmt::{self};
use std::sync::Arc;

use dashmap::DashMap;
use log::trace;
use tower_lsp::lsp_types::Position;

use crate::document::Document;

/// The `Value` struct represents a parsed source token
///
/// Properties:
///
/// * `l`: represents a left index in `Document`
/// * `r`: represents a right index in `Document`
/// * `type_id`: type constant
/// * `path`: shows the layer of the current `Value`
/// * `v`: parsed value
#[derive(Clone)]
pub(crate) struct Value {
    pub(crate) l:       usize,
    pub(crate) r:       usize,
    pub(crate) type_id: u8,
    pub(crate) path:    Vec<usize>,
    pub(crate) v:       Option<ParsedToken>,
}
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "( l: {}, r: {}, type_id: {}, path: {:?}, v: ",
            self.l, self.r, self.type_id, self.path,
        )?;
        match &self.v {
            Some(node) => write!(f, "{:?}", node)?,
            None => write!(f, "None")?,
        }
        write!(f, " )")
    }
}

/// Enum `ParsedToken` with different variants representing parsed tokens
/// in a data structure. The variants include `Controls` and `Array`, which contain vectors of `Value`,
/// `String` which contains an `Arc<str>`, `Number` which contains a floating-point number, `Bool` which
/// contains a boolean value, and `Colon`, `Comma`.
#[derive(Clone)]
pub enum ParsedToken {
    Controls(Vec<Value>),
    Array(Vec<Value>),
    String(Arc<str>),
    Number(f32),
    Bool(bool),
    Colon,
    Comma,
}
impl fmt::Debug for ParsedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_indented(f: &mut fmt::Formatter, tokens: &[Value], indent_level: usize) -> fmt::Result {
            let indent = " ".repeat(indent_level * 4);
            writeln!(f, "{}{{", indent)?;
            for (i, token) in tokens.iter().enumerate() {
                if i > 0 {
                    writeln!(f, ",")?;
                }
                write!(f, "{}", indent)?;
                write!(f, "{:?}", token)?;
            }
            writeln!(f, "\n{}}}", indent)
        }

        match self {
            ParsedToken::Controls(controls) => fmt_indented(f, controls, 1),
            ParsedToken::Array(array) => {
                writeln!(f, "[")?;
                for (i, item) in array.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ",")?;
                    }
                    write!(f, "{:?}", item)?;
                }
                writeln!(f, "\n]")
            }
            ParsedToken::String(s) => write!(f, "\"{}\"", s),
            ParsedToken::Number(n) => write!(f, "{}", n),
            ParsedToken::Bool(b) => write!(f, "{}", b),
            ParsedToken::Colon => write!(f, "Colon"),
            ParsedToken::Comma => write!(f, "Comma"),
        }
    }
}

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
struct PathInfo {
    stack:   RefCell<VecDeque<usize>>,
    current: RefCell<usize>,
}
impl PathInfo {
    fn new() -> Self {
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
    fn gen_path(&self) -> Vec<usize> {
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
    fn pop(&self, back_index: usize) {
        *self.current.borrow_mut() = back_index;
        self.stack.borrow_mut().pop_back();
    }

    /// This function is used to signify traversal deeper into the AST, where a new index of nesting
    /// is entered. The current index is pushed onto the stack, and the `current` index is reset to `0`
    /// to represent the start of a new subtree.
    fn push(&self) -> Vec<usize> {
        let index_value = { *self.current.borrow_mut() };
        let path = self.gen_path();
        let mut current = self.current.borrow_mut();
        let mut stack = self.stack.borrow_mut();
        *current = 0;
        stack.push_back(index_value);
        path
    }
}

/// Defines an enum named `Token` with variants representing different characters as
/// their integer values. Each variant is assigned a character value casted to an `isize` type
#[derive(Debug, Clone)]
pub enum Token {
    LeftBrace    = '{' as isize,
    RightBrace   = '}' as isize,
    LeftBracket  = '[' as isize,
    RightBracket = ']' as isize,
    Comma        = ',' as isize,
    Colon        = ':' as isize,
    Quote        = '"' as isize,
    OTHER        = '🤪' as isize,
}
impl From<&str> for Token {
    fn from(value: &str) -> Self {
        match value {
            "{" => Token::LeftBrace,
            "}" => Token::RightBrace,
            "[" => Token::LeftBracket,
            "]" => Token::RightBracket,
            "," => Token::Comma,
            ":" => Token::Colon,
            "\"" => Token::Quote,
            _ => Token::OTHER,
        }
    }
}
impl From<Token> for char {
    fn from(val: Token) -> Self {
        val as u8 as char
    }
}
impl Token {
    /// The function `is_ignore` returns a boolean value indicating whether the input character `c` is a
    /// whitespace character (space), carriage return ("\r\n"), or newline ("\n").
    pub fn is_ignore(c: &str) -> bool {
        matches!(c, " " | "\r\n" | "\n")
    }
}

pub const TYPE_CR: u8 = 2;
pub const TYPE_ARR: u8 = 3;
pub const TYPE_STR: u8 = 4;
pub const TYPE_NUM: u8 = 5;
pub const TYPE_BOL: u8 = 6;
pub const TYPE_COL: u8 = 7;
pub const TYPE_COM: u8 = 8;

pub struct Parser {
    symbol_t:     DashMap<String, (usize, usize)>,
    symbol_ref_t: DashMap<String, Vec<(usize, usize)>>,
    keyword:      HashSet<String>,
}

impl Parser {
    pub fn new(keyword: HashSet<String>) -> Self {
        Parser {
            symbol_t: DashMap::with_shard_amount(2),
            symbol_ref_t: DashMap::with_shard_amount(2),
            keyword,
        }
    }

    pub async fn init(&self, doc: &Vec<Document>) {
        for i in doc.iter() {
            let pos: Option<Position> = {
                let chars = i.content_chars.lock().await;
                if let Some(index) = chars.iter().position(|s| s.as_ref() == "{") {
                    i.get_position_from_index(index + 1).await
                } else {
                    None
                }
            };
            if let Some(pos_v) = pos {
                let index = i.get_index_from_position(pos_v).await;
                let b: Option<(usize, usize)> = if let Some(index_v) = index {
                    i.get_boundary_indices(index_v).await
                } else {
                    None
                };
                if let Some(tuple) = b {
                    let r = self.parse(tuple, i).await;
                    if let Some(rv) = r {
                        
                    }
                }
            }
        }
    }

    pub async fn parse(&self, pos: (usize, usize), doc: &Document) -> Option<Vec<Value>> {
        let (l, r) = pos;

        let chars = doc.content_chars.lock().await;
        let splice_control = &chars[l..r + 1];
        let mut stack: VecDeque<Value> = VecDeque::new();
        let mut str_builder = String::new();
        let iter = splice_control.iter().enumerate();
        let path_info: &PathInfo = &PathInfo::new();

        // trace!("{:?}", splice_control);
        for (index, c) in iter {
            let i = index + l;
            let str = c.as_ref();
            match str.into() {
                Token::Quote => Self::handle_quote(&mut stack, &mut str_builder, i, path_info),
                Token::LeftBrace if Self::not_quote_state(&stack) => stack.push_back(Value {
                    l:       i,
                    r:       0,
                    type_id: TYPE_CR,
                    path:    path_info.push(),
                    v:       None,
                }),
                Token::LeftBracket if Self::not_quote_state(&stack) => stack.push_back(Value {
                    l:       i,
                    r:       0,
                    type_id: TYPE_ARR,
                    path:    path_info.push(),
                    v:       None,
                }),
                Token::Colon if Self::not_quote_state(&stack) => {
                    stack.push_back(Value {
                        l:       i,
                        r:       i + 1,
                        type_id: TYPE_COL,
                        path:    path_info.gen_path(),
                        v:       Some(ParsedToken::Colon),
                    });
                }
                Token::RightBrace if Self::not_quote_state(&stack) => {
                    Self::handle_right_brace(&mut stack, i, path_info)
                }
                Token::RightBracket if Self::not_quote_state(&stack) => {
                    Self::handle_right_bracket(&mut stack, &mut str_builder, i, path_info)
                }
                Token::Comma if Self::not_quote_state(&stack) => {
                    Self::handle_comma(&mut stack, &mut str_builder, i, path_info)
                }
                _ if Self::not_quote_state(&stack) && Token::is_ignore(str) => continue,
                _ => str_builder.push_str(str),
            }
        }
        let r: Vec<Value> = stack.into_iter().collect();
        if r.is_empty() {
            trace!("build ast is none");
            None
        } else {
            Some(r)
        }
    }

    fn handle_symbol(self, doc: &Document, ast: &Vec<Value>) {
        for i in 0..ast.len() {
            let current = &ast[i];

            let prev = if i > 0 { Some(&ast[i - 1]) } else { None };

            let next = if i < ast.len() - 1 {
                Some(&ast[i + 1])
            } else {
                None
            };

            if let Some(next_v) = next
                && next_v.type_id == TYPE_COL
                && current.type_id == TYPE_STR
            {
                let v = &current.v;
                let vv = v.as_ref().unwrap();
                match vv {
                    ParsedToken::String(s) => {
                        let cur_str = s.to_string();
                        if !self.keyword.contains(&cur_str) {
                            let key = doc.namespace.clone() + "." + cur_str.as_ref();
                            self.symbol_t.insert(key, (current.l, current.r));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// The function `collect_value` parses a string into a boolean or float value and pushes to stack
    fn collect_value(
        stack: &mut VecDeque<Value>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        if !str_builder.is_empty() {
            let str_c = str_builder.len();
            if let Ok(boolean) = str_builder.parse::<bool>() {
                stack.push_back(Value {
                    l:       index,
                    r:       index + str_c + 1,
                    type_id: TYPE_BOL,
                    path:    path_info.gen_path(),
                    v:       Some(ParsedToken::Bool(boolean)),
                });
            } else if let Ok(float) = str_builder.parse::<f32>() {
                stack.push_back(Value {
                    l:       index,
                    r:       index + str_c + 1,
                    type_id: TYPE_NUM,
                    path:    path_info.gen_path(),
                    v:       Some(ParsedToken::Number(float)),
                });
            }
            str_builder.clear();
        }
    }

    fn not_quote_state(stack: &VecDeque<Value>) -> bool {
        stack
            .back()
            .map_or(true, |node| node.type_id != TYPE_STR || node.v.is_some())
    }

    fn handle_quote(
        stack: &mut VecDeque<Value>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        if !stack.iter().any(|f| TYPE_STR == f.type_id && f.v.is_none()) {
            str_builder.clear(); // remove potential comments
            stack.push_back(Value {
                l:       index,
                r:       0,
                type_id: TYPE_STR,
                path:    vec![],
                v:       None,
            });
        } else if let Some(value) = stack.pop_back()
            && TYPE_STR == value.type_id
            && value.v.is_none()
        {
            stack.push_back(Value {
                l:       value.l,
                r:       index + 1,
                type_id: TYPE_STR,
                path:    path_info.gen_path(),
                v:       Some(ParsedToken::String(Arc::from(str_builder.clone()))),
            });
            str_builder.clear();
        }
    }

    fn handle_right_brace(stack: &mut VecDeque<Value>, index: usize, path_info: &PathInfo) {
        let mut tmp_vec: Vec<Value> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_CR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                path_info.pop(f.path.last().unwrap() + 1);
                stack.push_back(Value {
                    l:       f.l,
                    r:       index + 1,
                    type_id: TYPE_CR,
                    path:    f.path,
                    v:       Some(ParsedToken::Controls(tmp_vec)),
                });
                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_right_bracket(
        stack: &mut VecDeque<Value>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        Self::collect_value(stack, str_builder, index, path_info);
        let mut tmp_vec: Vec<Value> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_ARR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                path_info.pop(f.path.last().unwrap() + 1);
                stack.push_back(Value {
                    l:       f.l,
                    r:       index + 1,
                    type_id: TYPE_ARR,
                    path:    f.path,
                    v:       Some(ParsedToken::Array(tmp_vec)),
                });
                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    /// The `handle_comma` function is represent comma grammar rule,it parses a string into a comma token
    /// and pushes to stack
    fn handle_comma(
        stack: &mut VecDeque<Value>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        Self::collect_value(stack, str_builder, index, path_info);
        stack.push_back(Value {
            l:       index,
            r:       index + 1,
            type_id: TYPE_COM,
            path:    path_info.gen_path(),
            v:       Some(ParsedToken::Comma),
        });
    }
}
