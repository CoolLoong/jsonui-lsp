use crate::completion_helper::from_color_value_to_color_arr;
use crate::Backend;
use crate::{completion_helper::create_completion, document::Document};
use log::{debug, trace};
use std::borrow::Borrow;
use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt::{self},
    sync::Arc,
    vec,
};
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{
    Color, ColorInformation, CompletionItem, CompletionParams, DidChangeTextDocumentParams,
    DocumentColorParams, Position, Range, Url,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftBrace = '{' as isize,
    RightBrace = '}' as isize,
    LeftBracket = '[' as isize,
    RightBracket = ']' as isize,
    Comma = ',' as isize,
    Colon = ':' as isize,
    Quote = '"' as isize,
    OTHER = '🤪' as isize,
}

pub const TYPE_CR: u8 = 2;
pub const TYPE_ARR: u8 = 3;
pub const TYPE_STR: u8 = 4;
pub const TYPE_NUM: u8 = 5;
pub const TYPE_BOL: u8 = 6;
pub const TYPE_COL: u8 = 7;
pub const TYPE_COM: u8 = 8;

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct Value {
    pub(crate) l: usize,
    pub(crate) r: usize,
    pub(crate) type_id: u8,
    pub(crate) path: Vec<usize>,
    pub(crate) v: Option<Node>,
}

impl Value {
    pub fn is_some(&self) -> bool {
        self.v.is_some()
    }

    pub fn is_none(&self) -> bool {
        self.v.is_none()
    }
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

#[derive(Clone)]
pub enum Node {
    Controls(Vec<Value>),
    Array(Vec<Value>),
    String(Arc<str>),
    Number(f32),
    Bool(bool),
    Colon,
    Comma,
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Controls(controls) => {
                writeln!(f, "{{")?;
                for (i, control) in controls.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ", ")?;
                    }
                    write!(f, "{:?}", control)?;
                }
                write!(f, "\n}}")
            }
            Node::Array(array) => {
                writeln!(f, "[")?;
                for (i, item) in array.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", item)?;
                }
                write!(f, "\n]")
            }
            Node::String(s) => write!(f, "\"{}\"", s),
            Node::Number(n) => write!(f, "{}", n),
            Node::Bool(b) => write!(f, "{}", b),
            Node::Colon => write!(f, "Colon"),
            Node::Comma => write!(f, "Comma"),
        }
    }
}

impl Eq for Node {}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Node::Bool(a), Node::Bool(b)) => a.eq(b),
            (Node::Number(a), Node::Number(b)) => (a - b).abs() < f32::EPSILON,
            (Node::String(a), Node::String(b)) => a.eq(b),
            (Node::Array(a), Node::Array(b)) => a.eq(b),
            (Node::Controls(a), Node::Controls(b)) => a.eq(b),
            (Node::Colon, Node::Colon) => true,
            (Node::Comma, Node::Comma) => true,
            _ => false,
        }
    }
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
    pub fn is_ignore(c: &str) -> bool {
        matches!(c, " " | "\r\n" | "\n")
    }
}

/// `CompleteContext` is a struct used to describe contextual information at the completion position within vscode.
#[derive(Debug)]
pub(crate) struct CompleteContext<'a> {
    pub(crate) control_type: Mutex<Option<Arc<str>>>,
    pub(crate) l: Mutex<usize>,
    pub(crate) r: Mutex<usize>,
    pub(crate) index: Mutex<usize>,
    pub(crate) input_char: Mutex<Arc<str>>,
    pub(crate) nodes: Mutex<Vec<Option<&'a Value>>>,
}

impl<'a> CompleteContext<'a> {
    /// create an empty `CompleteContext`
    pub fn empty() -> Self {
        CompleteContext {
            control_type: Mutex::new(None),
            r: Mutex::new(0),
            l: Mutex::new(0),
            input_char: Mutex::new(Arc::from("")),
            index: Mutex::new(0),
            nodes: Mutex::new(vec![]),
        }
    }
}

/// `PathInfo` is used to record the path information of nodes within the built AST.
struct PathInfo {
    stack: RefCell<VecDeque<usize>>,
    current: RefCell<usize>,
}

impl PathInfo {
    fn new() -> Self {
        PathInfo {
            stack: RefCell::new(VecDeque::new()),
            current: RefCell::new(0),
        }
    }

    /// Gets the current node path.  
    /// The method will automatically increment the index to the next one.  
    fn get_path(&self) -> Vec<usize> {
        let stack = self.stack.borrow_mut();
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

    /// back an index from path stack  
    /// then reset the current index  
    fn back_path(&self, back_index: usize) {
        let mut current = self.current.borrow_mut();
        let mut stack = self.stack.borrow_mut();
        *current = back_index;
        stack.pop_back();
    }

    /// Gets the current node path  
    /// push the currently index to path stack  
    /// then clear the current index  
    fn push_path(&self) -> Vec<usize> {
        let index_value = { *self.current.borrow_mut() };
        let path = self.get_path();
        let mut current = self.current.borrow_mut();
        let mut stack = self.stack.borrow_mut();
        *current = 0;
        stack.push_back(index_value);
        path
    }
}

#[derive(Debug)]
pub(crate) struct Completer {
    document: Document,
    ast: Mutex<Option<Vec<Value>>>,
}

impl Completer {
    pub fn from(str: Arc<str>) -> Self {
        Completer {
            document: Document::from(&str.to_string()),
            ast: Mutex::new(None),
        }
    }

    async fn fill_context<'a>(
        bk: &Backend,
        ast: &'a [Value],
        docs_url: &Url,
        context: &'a CompleteContext<'a>,
    ) {
        //fill context from ast
        let mut result = Vec::new();
        Self::flatten_ast(ast, &mut result);
        let v_index = *context.index.lock().await;
        let node_index = result.iter().rposition(|value| value.r <= v_index);

        //find the neighbors node corresponding to the current index
        let mut neighbors: Vec<Option<&'a Value>> = Vec::with_capacity(5);
        if let Some(i) = node_index {
            let i = i as i32;
            for offset in -1..4 {
                let index = i + offset;
                if index < 0 || index >= result.len() as i32 {
                    neighbors.push(None);
                } else {
                    neighbors.push(Some(result[index as usize]));
                }
            }
        }
        *context.nodes.lock().await = neighbors;

        //find control_type by context
        result.clear();
        Self::flatten_ast_one_layer(ast, &mut result);
        for (index, i) in result.iter().enumerate() {
            if i.type_id == TYPE_STR
                && let Some(Node::String(v)) = &i.v
                && v.as_ref() == "type"
            {
                let type_node_index = index + 2;
                if type_node_index < result.len()
                    && let Some(Node::String(type_v)) = &result[type_node_index].v
                {
                    *context.control_type.lock().await = Some(type_v.clone());
                }
            }
        }

        let mut control_type_mut = context.control_type.lock().await;
        if control_type_mut.is_none()
            && let Some(Node::String(control_name)) = &ast[0].v
        {
            let namespace: Option<Arc<str>> = bk.query_namespace(docs_url).await;
            let control_t = Self::fill_control_type(bk, namespace, control_name).await;
            if let Some(v) = control_t {
                *control_type_mut = Some(Arc::from(v.as_str()));
            } else {
                trace!("cant find type from type_cache {:?}", ast[0].v);
            }
        }
    }

    async fn fill_control_type(
        bk: &Backend,
        namespace: Option<Arc<str>>,
        control_name: &Arc<str>,
    ) -> Option<String> {
        let vec: Vec<&str> = control_name.as_ref().split("@").collect();
        if vec.len() == 1
            && let Some(np) = namespace
        {
            return bk.query_type(np, Arc::from(vec[0])).await;
        } else if vec.len() == 2 {
            let refer_namespace = vec[1];
            let namespace_sp: Vec<&str> = refer_namespace.split(".").collect();
            if namespace_sp.len() == 2 {
                let refer_namespace_n = namespace_sp[0];
                let refer_control_n = namespace_sp[1];
                return bk
                    .query_type(Arc::from(refer_namespace_n), Arc::from(refer_control_n))
                    .await;
            } else if namespace_sp.len() == 1
                && let Some(np) = namespace
            {
                return bk.query_type(np, Arc::from(namespace_sp[0])).await;
            }
        }
        None
    }

    pub async fn update_ast<'a>(&self, context: &'a CompleteContext<'a>, pos: &Position) {
        // get current index in content
        let index = self.document.get_index_from_position(pos).await;
        let index_value;
        if let Some(index_v) = index {
            *context.index.lock().await = index_v;
            index_value = index_v;
        } else {
            return;
        }
        //write the input char currently
        {
            let input_char_index = index_value - 1;
            if let Some(cr) = self.document.get_char(input_char_index).await {
                *context.input_char.lock().await = cr;
            }
        }
        self.build_ast(index_value, context).await;
    }

    pub async fn compelte(
        &self,
        bk: &Backend,
        param: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let context: CompleteContext = CompleteContext::empty();
        self.update_ast(&context, &param.text_document_position.position)
            .await;

        let ast_lock = self.ast.lock().await;
        if let Some(ast) = ast_lock.as_ref() {
            Self::fill_context(
                bk,
                ast,
                &param.text_document_position.text_document.uri,
                &context,
            )
            .await;
            let lang = bk.lang.lock().await;
            create_completion(lang.as_ref(), &bk.jsonui_define_map, param, &context, ast).await
        } else {
            None
        }
    }

    pub async fn complete_color(&self) -> Option<Vec<ColorInformation>> {
        trace!("complete_color");
        let pos = {
            let ast = self.ast.lock().await;
            if ast.is_none() {
                return None;
            }
            let inputs: &Vec<Value> = ast.as_ref().unwrap();
            if inputs.len() > 2
                && let Some(pos) = self.document.get_position_from_index(inputs[2].l + 1).await
            {
                Some(pos)
            } else {
                None
            }
        };
        if let Some(pos_v) = pos{
            let context: CompleteContext = CompleteContext::empty();
            self.update_ast(&context, &pos_v).await;
        }else{
            return None;
        }
        
        let ast = self.ast.lock().await;
        if ast.is_none() {
            return None;
        }
        let inputs: &Vec<Value> = ast.as_ref().unwrap();
        let mut results = Vec::new();
        Self::flatten_ast(inputs, &mut results);

        let mut color_infos = Vec::new();
        let mut iter = results.iter();
        while let Some(color_v) = iter
            .by_ref()
            .skip_while(|r| !matches!(&r.v, Some(Node::String(v)) if v.as_ref() == "color"))
            .nth(2)
        {
            if let (Some(left_v), Some(right_v)) = (
                self.document.get_position_from_index(color_v.l).await,
                self.document.get_position_from_index(color_v.r).await,
            ) {
                if let Some(color) = from_color_value_to_color_arr(color_v) {
                    color_infos.push(ColorInformation {
                        range: Range {
                            start: left_v,
                            end: right_v,
                        },
                        color,
                    });
                }
            }
        }

        if color_infos.is_empty() {
            None
        } else {
            Some(color_infos)
        }
    }

    pub async fn update_document(&self, request: &DidChangeTextDocumentParams) {
        self.document.apply_change(request).await;
    }

    fn not_quote_state(stack: &VecDeque<Value>) -> bool {
        stack
            .back()
            .map_or(true, |node| node.type_id != TYPE_STR || node.v.is_some())
    }

    async fn build_ast<'a>(&self, index: usize, context: &'a CompleteContext<'a>) {
        let indices = self.document.get_boundary_indices(index).await;
        if let Some((l, r)) = indices {
            let chars = self.document.content_chars.lock().await;
            let splice_control = &chars[l..r + 1];
            let mut stack: VecDeque<Value> = VecDeque::new();
            let mut str_builder = String::new();
            let iter = splice_control.iter().enumerate();
            let path_info: &PathInfo = &PathInfo::new();

            for (index, c) in iter {
                let i = index + l;
                let str = c.as_ref();
                match str.into() {
                    Token::Quote => Self::handle_quote(&mut stack, &mut str_builder, i, path_info),
                    Token::LeftBrace if Self::not_quote_state(&stack) => stack.push_back(Value {
                        l: i,
                        r: 0,
                        type_id: TYPE_CR,
                        path: path_info.push_path(),
                        v: None,
                    }),
                    Token::LeftBracket if Self::not_quote_state(&stack) => stack.push_back(Value {
                        l: i,
                        r: 0,
                        type_id: TYPE_ARR,
                        path: path_info.push_path(),
                        v: None,
                    }),
                    Token::Colon if Self::not_quote_state(&stack) => {
                        stack.push_back(Value {
                            l: i,
                            r: i + 1,
                            type_id: TYPE_COL,
                            path: path_info.get_path(),
                            v: Some(Node::Colon),
                        });
                    }
                    Token::RightBrace if Self::not_quote_state(&stack) => {
                        Self::handle_right_brace(&mut stack, i, path_info)
                    }
                    Token::RightBracket if Self::not_quote_state(&stack) => {
                        Self::handle_right_bracket(&mut stack, i, path_info)
                    }
                    Token::Comma if Self::not_quote_state(&stack) => {
                        Self::handle_comma(&mut stack, &mut str_builder, i, path_info)
                    }
                    _ if Token::is_ignore(str) => continue,
                    _ => str_builder.push_str(str),
                }
            }
            let ast_result: Vec<Value> = stack.into_iter().collect();
            *self.ast.lock().await = if ast_result.is_empty() {
                None
            } else {
                Some(ast_result)
            };
            *context.l.lock().await = l;
            *context.r.lock().await = r;
        } else {
            debug!("build ast error");
        }
    }

    fn flatten_ast<'a>(input: &'a [Value], result: &mut Vec<&'a Value>) {
        for i in input {
            if let Some(v) = &i.v {
                result.push(i);
                match v {
                    Node::Array(sub) | Node::Controls(sub) => {
                        Self::flatten_ast(sub, result);
                    }
                    _ => {}
                }
            }
        }
    }

    fn flatten_ast_one_layer<'a>(input: &'a [Value], result: &mut Vec<&'a Value>) {
        for i in input {
            if let Some(v) = &i.v {
                result.push(i);
                if let Node::Array(sub) | Node::Controls(sub) = v {
                    for sub_value in sub {
                        result.push(sub_value);
                    }
                }
            }
        }
    }

    fn handle_quote(
        stack: &mut VecDeque<Value>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        if !stack.iter().any(|f| TYPE_STR == f.type_id && f.v.is_none()) {
            str_builder.clear(); //remove potential comments
            stack.push_back(Value {
                l: index,
                r: 0,
                type_id: TYPE_STR,
                path: vec![],
                v: None,
            });
        } else if let Some(value) = stack.pop_back()
            && TYPE_STR == value.type_id
            && value.v.is_none()
        {
            stack.push_back(Value {
                l: value.l,
                r: index + 1,
                type_id: TYPE_STR,
                path: path_info.get_path(),
                v: Some(Node::String(Arc::from(str_builder.clone()))),
            });
            str_builder.clear();
        }
    }

    fn handle_right_brace(stack: &mut VecDeque<Value>, index: usize, path_info: &PathInfo) {
        let mut tmp_vec: Vec<Value> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_CR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                path_info.back_path(f.path.last().unwrap() + 1);
                stack.push_back(Value {
                    l: f.l,
                    r: index + 1,
                    type_id: TYPE_CR,
                    path: f.path,
                    v: Some(Node::Controls(tmp_vec)),
                });
                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_right_bracket(stack: &mut VecDeque<Value>, index: usize, path_info: &PathInfo) {
        let mut tmp_vec: Vec<Value> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_ARR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                path_info.back_path(f.path.last().unwrap() + 1);
                stack.push_back(Value {
                    l: f.l,
                    r: index + 1,
                    type_id: TYPE_ARR,
                    path: f.path,
                    v: Some(Node::Array(tmp_vec)),
                });
                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_comma(
        stack: &mut VecDeque<Value>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        if !str_builder.is_empty() {
            let str_c = str_builder.len();
            if let Ok(boolean) = str_builder.parse::<bool>() {
                stack.push_back(Value {
                    l: index,
                    r: index + str_c + 1,
                    type_id: TYPE_BOL,
                    path: path_info.get_path(),
                    v: Some(Node::Bool(boolean)),
                });
            } else if let Ok(float) = str_builder.parse::<f32>() {
                stack.push_back(Value {
                    l: index,
                    r: index + str_c + 1,
                    type_id: TYPE_NUM,
                    path: path_info.get_path(),
                    v: Some(Node::Number(float)),
                });
            }
            str_builder.clear();
        }

        stack.push_back(Value {
            l: index,
            r: index + 1,
            type_id: TYPE_COM,
            path: path_info.get_path(),
            v: Some(Node::Comma),
        });
    }
}

#[cfg(test)]
mod tests {
    use tower_lsp::lsp_types::Position;

    use super::*;

    const EXAMPLE1: &str = include_str!("../test/achievement_screen.json");

    #[tokio::test]
    async fn test_get_content_index() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer
            .document
            .get_index_from_position(&Position {
                line: 16,
                character: 5,
            })
            .await;
        let re_warp = completer.document.get_char(index.unwrap()).await;
        let result = re_warp.unwrap();
        assert_eq!(result.as_ref(), "t");

        let result = completer
            .document
            .get_index_from_position(&Position {
                line: 44,
                character: 1,
            })
            .await;
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_build_ast() {
        let completer = Completer::from(EXAMPLE1.into());
        let context = &CompleteContext::empty();
        completer
            .update_ast(
                context,
                &Position {
                    line: 16,
                    character: 5,
                },
            )
            .await;
        {
            let result = completer.ast.lock().await;
            assert!(result.is_some());
        }

        let completer = Completer::from(EXAMPLE1.into());
        let context = &CompleteContext::empty();
        completer
            .update_ast(
                context,
                &Position {
                    line: 0,
                    character: 1,
                },
            )
            .await;
        let result = completer.ast.lock().await;
        assert!(result.is_none());
    }
}
