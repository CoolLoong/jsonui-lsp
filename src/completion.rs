use crate::document::Document;
use crate::Backend;
use log::{debug, error};
use std::{
    collections::{HashMap, VecDeque},
    fmt,
    hash::Hash,
    io::{BufReader, Read},
    sync::Arc,
};
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionParams, DidChangeTextDocumentParams, Position,
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

const TYPE_KV: u8 = 1;
const TYPE_CR: u8 = 2;
const TYPE_ARR: u8 = 3;
const TYPE_STR: u8 = 4;
const TYPE_NUM: u8 = 5;
const TYPE_BOL: u8 = 6;
const TYPE_COL: u8 = 7;
const TYPE_COM: u8 = 8;

#[derive(Clone, PartialEq, Eq)]
pub struct Value {
    l: usize,
    r: usize,
    type_id: u8,
    v: Option<Node>,
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
            "( l: {}, r: {}, type_id: {}, v: ",
            self.l, self.r, self.type_id
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
                write!(f, "{{\n")?;
                for (i, control) in controls.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", \n")?;
                    }
                    write!(f, "{:?}", control)?;
                }
                write!(f, "\n}}")
            }
            Node::Array(array) => {
                write!(f, "[\n")?;
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

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '"' => Token::Quote,
            _ => Token::OTHER,
        }
    }
}

impl Into<char> for Token {
    fn into(self) -> char {
        self as u8 as char
    }
}

impl Token {
    pub fn is_ignore(c: &char) -> bool {
        matches!(c, ' ' | '\n')
    }
}

#[derive(Debug)]
struct CompleteContext<'a> {
    control_type: Mutex<Option<Arc<str>>>,
    l: Mutex<usize>,
    r: Mutex<usize>,
    index: Mutex<usize>,
    input_char: Mutex<char>,
    nodes: Mutex<(Option<&'a Value>, Option<&'a Value>, Option<&'a Value>)>,
}

impl<'a> CompleteContext<'a> {
    pub fn empty() -> Self {
        CompleteContext {
            control_type: Mutex::new(None),
            r: Mutex::new(0),
            l: Mutex::new(0),
            input_char: Mutex::new(' '),
            index: Mutex::new(0),
            nodes: Mutex::new((None, None, None)),
        }
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

    async fn find_closest_node<'a>(input: &'a [Value], context: &'a CompleteContext<'a>) {
        let mut result = Vec::new();
        Self::flatten_ast(input, &mut result);
        let v_index = *context.index.lock().await;
        let mut node: Option<&'a Value> = None;
        let mut node_index: usize = 0;
        for (index, value) in result.iter().enumerate() {
            if value.r < v_index {
                node = Some(value);
                node_index = index;
            }
            if value.type_id == TYPE_STR
                && let Some(Node::String(v)) = &value.v
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
        let right_bound: usize = result.len();
        let b1_i = node_index + 1;
        let b1 = if b1_i < right_bound{
            Some(result[b1_i])
        } else {
            None
        };
        let b2_i = node_index + 2;
        let b2 = if b2_i < right_bound{
            Some(result[b2_i])
        } else {
            None
        };
        *context.nodes.lock().await = (node, b1, b2);
    }

    async fn get_boundary_indices(&self, index: usize) -> Option<(usize, usize)> {
        let mut stack: VecDeque<char> = VecDeque::new();
        let content = self.document.content_cache.lock().await;
        let (forward, backward) = content.split_at(index);
        if forward.is_empty() || backward.is_empty() {
            return None;
        }
        let f_len = forward.chars().count();
        let mut start_index: Option<usize> = None;
        let mut forward_iter = forward.chars().rev().peekable().enumerate();
        while let Some((_, ch)) = forward_iter.next() {
            if ch == '{' {
                if stack.is_empty() {
                    let opt = forward_iter
                        .skip_while(|(_, c)| *c != '"')
                        .skip(1)
                        .skip_while(|(_, c)| *c != '"')
                        .skip(1)
                        .next();
                    if let Some((index, _)) = opt {
                        start_index = Some(f_len - index);
                        break;
                    } else {
                        return None;
                    }
                } else {
                    stack.pop_back();
                }
            } else if ch == '}' {
                stack.push_back(ch);
            }
        }
        if start_index.is_none() {
            return None;
        }

        stack.clear();
        let mut end_index: Option<usize> = None;
        let mut backend_iter = backward.chars().peekable().enumerate();
        while let Some((index, ch)) = backend_iter.next() {
            if ch == '}' {
                if stack.is_empty() {
                    end_index = Some(index + f_len);
                    break;
                } else {
                    stack.pop_back();
                }
            } else if ch == '{' {
                stack.push_back(ch);
            }
        }
        if end_index.is_none() {
            return None;
        }

        Some((start_index.unwrap(), end_index.unwrap()))
    }

    pub async fn compelte(
        &self,
        bk: &Backend,
        param: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let pos = &param.text_document_position.position;

        let context: CompleteContext = CompleteContext::empty();
        let index = self.document.get_content_index(pos).await;
        let index_value;
        if let Some(index_v) = index {
            *context.index.lock().await = index_v;
            index_value = index_v;
        } else {
            return None;
        }

        {
            let input_char_index = index_value - 1;
            let content = self.document.content_cache.lock().await;
            if input_char_index > 0
                && let Some(cr) = content.chars().nth(input_char_index)
            {
                *context.input_char.lock().await = cr;
            }
        }

        let cache_miss = {
            let ast = self.ast.lock().await;
            if let Some(ast) = ast.as_ref() {
                !ast.iter().any(|f| f.l <= index_value && index_value <= f.r)
            } else {
                true
            }
        };

        if cache_miss {
            debug!("rebuild ast");
            self.build_ast(index_value, &context).await;
        }

        let ast = self.ast.lock().await;
        if let Some(input) = ast.as_ref() {
            Self::find_closest_node(input, &context).await;
            let mut control_type_mut = context.control_type.lock().await;
            if control_type_mut.is_none()
                && let Some(Node::String(v)) = &input[0].v
            {
                let url = &param.text_document_position.text_document.uri;
                let namespace: Option<Arc<str>> = bk.query_namespace(url).await;
                let control_t = self.fill_control_type(bk, namespace, v).await;
                if let Some(v) = control_t {
                    *control_type_mut = Some(Arc::from(v.as_str()));
                }
                {
                    return None;
                }
            }
        } else {
            return None;
        }

        self.create_completion_information(param, &context, &bk.jsonui_define_map)
    }

    async fn fill_control_type(
        &self,
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

    fn create_completion_information(
        &self,
        param: &CompletionParams,
        context: &CompleteContext,
        define_map: &HashMap<String, Vec<serde_json::Value>>,
    ) -> Option<Vec<CompletionItem>> {
        debug!("{:?}", context);
        None
    }

    pub async fn update_ast(&self) {
        let mut ast = self.ast.lock().await;
        if ast.is_some() {
            *ast = None
        }
    }

    pub async fn update_document(&self, request: &DidChangeTextDocumentParams) {
        self.document.apply_change(request).await;
    }

    async fn build_ast<'a>(&self, index: usize, context: &'a CompleteContext<'a>) {
        let indices = self.get_boundary_indices(index).await;
        if let Some((l, r)) = indices {
            let content = self.document.content_cache.lock().await;
            let splice_control = &content[l..r + 1];
            let mut peekable = splice_control
                .chars()
                .enumerate()
                .map(|(i, c)| (i + l, c))
                .peekable();
            let mut stack: VecDeque<Value> = VecDeque::new();
            let mut str_builder = String::new();

            while let Some((index, c)) = peekable.next() {
                match c.into() {
                    Token::Quote => Self::handle_quote(&mut stack, &mut str_builder, index),
                    Token::LeftBrace => stack.push_back(Value {
                        l: index,
                        r: 0,
                        type_id: TYPE_CR,
                        v: None,
                    }),
                    Token::LeftBracket => stack.push_back(Value {
                        l: index,
                        r: 0,
                        type_id: TYPE_ARR,
                        v: None,
                    }),
                    Token::Colon => stack.push_back(Value {
                        l: index,
                        r: index + 1,
                        type_id: TYPE_COL,
                        v: Some(Node::Colon),
                    }),
                    Token::RightBrace => Self::handle_right_brace(&mut stack, index),
                    Token::RightBracket => Self::handle_right_bracket(&mut stack, index),
                    Token::Comma => Self::handle_comma(&mut stack, &mut str_builder, index),
                    Token::OTHER if Token::is_ignore(&c) => continue,
                    Token::OTHER => str_builder.push(c),
                }
            }

            *self.ast.lock().await = Some(stack.into_iter().collect());
            *context.l.lock().await = l;
            *context.r.lock().await = r;
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

    fn handle_quote(stack: &mut VecDeque<Value>, str_builder: &mut String, index: usize) {
        if !stack.iter().any(|f| TYPE_STR == f.type_id && f.v.is_none()) {
            stack.push_back(Value {
                l: index,
                r: 0,
                type_id: TYPE_STR,
                v: None,
            });
        } else {
            if let Some(value) = stack.pop_back()
                && TYPE_STR == value.type_id
                && value.v.is_none()
            {
                stack.push_back(Value {
                    l: value.l,
                    r: index + 1,
                    type_id: TYPE_STR,
                    v: Some(Node::String(Arc::from(str_builder.clone()))),
                });
                str_builder.clear();
            }
        }
    }

    fn handle_right_brace(stack: &mut VecDeque<Value>, index: usize) {
        let mut tmp_vec: Vec<Value> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_CR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                stack.push_back(Value {
                    l: f.l,
                    r: index + 1,
                    type_id: TYPE_CR,
                    v: Some(Node::Controls(tmp_vec)),
                });
                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_right_bracket(stack: &mut VecDeque<Value>, index: usize) {
        let mut tmp_vec: Vec<Value> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_ARR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                stack.push_back(Value {
                    l: f.l,
                    r: index + 1,
                    type_id: TYPE_ARR,
                    v: Some(Node::Array(tmp_vec)),
                });
                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_comma(stack: &mut VecDeque<Value>, str_builder: &mut String, index: usize) {
        if !str_builder.is_empty() {
            let str_c = str_builder.len();
            if let Ok(boolean) = str_builder.parse::<bool>() {
                stack.push_back(Value {
                    l: index,
                    r: index + str_c + 1,
                    type_id: TYPE_BOL,
                    v: Some(Node::Bool(boolean)),
                });
            } else if let Ok(float) = str_builder.parse::<f32>() {
                stack.push_back(Value {
                    l: index,
                    r: index + str_c + 1,
                    type_id: TYPE_NUM,
                    v: Some(Node::Number(float)),
                });
            }
            str_builder.clear();
        }

        stack.push_back(Value {
            l: index,
            r: index + 1,
            type_id: TYPE_COM,
            v: Some(Node::Comma),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &'static str = include_str!("../test/achievement_screen.json");

    #[tokio::test]
    async fn test_get_content_index() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer
            .document
            .get_content_index(&Position {
                line: 16,
                character: 12,
            })
            .await;
        let docu = completer.document.content_cache.lock().await;
        let result = docu.chars().nth(index.unwrap());
        assert_eq!(result, Some(':'));
    }

    #[tokio::test]
    async fn test_get_content_index_error() {
        let completer = Completer::from(EXAMPLE1.into());
        let result = completer
            .document
            .get_content_index(&Position {
                line: 15,
                character: 1,
            })
            .await;
        assert_eq!(result, None);
    }
    #[tokio::test]
    async fn test_get_content_index_error_2() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer
            .document
            .get_content_index(&Position {
                line: 14,
                character: 21,
            })
            .await;
        let docu = completer.document.content_cache.lock().await;
        let result = docu.chars().nth(index.unwrap());
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_get_boundary_indices() {
        let completer = Completer::from(EXAMPLE1.into());

        let v: usize = completer
            .document
            .get_content_index(&Position {
                line: 2,
                character: 25,
            })
            .await
            .unwrap();

        let result = completer.get_boundary_indices(v).await;
        let (l, r) = result.unwrap();
        let docu = completer.document.content_cache.lock().await;
        let lc = docu.chars().nth(l).unwrap();
        let rc = docu.chars().nth(r).unwrap();
        assert_eq!((lc, rc), ('"', '}'));
    }

    #[tokio::test]
    async fn test_get_boundary_indices_error() {
        let completer = Completer::from(EXAMPLE1.into());

        let v = completer
            .document
            .get_content_index(&Position {
                line: 14,
                character: 20,
            })
            .await
            .unwrap();

        let result = completer.get_boundary_indices(v).await;
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_build_ast() {
        let completer = Completer::from(EXAMPLE1.into());

        let v = completer
            .document
            .get_content_index(&Position {
                line: 2,
                character: 25,
            })
            .await
            .unwrap();
        let mut context = &CompleteContext::empty();
        completer.build_ast(v, &mut context).await;
        let result = completer.ast.lock().await;
        assert!(result.is_some());
    }

    #[tokio::test]
    async fn test_build_ast_error() {
        let completer = Completer::from(EXAMPLE1.into());
        let v = completer
            .document
            .get_content_index(&Position {
                line: 14,
                character: 21,
            })
            .await
            .unwrap();

        let mut context = &CompleteContext::empty();
        completer.build_ast(v, &mut context).await;
        let result = completer.ast.lock().await;
        assert!(result.is_none());
    }
}
