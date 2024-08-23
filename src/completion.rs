use crate::document::Document;
use crate::Backend;
use log::{debug, trace};
use std::{
    collections::{HashMap, VecDeque},
    fmt,
    sync::Arc,
    vec,
};
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemLabelDetails, CompletionParams, DidChangeTextDocumentParams,
    Documentation, MarkupContent,
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

    async fn fill_context<'a>(input: &'a [Value], context: &'a CompleteContext<'a>) {
        //find the neighbors node corresponding to the current index
        let mut result = Vec::new();
        Self::flatten_ast(input, &mut result);
        let v_index = *context.index.lock().await;
        let mut node: Option<&'a Value> = None;
        let mut node_index: usize = 0;
        for (index, value) in result.iter().enumerate() {
            if value.r < v_index {
                trace!("get left node {} {:?}", index, value);
                node = Some(value);
                node_index = index;
            }
        }
        
        //get neighbors node
        let right_bound: usize = result.len();
        let b1_i = node_index + 1;
        let b1 = if b1_i < right_bound {
            Some(result[b1_i])
        } else {
            None
        };
        let b2_i = node_index + 2;
        let b2 = if b2_i < right_bound {
            Some(result[b2_i])
        } else {
            None
        };
        *context.nodes.lock().await = (node, b1, b2);

        //find control_type by context
        result.clear();
        Self::flatten_ast_one_layer(input, &mut result);
        for (index, i) in result.iter().enumerate() {
            trace!("try find type from index {} context {:?}", index, i);
            if i.type_id == TYPE_STR
                && let Some(Node::String(v)) = &i.v
                && v.as_ref() == "type"
            {
                let type_node_index = index + 2;
                if type_node_index < result.len()
                    && let Some(Node::String(type_v)) = &result[type_node_index].v
                {
                    trace!("find type {} from context {:?}", type_v, i);
                    *context.control_type.lock().await = Some(type_v.clone());
                }
            }
        }
    }

    async fn get_boundary_indices(&self, index: usize) -> Option<(usize, usize)> {
        let content = self.document.content_chars.lock().await;
        let (forward, backward) = content.split_at(index);
        if forward.is_empty() || backward.is_empty() {
            return None;
        }

        let f_len = forward.len();
        let mut stack: VecDeque<&str> = VecDeque::new();
        let mut start_index: Option<usize> = None;
        let mut forward_iter = forward.iter().rev().peekable().enumerate();
        while let Some((_, ch)) = forward_iter.next() {
            let str = ch.as_ref();
            if str == "{" {
                if stack.is_empty() {
                    let opt = forward_iter
                        .skip_while(|(_, c)| c.as_ref() != "\"")
                        .skip(1)
                        .skip_while(|(_, c)| c.as_ref() != "\"").nth(1);
                    if let Some((index, _)) = opt {
                        start_index = Some(f_len - index);
                        break;
                    } else {
                        return None;
                    }
                } else {
                    stack.pop_back();
                }
            } else if str == "}" {
                stack.push_back(str);
            }
        }
        start_index?;

        stack.clear();
        let mut end_index: Option<usize> = None;
        let backend_iter = backward.iter().peekable().enumerate();
        for (index, ch) in backend_iter {
            let str = ch.as_ref();
            if str == "}" {
                if stack.is_empty() {
                    end_index = Some(index + f_len);
                    break;
                } else {
                    stack.pop_back();
                }
            } else if str == "{" {
                stack.push_back(str);
            }
        }
        end_index?;

        Some((start_index.unwrap(), end_index.unwrap()))
    }

    pub async fn compelte(
        &self,
        bk: &Backend,
        param: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let pos = &param.text_document_position.position;

        // get current index in content
        let context: CompleteContext = CompleteContext::empty();
        let index = self.document.get_content_index(pos).await;
        let index_value;
        if let Some(index_v) = index {
            *context.index.lock().await = index_v;
            index_value = index_v;
        } else {
            return None;
        }

        //write the input char currently
        {
            let input_char_index = index_value - 1;
            let content = self.document.content_cache.lock().await;
            if input_char_index > 0
                && let Some(cr) = content.chars().nth(input_char_index)
            {
                *context.input_char.lock().await = cr;
            }
        }

        //check ast whether dirty
        let cache_miss = {
            let ast = self.ast.lock().await;
            if let Some(ast) = ast.as_ref() {
                !ast.iter().any(|f| f.l <= index_value && index_value <= f.r)
            } else {
                true
            }
        };
        if cache_miss {
            self.build_ast(index_value, &context).await;
        }

        //fill context from ast
        let ast = self.ast.lock().await;
        if let Some(input) = ast.as_ref() {
            Self::fill_context(input, &context).await;
            let mut control_type_mut = context.control_type.lock().await;
            if control_type_mut.is_none()
                && let Some(Node::String(v)) = &input[0].v
            {
                let url = &param.text_document_position.text_document.uri;
                let namespace: Option<Arc<str>> = bk.query_namespace(url).await;
                let control_t = self.fill_control_type(bk, namespace, v).await;
                if let Some(v) = control_t {
                    trace!("find type from type_cache {:?}", &input[0].v);
                    *control_type_mut = Some(Arc::from(v.as_str()));
                } else {
                    trace!("cant find type from type_cache {:?}", &input[0].v);
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

        Some(vec![CompletionItem {
            label: "test".to_string(),
            label_details: Some(CompletionItemLabelDetails {
                description: Some("test des".to_string()),
                detail: Some("test detail".to_string()),
            }),
            kind: None,
            detail: Some("test detail 2".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                value: r#"我展示的是一级标题
=================

我展示的是二级标题
-----------------"#
                    .to_string(),
            })),
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: Some("test".to_string()),
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        }])
        // None
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
            let chars = self.document.content_chars.lock().await;
            let splice_control = &chars[l..r + 1];

            let mut stack: VecDeque<Value> = VecDeque::new();
            let mut str_builder = String::new();
            let iter = splice_control.iter().enumerate();
            for (index, c) in iter {
                let i = index + l;
                let str = c.as_ref();
                match str.into() {
                    Token::Quote => Self::handle_quote(&mut stack, &mut str_builder, i),
                    Token::LeftBrace => stack.push_back(Value {
                        l: i,
                        r: 0,
                        type_id: TYPE_CR,
                        v: None,
                    }),
                    Token::LeftBracket => stack.push_back(Value {
                        l: i,
                        r: 0,
                        type_id: TYPE_ARR,
                        v: None,
                    }),
                    Token::Colon => stack.push_back(Value {
                        l: i,
                        r: i + 1,
                        type_id: TYPE_COL,
                        v: Some(Node::Colon),
                    }),
                    Token::RightBrace => Self::handle_right_brace(&mut stack, i),
                    Token::RightBracket => Self::handle_right_bracket(&mut stack, i),
                    Token::Comma => Self::handle_comma(&mut stack, &mut str_builder, i),
                    Token::OTHER if Token::is_ignore(str) => continue,
                    Token::OTHER => str_builder.push_str(str),
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

    fn handle_quote(stack: &mut VecDeque<Value>, str_builder: &mut String, index: usize) {
        if !stack.iter().any(|f| TYPE_STR == f.type_id && f.v.is_none()) {
            str_builder.clear(); //remove potential comments
            stack.push_back(Value {
                l: index,
                r: 0,
                type_id: TYPE_STR,
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
                v: Some(Node::String(Arc::from(str_builder.clone()))),
            });
            str_builder.clear();
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
    use tower_lsp::lsp_types::Position;

    use super::*;

    const EXAMPLE1: &str = include_str!("../test/achievement_screen.json");

    #[tokio::test]
    async fn test_get_content_index() {
        let completer = Completer::from(EXAMPLE1.into());
        let docu = completer.document.content_cache.lock().await;
        let index = completer
            .document
            .get_content_index(&Position {
                line: 16,
                character: 5,
            })
            .await;
        let result = docu.chars().nth(index.unwrap());
        assert_eq!(result, Some('t'));

        let result = completer
            .document
            .get_content_index(&Position {
                line: 44,
                character: 1,
            })
            .await;
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_get_boundary_indices() {
        let completer = Completer::from(EXAMPLE1.into());

        let v: usize = completer
            .document
            .get_content_index(&Position {
                line: 16,
                character: 6,
            })
            .await
            .unwrap();

        let result = completer.get_boundary_indices(v).await;
        let (l, r) = result.unwrap();
        let docu = completer.document.content_cache.lock().await;
        let lc = docu.chars().nth(l).unwrap();
        let rc = docu.chars().nth(r).unwrap();
        assert_eq!((lc, rc), ('"', '}'));

        let v = completer
            .document
            .get_content_index(&Position {
                line: 6,
                character: 3,
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
                line: 16,
                character: 5,
            })
            .await
            .unwrap();
        let context = &CompleteContext::empty();
        completer.build_ast(v, context).await;
        {
            let result = completer.ast.lock().await;
            assert!(result.is_some());
        }

        let v = completer
            .document
            .get_content_index(&Position {
                line: 0,
                character: 1,
            })
            .await
            .unwrap();

        let completer = Completer::from(EXAMPLE1.into());
        let context = &CompleteContext::empty();
        completer.build_ast(v, context).await;
        let result = completer.ast.lock().await;
        assert!(result.is_none());
    }
}
