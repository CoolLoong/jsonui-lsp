use crate::Backend;
use log::{debug, error};
use std::{
    collections::VecDeque,
    fmt,
    fs::File,
    io::{BufReader, Read},
    path::PathBuf,
    sync::Arc,
};
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{CompletionItem, CompletionParams, Position};

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
    nodes: Mutex<(Option<&'a Value>, Option<&'a Value>, Option<&'a Value>)>,
}

impl<'a> CompleteContext<'a> {
    pub fn empty() -> Self {
        CompleteContext {
            control_type: Mutex::new(None),
            r: Mutex::new(0),
            l: Mutex::new(0),
            index: Mutex::new(0),
            nodes: Mutex::new((None, None, None)),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Completer {
    line_cache: Arc<Vec<Arc<str>>>,
    content_cache: Arc<str>,
    ast: Mutex<Option<Vec<Value>>>,
}

impl Drop for Completer {
    fn drop(&mut self) {
        error!(
            "Completer is being dropped with {} lines cached.",
            self.line_cache.len()
        );
    }
}

impl Completer {
    pub fn new(path: &PathBuf) -> Self {
        let file = File::open(path).unwrap();
        let mut reader = BufReader::new(file);
        let mut content_cache = String::new();
        reader.read_to_string(&mut content_cache).unwrap();

        let lines: Vec<Arc<str>> = content_cache
            .as_str()
            .lines()
            .map(|line| Arc::from(line))
            .collect();
        Completer {
            line_cache: lines.into(),
            content_cache: content_cache.into(),
            ast: Mutex::new(None),
        }
    }

    pub fn from(str: Arc<str>) -> Self {
        let lines: Vec<Arc<str>> = str.lines().map(|line| Arc::from(line)).collect();
        Completer {
            line_cache: Arc::new(lines),
            content_cache: str,
            ast: Mutex::new(None),
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

    async fn find_closest_node<'a>(input: &'a [Value], context: &'a CompleteContext<'a>) {
        let mut result = Vec::new();
        Self::flatten_ast(input, &mut result);
        let v_index = *context.index.lock().await;
        let mut containing_node: Option<&'a Value> = None;
        let mut contain_index: isize = -1;
        for (index, value) in result.iter().enumerate() {
            if value.l <= v_index && v_index <= value.r {
                containing_node = Some(value);
                contain_index = index as isize;
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
        let before = contain_index - 1;
        let closest_before = if before >= 0 {
            Some(result[before as usize])
        } else {
            None
        };
        let after = (contain_index + 1) as usize;
        let closest_after = if after < result.len() {
            Some(result[after])
        } else {
            None
        };
        *context.nodes.lock().await = (closest_before, containing_node, closest_after);
    }

    fn get_content_index(&self, pos: &Position) -> Option<usize> {
        let mut result = 0;
        let line = pos.line as usize; //index from 0
        if line >= self.line_cache.len() {
            return None;
        }
        let index = pos.character as usize;
        let forward = &self.line_cache[0..line];

        for i in forward {
            result += i.chars().count();
        }
        result = result + index + (line - 1) - 1;
        Some(result)
    }

    fn get_boundary_indices(&self, index: usize) -> Option<(usize, usize)> {
        let mut stack: VecDeque<char> = VecDeque::new();
        let (forward, backward) = self.content_cache.split_at(index);
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
        let index = self.get_content_index(pos);
        let mut index_value = 0;
        if let Some(index_v) = index {
            *context.index.lock().await = index_v;
            index_value = index_v;
        } else {
            return None;
        }

        let cache_miss = {
            let ast = self.ast.lock().await;
            if let Some(ast) = ast.as_ref() {
                ast.iter().all(|f| f.l > index_value && f.r < index_value)
            } else {
                true
            }
        };

        if cache_miss {
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

        self.create_completion_information(&context)
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
        context: &CompleteContext,
    ) -> Option<Vec<CompletionItem>> {
        debug!("{:?}", context);
        None
    }

    async fn build_ast<'a>(&self, index: usize, context: &'a CompleteContext<'a>) {
        let indices = self.get_boundary_indices(index);
        if let Some((l, r)) = indices {
            let splice_control = &self.content_cache[l..r + 1];
            let mut peekable = splice_control.chars().peekable().enumerate();
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
        stack.push_back(Value {
            l: index,
            r: index + 1,
            type_id: TYPE_COM,
            v: Some(Node::Comma),
        });

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
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &'static str = r#"{
                    "text@cc.l": {
                      "$button_text_font_scale_factor|default": 1,
                      "$button_text_bindings|default": [],
                      "font_scale_factor": "$button_text_font_scale_factor",
                      "anchor_from": "$button_text_anchor",
                      "anchor_to": "$button_text_anchor",
                      "bindings": "$button_text_bindings",
                      "shadow": "$button_text_shadow",
                      "offset": "$button_text_offset_holder",
                      "layer": 3,
                      "color": "$button_text_color",
                      "text": "$button_text"
                    }
                  }"#;

    #[test]
    fn test_get_content_index() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer.get_content_index(&Position {
            line: 2,
            character: 25,
        });
        let result = completer.content_cache.chars().nth(index.unwrap());
        assert_eq!(result, Some('$'));
    }

    #[test]
    fn test_get_content_index_2() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer.get_content_index(&Position {
            line: 13,
            character: 22,
        });
        let result = completer.content_cache.chars().nth(index.unwrap());
        assert_eq!(result, Some('}'));
    }

    #[test]
    fn test_get_content_index_3() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer.get_content_index(&Position {
            line: 12,
            character: 45,
        });
        let result = completer.content_cache.chars().nth(index.unwrap());
        assert_eq!(result, Some('"'));
    }

    #[test]
    fn test_get_content_index_4() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer.get_content_index(&Position {
            line: 14,
            character: 20,
        });
        let result = completer.content_cache.chars().nth(index.unwrap());
        assert_eq!(result, Some('}'));
    }

    #[test]
    fn test_get_content_index_error() {
        let completer = Completer::from(EXAMPLE1.into());
        let result = completer.get_content_index(&Position {
            line: 15,
            character: 1,
        });
        assert_eq!(result, None);
    }

    #[test]
    fn test_get_content_index_error_2() {
        let completer = Completer::from(EXAMPLE1.into());
        let index = completer.get_content_index(&Position {
            line: 14,
            character: 21,
        });
        let result = completer.content_cache.chars().nth(index.unwrap());
        assert_eq!(result, None);
    }

    #[test]
    fn test_get_boundary_indices() {
        let completer = Completer::from(EXAMPLE1.into());

        let v: usize = completer
            .get_content_index(&Position {
                line: 2,
                character: 25,
            })
            .unwrap();

        let result = completer.get_boundary_indices(v);
        let (l, r) = result.unwrap();
        let lc = completer.content_cache.chars().nth(l).unwrap();
        let rc = completer.content_cache.chars().nth(r).unwrap();
        assert_eq!((lc, rc), ('"', '}'));
    }

    #[test]
    fn test_get_boundary_indices_error() {
        let completer = Completer::from(EXAMPLE1.into());

        let v = completer
            .get_content_index(&Position {
                line: 14,
                character: 20,
            })
            .unwrap();

        let result = completer.get_boundary_indices(v);
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_build_ast() {
        let completer = Completer::from(EXAMPLE1.into());

        let v = completer
            .get_content_index(&Position {
                line: 2,
                character: 25,
            })
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
            .get_content_index(&Position {
                line: 14,
                character: 21,
            })
            .unwrap();

        let mut context = &CompleteContext::empty();
        completer.build_ast(v, &mut context).await;
        let result = completer.ast.lock().await;
        assert!(result.is_none());
    }
}
