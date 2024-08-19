use crate::Backend;
use log::{debug, error};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::{
    collections::VecDeque,
    fs::File,
    hash::Hasher,
    io::{BufRead, BufReader, Read},
    path::PathBuf,
    rc::Rc,
    sync::Arc,
};
use tower_lsp::lsp_types::{CompletionParams, Position};
use tree_ds::prelude::*;

type AutoTree<T> = Tree<u32, T>;

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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum JSONUIValue {
    KeyValuePair(Option<(Rc<str>, Box<JSONUIValue>)>),
    Controls(Option<Vec<Box<JSONUIValue>>>),
    String(Option<Rc<str>>),
    Array(Option<Vec<Box<JSONUIValue>>>),
    Number(f32),
    Bool(bool),
    Colon,
    Comma,
}
impl Eq for JSONUIValue {}

impl PartialEq for JSONUIValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JSONUIValue::Bool(a), JSONUIValue::Bool(b)) => a.eq(b),
            (JSONUIValue::Number(a), JSONUIValue::Number(b)) => (a - b).abs() < f32::EPSILON,
            (JSONUIValue::String(a), JSONUIValue::String(b)) => a.eq(b),
            (JSONUIValue::Array(a), JSONUIValue::Array(b)) => a.eq(b),
            (JSONUIValue::Controls(a), JSONUIValue::Controls(b)) => a.eq(b),
            (JSONUIValue::KeyValuePair(a), JSONUIValue::KeyValuePair(b)) => a.eq(b),
            (JSONUIValue::Colon, JSONUIValue::Colon) => true,
            (JSONUIValue::Comma, JSONUIValue::Comma) => true,
            _ => false,
        }
    }
}

impl std::hash::Hash for JSONUIValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            JSONUIValue::Array(v) => v.hash(state),
            JSONUIValue::Controls(v) => v.hash(state),
            JSONUIValue::KeyValuePair(v) => v.hash(state),
            JSONUIValue::String(v) => v.hash(state),
            JSONUIValue::Colon => 17.hash(state),
            JSONUIValue::Comma => 19.hash(state),
            JSONUIValue::Bool(b) => b.hash(state),
            JSONUIValue::Number(v) => {
                if v.is_nan() {
                    0.hash(state);
                } else {
                    v.to_bits().hash(state);
                }
            }
        }
    }
}

pub enum CompleteType {
    Property,
    PropertyValue(String), //string is property name
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
pub(crate) struct Completer {
    line_cache: Arc<Vec<Arc<str>>>,
    content_cache: Arc<str>,
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
        }
    }

    pub fn from(str: Arc<str>) -> Self {
        let lines: Vec<Arc<str>> = str.lines().map(|line| Arc::from(line)).collect();
        Completer {
            line_cache: Arc::new(lines),
            content_cache: str,
        }
    }

    pub fn get_content_index(&self, pos: &Position) -> Option<usize> {
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

    fn get_boundary_indices(&self, pos: &Position) -> Option<(usize, usize)> {
        let mut stack: VecDeque<char> = VecDeque::new();

        let index = self.get_content_index(pos);
        if let Some(index) = index {
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
        } else {
            return None;
        }
    }

    fn compelte_0(&self, pos: &Position) -> Option<Rc<str>> {
        //the goal is to get two pieces infomation
        //1. identify the type of control that current position belongs to.
        //2. retrieve the surrounding context in current position
        let indices = self.get_boundary_indices(pos);
        if let Some((l, r)) = indices {
            let splice_control = &self.content_cache[l..r];
            let mut peekable = splice_control.chars().peekable().enumerate();

            let mut stack: VecDeque<JSONUIValue> = VecDeque::new();

            let mut str_builder = String::new();

            while let Some((index, c)) = peekable.next() {
                match c.into() {
                    Token::Quote => {
                        if stack.iter().all(|f| {
                            if let JSONUIValue::String(None) = f {
                                false
                            } else {
                                true
                            }
                        }){
                            stack.push_back(JSONUIValue::String(None));
                        } else {
                            if let Some(JSONUIValue::String(None)) = stack.pop_back(){
                                stack.push_back(JSONUIValue::String(Some(Rc::from(
                                    str_builder.clone(),
                                ))));
                                str_builder.clear();
                            } else {
                                unreachable!("Error: An error occurred at index {}.", index)
                            }
                        }
                    }
                    Token::LeftBrace => stack.push_back(JSONUIValue::Controls(None)),
                    Token::RightBrace => {
                        let mut tmp_vec: Vec<Box<JSONUIValue>> = Vec::new();
                        let mut vaild = false;
                        while let Some(f) = stack.pop_back() {
                            if let JSONUIValue::Controls(None) = f {
                                stack.push_back(JSONUIValue::Controls(Some(tmp_vec.to_owned())));
                                vaild = true;
                                break;
                            } else {
                                tmp_vec.push(Box::new(f));
                                break;
                            }
                        }
                        if !vaild {
                            panic!()
                        }
                    }
                    Token::LeftBracket => stack.push_back(JSONUIValue::Array(None)),
                    Token::RightBracket => {
                        let mut tmp_vec: Vec<Box<JSONUIValue>> = Vec::new();
                        let mut vaild = false;
                        while let Some(f) = stack.pop_back() {
                            if let JSONUIValue::Array(None) = f {
                                stack.push_back(JSONUIValue::Array(Some(tmp_vec.to_owned())));
                                vaild = true;
                                break;
                            } else {
                                tmp_vec.push(Box::new(f));
                                break;
                            }
                        }
                        if !vaild {
                            panic!()
                        }
                    }
                    Token::OTHER => {
                        if Token::is_ignore(&c) {
                            continue;
                        }
                        str_builder.push(c);
                    }
                    Token::Comma => {
                        stack.push_back(JSONUIValue::Comma);
                        //extra tow pattern for number and bool
                        if !str_builder.is_empty() {
                            if let Ok(boolean) = str_builder.parse::<bool>() {
                                stack.push_back(JSONUIValue::Bool(boolean));
                                str_builder.clear();
                            } else if let Ok(float) = str_builder.parse::<f32>() {
                                stack.push_back(JSONUIValue::Number(float));
                                str_builder.clear();
                            }
                        }
                    }
                    Token::Colon => stack.push_back(JSONUIValue::Colon),
                }
            }

            println!("{}", json!(stack));

            return None;
        } else {
            return None;
        }
    }

    pub fn compelte(&self, bk: &Backend, param: &CompletionParams) {
        debug!("{}", json!(param));
        self.compelte_0(&param.text_document_position.position);
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
        let result = completer.get_boundary_indices(&Position {
            line: 2,
            character: 25,
        });
        let (l, r) = result.unwrap();
        let lc = completer.content_cache.chars().nth(l).unwrap();
        let rc = completer.content_cache.chars().nth(r).unwrap();
        assert_eq!((lc, rc), ('"', '}'));
    }

    #[test]
    fn test_get_boundary_indices_error() {
        let completer = Completer::from(EXAMPLE1.into());
        let result = completer.get_boundary_indices(&Position {
            line: 14,
            character: 20,
        });
        assert_eq!(result, None);
    }

    #[test]
    fn test_compelte_0() {
        let completer = Completer::from(EXAMPLE1.into());
        completer.compelte_0(&Position {
            line: 2,
            character: 25,
        });
    }
}
