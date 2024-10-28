use std::collections::VecDeque;
use std::fmt;
use std::sync::Arc;

use log::trace;

use crate::document::Document;
use crate::path_info::PathInfo;

/// The `TokenValue` struct represents a parsed source token
///
/// Properties:
///
/// * `l`: represents a left index in `Document`
/// * `r`: represents a right index in `Document`
/// * `type_id`: type constant
/// * `path`: shows the layer of the current `Value`
/// * `v`: parsed value
#[derive(Clone)]
pub(crate) struct TokenValue {
    pub(crate) l:       usize,
    pub(crate) r:       usize,
    pub(crate) type_id: u8,
    pub(crate) path:    Vec<usize>,
    pub(crate) v:       Option<ParsedToken>,
}

impl fmt::Debug for TokenValue {
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
pub(crate) enum ParsedToken {
    Controls(Vec<TokenValue>),
    Array(Vec<TokenValue>),
    String(Arc<str>),
    Number(f32),
    Bool(bool),
    Colon,
    Comma,
}

impl fmt::Debug for ParsedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_indented(
            f: &mut fmt::Formatter,
            tokens: &[TokenValue],
            indent_level: usize,
        ) -> fmt::Result {
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

pub(crate) const TYPE_CR: u8 = 2;
pub(crate) const TYPE_ARR: u8 = 3;
pub(crate) const TYPE_STR: u8 = 4;
pub(crate) const TYPE_NUM: u8 = 5;
pub(crate) const TYPE_BOL: u8 = 6;
pub(crate) const TYPE_COL: u8 = 7;
pub(crate) const TYPE_COM: u8 = 8;

pub(crate) mod prelude {
    pub(crate) use super::{
        ParsedToken, TokenValue, Tokenizer, TYPE_ARR, TYPE_BOL, TYPE_COL, TYPE_COM, TYPE_CR, TYPE_NUM,
        TYPE_STR,
    };
}

pub(crate) struct Tokenizer {}

impl Tokenizer {
    pub(crate) fn new() -> Self {
        Tokenizer {}
    }

    /// The function `is_ignore` returns a boolean value indicating whether the input character `c` is a
    /// whitespace character (space), carriage return ("\r\n"), or newline ("\n").
    pub(crate) fn is_ignore(c: &str) -> bool {
        matches!(c, " " | "\r\n" | "\n")
    }

    pub(crate) async fn parse(&self, pos: (usize, usize), doc: &Document) -> Option<Vec<TokenValue>> {
        let (l, r) = pos;

        let chars = doc.content_chars.lock().await;
        let splice_control = &chars[l..r + 1];
        let mut stack: VecDeque<TokenValue> = VecDeque::new();
        let mut str_builder = String::new();
        let iter = splice_control.iter().enumerate();
        let path_info: &PathInfo = &PathInfo::new();

        // trace!("{:?}", splice_control);
        for (index, c) in iter {
            let i = index + l;
            let str = c.as_ref();
            match str.into() {
                Token::Quote => Self::handle_quote(&mut stack, &mut str_builder, i, path_info),
                Token::LeftBrace if Self::not_quote_state(&stack) => stack.push_back(TokenValue {
                    l:       i,
                    r:       0,
                    type_id: TYPE_CR,
                    path:    path_info.push(),
                    v:       None,
                }),
                Token::LeftBracket if Self::not_quote_state(&stack) => stack.push_back(TokenValue {
                    l:       i,
                    r:       0,
                    type_id: TYPE_ARR,
                    path:    path_info.push(),
                    v:       None,
                }),
                Token::Colon if Self::not_quote_state(&stack) => {
                    stack.push_back(TokenValue {
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
                _ if Self::not_quote_state(&stack) && Self::is_ignore(str) => continue,
                _ => str_builder.push_str(str),
            }
        }
        let r: Vec<TokenValue> = stack.into_iter().collect();
        if r.is_empty() {
            trace!("build ast is none");
            None
        } else {
            Some(r)
        }
    }

    /// The function `collect_value` parses a string into a boolean or float value and pushes to stack
    fn collect_value(
        stack: &mut VecDeque<TokenValue>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        if !str_builder.is_empty() {
            let str_c = str_builder.len();
            if let Ok(boolean) = str_builder.parse::<bool>() {
                stack.push_back(TokenValue {
                    l:       index,
                    r:       index + str_c + 1,
                    type_id: TYPE_BOL,
                    path:    path_info.gen_path(),
                    v:       Some(ParsedToken::Bool(boolean)),
                });
            } else if let Ok(float) = str_builder.parse::<f32>() {
                stack.push_back(TokenValue {
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

    fn not_quote_state(stack: &VecDeque<TokenValue>) -> bool {
        stack
            .back()
            .map_or(true, |node| node.type_id != TYPE_STR || node.v.is_some())
    }

    fn handle_quote(
        stack: &mut VecDeque<TokenValue>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        if !stack.iter().any(|f| TYPE_STR == f.type_id && f.v.is_none()) {
            str_builder.clear(); // remove potential comments
            stack.push_back(TokenValue {
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
            stack.push_back(TokenValue {
                l:       value.l,
                r:       index + 1,
                type_id: TYPE_STR,
                path:    path_info.gen_path(),
                v:       Some(ParsedToken::String(Arc::from(str_builder.clone()))),
            });
            str_builder.clear();
        }
    }

    fn handle_right_brace(stack: &mut VecDeque<TokenValue>, index: usize, path_info: &PathInfo) {
        let mut tmp_vec: Vec<TokenValue> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_CR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                path_info.pop(f.path.last().unwrap() + 1);
                stack.push_back(TokenValue {
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
        stack: &mut VecDeque<TokenValue>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        Self::collect_value(stack, str_builder, index, path_info);
        let mut tmp_vec: Vec<TokenValue> = Vec::new();
        while let Some(f) = stack.pop_back() {
            if TYPE_ARR == f.type_id && f.v.is_none() {
                tmp_vec.reverse();
                path_info.pop(f.path.last().unwrap() + 1);
                stack.push_back(TokenValue {
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
        stack: &mut VecDeque<TokenValue>,
        str_builder: &mut String,
        index: usize,
        path_info: &PathInfo,
    ) {
        Self::collect_value(stack, str_builder, index, path_info);
        stack.push_back(TokenValue {
            l:       index,
            r:       index + 1,
            type_id: TYPE_COM,
            path:    path_info.gen_path(),
            v:       Some(ParsedToken::Comma),
        });
    }
}