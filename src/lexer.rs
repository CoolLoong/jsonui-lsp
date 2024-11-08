use std::collections::VecDeque;
use std::sync::Arc;

use log::trace;

use crate::document::Document;

#[derive(Debug, Clone)]

pub enum TokenChar {
    Slash = '/' as isize,
    Escape = '\\' as isize,
    LeftBrace = '{' as isize,
    RightBrace = '}' as isize,
    LeftBracket = '[' as isize,
    RightBracket = ']' as isize,
    Comma = ',' as isize,
    Colon = ':' as isize,
    Quote = '"' as isize,
    OTHER = '🤪' as isize,
}

impl From<&str> for TokenChar {
    fn from(value: &str) -> Self {
        match value {
            "/" => TokenChar::Slash,
            "\\" => TokenChar::Escape,
            "{" => TokenChar::LeftBrace,
            "}" => TokenChar::RightBrace,
            "[" => TokenChar::LeftBracket,
            "]" => TokenChar::RightBracket,
            "," => TokenChar::Comma,
            ":" => TokenChar::Colon,
            "\"" => TokenChar::Quote,
            _ => TokenChar::OTHER,
        }
    }
}

impl From<TokenChar> for char {
    fn from(val: TokenChar) -> Self {
        val as u8 as char
    }
}

#[derive(PartialEq, Clone)]
pub(crate) enum Token {
    Null(),
    Bool((usize, usize), bool),
    Str((usize, usize), String),
    Num((usize, usize), f32),
    Colon((usize, usize)),
    Comma((usize, usize)),
    Array((usize, usize), Option<Vec<Token>>),
    Object((usize, usize), Option<Vec<Token>>),
}

impl Token {
    pub(crate) fn pos(&self) -> (usize, usize) {
        match self {
            Token::Null() => (0, 0),
            Token::Bool(pos, _) => *pos,
            Token::Str(pos, _) => *pos,
            Token::Num(pos, _) => *pos,
            Token::Colon(pos) => *pos,
            Token::Comma(pos) => *pos,
            Token::Array(pos, _) => *pos,
            Token::Object(pos, _) => *pos,
        }
    }
}

impl Token {
    fn format_tree(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent); // Use two spaces for indentation
        match self {
            Token::Null() => "".to_string(),
            Token::Bool(extra_info, value) => {
                format!("{}Bool({:?}, {})", indent_str, extra_info, value)
            }
            Token::Str(extra_info, string) => {
                format!("{}Str({:?}, v'{}')", indent_str, extra_info, string)
            }
            Token::Num(extra_info, num) => format!("{}Num({:?}, {})", indent_str, extra_info, num),
            Token::Colon(extra_info) => format!("{}Colon({:?})", indent_str, extra_info),
            Token::Comma(extra_info) => format!("{}Comma({:?})", indent_str, extra_info),
            Token::Array(extra_info, Some(tokens)) => {
                let array_items: Vec<String> =
                    tokens.iter().map(|t| t.format_tree(indent + 1)).collect();

                format!(
                    "{}Arr({:?}, [\n{}]\n{})",
                    indent_str,
                    extra_info,
                    array_items.join("\n"),
                    indent_str
                )
            }
            Token::Object(extra_info, Some(tokens)) => {
                let object_items: Vec<String> =
                    tokens.iter().map(|t| t.format_tree(indent + 1)).collect();

                format!(
                    "{}Obj({:?}, [\n{}]\n{})",
                    indent_str,
                    extra_info,
                    object_items.join("\n"),
                    indent_str
                )
            }
            Token::Array(extra_info, None) => format!("{}Arr({:?}, None)", indent_str, extra_info),
            Token::Object(extra_info, None) => format!("{}Obj({:?}, None)", indent_str, extra_info),
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_tree(0))
    }
}

struct ParseContext<'a> {
    stack: &'a mut VecDeque<Token>,
    value: String,
    l: &'a mut usize,
    offset: usize,
    index: &'a mut usize,
    chars: &'a [Arc<str>],
    current: &'a mut ParseState,
    last: &'a mut ParseState,
}
impl<'a> ParseContext<'a> {
    pub(crate) fn current(&mut self) -> Option<Arc<str>> {
        let r = self.chars.get(*self.index);
        r.cloned()
    }

    pub(crate) fn peek(&mut self) -> Option<Arc<str>> {
        let r = self.chars.get(*self.index + 1);
        r.cloned()
    }

    pub(crate) fn peek_n(&mut self, index: usize) -> Option<Arc<str>> {
        let r = self.chars.get(*self.index + index);
        r.cloned()
    }

    pub(crate) fn skip(&mut self, n: usize) {
        *self.index += n;
    }

    pub(crate) fn index(&self) -> usize {
        *self.index + self.offset
    }
}

#[derive(Debug, Clone)]
enum ParseState {
    None,
    Quote,
    Escape,
    LeftBrace,
    LeftBracket,
    Slash,
    SlashStar,
    Other,
}

pub(crate) struct Lexer {}

impl Lexer {
    pub(crate) fn new() -> Self {
        Lexer {}
    }

    fn is_ignore(c: &str) -> bool {
        matches!(c, " " | "\r\n" | "\n")
    }

    fn is_comment(ctx: &mut ParseContext) -> bool {
        matches!(ctx.current, ParseState::Slash | ParseState::SlashStar)
    }

    fn is_escape(ctx: &mut ParseContext) -> bool {
        matches!(ctx.current, ParseState::Escape)
    }

    pub(crate) async fn parse(&self, pos: Option<(usize, usize)>, doc: &Document) -> Option<Vec<Token>> {
        let chars = doc.chars.lock().await;

        let (offset, input) = {
            if let Some((l, r)) = pos {
                (l, &chars[l..r + 1])
            } else {
                (0, &chars[..])
            }
        };
        let ctx: &mut ParseContext<'_> = &mut ParseContext {
            stack: &mut VecDeque::new(),
            value: String::new(),
            l: &mut 0,
            offset,
            index: &mut 0,
            chars: input,
            current: &mut ParseState::None,
            last: &mut ParseState::None,
        };

        while let Some(s) = ctx.current() {
            let cr = s.as_ref();
            #[cfg(feature = "debug-parse")]
            trace!("['{}', state {:?}]", cr, ctx.current);
            match cr.into() {
                _ if Self::is_escape(ctx) => Self::handle_escape_char(ctx, cr),
                TokenChar::Slash => Self::handle_slash(ctx, cr),
                TokenChar::Escape => Self::handle_escape(ctx),
                TokenChar::Quote => Self::handle_quote(ctx),
                TokenChar::Colon => Self::handle_colon(ctx),
                TokenChar::Comma => Self::handle_comma(ctx),
                TokenChar::LeftBrace => Self::handle_left_brace(ctx),
                TokenChar::RightBrace => Self::handle_right_brace(ctx),
                TokenChar::LeftBracket => Self::handle_left_bracket(ctx),
                TokenChar::RightBracket => Self::handle_right_bracket(ctx),
                _ => Self::handle_other(ctx, cr),
            }
            ctx.skip(1);
        }

        let r: Vec<Token> = std::mem::take(ctx.stack).into();

        if r.is_empty() {
            trace!("build ast is none");
            None
        } else {
            Some(r)
        }
    }

    fn handle_escape_char(ctx: &mut ParseContext, c: &str) {
        let r = match c {
            "n" => Arc::from("\n"),
            "t" => Arc::from("\t"),
            "r" => Arc::from("\r"),
            "\\" => Arc::from("\\"),
            "\"" => Arc::from("\""),
            "b" => Arc::from("\x08"),
            "f" => Arc::from("\x0C"),
            "0" => Arc::from("\0"),
            "u" => Self::handle_unicode_escape(ctx),
            _ => {
                let r = format!("\\{}", c);
                Arc::from(r.as_str())
            }
        };
        ctx.value.push_str(r.as_ref());
        *ctx.current = ParseState::Quote;
    }

    fn handle_unicode_escape(ctx: &mut ParseContext) -> Arc<str> {
        let mut hex_digits = String::new();
        for i in 1..5 {
            if let Some(next_char) = ctx.peek_n(i) {
                if next_char.as_ref().chars().all(|c| c.is_ascii_hexdigit()) {
                    hex_digits.push_str(&next_char);
                    ctx.skip(1);
                } else {
                    return Arc::from("\u{FFFD}");
                }
            } else {
                return Arc::from("\u{FFFD}");
            }
        }
        match u32::from_str_radix(&hex_digits, 16).ok().and_then(char::from_u32) {
            Some(unicode_char) => Arc::from(unicode_char.to_string().as_str()),
            None => Arc::from("\u{FFFD}"),
        }
    }

    fn collect_other_value(ctx: &mut ParseContext) {
        if matches!(ctx.current, ParseState::Other) {
            *ctx.current = ctx.last.clone();
            if !ctx.value.is_empty() {
                if let Ok(boolean) = ctx.value.parse::<bool>() {
                    ctx.stack
                        .push_back(Token::Bool((*ctx.l, *ctx.l + ctx.value.len()), boolean));
                } else if let Ok(float) = ctx.value.parse::<f32>() {
                    ctx.stack
                        .push_back(Token::Num((*ctx.l, *ctx.l + ctx.value.len()), float));
                }
                ctx.value.clear();
            }
        }
    }

    fn handle_slash(ctx: &mut ParseContext, char: &str) {
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push_str(char);
        } else {
            let r = ctx.peek();
            if let Some(r) = r {
                if r.as_ref() == "/" {
                    *ctx.current = ParseState::Slash;
                } else if r.as_ref() == "*" {
                    *ctx.current = ParseState::SlashStar;
                }
            }
        }
    }

    fn handle_escape(ctx: &mut ParseContext) {
        if let ParseState::Quote = ctx.current {
            *ctx.current = ParseState::Escape;
        }
    }

    fn handle_quote(ctx: &mut ParseContext) {
        if let ParseState::Quote = ctx.current {
            let v = std::mem::take(&mut ctx.value);
            ctx.stack.push_back(Token::Str((*ctx.l, ctx.index() + 1), v));
            *ctx.current = ctx.last.clone();
        } else if !Self::is_comment(ctx) {
            let current = std::mem::replace(ctx.current, ParseState::Quote);
            *ctx.last = current;
            *ctx.l = ctx.index();
        }
    }

    fn handle_colon(ctx: &mut ParseContext) {
        if Self::is_comment(ctx) {
            return;
        }
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push(TokenChar::Colon.into());
            return;
        }

        ctx.stack.push_back(Token::Colon((ctx.index(), ctx.index() + 1)));
    }

    fn handle_comma(ctx: &mut ParseContext) {
        if Self::is_comment(ctx) {
            return;
        }
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push(TokenChar::Comma.into());
            return;
        }

        Self::collect_other_value(ctx);

        ctx.stack.push_back(Token::Comma((ctx.index(), ctx.index() + 1)));
    }

    fn handle_left_brace(ctx: &mut ParseContext) {
        if Self::is_comment(ctx) {
            return;
        }
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push(TokenChar::LeftBrace.into());
            return;
        }

        let current = std::mem::replace(ctx.current, ParseState::LeftBrace);
        *ctx.last = current;
        *ctx.l = ctx.index();

        ctx.stack.push_back(Token::Object((ctx.index(), 0), None));
    }

    fn handle_right_brace(ctx: &mut ParseContext) {
        if Self::is_comment(ctx) {
            return;
        }
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push(TokenChar::RightBrace.into());
            return;
        }

        Self::collect_other_value(ctx);

        let tmp_vec: &mut Vec<Token> = &mut Vec::new();
        while let Some(f) = ctx.stack.pop_back() {
            if let Token::Object(ref v, ref value) = f
                && value.is_none()
            {
                tmp_vec.reverse();

                ctx.stack
                    .push_back(Token::Object((v.0, ctx.index() + 1), Some(std::mem::take(tmp_vec))));

                *ctx.current = ctx.last.clone();

                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_left_bracket(ctx: &mut ParseContext) {
        if Self::is_comment(ctx) {
            return;
        }
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push(TokenChar::LeftBracket.into());
            return;
        }

        let current = std::mem::replace(ctx.current, ParseState::LeftBracket);

        *ctx.last = current;
        *ctx.l = ctx.index();

        ctx.stack.push_back(Token::Array((ctx.index(), 0), None));
    }

    fn handle_right_bracket(ctx: &mut ParseContext) {
        if Self::is_comment(ctx) {
            return;
        }
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push(TokenChar::RightBracket.into());
            return;
        }
        Self::collect_other_value(ctx);

        let tmp_vec: &mut Vec<Token> = &mut Vec::new();
        while let Some(f) = ctx.stack.pop_back() {
            if let Token::Array(ref v, ref value) = f
                && value.is_none()
            {
                tmp_vec.reverse();

                ctx.stack
                    .push_back(Token::Array((v.0, ctx.index() + 1), Some(std::mem::take(tmp_vec))));

                *ctx.current = ctx.last.clone();

                return;
            } else {
                tmp_vec.push(f);
            }
        }
    }

    fn handle_other(ctx: &mut ParseContext, char: &str) {
        if matches!(ctx.current, ParseState::Quote) {
            ctx.value.push_str(char);
        } else if matches!(ctx.current, ParseState::Slash) {
            if matches!(char, "\r\n" | "\n" | "\r") {
                *ctx.current = ctx.last.clone();
            }
        } else if matches!(ctx.current, ParseState::SlashStar) {
            if let Some(v) = ctx.peek() {
                if char == "*" && v.as_ref() == "/" {
                    ctx.skip(1);
                    *ctx.current = ctx.last.clone();
                }
            }
        } else if Self::is_ignore(char) {
            return;
        } else {
            if !matches!(ctx.current, ParseState::Other) {
                let current = std::mem::replace(ctx.current, ParseState::Other);
                *ctx.last = current;
                *ctx.l = ctx.index();
            }
            ctx.value.push_str(char);
        }
    }
}

pub(crate) async fn parse_full(input: &str) -> Option<((usize, usize), Vec<Token>)> {
    let doc = Document::from(Arc::from(input));
    let r = Lexer::new().parse(None, &doc).await;
    if let Some(mut r) = r
        && let Token::Object(info, v) = std::mem::replace(&mut r[0], Token::Null())
        && let Some(v) = v
    {
        Some((info, v))
    } else {
        None
    }
}

pub(crate) async fn parse(range: Option<(usize, usize)>, input: &str) -> Option<Vec<Token>> {
    let doc = Document::from(Arc::from(input));

    Lexer::new().parse(range, &doc).await
}

pub(crate) fn to_map(tokens: Vec<Token>) -> std::collections::HashMap<String, Token> {
    let mut key = String::new();

    let mut collect = false;

    let mut r = std::collections::HashMap::new();

    for i in tokens {
        match i {
            Token::Str(_, ref v) => {
                if collect {
                    r.insert(std::mem::take(&mut key), i);

                    collect = false;
                } else {
                    key.push_str(v.as_str());
                }
            }
            Token::Colon(_) => collect = true,
            _ if collect => {
                r.insert(std::mem::take(&mut key), i);

                collect = false;
            }
            _ => {}
        }
    }

    r
}

pub(crate) fn to_map_ref(tokens: &Vec<Token>) -> std::collections::HashMap<String, &Token> {
    let mut key = String::new();

    let mut collect = false;

    let mut r = std::collections::HashMap::new();

    for i in tokens {
        match i {
            Token::Str(_, v) => {
                if collect {
                    r.insert(std::mem::take(&mut key), i);

                    collect = false;
                } else {
                    key.push_str(v);
                }
            }
            Token::Colon(_) => collect = true,
            _ if collect => {
                r.insert(std::mem::take(&mut key), i);

                collect = false;
            }
            _ => {}
        }
    }

    r
}

pub(crate) fn to_string(token: Token) -> String {
    if let Token::Str(_, v) = token {
        v
    } else {
        String::new()
    }
}

pub(crate) fn to_string_ref(token: &Token) -> String {
    if let Token::Str(_, v) = token {
        v.clone()
    } else {
        String::new()
    }
}

pub(crate) fn to_array(token: Token) -> Vec<Token> {
    if let Token::Array(_, v) = token {
        v.unwrap()
    } else {
        vec![]
    }
}

pub(crate) fn to_num_array(token: Token) -> Vec<f32> {
    if let Token::Array(_, v) = token {
        v.unwrap()
            .into_iter()
            .filter_map(|f| if let Token::Num(_, t) = f { Some(t) } else { None })
            .collect()
    } else {
        vec![]
    }
}

pub(crate) fn to_array_ref(token: &Token) -> Option<&Vec<Token>> {
    if let Token::Array(_, v) = token {
        Some(v.as_ref().unwrap())
    } else {
        None
    }
}

pub(crate) fn to_bool(token: Token) -> bool {
    if let Token::Bool(_, v) = token {
        v
    } else {
        false
    }
}

pub(crate) fn to_bool_ref(token: &Token) -> bool {
    if let Token::Bool(_, v) = token {
        *v
    } else {
        false
    }
}

pub(crate) fn to_number(token: Token) -> f32 {
    if let Token::Num(_, v) = token {
        v
    } else {
        0 as f32
    }
}

pub(crate) fn to_number_ref(token: &Token) -> f32 {
    if let Token::Num(_, v) = token {
        *v
    } else {
        0 as f32
    }
}

#[allow(unused_imports)]

pub(crate) mod prelude {
    pub(crate) use super::{
        parse, parse_full, to_array, to_array_ref, to_bool, to_bool_ref, to_map, to_map_ref, to_number,
        to_number_ref, to_string, to_string_ref, Token,
    };
}

#[cfg(test)]

mod tests {
    use std::sync::Arc;

    use flexi_logger::{FileSpec, LogSpecification, Logger, LoggerHandle, WriteMode};


    use crate::document::Document;
    use crate::lexer::{to_num_array, Lexer, Token};
    use crate::{JSONUI_DEFINE, VANILLAPACK_DEFINE};

    #[tokio::test]
    async fn test_parse_full_1() -> Result<(), Box<dyn std::error::Error>> {
        let r = Lexer::new();
        let input = include_str!("../test/achievement.json");
        let doc = Document::from(Arc::from(input));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.first().unwrap()
        {
            let r = v.as_ref().unwrap().first().unwrap();
            if let Token::Str(_, r) = r {
                assert_eq!(r, "namespace");
            } else {
                panic!()
            }
        } else {
            panic!()
        }
        Ok(())
    }

    #[tokio::test]
    async fn test_parse_full_2() -> Result<(), Box<dyn std::error::Error>> {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(VANILLAPACK_DEFINE));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.first().unwrap()
        {
            let r = v.as_ref().unwrap().first().unwrap();
            if let Token::Str(_, r) = r {
                assert_eq!(r, "update_dimensions");
            } else {
                panic!()
            }
        } else {
            panic!()
        }
        Ok(())
    }
    #[tokio::test]
    async fn test_parse_full_3() -> Result<(), Box<dyn std::error::Error>> {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(JSONUI_DEFINE));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.first().unwrap()
        {
            let r = v.as_ref().unwrap().first().unwrap();
            if let Token::Str(_, r) = r {
                assert_eq!(r, "common");
            } else {
                panic!()
            }
        } else {
            panic!()
        }
        Ok(())
    }

    #[tokio::test]
    async fn test_parse_1() {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(
            r#"
                {
  "radio_toggle_group": {
    "description": { "zh-cn": "是否启用开关组", "en-us":
"radio_toggle_group" },     "values": [
      { "label": "true", "kind": 12, "insert_text_format": 1 },
      { "label": "false", "kind": 12, "insert_text_format": 1 }
    ]
  }
}"#,
        ));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.first().unwrap()
        {
            let r = v.as_ref().unwrap().first().unwrap();
            if let Token::Str(_, r) = r {
                assert_eq!("radio_toggle_group", r);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }

    #[tokio::test]
    async fn test_parse_2() {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(
            r#"{
            "insert_text": "[\n\t{\n\t\t\"$1\": \"$0\"\n\t}\n]",
            "test" : "exxx"
            }"#,
        ));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.first().unwrap()
        {
            let r = v.as_ref().unwrap().first().unwrap();
            if let Token::Str(_, r) = r {
                assert_eq!("insert_text", r.as_str());
            } else {
                panic!()
            }
            let r = v.as_ref().unwrap().get(2).unwrap();
            if let Token::Str(_, r) = r {
                assert_eq!("[\n\t{\n\t\t\"$1\": \"$0\"\n\t}\n]", r.as_str());
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }

    #[tokio::test]
    async fn test_parse_3() {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(
            r#"{
                /******/
                "namespace": "test"
            }"#,
        ));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.first().unwrap()
        {
            let r = v.as_ref().unwrap().first().unwrap();
            assert!(matches!(r, Token::Str(_, _)));
            if let Token::Str(_, r) = r {
                assert_eq!("namespace", r.as_str());
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }

    #[tokio::test]
    async fn test_parse_4() {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(
            r#""xxx":{
    "type": "label",
    "color": [0.941, 0.941, 0.035, 0.623]
  },"#,
        ));
        #[cfg(feature = "debug-parse")]
        setup_logger();
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.get(2).unwrap()
        {
            let r = v.as_ref().unwrap().get(6).unwrap();
            assert_eq!(vec![0.941, 0.941, 0.035, 0.623], to_num_array(r.to_owned()));
        } else {
            panic!()
        }
    }

    fn setup_logger() -> LoggerHandle {
        Logger::with(LogSpecification::trace())
            .log_to_file(FileSpec::default().directory("logs").basename("debug"))
            .write_mode(WriteMode::Direct)
            .start()
            .unwrap()
    }
}
