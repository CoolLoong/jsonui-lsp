use std::collections::VecDeque;
use std::sync::Arc;

use log::trace;

use crate::document::Document;
use crate::path_info::PathInfo;

#[derive(Debug, Clone)]
pub enum TokenChar {
    Slash        = '/' as isize,
    Escape       = '\\' as isize,
    LeftBrace    = '{' as isize,
    RightBrace   = '}' as isize,
    LeftBracket  = '[' as isize,
    RightBracket = ']' as isize,
    Comma        = ',' as isize,
    Colon        = ':' as isize,
    Quote        = '"' as isize,
    OTHER        = '🤪' as isize,
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

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct ExtraInfo {
    pub(crate) range: (usize, usize),
    pub(crate) path:  Vec<usize>,
}

#[derive(PartialEq, Clone)]
pub(crate) enum Token {
    Bool(ExtraInfo, bool),
    Str(ExtraInfo, String),
    Num(ExtraInfo, f64),
    Colon(ExtraInfo),
    Comma(ExtraInfo),
    Array(ExtraInfo, Option<Vec<Token>>),
    Object(ExtraInfo, Option<Vec<Token>>),
}
impl Token {
    fn format_tree(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent); // Use two spaces for indentation
        match self {
            Token::Bool(extra_info, value) => format!("{}Bool({:?}, {})", indent_str, extra_info, value),
            Token::Str(extra_info, string) => format!("{}Str({:?}, {})", indent_str, extra_info, string),
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
    stack:     &'a mut VecDeque<Token>,
    value:     String,
    l:         &'a mut usize,
    chars:     &'a [Arc<str>],
    path_info: PathInfo,
    current:   &'a mut ParseState,
    last:      &'a mut ParseState,
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
            stack:     &mut VecDeque::new(),
            value:     String::new(),
            l:         &mut 0,
            chars:     input,
            path_info: PathInfo::new(),
            current:   &mut ParseState::None,
            last:      &mut ParseState::None,
        };
        for (index, c) in input.iter().enumerate() {
            let i = index + offset;
            let str = c.as_ref();
            trace!("['{}', state {:?}]", str, ctx.current);
            match str.into() {
                TokenChar::Slash => Self::handle_slash(ctx, str, i),
                TokenChar::Escape => Self::handle_escape(ctx),
                TokenChar::Quote => Self::handle_quote(ctx, i),
                TokenChar::Colon => Self::handle_colon(ctx, i),
                TokenChar::Comma => Self::handle_comma(ctx, i),
                TokenChar::LeftBrace => Self::handle_left_brace(ctx, i),
                TokenChar::RightBrace => Self::handle_right_brace(ctx, i),
                TokenChar::LeftBracket => Self::handle_left_bracket(ctx, i),
                TokenChar::RightBracket => Self::handle_right_bracket(ctx, i),
                _ => Self::handle_other(ctx, str, i),
            }
        }
        let r: Vec<Token> = ctx.stack.to_owned().into_iter().collect();
        if r.is_empty() {
            trace!("build ast is none");
            None
        } else {
            Some(r)
        }
    }

    fn handle_slash(ctx: &mut ParseContext, char: &str, index: usize) {
        if !matches!(ctx.current, ParseState::Quote | ParseState::Escape) {
            let r = ctx.chars.get(index + 1);
            if let Some(r) = r {
                if r.as_ref() == "/" {
                    *ctx.current = ParseState::Slash;
                } else if r.as_ref() == "*" {
                    *ctx.current = ParseState::SlashStar;
                }
            }
        } else {
            ctx.value.push_str(char)
        }
    }

    fn handle_escape(ctx: &mut ParseContext) {
        if let ParseState::Quote = ctx.current {
            *ctx.current = ParseState::Escape;
            ctx.value.push(TokenChar::Escape.into());
        }
    }

    fn handle_quote(ctx: &mut ParseContext, index: usize) {
        if let ParseState::Escape = ctx.current {
            *ctx.current = ParseState::Quote;
            ctx.value.push(TokenChar::Quote.into());
            return;
        } else if let ParseState::Quote = ctx.current {
            let v = std::mem::take(&mut ctx.value);
            ctx.stack.push_back(Token::Str(
                ExtraInfo {
                    range: (*ctx.l, index + 1),
                    path:  ctx.path_info.gen_path(),
                },
                v,
            ));
            *ctx.current = ctx.last.clone();
        } else if !Self::is_comment(ctx) {
            let current = std::mem::replace(ctx.current, ParseState::Quote);
            *ctx.last = current;
            *ctx.l = index;
        }
    }

    fn handle_colon(ctx: &mut ParseContext, index: usize) {
        if matches!(ctx.current, ParseState::Slash) {
            return;
        }

        let str_builder = &mut ctx.value;
        if !str_builder.is_empty() {
            let str_c = str_builder.len();
            if let Ok(boolean) = str_builder.parse::<bool>() {
                ctx.stack.push_back(Token::Bool(
                    ExtraInfo {
                        range: (*ctx.l, index + str_c + 1),
                        path:  ctx.path_info.gen_path(),
                    },
                    boolean,
                ));
            } else if let Ok(float) = str_builder.parse::<f64>() {
                ctx.stack.push_back(Token::Num(
                    ExtraInfo {
                        range: (*ctx.l, index + str_c + 1),
                        path:  ctx.path_info.gen_path(),
                    },
                    float,
                ));
            }
            str_builder.clear();
        }

        if !matches!(ctx.current, ParseState::Quote | ParseState::Escape) {
            ctx.stack.push_back(Token::Colon(ExtraInfo {
                range: (index, index + 1),
                path:  ctx.path_info.gen_path(),
            }));
        }
    }

    fn handle_comma(ctx: &mut ParseContext, index: usize) {
        if !matches!(
            ctx.current,
            ParseState::Quote | ParseState::Escape | ParseState::Slash | ParseState::SlashStar
        ) {
            ctx.stack.push_back(Token::Comma(ExtraInfo {
                range: (index, index + 1),
                path:  ctx.path_info.gen_path(),
            }));
        }
    }

    fn handle_left_brace(ctx: &mut ParseContext, index: usize) {
        if !matches!(
            ctx.current,
            ParseState::Quote | ParseState::Escape | ParseState::Slash | ParseState::SlashStar
        ) {
            let current = std::mem::replace(ctx.current, ParseState::LeftBrace);
            std::mem::replace(ctx.last, current);
            *ctx.l = index;
            ctx.stack.push_back(Token::Object(
                ExtraInfo {
                    range: (index, 0),
                    path:  ctx.path_info.push(),
                },
                None,
            ));
        }
    }

    fn handle_right_brace(ctx: &mut ParseContext, index: usize) {
        if !matches!(
            ctx.current,
            ParseState::Quote | ParseState::Escape | ParseState::Slash | ParseState::SlashStar
        ) {
            let tmp_vec: &mut Vec<Token> = &mut Vec::new();
            while let Some(f) = ctx.stack.pop_back() {
                if let Token::Object(ref v, ref value) = f
                    && value.is_none()
                {
                    tmp_vec.reverse();
                    ctx.path_info.pop(v.path.last().unwrap() + 1);
                    ctx.stack.push_back(Token::Object(
                        ExtraInfo {
                            range: (v.range.0, index + 1),
                            path:  v.path.to_vec(),
                        },
                        Some(tmp_vec.clone()),
                    ));
                    *ctx.current = ctx.last.clone();
                    return;
                } else {
                    tmp_vec.push(f);
                }
            }
        }
    }

    fn handle_left_bracket(ctx: &mut ParseContext, index: usize) {
        if !matches!(
            ctx.current,
            ParseState::Quote | ParseState::Escape | ParseState::Slash | ParseState::SlashStar
        ) {
            let current = std::mem::replace(ctx.current, ParseState::LeftBracket);
            *ctx.last = current;
            *ctx.l = index;
            ctx.stack.push_back(Token::Array(
                ExtraInfo {
                    range: (index, 0),
                    path:  ctx.path_info.push(),
                },
                None,
            ));
        }
    }

    fn handle_right_bracket(ctx: &mut ParseContext, index: usize) {
        if !matches!(
            ctx.current,
            ParseState::Quote | ParseState::Escape | ParseState::Slash | ParseState::SlashStar
        ) {
            let tmp_vec: &mut Vec<Token> = &mut Vec::new();
            while let Some(f) = ctx.stack.pop_back() {
                if let Token::Array(ref v, ref value) = f
                    && value.is_none()
                {
                    tmp_vec.reverse();
                    ctx.path_info.pop(v.path.last().unwrap() + 1);
                    ctx.stack.push_back(Token::Array(
                        ExtraInfo {
                            range: (v.range.0, index + 1),
                            path:  v.path.to_vec(),
                        },
                        Some(tmp_vec.clone()),
                    ));
                    *ctx.current = ctx.last.clone();
                    return;
                } else {
                    tmp_vec.push(f);
                }
            }
        }
    }

    fn handle_other(ctx: &mut ParseContext, char: &str, index: usize) {
        if matches!(ctx.current, ParseState::Quote | ParseState::Escape) {
            ctx.value.push_str(char);
            return;
        } else if matches!(ctx.current, ParseState::Slash) {
            let quit = if matches!(char, "\r\n" | "\n" | "\r") {
                true
            } else {
                false
            };
            if quit {
                trace!("revert slash use {:?}", ctx.last);
                *ctx.current = ctx.last.clone();
            }
            return;
        } else if matches!(ctx.current, ParseState::SlashStar) {
            let quit = if let Some(v) = ctx.chars.get(index + 1) {
                char == "*" && v.as_ref() == "/"
            } else {
                false
            };
            if quit {
                trace!("revert slash use {:?}", ctx.last);
                *ctx.current = ctx.last.clone();
            }
            return;
        } else if Self::is_ignore(char) {
            return;
        } else if !matches!(ctx.current, ParseState::Other) {
            let current = std::mem::replace(ctx.current, ParseState::Other);
            *ctx.last = current;
            *ctx.l = index;
            ctx.value.push_str(char)
        } else {
            ctx.value.push_str(char)
        }
    }
}

// pub(crate) fn lexer<'a>() -> ChumskyParser<'a> {
// }

// pub(crate) fn parse<'a>(
//     parser: ChumskyParser<'a>,
//     input: &'a str,
// ) -> Result<((usize, usize), Vec<Token<'a>>), Vec<Rich<'a, char>>> {
// }

// pub(crate) fn to_map(tokens: Vec<Token>) -> std::collections::HashMap<String, Token> {
//     let mut key = String::new();
//     let mut collect = false;
//     let mut r = std::collections::HashMap::new();
//     for i in tokens {
//         match i {
//             Token::Str(_, v) => {
//                 if collect {
//                     r.insert(std::mem::take(&mut key), i);
//                     collect = false;
//                 } else {
//                     key.push_str(v);
//                 }
//             }
//             Token::Colon(_) => collect = true,
//             _ if collect => {
//                 r.insert(std::mem::take(&mut key), i);
//                 collect = false;
//             }
//             _ => {}
//         }
//     }
//     r
// }

// pub(crate) fn to_map_ref<'a>(
//     tokens: &'a Vec<Token<'a>>,
// ) -> std::collections::HashMap<String, &'a Token<'a>> {
//     let mut key = String::new();
//     let mut collect = false;
//     let mut r = std::collections::HashMap::new();
//     for i in tokens {
//         match i {
//             Token::Str(_, v) => {
//                 if collect {
//                     r.insert(std::mem::take(&mut key), i);
//                     collect = false;
//                 } else {
//                     key.push_str(v);
//                 }
//             }
//             Token::Colon(_) => collect = true,
//             _ if collect => {
//                 r.insert(std::mem::take(&mut key), i);
//                 collect = false;
//             }
//             _ => {}
//         }
//     }
//     r
// }

// pub(crate) fn to_map_with_span_ref<'a>(
//     tokens: &'a Vec<Token<'a>>,
// ) -> std::collections::HashMap<String, ((usize, usize), &'a Token<'a>)> {
//     let mut key = String::new();
//     let mut start_span = None;
//     let mut collect = false;
//     let mut r = std::collections::HashMap::new();

//     for i in tokens {
//         match i {
//             Token::Str(span, v) => {
//                 if collect {
//                     let start = start_span.expect("Missing key for value in `to_map_with_span_ref`");
//                     r.insert(std::mem::take(&mut key), collect_v(start, span, i));
//                     collect = false;
//                 } else {
//                     key.push_str(v);
//                     start_span = Some(span);
//                 }
//             }
//             Token::Colon(_) => collect = true,
//             Token::Array(span, _)
//             | Token::Object(span, _)
//             | Token::Num(span, _)
//             | Token::Bool(span, _)
//                 if collect =>
//             {
//                 let start = start_span.expect("Missing key for value in `to_map_with_span_ref`");
//                 r.insert(std::mem::take(&mut key), collect_v(start, span, i));
//                 collect = false;
//             }
//             _ => {}
//         }
//     }
//     r
// }

// pub(crate) fn to_arc_str_hashset(tokens: &Vec<Token>) -> HashSet<std::sync::Arc<str>> {
//     let mut r = HashSet::new();
//     for i in tokens {
//         if let Token::Str(_, v) = i {
//             r.insert(std::sync::Arc::from(*v));
//         }
//     }
//     r
// }

// pub(crate) fn to_string(token: Token) -> String {
//     if let Token::Str(_, v) = token {
//         String::from(v)
//     } else {
//         String::new()
//     }
// }

// pub(crate) fn to_string_ref<'a>(token: &'a Token<'a>) -> String {
//     if let Token::Str(_, v) = token {
//         String::from(*v)
//     } else {
//         String::new()
//     }
// }

// pub(crate) fn to_array(token: Token) -> Vec<Token> {
//     if let Token::Array(_, v) = token {
//         v
//     } else {
//         vec![]
//     }
// }

// pub(crate) fn to_array_ref<'a>(token: &'a Token<'a>) -> Option<&'a Vec<Token<'a>>> {
//     if let Token::Array(_, v) = token {
//         Some(v)
//     } else {
//         None
//     }
// }

// pub(crate) fn to_bool(token: Token) -> bool {
//     if let Token::Bool(_, v) = token {
//         v
//     } else {
//         false
//     }
// }

// pub(crate) fn to_bool_ref<'a>(token: &'a Token<'a>) -> bool {
//     if let Token::Bool(_, v) = token {
//         *v
//     } else {
//         false
//     }
// }

// pub(crate) fn to_number(token: Token) -> f64 {
//     if let Token::Num(_, v) = token {
//         v
//     } else {
//         0 as f64
//     }
// }

// pub(crate) fn to_number_ref<'a>(token: &'a Token<'a>) -> f64 {
//     if let Token::Num(_, v) = token {
//         *v
//     } else {
//         0 as f64
//     }
// }

// #[allow(unused_imports)]
// pub(crate) mod prelude {
//     pub(crate) use super::{
//         parse, parser, to_arc_str_hashset, to_array, to_array_ref, to_bool, to_bool_ref, to_map,
//         to_map_with_span_ref, to_number, to_number_ref, to_string, to_string_ref, Token,
//     };
// }

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use flexi_logger::{FileSpec, LogSpecification, Logger, LoggerHandle, WriteMode};

    use crate::document::Document;
    use crate::lexer::{Lexer, Token};

    const EXAMPLE1: &str = include_str!("../test/achievement_screen.json");
    const VANILLAPACK_DEFINE: &str = include_str!("../resources/vanillapack_define_1.21.40.3.json");

    #[tokio::test]
    async fn test_parse_full_1() -> Result<(), Box<dyn std::error::Error>> {
        let r = Lexer::new();
        let doc = Document::from(Arc::from(EXAMPLE1));
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(e, v) = r.get(0).unwrap()
        {
            let r = v.as_ref().unwrap().get(0).unwrap();
            assert!(matches!(r, Token::Str(_, _)));
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
        let r = r.parse(None, &doc).await;
        if let Some(r) = r
            && let Token::Object(_, v) = r.get(0).unwrap()
        {
            let r = v.as_ref().unwrap().get(0).unwrap();
            assert!(matches!(r, Token::Str(_, _)));
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

    fn setup_logger() -> LoggerHandle {
        Logger::with(LogSpecification::trace())
            .log_to_file(FileSpec::default().directory("log_files").basename("app_log"))
            .rotate(
                flexi_logger::Criterion::Size(5 * 1024 * 1024),
                flexi_logger::Naming::Timestamps,
                flexi_logger::Cleanup::KeepLogFiles(10),
            )
            .write_mode(WriteMode::BufferAndFlush)
            .start()
            .unwrap()
    }

    //     #[test]
    //     fn test_parse_full_2() {
    //         let parser = parser();
    //         let r = parser.parse(VANILLAPACK_DEFINE).into_result();
    //         match r {
    //             Ok(ref r) => {
    //                 if let Token::Object(_, ref v) = r[0] {
    //                     assert_eq!(863, v.len());
    //                 } else {
    //                     panic!();
    //                 }
    //             }
    //             Err(e) => {
    //                 panic!("{:?}", e);
    //             }
    //         }
    //     }

    //     #[test]
    //     fn test_parse_2() {
    //         let parser = parser();
    //         let r = parser
    //             .parse(
    //                 r#"
    //                 {
    //   "radio_toggle_group": {
    //     "description": { "zh-cn": "是否启用开关组", "en-us": "radio_toggle_group" },
    //     "values": [
    //       { "label": "true", "kind": 12, "insert_text_format": 1 },
    //       { "label": "false", "kind": 12, "insert_text_format": 1 }
    //     ]
    //   }
    // }"#,
    //             )
    //             .into_result();
    //         match r {
    //             Ok(ref r) => {
    //                 println!("{:?}", r);
    //                 assert_eq!(1, r.len());
    //                 if let Token::Object(_, ref v) = r[0]
    //                     && let Token::Str(_, v) = v[0]
    //                 {
    //                     assert_eq!("radio_toggle_group", v);
    //                 } else {
    //                     panic!()
    //                 }
    //             }
    //             Err(ref e) => {
    //                 panic!("{:?}", e);
    //             }
    //         }
    //     }

    //     #[test]
    //     fn test_parse_3() {
    //         let parser = parser();
    //         let r = parser
    //             .parse(
    //                 r#"
    //                 {
    //                 "add_external_server_screen_new@add_external_server_screen": {}
    //                 }
    //                 "#,
    //             )
    //             .into_result();
    //         match r {
    //             Ok(ref r) => {
    //                 println!("{:?}", r);
    //                 assert_eq!(1, r.len());
    //                 if let Token::Object(_, ref v) = r[0]
    //                     && let Token::Object(_, ref v) = v[2]
    //                 {
    //                     assert_eq!(&Vec::<Token<'_>>::new(), v);
    //                 } else {
    //                     panic!()
    //                 }
    //             }
    //             Err(ref e) => {
    //                 panic!("{:?}", e);
    //             }
    //         }
    //     }

    //     #[test]
    //     fn test_parse_4() {
    //         let parser = parser();
    //         let r = parser
    //             .parse(
    //                 r#"
    //                 {
    //                 "add_external_server_screen_new@add_external_server_screen": [ ]
    //                 }
    //                 "#,
    //             )
    //             .into_result();
    //         match r {
    //             Ok(ref r) => {
    //                 println!("{:?}", r);
    //                 assert_eq!(1, r.len());
    //                 if let Token::Object(_, ref v) = r[0]
    //                     && let Token::Array(_, ref v) = v[2]
    //                 {
    //                     assert_eq!(&Vec::<Token<'_>>::new(), v);
    //                 } else {
    //                     panic!()
    //                 }
    //             }
    //             Err(ref e) => {
    //                 panic!("{:?}", e);
    //             }
    //         }
    //     }

    //     #[test]
    //     fn test_to_map() {
    //         let parser = parser();
    //         let r = parse(parser, EXAMPLE1);

    //         match r {
    //             Ok((_, r)) => {
    //                 let r = to_map(r);
    //                 assert!(r.contains_key("namespace"));
    //                 assert!(r.contains_key("full_progress_bar_icon_base_test"));
    //                 println!("{:?}", r);
    //             }
    //             Err(e) => {
    //                 panic!("{:?}", e);
    //             }
    //         }
    //     }
}
