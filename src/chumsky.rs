use std::collections::HashSet;

use chumsky::prelude::*;

#[derive(PartialEq, Debug)]
pub(crate) enum Token<'a> {
    Invalid(SimpleSpan),
    Bool(SimpleSpan, bool),
    Comment(SimpleSpan, &'a str),
    Str(SimpleSpan, &'a str),
    Num(SimpleSpan, f64),
    Colon(SimpleSpan),
    Comma(SimpleSpan),
    Array(SimpleSpan, Vec<Token<'a>>),
    Controls(SimpleSpan, (&'a str, char, Vec<Token<'a>>)),
}

pub(crate) type ChumskyParser<'a> = Boxed<'a, 'a, &'a str, Vec<Token<'a>>, extra::Err<Rich<'a, char>>>;

pub(crate) fn parser<'a>() -> ChumskyParser<'a> {
    recursive(|value| {
        let digits = text::digits(10).to_slice();

        let frac = just('.').then(digits);

        let exp = just('e').or(just('E')).then(one_of("+-").or_not()).then(digits);

        let number = just('-')
            .or_not()
            .then(text::int(10))
            .then(frac.or_not())
            .then(exp.or_not())
            .padded_by(text::whitespace())
            .to_slice()
            .map_with(|s: &str, e| Token::Num(e.span(), s.parse().unwrap()));

        let escape = just('\\')
            .then(choice((
                just('\\'),
                just('/'),
                just('"'),
                just('b').to('\x08'),
                just('f').to('\x0C'),
                just('n').to('\n'),
                just('r').to('\r'),
                just('t').to('\t'),
                just('u').ignore_then(text::digits(16).exactly(4).to_slice().validate(
                    |digits, e, emitter| {
                        char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                            emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
                            '\u{FFFD}' // unicode replacement character
                        })
                    },
                )),
            )))
            .ignored();

        let string = none_of("\\\"")
            .ignored()
            .or(escape)
            .repeated()
            .to_slice()
            .map_with(|s, e| Token::Str(e.span(), s))
            .delimited_by(just('"'), just('"'))
            .padded_by(text::whitespace());

        let line_comment = just("//").ignore_then(none_of("\r\n").repeated());
        let block_comment = just("/*")
            .ignore_then(any().and_is(just("*/").not()).repeated())
            .then_ignore(just("*/"));

        let comment = line_comment
            .or(block_comment)
            .to_slice()
            .map_with(|v, e| Token::Comment(e.span(), v));

        let array = value
            .clone()
            .separated_by(
                just(',')
                    .padded_by(text::whitespace())
                    .recover_with(skip_then_retry_until(any().ignored(), one_of(",]").ignored())),
            )
            .allow_trailing()
            .collect()
            .map_with(|s, e| Token::Array(e.span(), s))
            .padded_by(text::whitespace())
            .delimited_by(
                just('['),
                just(']')
                    .ignored()
                    .recover_with(via_parser(end()))
                    .recover_with(skip_then_retry_until(any().ignored(), end())),
            );

        let controls = string
            .clone()
            .then(just(':').padded_by(text::whitespace()))
            .then(value.clone().repeated().collect::<Vec<Token<'a>>>())
            .delimited_by(
                just('{'),
                just('}')
                    .ignored()
                    .recover_with(via_parser(end()))
                    .recover_with(skip_then_retry_until(any().ignored(), end())),
            )
            .padded_by(text::whitespace())
            .map_with(|s, e| {
                let t1 = s.0;
                let (v1, v2) = t1;
                let x = match v1 {
                    Token::Str(_, str) => str,
                    _ => panic!(),
                };
                Token::Controls(e.span(), (x, v2, s.1))
            });

        choice((
            just("true").map_with(|_, e| Token::Bool(e.span(), true)),
            just("false").map_with(|_, e| Token::Bool(e.span(), false)),
            number,
            string,
            array,
            controls,
            comment,
            just(",").map_with(|_, e| Token::Comma(e.span())),
            just(":").map_with(|_, e| Token::Colon(e.span())),
        ))
        .recover_with(via_parser(nested_delimiters('{', '}', [('[', ']')], |e| Token::Invalid(e))))
        .recover_with(via_parser(nested_delimiters('[', ']', [('{', '}')], |e| Token::Invalid(e))))
        .recover_with(skip_then_retry_until(any().ignored(), one_of(",]}").ignored()))
        .padded_by(text::whitespace())
    })
    .repeated()
    .collect()
    .boxed()
}

pub(crate) fn parse<'a>(
    parser: ChumskyParser<'a>,
    input: &'a str,
) -> Result<Vec<Token<'a>>, Vec<Rich<'a, char>>> {
    parser.parse(input).into_result()
}

pub(crate) fn to_map(tokens: Vec<Token>) -> std::collections::HashMap<String, Token> {
    let mut key: String = String::new();
    let mut collect: bool = false;
    let mut r = std::collections::HashMap::<String, Token>::new();
    for i in tokens {
        match i {
            Token::Controls(_, (key, _, _)) => {
                r.insert(key.to_string(), i);
            }
            Token::Str(_, v) => {
                key.push_str(v);
            }
            Token::Colon(_) => collect = true,
            _ => {
                if collect {
                    r.insert(key.clone(), i);
                    collect = false;
                    key.clear();
                }
            }
        }
    }
    r
}

pub(crate) fn to_arc_str_hashset<T>(tokens: Vec<Token>) -> HashSet<std::sync::Arc<str>> {
    let mut r = HashSet::new();
    for i in tokens {
        if let Token::Str(_, v) = i {
            r.insert(std::sync::Arc::from(v));
        }
    }
    r
}

pub(crate) fn to_string(token: Token) -> String {
    if let Token::Str(_, v) = token {
        String::from(v)
    } else {
        String::new()
    }
}

pub(crate) fn to_string_ref<'a>(token: &'a Token<'a>) -> String {
    if let Token::Str(_, v) = token {
        String::from(*v)
    } else {
        String::new()
    }
}

pub(crate) fn to_array(token: Token) -> Vec<Token> {
    if let Token::Array(_, v) = token {
        v
    } else {
        vec![]
    }
}

pub(crate) fn to_array_ref<'a>(token: &'a Token<'a>) -> Option<&'a Vec<Token<'a>>> {
    if let Token::Array(_, v) = token {
        Some(v)
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

pub(crate) fn to_bool_ref<'a>(token: &'a Token<'a>) -> bool {
    if let Token::Bool(_, v) = token {
        *v
    } else {
        false
    }
}

pub(crate) fn to_number(token: Token) -> f64 {
    if let Token::Num(_, v) = token {
        v
    } else {
        0 as f64
    }
}

pub(crate) fn to_number_ref<'a>(token: &'a Token<'a>) -> f64 {
    if let Token::Num(_, v) = token {
        *v
    } else {
        0 as f64
    }
}

#[allow(unused_imports)]
pub(crate) mod prelude {
    pub(crate) use super::{
        parse, parser, to_arc_str_hashset, to_array, to_array_ref, to_bool, to_bool_ref, to_map,
        to_number, to_number_ref, to_string, to_string_ref, ChumskyParser, Token,
    };
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use super::parser;
    use crate::chumsky::Token;

    const EXAMPLE1: &str = include_str!("../test/achievement_screen.json");

    #[test]
    fn test_parse() {
        let parser = parser();
        let r = parser.parse(EXAMPLE1).into_result();
        match r {
            Ok(r) => {
                println!("{:?}", r);
                assert_eq!(2, r.len());
                assert!(matches!(r.get(1).unwrap(), Token::Controls(_, _)));
            }
            Err(e) => {
                panic!("{:?}", e);
            }
        }
    }
}
