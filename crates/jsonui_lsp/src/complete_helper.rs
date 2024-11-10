use std::sync::Arc;

use log::trace;

use crate::completer::{split_control_name, AutoTree, Completer, ControlNameSymbol, ControlNode};
use crate::document::Document;
use crate::lexer::prelude::*;
use crate::museair::{BfastHashMap, BfastHashSet};
use crate::towerlsp::*;
use crate::tree_ds::prelude::{AutomatedId, Node};

const BIND: &str = "bindings";
const CONTROLS: &str = "controls";

pub(crate) fn color(doc: &Document, tokens: &[Token]) -> Option<Vec<ColorInformation>> {
    let mut color_infos = Vec::new();
    let tokens = flatten_tokens(tokens);
    let mut iter = tokens.iter();
    while let Some(color_v) = iter
        .by_ref()
        .skip_while(|r| !matches!(r, Token::Str(_, v) if v.as_str() == "color"))
        .nth(2)
    {
        let pos = color_v.pos();
        if let (Some(left_v), Some(right_v)) =
            (doc.get_position_from_index(pos.0), doc.get_position_from_index(pos.1))
        {
            if let Some(color) = from_color_value_to_color_arr(color_v) {
                color_infos.push(ColorInformation {
                    range: Range {
                        start: left_v,
                        end:   right_v,
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

pub(crate) fn normal(
    completer: &Completer,
    index: usize,
    boundary_indices: (usize, usize),
    pos: Position,
    char: Arc<str>,
    lang: Arc<str>,
    tokens: &[Token],
    tree: &AutoTree<ControlNode>,
    define_map: &BfastHashMap<String, Token>,
) -> Option<Vec<CompletionItem>> {
    let root = tree.get_root_node().expect("cant get root node");
    let r = find_closest_node(tree, &root, index);
    if let Some(r) = r {
        let n = tree.get_node_by_id(&r).unwrap();

        let result = flatten_tokens(tokens);
        let boundary = boundary_tokens(&result, boundary_indices);
        let neighbors = find_neighbors_token(&result, index);

        let default1: &Option<&Token> = &None;
        let n1 = neighbors.first().unwrap_or(default1);
        let n2 = neighbors.get(1).unwrap_or(default1);
        let current = neighbors.get(2).unwrap_or(default1);
        let n3 = neighbors.get(3).unwrap_or(default1);
        let n4 = neighbors.get(4).unwrap_or(default1);
        let r = boundary.first();

        if let Some(Token::Str(_, str)) = r {
            #[cfg(feature = "debug-parse")]
            trace!(
                "input |n1 {:?}| |n2 {:?}| |current {:?}| |n3 {:?}| |n4 {:?}| char '{}'",
                n1,
                n2,
                current,
                n3,
                n4,
                char
            );
            match str.as_str() {
                CONTROLS => {}
                BIND if char.as_ref() == "\""
                    && !matches!(n2, Some(Token::Colon(_)))
                    && current.is_some()
                    && matches!(current.unwrap(), Token::Str(_, str) if str.is_empty()) =>
                {
                    trace!("create_bindings_type_completion");
                    let inputs =
                        create_binding_type_input(index, boundary, define_map, current.unwrap());
                    return create_type_completion(inputs, pos, lang, define_map);
                }
                _ => {
                    if let Some(Token::Str(_, str)) = n1
                        && let Some(Token::Colon(_)) = n2
                        && let Some(Token::Str(_, v)) = current
                        && v.is_empty()
                    {
                        trace!("create_value_completion");
                        let extra_values = create_variables_completion(completer, n);
                        return create_value_completion(
                            pos,
                            n3,
                            char,
                            lang,
                            str,
                            define_map,
                            &extra_values,
                        );
                    } else if let Some(Token::Str(_, str)) = n2
                        && let Some(Token::Colon(_)) = current
                    {
                        trace!("create_value_completion");
                        let extra_values = create_variables_completion(completer, n);
                        return create_value_completion(
                            pos,
                            n3,
                            char,
                            lang,
                            str,
                            define_map,
                            &extra_values,
                        );
                    } else if char.as_ref() == "\""
                        && let Some(Token::Str(_, str)) = current
                        && str.is_empty()
                    {
                        let node = n.get_value().unwrap();
                        let r = if let Some(r) = node.get_type() {
                            r
                        } else if let Some(extend) = &node.define.name.1
                            && let Some((r, _)) = completer.find_extend_value(extend)
                        {
                            r.to_string()
                        } else {
                            trace!("cant find type_completion {}", node);
                            return None;
                        };
                        trace!("create_type_completion {}", r);
                        let inputs = create_type_input(&r, define_map);
                        return create_type_completion(inputs, pos, lang, define_map);
                    }
                }
            }
        }
    }
    None
}

pub(crate) fn goto_definition(
    completer: &Completer,
    namespace: Arc<str>,
    tokens: &[Token],
    index: usize,
) -> Option<GotoDefinitionResponse> {
    let tokens = flatten_tokens(tokens);
    let token = tokens
        .iter()
        .find(|f| matches!(f, Token::Str(range, _) if range.0 <= index && index<=range.1));
    if let Some(Token::Str(_, token)) = token {
        if token.contains("@") {
            let name = split_control_name(token.as_str(), namespace.as_ref());
            if let Some(name) = name.1 {
                let symbol = ControlNameSymbol::new(name);
                let r = completer.find_symbol(&symbol)?;
                return Some(GotoDefinitionResponse::Scalar(r));
            }
        }
    }
    None
}

fn create_type_input<'a>(
    type_n: &'a String,
    define_map: &'a BfastHashMap<String, Token>,
) -> Vec<&'a Token> {
    let mut inputs: Vec<&Token> = vec![];
    if let Some(Token::Array(_, arr)) = define_map.get(type_n) {
        inputs.extend(arr.as_ref().unwrap());
    }
    inputs.extend(to_array_ref(define_map.get("common").unwrap()).unwrap());
    inputs
}

fn create_binding_type_input<'a>(
    index: usize,
    boundary: Vec<&'a Token>,
    define_map: &'a BfastHashMap<String, Token>,
    current: &'a Token,
) -> Vec<&'a Token> {
    let mut r = vec![];
    let bindings = boundary.iter().filter(|f| matches!(f, Token::Object(_, _)));
    let mut in_binding = false;
    for binding in bindings {
        if let Token::Object(pos, Some(binding)) = binding {
            let c_pos = current.pos();
            if pos.0 <= index && index <= pos.1 {
                in_binding = true;
            }
            if pos.0 <= c_pos.0 && c_pos.1 <= pos.1 {
                if let Some(Token::Str(_, type_v)) =
                    binding.iter().enumerate().find_map(|(index, token)| {
                        if let Token::Str(_, s) = token {
                            if s.as_str() == "binding_type" && index + 2 < binding.len() {
                                Some(&binding[index + 2])
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                {
                    if let Some(Token::Array(_, Some(arr))) = define_map.get(type_v) {
                        r.extend(arr);
                    }
                    break;
                }
            }
        }
    }
    if let Some(Token::Array(_, Some(arr))) = define_map.get("bindings_properties")
        && in_binding
    {
        r.extend(arr);
    }
    r
}

fn create_variables_completion(
    completer: &Completer,
    n: Node<AutomatedId, ControlNode>,
) -> BfastHashSet<Arc<str>> {
    if let Some(v) = n.get_value()
        && let Some(extend) = &v.define.name.1
    {
        let extend_v = completer.find_extend_value(extend);
        if let Some((_, vars)) = extend_v {
            if let Some(r) = v.define.variables.try_lock() {
                let mut r = r.clone();
                r.extend(vars.iter().cloned());
                return r;
            }
        }
    }
    BfastHashSet::default()
}

fn find_neighbors_token<'a>(flatted_tokens: &[&'a Token], index: usize) -> Vec<Option<&'a Token>> {
    let closed_index = flatted_tokens.iter().map(|v| v.pos()).rposition(|s| s.0 <= index);
    let mut neighbors: Vec<Option<&'a Token>> = Vec::with_capacity(5);
    if let Some(i) = closed_index {
        let i = i as isize;
        for offset in -2..3 {
            let index = i + offset;
            if index < 0 || index >= flatted_tokens.len() as isize {
                neighbors.push(None);
            } else {
                neighbors.push(flatted_tokens.get(index as usize).copied());
            }
        }
    }
    neighbors
}

pub fn boundary_tokens<'a>(tokens: &'a [&'a Token], boundary_indices: (usize, usize)) -> Vec<&'a Token> {
    tokens
        .iter()
        .filter_map(|token| {
            let range = token.pos();
            let r = boundary_indices.0 <= range.0 && range.1 <= boundary_indices.1;
            if r {
                Some(*token)
            } else {
                None
            }
        })
        .collect()
}

pub fn flatten_tokens(tokens: &[Token]) -> Vec<&Token> {
    let mut result = Vec::new();
    for token in tokens.iter() {
        match token {
            Token::Array(_, inner_tokens) => {
                result.push(token);
                result.extend(flatten_tokens(inner_tokens.as_ref().unwrap()));
            }
            Token::Object(_, inner_tokens) => {
                result.push(token);
                result.extend(flatten_tokens(inner_tokens.as_ref().unwrap()));
            }
            token => {
                result.push(token);
            }
        }
    }
    result
}

fn find_closest_node(
    tree: &AutoTree<ControlNode>,
    n: &Node<AutomatedId, ControlNode>,
    index: usize,
) -> Option<AutomatedId> {
    let mut closest_node = None;

    fn search(
        tree: &AutoTree<ControlNode>,
        n: &Node<AutomatedId, ControlNode>,
        index: usize,
        closest_node: &mut Option<AutomatedId>,
    ) {
        if let Some(ref v) = n.get_value() {
            let (l, r) = v.loc;
            if l <= index && index <= r {
                *closest_node = Some(n.get_node_id());
                for i in n.get_children_ids() {
                    if let Some(child_node) = tree.get_node_by_id(&i) {
                        search(tree, &child_node, index, closest_node);
                    }
                }
            }
        }
    }
    search(tree, n, index, &mut closest_node);
    closest_node
}

fn from_number_to_insert_text_format(kind: u64) -> Option<InsertTextFormat> {
    match kind {
        1 => Some(InsertTextFormat::PLAIN_TEXT),
        2 => Some(InsertTextFormat::SNIPPET),
        _ => None,
    }
}

fn from_number_to_completion_item_kind(kind: u64) -> Option<CompletionItemKind> {
    match kind {
        1 => Some(CompletionItemKind::TEXT),
        2 => Some(CompletionItemKind::METHOD),
        3 => Some(CompletionItemKind::FUNCTION),
        4 => Some(CompletionItemKind::CONSTRUCTOR),
        5 => Some(CompletionItemKind::FIELD),
        6 => Some(CompletionItemKind::VARIABLE),
        7 => Some(CompletionItemKind::CLASS),
        8 => Some(CompletionItemKind::INTERFACE),
        9 => Some(CompletionItemKind::MODULE),
        10 => Some(CompletionItemKind::PROPERTY),
        11 => Some(CompletionItemKind::UNIT),
        12 => Some(CompletionItemKind::VALUE),
        13 => Some(CompletionItemKind::ENUM),
        14 => Some(CompletionItemKind::KEYWORD),
        15 => Some(CompletionItemKind::SNIPPET),
        16 => Some(CompletionItemKind::COLOR),
        17 => Some(CompletionItemKind::FILE),
        18 => Some(CompletionItemKind::REFERENCE),
        19 => Some(CompletionItemKind::FOLDER),
        20 => Some(CompletionItemKind::ENUM_MEMBER),
        21 => Some(CompletionItemKind::CONSTANT),
        22 => Some(CompletionItemKind::STRUCT),
        23 => Some(CompletionItemKind::EVENT),
        24 => Some(CompletionItemKind::OPERATOR),
        25 => Some(CompletionItemKind::TYPE_PARAMETER),
        _ => None,
    }
}

fn from_color_value_to_color_arr(v: &Token) -> Option<Color> {
    if let Token::Str(_, color_str) = v {
        return match color_str.as_ref() {
            "white" => Some(Color {
                red:   1.0,
                green: 1.0,
                blue:  1.0,
                alpha: 1.0,
            }),
            "silver" => Some(Color {
                red:   0.776,
                green: 0.776,
                blue:  0.776,
                alpha: 1.0,
            }),
            "gray grey" => Some(Color {
                red:   0.333,
                green: 0.333,
                blue:  0.333,
                alpha: 1.0,
            }),
            "black" => Some(Color {
                red:   0.0,
                green: 0.0,
                blue:  0.0,
                alpha: 1.0,
            }),
            "red" => Some(Color {
                red:   1.0,
                green: 0.333,
                blue:  0.333,
                alpha: 1.0,
            }),
            "green" => Some(Color {
                red:   0.333,
                green: 1.0,
                blue:  0.333,
                alpha: 1.0,
            }),
            "yellow" => Some(Color {
                red:   1.0,
                green: 1.0,
                blue:  0.333,
                alpha: 1.0,
            }),
            "brown" => Some(Color {
                red:   0.706,
                green: 0.408,
                blue:  0.302,
                alpha: 1.0,
            }),
            "cyan" => Some(Color {
                red:   0.0,
                green: 0.667,
                blue:  0.667,
                alpha: 1.0,
            }),
            "blue" => Some(Color {
                red:   0.333,
                green: 0.333,
                blue:  1.0,
                alpha: 1.0,
            }),
            "orange" => Some(Color {
                red:   1.0,
                green: 0.667,
                blue:  0.0,
                alpha: 1.0,
            }),
            "purple" => Some(Color {
                red:   1.0,
                green: 0.333,
                blue:  1.0,
                alpha: 1.0,
            }),
            "nil" => Some(Color {
                red:   1.0,
                green: 1.0,
                blue:  1.0,
                alpha: 0.0,
            }),
            _ => None,
        };
    } else if let Token::Array(_, Some(color_arr)) = &v {
        if color_arr.len() >= 5
            && let Token::Num(_, v1) = color_arr[0]
            && v1 >= 0.0
            && v1 <= 1.0
            && let Token::Num(_, v2) = color_arr[2]
            && v2 >= 0.0
            && v2 <= 1.0
            && let Token::Num(_, v3) = color_arr[4]
            && v3 >= 0.0
            && v3 <= 1.0
        {
            return if color_arr.len() == 7
                && let Token::Num(_, v4) = color_arr[6]
                && v4 >= 0.0
                && v4 <= 1.0
            {
                Some(Color {
                    red:   v1,
                    green: v2,
                    blue:  v3,
                    alpha: v4,
                })
            } else {
                Some(Color {
                    red:   v1,
                    green: v2,
                    blue:  v3,
                    alpha: 1.0,
                })
            };
        }
    }
    None
}

fn create_value_completion(
    pos: Position,
    n3: &Option<&Token>,
    char: Arc<str>,
    lang: Arc<str>,
    property: &str,
    define_map: &BfastHashMap<String, Token>,
    extra_values: &BfastHashSet<Arc<str>>,
) -> Option<Vec<CompletionItem>> {
    let mut result = Vec::new();
    let c = char.as_ref();
    let is_colon = c == ":";
    let suffix = if matches!(n3, Some(Token::Comma(_))) {
        ""
    } else {
        ","
    };

    if let Some(v) = define_map.get(property)
        && let Token::Object(_, v) = v
    {
        let properties = to_map_ref(v.as_ref().unwrap());
        if let Some(Token::Array(_, values)) = properties.get("values") {
            for (index, v) in values.as_ref().unwrap().iter().enumerate() {
                let v = if let Token::Object(_, v) = v {
                    v.as_ref().unwrap()
                } else {
                    continue;
                };

                let value = to_map_ref(v);
                let des = value.get("description");
                let insert_text_format = value
                    .get("insert_text_format")
                    .and_then(|k| from_number_to_insert_text_format(to_number_ref(k) as u64));
                let kind = value
                    .get("kind")
                    .and_then(|k| from_number_to_completion_item_kind(to_number_ref(k) as u64));
                if char.as_ref() == "\""
                    && let Some(format) = insert_text_format
                    && ((format == InsertTextFormat::PLAIN_TEXT && kind.is_some())
                        || format == InsertTextFormat::SNIPPET)
                {
                    continue;
                }

                let needs_quotes = c == ":" && insert_text_format.is_none();
                let mut insert_text = None;
                let mut text_edit = None;
                if is_colon {
                    insert_text = value.get("insert_text").or(value.get("label")).map(|k| {
                        let fill = to_string_ref(k);
                        if needs_quotes {
                            format!(" \"{}\"{}", fill, suffix)
                        } else if is_colon {
                            format!(" {}{}", fill, suffix)
                        } else {
                            fill
                        }
                    });
                } else if kind.is_none() {
                    text_edit = value.get("insert_text").or(value.get("label")).map(|k| {
                        let fill = to_string_ref(k);
                        CompletionTextEdit::Edit(TextEdit {
                            range:    Range {
                                start: pos,
                                end:   Position {
                                    line:      pos.line,
                                    character: pos.character + 1,
                                },
                            },
                            new_text: format!("{}\"{}", fill, suffix),
                        })
                    })
                }

                result.push(CompletionItem {
                    label: value
                        .get("label")
                        .map(|v| to_string_ref(v))
                        .unwrap_or("unknown label".to_string()),
                    label_details: Some(CompletionItemLabelDetails {
                        description: des.map_or(Some("jsonui support".to_string()), |d| {
                            let d = if let Token::Object(_, d) = d {
                                to_map_ref(d.as_ref().unwrap())
                            } else {
                                BfastHashMap::default()
                            };
                            d.get(&lang.to_string())
                                .or(d.get("en-us"))
                                .map(|f| to_string_ref(f))
                        }),
                        detail:      None,
                    }),
                    kind,
                    insert_text_format,
                    insert_text,
                    text_edit,
                    preselect: Some(true),
                    sort_text: Some(format!("00{}", index + 1)),
                    ..Default::default()
                })
            }
        }
    }

    // completion variables
    for i in extra_values {
        let mut insert_text = None;
        let mut text_edit = None;
        if is_colon {
            insert_text = Some(format!(" \"{}\"{}", i, suffix))
        } else {
            text_edit = Some(CompletionTextEdit::Edit(TextEdit {
                range:    Range {
                    start: pos,
                    end:   Position {
                        line:      pos.line,
                        character: pos.character + 1,
                    },
                },
                new_text: format!("{}\"{}", i, suffix),
            }))
        }
        result.push(CompletionItem {
            label: i.to_string(),
            label_details: Some(CompletionItemLabelDetails {
                description: Some("var".to_string()),
                detail:      None,
            }),
            kind: None,
            insert_text_format: None,
            insert_text,
            text_edit,
            preselect: Some(true),
            sort_text: Some("999".to_string()),
            ..Default::default()
        })
    }

    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

fn create_type_completion(
    inputs: Vec<&Token>,
    pos: Position,
    lang: Arc<str>,
    define_map: &BfastHashMap<String, Token>,
) -> Option<Vec<CompletionItem>> {
    let mut result = Vec::new();
    for (index, av) in inputs.into_iter().enumerate() {
        if let Token::Str(_, str) = av {
            if let Some(Token::Object(_, v)) = define_map.get(&str.to_string()) {
                let v = to_map_ref(v.as_ref().unwrap());
                let des = v
                    .get("description")
                    .map(|f| match f {
                        Token::Object(_, v) => to_map_ref(v.as_ref().unwrap()),
                        _ => BfastHashMap::default(),
                    })
                    .unwrap_or_default();
                result.push(CompletionItem {
                    label: str.to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: des
                            .get(lang.as_ref())
                            .or(des.get("en-us"))
                            .map(|f| to_string_ref(f)),
                        ..Default::default()
                    }),
                    kind: Some(CompletionItemKind::TEXT),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range:    Range {
                            start: Position {
                                line:      pos.line,
                                character: pos.character,
                            },
                            end:   Position {
                                line:      pos.line,
                                character: pos.character + 1,
                            },
                        },
                        new_text: format!("{}\"", str.to_owned()),
                    })),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    sort_text: Some(format!("00{}", index + 1)),
                    ..Default::default()
                })
            }
        }
    }
    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Arc;

    use tower_lsp::lsp_types::Position;

    use crate::document::Document;
    use crate::lexer::{Lexer, Token};

    #[test]
    fn test() {
        let r = Lexer::new();
        let input = include_str!("../test/achievement.json");
        let doc = Document::from(Arc::from(input));
        let r = r.parse(None, &doc);
        let r = r.unwrap();
        let r = super::flatten_tokens(&r);
        let index = doc.get_index_from_position(Position {
            line:      32,
            character: 23,
        });
        let boundary = doc.get_boundary_indices(index.unwrap());
        let bd = boundary.unwrap();
        let r = super::boundary_tokens(&r, (bd.0, bd.1));
        let expect = Token::Str((1015, 1025), "controls".to_string());
        assert_eq!(expect, **r.first().unwrap());
    }

    #[tokio::test]
    async fn test_find_closest_node() {
        let p = crate::load_completer();
        let path = PathBuf::from("test");
        assert!(path.exists());

        crate::tests::setup_logger();
        p.init(&path).await;

        let trees = p.trees.read();
        let r = trees.get("add_external_server");
        let r = r.unwrap();
        let cr = super::find_closest_node(r, &r.get_root_node().unwrap(), 1006);
        let cr = cr.expect("cant find closest node");
        let r = r.get_node_by_id(&cr);
        if let Some(v) = r {
            assert_eq!("remove_button", v.get_value().unwrap().define.name.0.as_ref());
        } else {
            panic!()
        }
    }
}
