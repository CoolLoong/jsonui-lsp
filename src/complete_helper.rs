use std::collections::HashMap;
use std::sync::Arc;

use log::trace;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionTextEdit,
    InsertTextFormat, Position, Range, TextEdit,
};

use crate::chumsky::{to_array_ref, to_map_ref, to_number_ref, to_string_ref, Token};
use crate::completer::{AutoTree, ControlNode};
use crate::tree_ds::prelude::{AutomatedId, Node};

const ARRAY: &str = "Array";
const OBJECT: &str = "Object";
const BIND: &str = "bindings";
const CONTROLS: &str = "controls";

fn find_neighbors_token<'a>(
    flatted_tokens: &'a Vec<FlattenToken<'a>>,
    index: usize,
) -> Vec<Option<&'a FlattenToken<'a>>> {
    let closed_index = flatted_tokens
        .iter()
        .map(|v| match v.token {
            Token::Invalid(span)
            | Token::Bool(span, _)
            | Token::Comment(span, _)
            | Token::Str(span, _)
            | Token::Num(span, _)
            | Token::Colon(span)
            | Token::Comma(span)
            | Token::Array(span, _)
            | Token::Object(span, _) => span,
        })
        .rposition(|s| s.start <= index);

    let mut neighbors: Vec<Option<&'a FlattenToken>> = Vec::with_capacity(5);
    if let Some(i) = closed_index {
        let i = i as isize;
        for offset in -1..4 {
            let index = i + offset;
            if index < 0 || index >= flatted_tokens.len() as isize {
                neighbors.push(None);
            } else {
                neighbors.push(flatted_tokens.get(index as usize));
            }
        }
    }
    neighbors
}

pub struct FlattenToken<'a> {
    pub token: &'a Token<'a>,
    pub extra: Option<(&'a str, &'a str)>,
}
pub fn flatten_tokens<'a>(
    tokens: &'a Vec<Token<'a>>,
    extra: Option<(&'a str, &'a str)>,
) -> Vec<FlattenToken<'a>> {
    let mut result = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::Array(_, inner_tokens) => {
                let local_extra = if let Some(Token::Str(_, v)) = tokens.get(i - 2) {
                    Some((*v, ARRAY))
                } else {
                    None
                };
                result.push(FlattenToken { token, extra });
                result.extend(flatten_tokens(inner_tokens, local_extra));
            }
            Token::Object(_, inner_tokens) => {
                let local_extra = if let Some(Token::Str(_, v)) = tokens.get(i - 2) {
                    Some((*v, OBJECT))
                } else {
                    None
                };
                result.push(FlattenToken { token, extra });
                result.extend(flatten_tokens(inner_tokens, local_extra));
            }
            other => {
                result.push(FlattenToken { token: other, extra });
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
    let mut min_distance = usize::MAX;

    fn search(
        tree: &AutoTree<ControlNode>,
        n: &Node<AutomatedId, ControlNode>,
        index: usize,
        closest_node: &mut Option<AutomatedId>,
        min_distance: &mut usize,
    ) {
        if let Some(ref v) = n.get_value() {
            let (l, r) = v.loc;
            let distance = index.abs_diff(l) + r.abs_diff(index);
            if distance < *min_distance {
                *min_distance = distance;
                *closest_node = Some(n.get_node_id());
            }

            if index >= l {
                for i in n.get_children_ids() {
                    if let Some(child_node) = tree.get_node_by_id(&i) {
                        search(tree, &child_node, index, closest_node, min_distance);
                    }
                }
            }
        }
    }

    search(tree, n, index, &mut closest_node, &mut min_distance);
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

pub(crate) fn normal(
    index: usize,
    pos: Position,
    char: Arc<str>,
    lang: Arc<str>,
    tokens: Vec<Token>,
    tree: &AutoTree<ControlNode>,
    define_map: &HashMap<String, Token>,
) -> Option<Vec<CompletionItem>> {
    let root = tree.get_root_node().expect("cant get root node");
    let r = find_closest_node(tree, &root, index);
    if let Some(r) = r {
        let result = flatten_tokens(&tokens, None);

        let n = tree.get_node_by_id(&r).unwrap();
        let neighbors = find_neighbors_token(&result, index);

        let default1: &Option<&FlattenToken> = &None;
        let n1 = neighbors.first().unwrap_or(default1);
        let n2 = neighbors.get(1).unwrap_or(default1);
        let current = neighbors.get(2).unwrap_or(default1);

        if let Some(nv1) = n1
            && let Token::Str(_, str) = nv1.token
            && let Some(nv2) = n2
            && let Token::Colon(_) = nv2.token
            && (current.is_none() || !matches!(current.unwrap().token, Token::Object(_, _)))
        {
            trace!("create_value_completion");
            return create_value_completion(char, lang, str, define_map);
        } else if char.as_ref() == "\""
            && let Some(current_v) = current
            && let Some((parent_token, _)) = current_v.extra
            && ((parent_token != BIND) || !matches!(current_v.token, Token::Object(_, _)))
        {
            trace!("create_type_completion");
            let r = n.get_value().unwrap().get_type();
            if let Some(r) = r {
                return create_type_completion(r, pos, lang, define_map);
            }
        }
    }
    None
}

fn create_value_completion(
    char: Arc<str>,
    lang: Arc<str>,
    property: &str,
    define_map: &HashMap<String, Token>,
) -> Option<Vec<CompletionItem>> {
    let mut result = Vec::new();
    if let Some(v) = define_map.get(property)
        && let Token::Object(_, v) = v
    {
        let property = to_map_ref(v);
        if let Some(Token::Array(_, values)) = property.get("values") {
            for (index, v) in values.iter().enumerate() {
                let v = if let Token::Object(_, v) = v {
                    v
                } else {
                    return None;
                };

                let value = to_map_ref(v);
                let des = value.get("description");
                let insert_text_format = value
                    .get("insert_text_format")
                    .and_then(|k| from_number_to_insert_text_format(to_number_ref(k) as u64));
                if char.as_ref() == "\""
                    && let Some(format) = insert_text_format
                    && format == InsertTextFormat::SNIPPET
                {
                    continue;
                }
                result.push(CompletionItem {
                    label: value
                        .get("label")
                        .map(|v| to_string_ref(v))
                        .unwrap_or("unknown label".to_string()),
                    label_details: Some(CompletionItemLabelDetails {
                        description: des.map_or(Some("jsonui support".to_string()), |d| {
                            let d = if let Token::Object(_, d) = d {
                                to_map_ref(d)
                            } else {
                                HashMap::new()
                            };
                            d.get(&lang.to_string())
                                .or(d.get("en-us")).map(|f| to_string_ref(f))
                        }),
                        detail:      None,
                    }),
                    kind: value
                        .get("kind")
                        .and_then(|k| from_number_to_completion_item_kind(to_number_ref(k) as u64)),
                    insert_text_format,
                    insert_text: value.get("insert_text").or(value.get("label")).map(|k| {
                        let c = char.as_ref();
                        let fill = to_string_ref(k);
                        if c == ":" && insert_text_format.is_some() {
                            format!(" {}", fill)
                        } else if c == ":" {
                            format!(" \"{}\"", fill)
                        } else {
                            fill
                        }
                    }),
                    preselect: Some(true),
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

fn create_type_completion(
    type_n: String,
    pos: Position,
    lang: Arc<str>,
    define_map: &HashMap<String, Token>,
) -> Option<Vec<CompletionItem>> {
    let mut result = Vec::new();

    let mut inputs: Vec<&Token> = vec![];
    if let Some(Token::Array(_, arr)) = define_map.get(&type_n) {
        inputs.extend(arr);
    }
    inputs.extend(to_array_ref(define_map.get("common").unwrap()).unwrap());

    for (index, av) in inputs.into_iter().enumerate() {
        if let Token::Str(_, str) = av {
            if let Some(Token::Object(_, v)) = define_map.get(&str.to_string()) {
                let v = to_map_ref(v);
                let des = v
                    .get("description")
                    .map(|f| match f {
                        Token::Object(_, v) => to_map_ref(v),
                        _ => HashMap::new(),
                    })
                    .unwrap_or_default();
                result.push(CompletionItem {
                    label: str.to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: des
                            .get(lang.as_ref())
                            .or(des.get("en-us")).map(|f| to_string_ref(f)),
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

    #[cfg(test)]
    #[test]
    fn test_find_closest_node() {
        use crate::complete_helper::find_closest_node;

        let p = crate::load_completer();
        let path = PathBuf::from("test");
        p.init(&path);
        let trees = p.trees.read().unwrap();
        let r = trees.get("add_external_server");
        let r = r.unwrap();

        let cr = find_closest_node(r, &r.get_root_node().unwrap(), 1000);
        let cr = cr.expect("cant find closest node");
        let r = r.get_node_by_id(&cr);
        if let Some(v) = r {
            assert_eq!("remove_button", v.get_value().unwrap().define.name.as_ref());
        } else {
            panic!()
        }
    }
}
