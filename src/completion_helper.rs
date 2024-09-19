use crate::completion::{
    CompleteContext, ParsedToken, Value, TYPE_ARR, TYPE_COL, TYPE_COM, TYPE_CR, TYPE_STR,
};
use log::trace;
use std::collections::HashMap;
use tower_lsp::lsp_types::{
    Color, CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionParams,
    InsertTextFormat, Position, Range, TextEdit,
};

const BINDINGS: &str = "bindings";
const CONTROLS: &str = "controls";

fn create_binding_type_input<'a>(
    ast: &'a Vec<Value>,
    define_map: &'a HashMap<String, serde_json::Value>,
    current: &'a Value,
) -> Vec<&'a serde_json::Value> {
    let mut inputs: Vec<&serde_json::Value> = vec![];
    let path = &current.path;
    let binding_type: Option<&Value> = if path.len() > 1
        && let Some(ParsedToken::Array(arr)) = &ast[path[0]].v
        && let Some(ParsedToken::Controls(obj)) = &arr[path[1]].v
    {
        obj.iter()
            .skip_while(|&v| {
                if let Some(ParsedToken::String(s)) = &v.v
                    && s.as_ref() == "binding_type"
                {
                    return false;
                }
                true
            })
            .nth(2)
    } else {
        trace!("cant find binding_type");
        None
    };
    if let Some(bt) = binding_type
        && let Some(ParsedToken::String(bt_n)) = &bt.v
        && let Some(serde_json::Value::Array(arr)) = define_map.get(bt_n.as_ref())
    {
        inputs.extend(&mut arr.iter());
    }
    inputs.extend(
        &mut define_map
            .get("bindings_properties")
            .unwrap()
            .as_array()
            .unwrap()
            .iter(),
    );
    inputs
}

pub(crate) async fn create_completion<'a>(
    lang: &str,
    define_map: &HashMap<String, serde_json::Value>,

    param: &CompletionParams,
    context: &CompleteContext<'a>,
    ast: &Vec<Value>,
) -> Option<Vec<CompletionItem>> {
    trace!(
        "{:?}\n\n AST {:?}\n--------------------------------",
        context,
        ast
    );

    let type_c = context.control_type.lock().await;
    let nodes = context.nodes.lock().await;
    let input_c = context.input_char.lock().await;

    let n1 = nodes[0];
    let n2 = nodes[1];
    let current = nodes[2];

    if let Some(first_ast_v) = ast.first()
        && let Some(ParsedToken::String(control_name)) = &first_ast_v.v
    {
        match control_name.as_ref() {
            BINDINGS => {
                if let Some(nv1) = n1
                    && let Some(ParsedToken::String(pn)) = &nv1.v
                    && let Some(nv2) = n2
                    && nv2.type_id == TYPE_COL
                    && (current.is_none() || current.unwrap().type_id != TYPE_ARR)
                {
                    trace!("create_bindings_value_completion");
                    return create_value_completion(
                        input_c.as_ref(),
                        pn.as_ref(),
                        lang,
                        define_map,
                    );
                } else if let Some(current_v) = current
                    && input_c.as_ref() == "\""
                    && (current_v.path.len() == 3
                        || current_v.type_id == TYPE_ARR
                        || current_v.type_id == TYPE_CR)
                {
                    trace!("create_bindings_type_completion");
                    let inputs = &create_binding_type_input(ast, define_map, current_v);
                    return create_type_completion(inputs, param, lang, define_map);
                }
            }
            _ => {
                if let Some(nv1) = n1
                    && let Some(ParsedToken::String(pn)) = &nv1.v
                    && let Some(nv2) = n2
                    && nv2.type_id == TYPE_COL
                    && (current.is_none() || current.unwrap().type_id != TYPE_CR)
                {
                    trace!("create_value_completion");
                    return create_value_completion(
                        input_c.as_ref(),
                        pn.as_ref(),
                        lang,
                        define_map,
                    );
                } else if input_c.as_ref() == "\""
                    && let Some(current_v) = current
                    && (current_v.path.len() == 2 || current_v.type_id == TYPE_CR)
                {
                    trace!("create_type_completion");
                    let mut inputs: Vec<&serde_json::Value> = vec![];

                    if let Some(c_type) = type_c.as_ref() //fill type property
                        && let Some(serde_json::Value::Array(arr)) = define_map.get(c_type.as_ref())
                    {
                        inputs.extend(arr.iter());
                    }
                    inputs
                        .extend(&mut define_map.get("common").unwrap().as_array().unwrap().iter()); //fill common property
                    return create_type_completion(&inputs, param, lang, define_map);
                }
            }
        }
    }
    None
}

fn create_type_completion(
    inputs: &Vec<&serde_json::Value>,
    param: &CompletionParams,
    lang: &str,
    define_map: &HashMap<String, serde_json::Value>,
) -> Option<Vec<CompletionItem>> {
    let mut result = Vec::new();
    for (index, av) in inputs.iter().enumerate() {
        if let serde_json::Value::String(str) = av {
            if let Some(v) = define_map.get(str) {
                let des = v.get("description").unwrap();
                result.push(CompletionItem {
                    label: str.to_owned(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: des
                            .get(lang)
                            .or(des.get("en-us"))
                            .and_then(|f| f.as_str().map(|ff| ff.to_string())),
                        ..Default::default()
                    }),
                    kind: Some(CompletionItemKind::TEXT),
                    text_edit: Some(tower_lsp::lsp_types::CompletionTextEdit::Edit(TextEdit {
                        range: Range {
                            start: Position {
                                line: param.text_document_position.position.line,
                                character: param.text_document_position.position.character,
                            },
                            end: Position {
                                line: param.text_document_position.position.line,
                                character: param.text_document_position.position.character + 1,
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

fn create_value_completion(
    input_c: &str,
    property: &str,
    lang: &str,
    define_map: &HashMap<String, serde_json::Value>,
) -> Option<Vec<CompletionItem>> {
    let mut result = Vec::new();
    if let Some(v) = define_map.get(property)
        && let Some(serde_json::Value::Array(values)) = v.get("values")
    {
        for (index, i) in values.iter().enumerate() {
            let value = i.as_object().unwrap();
            let des = value.get("description");
            let insert_text_format = value
                .get("insert_text_format")
                .and_then(|k| from_number_to_insert_text_format(k.as_u64().unwrap()));
            if input_c == "\""
                && let Some(format) = insert_text_format
                && format == InsertTextFormat::SNIPPET
            {
                continue;
            }
            result.push(CompletionItem {
                label: value.get("label").unwrap().to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    description: des.map_or(Some("jsonui support".to_string()), |d| {
                        d.get(lang)
                            .or(d.get("en-us"))
                            .and_then(|f| f.as_str().map(|ff| ff.to_string()))
                    }),
                    detail: None,
                }),
                kind: value
                    .get("kind")
                    .and_then(|k| from_number_to_completion_item_kind(k.as_u64().unwrap())),
                insert_text_format,
                insert_text: value.get("insert_text").or(value.get("label")).map(|k| {
                    if input_c == ":" && insert_text_format.is_some() {
                        format!(" {}", k.as_str().unwrap())
                    } else if input_c == ":" {
                        format!(" \"{}\"", k.as_str().unwrap())
                    } else {
                        k.as_str().unwrap().to_string()
                    }
                }),
                preselect: Some(true),
                sort_text: Some(format!("00{}", index + 1)),
                ..Default::default()
            })
        }
    }

    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

pub(crate) fn from_color_value_to_color_arr(v: &Value) -> Option<Color> {
    if let Some(ParsedToken::String(color_str)) = &v.v {
        return match color_str.as_ref() {
            "white" => Some(Color {
                red: 1.0,
                green: 1.0,
                blue: 1.0,
                alpha: 1.0,
            }),
            "silver" => Some(Color {
                red: 0.776,
                green: 0.776,
                blue: 0.776,
                alpha: 1.0,
            }),
            "gray grey" => Some(Color {
                red: 0.333,
                green: 0.333,
                blue: 0.333,
                alpha: 1.0,
            }),
            "black" => Some(Color {
                red: 0.0,
                green: 0.0,
                blue: 0.0,
                alpha: 1.0,
            }),
            "red" => Some(Color {
                red: 1.0,
                green: 0.333,
                blue: 0.333,
                alpha: 1.0,
            }),
            "green" => Some(Color {
                red: 0.333,
                green: 1.0,
                blue: 0.333,
                alpha: 1.0,
            }),
            "yellow" => Some(Color {
                red: 1.0,
                green: 1.0,
                blue: 0.333,
                alpha: 1.0,
            }),
            "brown" => Some(Color {
                red: 0.706,
                green: 0.408,
                blue: 0.302,
                alpha: 1.0,
            }),
            "cyan" => Some(Color {
                red: 0.0,
                green: 0.667,
                blue: 0.667,
                alpha: 1.0,
            }),
            "blue" => Some(Color {
                red: 0.333,
                green: 0.333,
                blue: 1.0,
                alpha: 1.0,
            }),
            "orange" => Some(Color {
                red: 1.0,
                green: 0.667,
                blue: 0.0,
                alpha: 1.0,
            }),
            "purple" => Some(Color {
                red: 1.0,
                green: 0.333,
                blue: 1.0,
                alpha: 1.0,
            }),
            "nil" => Some(Color {
                red: 1.0,
                green: 1.0,
                blue: 1.0,
                alpha: 0.0,
            }),
            _ => None,
        };
    } else if let Some(ParsedToken::Array(color_arr)) = &v.v {
        if color_arr.len() >= 5
            && let Some(ParsedToken::Number(v1)) = color_arr[0].v
            && v1 >= 0.0
            && v1 <= 1.0
            && let Some(ParsedToken::Number(v2)) = color_arr[2].v
            && v2 >= 0.0
            && v2 <= 1.0
            && let Some(ParsedToken::Number(v3)) = color_arr[4].v
            && v3 >= 0.0
            && v3 <= 1.0
        {
            if color_arr.len() == 7
                && let Some(ParsedToken::Number(v4)) = color_arr[6].v
                && v4 >= 0.0
                && v4 <= 1.0
            {
                return Some(Color {
                    red: v1,
                    green: v2,
                    blue: v3,
                    alpha: v4,
                });
            } else {
                return Some(Color {
                    red: v1,
                    green: v2,
                    blue: v3,
                    alpha: 1.0,
                });
            }
        }
    }
    None
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

fn from_number_to_insert_text_format(kind: u64) -> Option<InsertTextFormat> {
    match kind {
        1 => Some(InsertTextFormat::PLAIN_TEXT),
        2 => Some(InsertTextFormat::SNIPPET),
        _ => None,
    }
}
