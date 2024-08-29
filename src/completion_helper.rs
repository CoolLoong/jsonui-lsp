use crate::completion::{CompleteContext, Node, Value, TYPE_ARR, TYPE_COL, TYPE_CR};
use log::trace;
use std::collections::HashMap;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionParams,
    InsertTextFormat, Position, Range, TextEdit,
};

fn create_binding_type_input(
    ast: &Vec<Value>,
    define_map: &HashMap<String, serde_json::Value>,
    current: &Value,
) -> Vec<serde_json::Value> {
    let mut inputs: Vec<serde_json::Value> = vec![];
    let path = &current.path;
    let binding_type: Option<&Value> = if path.len()>1 && let Some(Node::Array(arr)) = &ast[path[0]].v
        && let Some(Node::Controls(obj)) = &arr[path[1]].v
    {
        obj.iter()
            .skip_while(|&v| {
                if let Some(Node::String(s)) = &v.v
                    && s.as_ref() == "binding_type"
                {
                    return false;
                }
                true
            })
            .nth(2)
    } else {
        None
    };
    trace!("binding_type {:?}", binding_type);
    if let Some(bt) = binding_type
        && let Some(Node::String(bt_n)) = &bt.v
        && let Some(serde_json::Value::Array(arr)) = define_map.get(bt_n.as_ref())
    {
        inputs.append(&mut arr.clone());
    }
    inputs.append(
        &mut define_map
            .get("bindings_properties")
            .unwrap()
            .as_array()
            .unwrap()
            .clone(),
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
    trace!("{:?}\n\n ast {:?}", context, ast);
    let type_c = context.control_type.lock().await;
    let nodes = context.nodes.lock().await;
    let input_c = context.input_char.lock().await;

    let n1 = nodes[0];
    let n2 = nodes[1];
    let current = nodes[2];
    if let Some(first_ast_v) = ast.first()
        && let Some(Node::String(if_bindings)) = &first_ast_v.v
        && if_bindings.as_ref() == "bindings"
    {
        if let Some(nv1) = n1
            && let Some(Node::String(pn)) = &nv1.v
            && let Some(nv2) = n2
            && nv2.type_id == TYPE_COL
            && (current.is_none() || current.unwrap().type_id != TYPE_ARR)
        {
            trace!("create_bindings_value_completion");
            return create_value_completion(input_c.as_ref(), pn.as_ref(), lang, define_map);
        } else if input_c.as_ref() == "\""
            && let Some(current_v) = current
            && (current_v.path.len() == 3 || current_v.type_id == TYPE_ARR)
        {
            trace!("create_bindings_type_completion");
            let inputs = &create_binding_type_input(ast,define_map,current_v);
            return create_type_completion(inputs, param, lang, define_map);
        }
    } else if let Some(nv1) = n1
        && let Some(Node::String(pn)) = &nv1.v
        && let Some(nv2) = n2
        && nv2.type_id == TYPE_COL
        && (current.is_none() || current.unwrap().type_id != TYPE_CR)
    {
        trace!("create_value_completion");
        return create_value_completion(input_c.as_ref(), pn.as_ref(), lang, define_map);
    } else if input_c.as_ref() == "\""
        && let Some(current_v) = current
        && (current_v.path.len() == 2 || current_v.type_id == TYPE_CR)
    {
        trace!("create_type_completion");
        let mut inputs: Vec<serde_json::Value> = vec![];
        if let Some(c_type) = type_c.as_ref()//fill type property
            && let Some(serde_json::Value::Array(arr)) = define_map.get(c_type.as_ref())
        {
            inputs.append(&mut arr.clone());
        }
        inputs.append(
            //fill common property
            &mut define_map
                .get("common")
                .unwrap()
                .as_array()
                .unwrap()
                .clone(),
        );
        return create_type_completion(&inputs, param, lang, define_map);
    }
    None
}

fn create_type_completion(
    inputs: &Vec<serde_json::Value>,
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
                            .or(des.get("en-us")).and_then(|f| f.as_str().map(|ff| ff.to_string())),
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
            let insert_text_format = value.get("insert_text_format").and_then(|k| {
                from_number_to_insert_text_format(k.as_u64().unwrap())
            });
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
                            .or(d.get("en-us")).and_then(|f| f.as_str().map(|ff| ff.to_string()))
                    }),
                    detail: None,
                }),
                kind: value.get("kind").and_then(|k| {
                    from_number_to_completion_item_kind(k.as_u64().unwrap())
                }),
                insert_text_format,
                insert_text: value
                    .get("insert_text")
                    .or(value.get("label")).map(|k| {
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
