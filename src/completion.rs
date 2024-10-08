use std::sync::Arc;
use std::vec;

use log::trace;
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{
    ColorInformation, CompletionItem, CompletionParams, DidChangeTextDocumentParams, Position, Range,
    Url,
};

use crate::completion_helper::{create_completion, from_color_value_to_color_arr};
use crate::document::Document;
use crate::parser::{ParsedToken, Value, TYPE_STR};
use crate::Backend;

/// `CompleteContext` is a struct used to describe contextual information at the completion position
/// within vscode.
#[derive(Debug)]
pub(crate) struct CompleteContext<'a> {
    pub(crate) control_type: Mutex<Option<Arc<str>>>,
    pub(crate) l:            Mutex<usize>,
    pub(crate) r:            Mutex<usize>,
    pub(crate) index:        Mutex<usize>,
    pub(crate) input_char:   Mutex<Arc<str>>,
    pub(crate) nodes:        Mutex<Vec<Option<&'a Value>>>,
}
impl<'a> CompleteContext<'a> {
    /// create an empty `CompleteContext`
    pub fn new() -> Self {
        CompleteContext {
            control_type: Mutex::new(None),
            r:            Mutex::new(0),
            l:            Mutex::new(0),
            input_char:   Mutex::new(Arc::from("")),
            index:        Mutex::new(0),
            nodes:        Mutex::new(vec![]),
        }
    }
}

/// The `Completer` struct in Rust contains a `Document` and a Node stack call `ast`
///
/// Properties:
///
/// * `document`: The `document` property holds the source data and can be updated during editing.
/// * `ast`: The `ast` property in the `Completer` struct is a `Mutex` that contains an `Option` of a
/// vector of `Value` elements. This allows for safe concurrent access to the AST (Abstract Syntax Tree)
/// data stored within the `Completer` struct.
#[derive(Debug)]
pub(crate) struct Completer {
    document: Document,
    ast:      Mutex<Option<Vec<Value>>>,
}

impl Completer {
    pub fn new(str: Arc<str>) -> Self {
        Completer {
            document: Document::from(str),
            ast:      Mutex::new(None),
        }
    }

    pub async fn compelte(&self, bk: &Backend, param: &CompletionParams) -> Option<Vec<CompletionItem>> {
        let context: CompleteContext = CompleteContext::new();
        let pos = param.text_document_position.position;

        self.build_context(pos, &context).await;

        let ast = bk
            .parser
            .parse((*context.l.lock().await, *context.r.lock().await), &self.document)
            .await;

        let r: Option<Vec<CompletionItem>> = if let Some(ref ast_v) = ast {
            Self::fill_context(bk, ast_v, &param.text_document_position.text_document.uri, &context)
                .await;
            let lang = bk.lang.lock().await;
            create_completion(lang.as_ref(), &bk.jsonui_define_map, param, &context, ast_v).await
        } else {
            None
        };

        *self.ast.lock().await = ast;
        r
    }

    pub async fn complete_color(&self, bk: &Backend) -> Option<Vec<ColorInformation>> {
        let tuple = {
            let chars = self.document.content_chars.lock().await;
            (0, chars.len() - 1)
        };
        let ast: Option<Vec<Value>> = bk.parser.parse(tuple, &self.document).await;
        if ast.is_none() {
            trace!("cant build full ast!");
            return None;
        }
        let inputs: &Vec<Value> = ast.as_ref().unwrap();
        let mut results = Vec::new();
        Self::flatten_ast(inputs, &mut results);

        let mut color_infos = Vec::new();
        let mut iter = results.iter();
        while let Some(color_v) = iter
            .by_ref()
            .skip_while(|r| !matches!(&r.v, Some(ParsedToken::String(v)) if v.as_ref() == "color"))
            .nth(2)
        {
            if let (Some(left_v), Some(right_v)) = (
                self.document.get_position_from_index(color_v.l).await,
                self.document.get_position_from_index(color_v.r).await,
            ) {
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

    pub async fn build_context<'a>(&self, pos: Position, context: &'a CompleteContext<'a>) {
        // get current index in content
        let index = self.document.get_index_from_position(pos).await;
        let index_value;
        if let Some(index_v) = index {
            *context.index.lock().await = index_v;
            index_value = index_v;
        } else {
            trace!("cant get_index_from_position {:?}", pos);
            *self.ast.lock().await = None;
            return;
        }
        // write the input char currently
        {
            let input_char_index = index_value - 1;
            if let Some(cr) = self.document.get_char(input_char_index).await {
                *context.input_char.lock().await = cr;
            }
        }

        let indices = self.document.get_boundary_indices(index_value).await;
        if let Some((l, r)) = indices {
            *context.l.lock().await = l;
            *context.r.lock().await = r;
        } else {
            *self.ast.lock().await = None;
        }
    }

    async fn fill_context<'a>(
        bk: &Backend,
        ast: &'a Vec<Value>,
        docs_url: &Url,
        context: &'a CompleteContext<'a>,
    ) {
        // fill context from ast
        let mut result = Vec::new();
        Self::flatten_ast(ast, &mut result);
        let v_index = *context.index.lock().await;
        let node_index = result.iter().rposition(|value| value.r <= v_index);

        // find the neighbors node corresponding to the current index
        let mut neighbors: Vec<Option<&'a Value>> = Vec::with_capacity(5);
        if let Some(i) = node_index {
            let i = i as i32;
            for offset in -1..4 {
                let index = i + offset;
                if index < 0 || index >= result.len() as i32 {
                    neighbors.push(None);
                } else {
                    neighbors.push(Some(result[index as usize]));
                }
            }
        }
        *context.nodes.lock().await = neighbors;

        // find control_type by context
        result.clear();
        Self::flatten_ast_one_layer(ast, &mut result);
        for (index, i) in result.iter().enumerate() {
            if i.type_id == TYPE_STR
                && let Some(ParsedToken::String(v)) = &i.v
                && v.as_ref() == "type"
            {
                let type_node_index = index + 2;
                if type_node_index < result.len()
                    && let Some(ParsedToken::String(type_v)) = &result[type_node_index].v
                {
                    *context.control_type.lock().await = Some(type_v.clone());
                }
            }
        }

        let mut control_type_mut = context.control_type.lock().await;
        if control_type_mut.is_none()
            && let Some(ParsedToken::String(control_name)) = &ast[0].v
        {
            let namespace: Option<Arc<str>> = bk.query_namespace(docs_url).await;
            let control_t = Self::fill_control_type(bk, namespace, control_name).await;
            if let Some(v) = control_t {
                *control_type_mut = Some(Arc::from(v.as_str()));
            } else {
                trace!("cant find type from type_cache {:?}", ast[0].v);
            }
        }
    }

    async fn fill_control_type(
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

    pub async fn update_document(&self, request: &DidChangeTextDocumentParams) {
        self.document.apply_change(request).await;
    }

    fn flatten_ast<'a>(input: &'a [Value], result: &mut Vec<&'a Value>) {
        for i in input {
            if let Some(v) = &i.v {
                result.push(i);
                match v {
                    ParsedToken::Array(sub) | ParsedToken::Controls(sub) => {
                        Self::flatten_ast(sub, result);
                    }
                    _ => {}
                }
            }
        }
    }

    fn flatten_ast_one_layer<'a>(input: &'a [Value], result: &mut Vec<&'a Value>) {
        for i in input {
            if let Some(v) = &i.v {
                result.push(i);
                if let ParsedToken::Array(sub) | ParsedToken::Controls(sub) = v {
                    for sub_value in sub {
                        result.push(sub_value);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use tower_lsp::lsp_types::Position;

    use super::*;
    use crate::parser::Parser;

    const EXAMPLE1: &str = include_str!("../test/achievement_screen.json");

    #[tokio::test]
    async fn test_get_content_index() {
        let completer = Completer::new(EXAMPLE1.into());
        let index = completer
            .document
            .get_index_from_position(Position {
                line:      16,
                character: 5,
            })
            .await;
        let re_warp = completer.document.get_char(index.unwrap()).await;
        let result = re_warp.unwrap();
        assert_eq!(result.as_ref(), "t");

        let result = completer
            .document
            .get_index_from_position(Position {
                line:      44,
                character: 1,
            })
            .await;
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_build_ast() {
        let completer = Completer::new(EXAMPLE1.into());
        let context = &CompleteContext::new();
        let parser = Parser::new(HashSet::new());

        completer
            .build_context(
                Position {
                    line:      16,
                    character: 5,
                },
                context,
            )
            .await;
        {
            let ast = parser
                .parse((*context.l.lock().await, *context.r.lock().await), &completer.document)
                .await;
            assert!(ast.is_some());
        }

        let completer = Completer::new(EXAMPLE1.into());
        let context = &CompleteContext::new();
        completer
            .build_context(
                Position {
                    line:      0,
                    character: 1,
                },
                context,
            )
            .await;
        let ast = parser
            .parse((*context.l.lock().await, *context.r.lock().await), &completer.document)
            .await;
        assert!(ast.is_none());
    }
}
