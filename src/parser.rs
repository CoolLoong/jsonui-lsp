use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use chumsky::prelude::*;
use dashmap::DashMap;
use log::trace;
use tower_lsp::lsp_types::Url;
use crate::tree_ds::prelude::*;
use walkdir::WalkDir;

use crate::chumsky::Token;
use crate::document::Document;
use crate::hash_uri;

fn extract_prefix(input: &str) -> &str {
    match input.find('@') {
        Some(index) => &input[..index],
        None => input,
    }
}

type ChumskyParser<'a> = Boxed<'a, 'a, &'a str, Vec<Token<'a>>, extra::Err<Rich<'a, char>>>;
type AutoTree<T> = Tree<u32, T>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct ControlNode {
    define: ControlDefine,
    loc:    (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct ControlDefine {
    pub(crate) name:      Arc<str>,
    pub(crate) type_n:    Arc<str>,
    pub(crate) variables: std::collections::HashSet<Arc<str>>,
}

pub struct Parser {
    trees:                  DashMap<String, AutoTree<ControlNode>>,
    keyword:                HashSet<String>,
    vanilla_controls_tabel: HashMap<String, ControlDefine>,
}

impl Parser {
    
    pub fn new(
        keyword: HashSet<String>,
        vanilla_controls_tabel: HashMap<String, ControlDefine>,
    ) -> Self {
        Parser {
            trees: DashMap::with_shard_amount(2),
            keyword,
            vanilla_controls_tabel,
        }
    }

    pub(crate) fn init(&self, workspace_folders: &PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
            .filter(|e| !e.path().ends_with("_global_variables.json"))
            .filter(|e| !e.path().ends_with("_ui_defs.json"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                let tokenizer = crate::chumsky::parser::<'_>();
                let r = crate::chumsky::parse(tokenizer, content.as_ref());
                match r {
                    Ok(r) => Self::handle_tokens(self, r),
                    Err(e) => {
                        trace!("error in new parser{:?}", e)
                    }
                }
            }
        }
    }

    fn handle_tokens(parser: &Parser, tokens: Vec<Token<'_>>) {
        let np = Self::find_namespace(&tokens);
        if let Some(np) = np {
            let tr: Tree<u32, ControlNode> = AutoTree::<ControlNode>::new(Option::Some(np.as_str()));
            Self::build_control_tree(&tr, &tokens);
        } else {
            trace!("cant find namespace from tokens {:?}", tokens);
        }
    }

    fn find_namespace(tokens: &Vec<Token<'_>>) -> Option<String> {
        tokens
            .iter()
            .enumerate()
            .find(|(_, token)| matches!(token, Token::Str(_, "namespace")))
            .and_then(|(index, _)| match tokens.get(index + 2) {
                Some(Token::Str(_, s)) => Some(String::from(*s)),
                _ => None,
            })
    }

    fn find_type(tokens: &Vec<Token<'_>>) -> Option<String> {
        tokens
            .iter()
            .enumerate() // 加上索引以便找到 "namespace" 的位置
            .find(|(_, token)| matches!(token, Token::Str(_, "namespace"))) // 找到第一个 "namespace"
            .and_then(|(index, _)| match tokens.get(index + 2) {
                Some(Token::Str(_, s)) => Some(String::from(*s)),
                _ => None,
            })
    }

    fn build_control_tree(tr: &Tree<u32, ControlNode>, tokens: &Vec<Token<'_>>) {
        for i in tokens {
            match i {
                Token::Controls(a, b) => {}
                _ => {}
            }
        }
    }

    pub(crate) async fn input(pos: (usize, usize), doc: &Document) -> String {
        let chars = doc.content_chars.lock().await;
        let (l, r) = pos;
        Self::join(&chars[l..r + 1])
    }

    pub(crate) fn parse(&'a self, input: &'a str) -> Result<Vec<Token<'a>>, Vec<Rich<'a, char>>> {
        crate::chumsky::parse(&self.tokenizer, input)
    }

    fn join(arcs: &[Arc<str>]) -> String {
        arcs.iter().map(|s| s.as_ref()).collect::<Vec<&str>>().join("")
    }

    pub async fn init(&self, workspace_folders: &PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
            .filter(|e| !e.path().ends_with("_global_variables.json"))
            .filter(|e| !e.path().ends_with("_ui_defs.json"))
        {
            let mut tmp_content_cache = HashMap::new();
            if let Ok(content) = fs::read_to_string(entry.path()) {
                if let Ok(Some(serde_json::Value::Object(obj))) =
                    parse_to_serde_value(&content, &ParseOptions::default())
                {
                    if let Some(serde_json::Value::String(namespace)) = obj.get("namespace") {
                        tmp_content_cache.entry(namespace.to_string()).or_insert(obj);
                    }
                }
            }

            let arc: Arc<HashMap<String, serde_json::Map<String, serde_json::Value>>> =
                Arc::new(tmp_content_cache);
            for (k, v) in arc.iter() {
                self.process_workspace_file(Some(&arc), k, None, v).await;
            }
        }

        // for i in doc.iter() {
        //     let pos: Option<Position> = {
        //         let chars = i.content_chars.lock().await;
        //         if let Some(index) = chars.iter().position(|s| s.as_ref() == "{") {
        //             i.get_position_from_index(index + 1).await
        //         } else {
        //             None
        //         }
        //     };
        //     if let Some(pos_v) = pos {
        //         let index = i.get_index_from_position(pos_v).await;
        //         let b: Option<(usize, usize)> = if let Some(index_v) = index {
        //             i.get_boundary_indices(index_v).await
        //         } else {
        //             None
        //         };
        //         if let Some(tuple) = b {
        //             let r = self.parse(tuple, i).await;
        //             if let Some(ref ast) = r {
        //                 self.handle_symbol(i, ast);
        //             }
        //         }
        //     }
        // }
    }

    pub(crate) async fn process_workspace_file_by_url(&self, url: &Url) {
        if let Ok(r) = url.to_file_path() {
            if let Ok(content) = fs::read_to_string(r) {
                if let Ok(Some(serde_json::Value::Object(obj))) =
                    parse_to_serde_value(&content, &ParseOptions::default())
                {
                    if let Some(serde_json::Value::String(namespace)) = obj.get("namespace") {
                        self.insert_namespace(url, Arc::from(namespace.as_str())).await;
                        self.process_workspace_file(None, namespace, None, &obj).await;
                    }
                }
            }
        }
    }

    pub(crate) async fn process_workspace_file(
        &self,
        temp_content_cache: Option<&Arc<HashMap<String, Map<String, serde_json::Value>>>>,
        namespace: &String,
        control_name: Option<&str>,
        root: &serde_json::Map<String, serde_json::Value>,
    ) {
        for (key, value) in root {
            let sp: Vec<&str> = key.split('@').collect();
            let (prefix, suffix) = if sp.len() == 2 {
                (Some(sp[0]), Some(sp[1]))
            } else {
                (Some(key.as_str()), None)
            };

            if let Some(type_value) = value.get("type").and_then(serde_json::Value::as_str) {
                let control_name = control_name.unwrap_or(prefix.unwrap());
                self.insert_control_type(namespace, control_name.to_string(), type_value.to_string())
                    .await;
            } else if let Some(suffix) = suffix {
                let mut parts: Vec<&str> = suffix.split('.').collect();
                if parts.len() == 1 {
                    parts.insert(0, namespace);
                }

                if parts.len() == 2 {
                    let part_namespace = parts[0];
                    let part_name = parts[1];

                    let type_n_option = {
                        self.cache_type_map
                            .get(part_namespace)
                            .and_then(|namespace_object| namespace_object.get(part_name).cloned())
                    };

                    if let Some(type_n) = type_n_option {
                        self.insert_control_type(namespace, part_name.to_string(), type_n)
                            .await;
                    } else if let Some(cache_v) = &temp_content_cache
                        && let Some(namespace_object) = cache_v.get(namespace)
                    {
                        if let Some((_, vv)) = namespace_object.iter().find(|(kk, vv)| {
                            if let serde_json::Value::Object(_) = vv {
                                extract_prefix(kk) == part_name
                            } else {
                                false
                            }
                        }) {
                            if let serde_json::Value::Object(obj) = vv {
                                let next = control_name.or(Some(prefix.unwrap()));
                                Box::pin(self.process_workspace_file(
                                    temp_content_cache,
                                    namespace,
                                    next,
                                    obj,
                                ))
                                .await;
                            }
                        }
                    }
                }
            } else if key == "type" {
                if let serde_json::Value::String(v) = value {
                    let control_name = control_name.unwrap_or(prefix.unwrap());
                    self.insert_control_type(namespace, control_name.to_string(), v.clone())
                        .await;
                }
            }
        }
    }

    pub(crate) async fn insert_namespace(&self, url: &Url, namespace: Arc<str>) {
        let hash_value = hash_uri(url);
        self.id_2_namespace_map.entry(hash_value).or_insert(namespace);
    }

    pub(crate) async fn query_namespace(&self, url: &Url) -> Option<Arc<str>> {
        let hash_value = hash_uri(url);
        self.id_2_namespace_map.get(&hash_value).map(|v| v.to_owned())
    }

    pub(crate) async fn query_type(&self, namespace: Arc<str>, control_n: Arc<str>) -> Option<String> {
        let map = self.cache_type_map.get(namespace.as_ref());
        if let Some(map_v) = map {
            let type_n = map_v.get(control_n.as_ref());
            if let Some(type_name) = type_n {
                return Some(type_name.clone());
            }
        }
        None
    }

    async fn insert_control_type(&self, namespace: &String, control_name: String, type_name: String) {
        let mut value = self
            .cache_type_map
            .entry(namespace.to_string())
            .or_insert(HashMap::new());
        value.insert(control_name, type_name);
    }

    pub(crate) fn close(&self) {
        self.cache_type_map.clear();
        self.id_2_namespace_map.clear();
    }
}
