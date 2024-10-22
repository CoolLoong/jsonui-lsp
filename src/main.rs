#![feature(let_chains)]

mod completion;
mod completion_helper;
mod document;
mod parser;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use completion::Completer;
use dashmap::DashMap;
use flexi_logger::{LogSpecification, Logger, LoggerHandle};
use jsonc_parser::{parse_to_serde_value, ParseOptions};
use log::{set_max_level, trace};
use parser::Parser;
use serde_json::{json, Map, Value};
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use walkdir::WalkDir;

const VANILLAPACK_DEFINE: &str = include_str!("../resources/vanillapack_define_1.21.40.3.json");
const JSONUI_DEFINE: &str = include_str!("../resources/jsonui_define.json");
const SEED: u64 = 32;

struct Backend {
    client:             Client,
    log:                Arc<LoggerHandle>,
    completers:         DashMap<u64, Completer>,
    /// cache namespace -> control_name -> control_type
    cache_type_map:     DashMap<String, HashMap<String, String>>,
    jsonui_define_map:  HashMap<String, Value>,
    /// cache url -> namespace
    id_2_namespace_map: DashMap<u64, Arc<str>>,
    lang:               Mutex<Arc<str>>,
    parser:             Parser,
}

fn extract_prefix(input: &str) -> &str {
    match input.find('@') {
        Some(index) => &input[..index],
        None => input,
    }
}

fn get_namespace(s: Arc<str>) -> Option<String> {
    const NAMESPACE: &str = "namespace\"";
    let mut crs = s.chars().peekable();

    while let Some(ch) = crs.next() {
        if ch == '"' && NAMESPACE.chars().all(|c| Some(c) == crs.next()) {
            let mut skip_w = crs.skip_while(|&c| c != '"');
            skip_w.next();
            let result: String = skip_w.take_while(|&c| c != '"').collect();
            return Some(result);
        }
    }
    None
}

#[inline]
fn hash_uri(url: &Url) -> u64 {
    museair::bfast::hash(url.path().as_bytes(), SEED)
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        if let Some(v) = params.settings.get("log").unwrap().get("level") {
            self.client
                .log_message(MessageType::INFO, format!("set config level is {}", v))
                .await;
            match v.as_str().unwrap() {
                "off" => {
                    set_max_level(log::LevelFilter::Off);
                }
                "messages" => {
                    set_max_level(log::LevelFilter::Error);
                }
                "verbose" => {
                    set_max_level(log::LevelFilter::Trace);
                }
                _ => {}
            }
        }
    }

    async fn initialize(&self, param: InitializeParams) -> Result<InitializeResult> {
        let init_config = &param.initialization_options.unwrap();
        if let Some(v) = init_config
            .get("settings")
            .unwrap()
            .get("log")
            .unwrap()
            .get("level")
        {
            self.client
                .log_message(MessageType::INFO, format!("init config level is {}", v))
                .await;
            match v.as_str().unwrap() {
                "off" => {
                    self.log.set_new_spec(LogSpecification::off());
                }
                "messages" => {
                    self.log.set_new_spec(LogSpecification::error());
                }
                "verbose" => {
                    self.log.set_new_spec(LogSpecification::trace());
                }
                _ => {}
            }
        }
        let client_lang = init_config.get("locale").unwrap();
        trace!("client lang is {}", json!(client_lang));
        *self.lang.lock().await = Arc::from(client_lang.as_str().unwrap());

        if let Some(root_url) = param.root_uri
            && let Ok(workspace) = root_url.to_file_path()
        {
            self.init_workspace(workspace).await;
        }

        let file_operation_filters = vec![FileOperationFilter {
            scheme:  Some("file".to_string()),
            pattern: FileOperationPattern {
                glob:    "**/*.json".to_string(),
                matches: None,
                options: None,
            },
        }];
        let registration_options = FileOperationRegistrationOptions {
            filters: file_operation_filters,
        };
        Ok(InitializeResult {
            server_info:     Some(ServerInfo {
                name:    "jsonui support".to_string(),
                version: None,
            }),
            offset_encoding: None,
            capabilities:    ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider:           Some(false),
                    trigger_characters:         Some(vec!["\"".to_string(), ":".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters:      None,
                    completion_item:            Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                }),
                definition_provider: None,
                references_provider: None,
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported:            Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations:   Some(WorkspaceFileOperationsServerCapabilities {
                        did_create:  Some(registration_options.clone()),
                        will_create: None,
                        did_rename:  Some(registration_options.clone()),
                        will_rename: None,
                        did_delete:  Some(registration_options.clone()),
                        will_delete: None,
                    }),
                }),
                color_provider: Some(ColorProviderCapability::Options(
                    StaticTextDocumentColorProviderOptions {
                        document_selector: Some(vec![DocumentFilter {
                            language: Some("json".to_string()),
                            scheme:   None,
                            pattern:  None,
                        }]),
                        id:                None,
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn document_color(&self, params: DocumentColorParams) -> Result<Vec<ColorInformation>> {
        let url = &params.text_document.uri;
        let hash_value = hash_uri(url);

        let cmp_v = self.completers.get(&hash_value);
        if let Some(vv) = cmp_v {
            if let Some(result) = vv.complete_color(self).await {
                return Ok(result);
            }
        }
        Ok(vec![])
    }

    async fn color_presentation(
        &self,
        params: ColorPresentationParams,
    ) -> Result<Vec<ColorPresentation>> {
        let ColorPresentationParams { color, range, .. } = params;
        let color_presentation = ColorPresentation {
            label:                 format!(
                "rgba({:.3}, {:.3}, {:.3}, {:.3})",
                color.red, color.green, color.blue, color.alpha
            ),
            text_edit:             Some(TextEdit {
                range,
                new_text: format!(
                    "[{:.3}, {:.3}, {:.3}, {:.3}]",
                    color.red, color.green, color.blue, color.alpha
                ),
            }),
            additional_text_edits: None,
        };

        Ok(vec![color_presentation])
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.cache_type_map.clear();
        self.completers.clear();
        self.id_2_namespace_map.clear();
        Ok(())
    }

    // trigger in file change
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let url = &params.text_document.uri;
        let key = hash_uri(url);
        if let Some(cmp) = self.completers.get(&key) {
            cmp.update_document(&params).await;
        }
    }

    /// do insert namespace and cache_type map for save file
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.process_workspace_file_by_url(&params.text_document.uri)
            .await;
    }

    /// do insert namespace and cache_type map for create file
    async fn did_create_files(&self, params: CreateFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                self.process_workspace_file_by_url(&url).await;
            }
        }
    }

    /// do udpate namespace map for rename file
    async fn did_rename_files(&self, params: RenameFilesParams) {
        let idmap = &self.id_2_namespace_map;
        for i in params.files.iter() {
            if let Ok(o_url) = Url::parse(&i.old_uri)
                && let Ok(n_url) = Url::parse(&i.new_uri)
            {
                let ho = hash_uri(&o_url);
                let hn = hash_uri(&n_url);
                if let Some((_, v)) = idmap.remove(&ho) {
                    idmap.insert(hn, v);
                }
            }
        }
    }

    /// do clean namespace and cache_type map for delete file
    async fn did_delete_files(&self, params: DeleteFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                let x = hash_uri(&url);
                if let Some((_, v)) = self.id_2_namespace_map.remove(&x) {
                    self.cache_type_map.remove(v.as_ref());
                }
            }
        }
    }

    /// do insert namespace and completers for open file
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if params.text_document.language_id != "json" {
            return;
        }
        trace!("open {}", json!(params.text_document.uri));
        let content: String = params.text_document.text.as_str().chars().collect();
        let arc_content: Arc<str> = Arc::from(content);

        let url = &params.text_document.uri;
        if let Ok(()) = self.insert_namespace_by_content(url, arc_content.clone()).await {
            let hash_value = hash_uri(url);
            let new_cmp = Completer::new(arc_content);
            self.completers.entry(hash_value).or_insert(new_cmp);
        }
    }

    /// do clean completer for close file
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let url = &params.text_document.uri;
        trace!("close {}", json!(url));
        let hash_value = hash_uri(url);
        self.completers.remove(&hash_value);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let url = &params.text_document_position.text_document.uri;
        let hash_value = hash_uri(url);

        let cmp_v = self.completers.get(&hash_value);
        if let Some(vv) = cmp_v {
            if let Some(result) = vv.compelte(self, &params).await {
                return Ok(Some(CompletionResponse::Array(result)));
            }
        }
        Ok(None)
    }
}

impl Backend {
    fn namespace_not_find_error(&self, path: &str) -> Error {
        let e: ErrorCode = ErrorCode::ServerError(-35001);
        Error {
            code:    e,
            message: Cow::Owned(format!("cant find namespace from {}", path)),
            data:    None,
        }
    }

    async fn init_workspace(&self, workspace_folders: PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
            .filter(|e| !e.path().ends_with("_global_variables.json"))
            .filter(|e| !e.path().ends_with("_ui_defs.json"))
        {
            let mut tmp_content_cache = HashMap::new();
            if let Ok(content) = fs::read_to_string(entry.path()) {
                if let Ok(Some(Value::Object(obj))) =
                    parse_to_serde_value(&content, &ParseOptions::default())
                {
                    if let Some(Value::String(namespace)) = obj.get("namespace") {
                        tmp_content_cache.entry(namespace.to_string()).or_insert(obj);
                    }
                }
            }

            let arc: Arc<HashMap<String, Map<String, Value>>> = Arc::new(tmp_content_cache);
            for (k, v) in arc.iter() {
                self.process_workspace_file(Some(&arc), k, None, v).await;
            }
        }
    }

    async fn insert_namespace_by_content(&self, url: &Url, content: Arc<str>) -> Result<()> {
        let hash_value = hash_uri(url);

        let namespace_op = get_namespace(content);
        if let Some(v) = namespace_op {
            self.id_2_namespace_map.entry(hash_value).or_insert(Arc::from(v));
            return Ok(());
        }
        Err(Self::namespace_not_find_error(self, url.path()))
    }

    async fn insert_namespace(&self, url: &Url, namespace: Arc<str>) {
        let hash_value = hash_uri(url);
        self.id_2_namespace_map.entry(hash_value).or_insert(namespace);
    }

    async fn query_namespace(&self, url: &Url) -> Option<Arc<str>> {
        let hash_value = hash_uri(url);
        self.id_2_namespace_map.get(&hash_value).map(|v| v.to_owned())
    }

    async fn query_type(&self, namespace: Arc<str>, control_n: Arc<str>) -> Option<String> {
        let map = self.cache_type_map.get(namespace.as_ref());
        if let Some(map_v) = map {
            let type_n = map_v.get(control_n.as_ref());
            if let Some(type_name) = type_n {
                return Some(type_name.clone());
            }
        }
        None
    }

    async fn insert_control_type<'a>(
        &self,
        namespace: &String,
        control_name: String,
        type_name: String,
    ) {
        let mut value = self.cache_type_map.entry(namespace.to_string()).or_insert(HashMap::new());
        value.insert(control_name, type_name);
    }

    async fn process_workspace_file_by_url(&self, url: &Url) {
        if let Ok(r) = url.to_file_path() {
            if let Ok(content) = fs::read_to_string(r) {
                if let Ok(Some(Value::Object(obj))) =
                    parse_to_serde_value(&content, &ParseOptions::default())
                {
                    if let Some(Value::String(namespace)) = obj.get("namespace") {
                        self.insert_namespace(url, Arc::from(namespace.as_str())).await;
                        self.process_workspace_file(None, namespace, None, &obj).await;
                    }
                }
            }
        }
    }

    async fn process_workspace_file(
        &self,
        temp_content_cache: Option<&Arc<HashMap<String, Map<String, Value>>>>,
        namespace: &String,
        control_name: Option<&str>,
        root: &Map<String, Value>,
    ) {
        for (key, value) in root {
            let sp: Vec<&str> = key.split('@').collect();
            let (prefix, suffix) = if sp.len() == 2 {
                (Some(sp[0]), Some(sp[1]))
            } else {
                (Some(key.as_str()), None)
            };

            if let Some(type_value) = value.get("type").and_then(Value::as_str) {
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
                            if let Value::Object(_) = vv {
                                extract_prefix(kk) == part_name
                            } else {
                                false
                            }
                        }) {
                            if let Value::Object(obj) = vv {
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
                if let Value::String(v) = value {
                    let control_name = control_name.unwrap_or(prefix.unwrap());
                    self.insert_control_type(namespace, control_name.to_string(), v.clone())
                        .await;
                }
            }
        }
    }
}

fn extract_keyword_from_json(
    keys: &[&str],
    json_map: &HashMap<String, Value>,
) -> HashSet<String> {
    let mut keyword = HashSet::new();
    for key in keys {
        if let Some(value) = json_map.get(*key) {
            if let Some(array) = value.as_array() {
                for item in array {
                    if let Some(string_value) = item.as_str() {
                        keyword.insert(string_value.to_string());
                    }
                }
            }
        }
    }
    keyword
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let log: flexi_logger::LoggerHandle = Logger::with(LogSpecification::info()).start().unwrap();

    let json_value: Value = serde_json::from_str(VANILLAPACK_DEFINE).unwrap();
    
    let cache_type_map = DashMap::with_shard_amount(2);
    if let Some(json_object) = json_value.as_object() {
        for (key, value) in json_object {
            let inner_map: HashMap<String, String> = value
                .as_object()
                .unwrap() // Handle None similarly for nested map
                .into_iter()
                .map(|(inner_key, inner_value)| {
                    (
                        inner_key.to_string(),
                        inner_value.as_str().unwrap_or_default().to_string(),
                    )
                })
                .collect();
            cache_type_map.insert(key.to_string(), inner_map);
        }
    }

    let jsonui_define: Value = serde_json::from_str(JSONUI_DEFINE).unwrap();
    let obj = jsonui_define.as_object().ok_or("Expected a JSON object").unwrap();
    let mut jsonui_define_map: HashMap<String, Value> = HashMap::new();
    for (key, value) in obj {
        jsonui_define_map.insert(key.to_string(), value.clone());
    }

    let keys = [
        "common",
        "bindings_properties",
        "none",
        "global",
        "collection",
        "collection_details",
        "view",
        "label",
        "image",
        "stack_panel",
        "input_panel",
        "collection_panel",
        "button",
        "toggle",
        "dropdown",
        "slider",
        "slider_box",
        "edit_box",
        "grid",
        "scroll_view",
        "selection_wheel",
        "screen",
        "custom",
    ];
    let keyword = extract_keyword_from_json(&keys, &jsonui_define_map);

    let (service, socket) = LspService::build(|client| Backend {
        client,
        log: Arc::new(log),
        completers:  DashMap::with_shard_amount(2),
        cache_type_map,
        jsonui_define_map,
        id_2_namespace_map: DashMap::with_shard_amount(2),
        lang: Mutex::new(Arc::from("zh-cn")),
        parser: Parser::new(keyword),
    })
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_namespace_esay() {
        let result = get_namespace(Arc::from(r#"{"namespace": "test"}"#)).unwrap();
        let expect = "test";
        assert_eq!(result, expect);
    }

    #[test]
    fn test_get_namespace_hard() {
        let result = get_namespace(Arc::from(
            r#"// this is comment{"test_control": {"type": "panel"},"namespace": "test"}"#,
        ))
        .unwrap();
        let expect = "test";
        assert_eq!(result, expect);
    }
}
