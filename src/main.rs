use completion::Completer;
use log::{debug, set_max_level};
use serde_json::{Map, Value};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod completion;

const VANILLAPACK_DEFINE: &[u8] = include_bytes!("../out/vanillapack_define_1.21.20.3.json");

#[derive(Debug)]
struct Backend {
    client: Client,
    completers: Arc<Mutex<HashMap<u64, Completer>>>,
    vanilla_type_map: Arc<Map<String, Value>>,
    cache_type_map: Arc<Mutex<HashMap<String, Value>>>,
    id_2_namespace_map: Arc<Mutex<HashMap<u64, Arc<str>>>>,
}

fn get_namespace(s: &Arc<str>) -> Option<String> {
    const NAMESPACE: &str = "namespace\"";
    let mut crs = s.chars().peekable();

    while let Some(ch) = crs.next() {
        if ch == '"' {
            if NAMESPACE.chars().all(|c| Some(c) == crs.next()) {
                let mut skip_w = crs.skip_while(|&c| c != '"');
                skip_w.next();
                let result: String = skip_w.take_while(|&c| c != '"').collect();
                return Some(result);
            }
        }
    }
    None
}

#[inline]
fn hash_uri(url: &Url) -> u64 {
    let mut hasher = DefaultHasher::new();
    url.hash(&mut hasher);
    hasher.finish()
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        //like {"settings":{"log":{"level":"off"}}}
        if let Some(v) = params.settings.get("log").unwrap().get("level") {
            self.client.log_message(MessageType::INFO, format!("set config level is {}",v)).await;
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
        if let Some(v) = init_config.get("settings").unwrap().get("log").unwrap().get("level") {
            self.client.log_message(MessageType::INFO, format!("init config level is {}",v)).await;
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


        Ok(InitializeResult {
            server_info: Some(ServerInfo{
                name: "jsonui support".to_string(),
                version: None
            }),
            offset_encoding: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["\"".to_owned(), ":".to_owned()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                signature_help_provider: None,
                hover_provider: None,
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // trigger in file open
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if params.text_document.language_id != "json" {
            return;
        }
        debug!("open new file");

        let content = params.text_document.text.as_str();
        let arc_content: Arc<str> = Arc::from(content);

        let url = &params.text_document.uri;
        let hash_value = hash_uri(url);
        
        let namespace_op = get_namespace(&arc_content);
        if let Some(v) = namespace_op {
            let mut idmap = self.id_2_namespace_map.lock().await;
            idmap.entry(hash_value).or_insert(Arc::from(v));
        }

        let mut cmp_map = self.completers.lock().await;
        let new_cmp = Completer::from(arc_content);
        cmp_map.entry(hash_value).or_insert(new_cmp);
    }

    // trigger in file change
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {}

    // trigger in file save
    async fn did_save(&self, params: DidSaveTextDocumentParams) {}

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        debug!("close a file");

        let url = &params.text_document.uri;
        let hash_value = hash_uri(url);

        let mut idmap = __self.id_2_namespace_map.lock().await;
        idmap.remove(&hash_value);

        let mut cmp_map = self.completers.lock().await;
        cmp_map.remove(&hash_value);
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        Ok(None)
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let cmp = self.completers.lock().await;
        let url = &params.text_document_position.text_document.uri;
        let hash_value = hash_uri(url);

        let cmp_v = cmp.get(&hash_value);
        if let Some(vv) = cmp_v {
            vv.compelte(&self, &params);
        }
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        Ok(None)
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        Ok(None)
    }
}

impl Backend {
    fn file_not_find_error(&self, path: &str) -> Error {
        let e: ErrorCode = ErrorCode::ServerError(-35000);
        Error {
            code: e,
            message: Cow::Owned(format!("cant find file from {}", path)),
            data: None,
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    env_logger::init();

    let json_value: Value = serde_json::from_slice(VANILLAPACK_DEFINE).unwrap();
    let vanilla_type_map = Arc::new(json_value.as_object().unwrap().to_owned());

    let (service, socket) = LspService::build(|client| Backend {
        client,
        completers: Arc::new(Mutex::new(HashMap::new())),
        vanilla_type_map,
        cache_type_map: Arc::new(Mutex::new(HashMap::new())),
        id_2_namespace_map: Arc::new(Mutex::new(HashMap::new())),
    })
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_namespace_esay() {
        let result = get_namespace(&r#"{"namespace": "test"}"#.into()).unwrap();
        let expect = "test";
        assert_eq!(result, expect);
    }

    #[test]
    fn test_get_namespace_hard() {
        let result = get_namespace(&r#"// this is comment{"test_control": {"type": "panel"},"namespace": "test"}"#.into()).unwrap();
        let expect = "test";
        assert_eq!(result, expect);
    }
}
