#![feature(let_chains)]

mod completer;
mod file_queue;
mod museair;
mod parser;
mod stringpool;
mod utils;

pub(crate) mod towerlsp {
    pub(crate) use tower_lsp::lsp_types::*;
    pub(crate) use tower_lsp::{async_trait, Client, LanguageServer, LspService, Server};
}

use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;
use std::time::Duration;

use completer::Completer;
use file_queue::{CloseFileRequest, CloseFileRequestQueue, OpenFileRequest, OpenFileRequestQueue};
use flexi_logger::{LogSpecification, Logger, LoggerHandle};
use log::{info, trace};
use parser::{DocumentParser, Value};
use stringpool::StringPool;
use tokio::sync::Mutex as TokioMutex;
use towerlsp::*;
use walkdir::WalkDir;

use crate::completer::VanillaControlDefine;
use crate::museair::BfastHashMap;

const VANILLA_PACK_DEFINE: &str = include_str!("resources/vanillapack_define_1.21.80.3.json");
const JSONUI_DEFINE: &str = include_str!("resources/jsonui_define.json");
#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

pub(crate) struct Config {
    log:           Arc<LoggerHandle>,
    lang:          Arc<str>,
    append_suffix: bool,
}

enum GotoDefSequence {
    Normal            = 0,
    ExpectFirstOpen   = 1,
    ExpectSecondClose = 2,
}

impl GotoDefSequence {
    fn from_u8(value: u8) -> Self {
        match value {
            1 => Self::ExpectFirstOpen,
            2 => Self::ExpectSecondClose,
            _ => Self::Normal,
        }
    }
}

struct Backend {
    client:           Client,
    config:           Arc<TokioMutex<Config>>,
    completer:        Arc<Completer>,
    open_queue:       Arc<OpenFileRequestQueue>,
    close_queue:      Arc<CloseFileRequestQueue>,
    ignore_semaphore: AtomicU8,
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, param: InitializeParams) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        let init_config = param
            .initialization_options
            .expect("initialization options cant be empty!");
        let config = self.config.try_lock();
        if let Ok(mut config) = config {
            if let Some(level) = init_config
                .get("settings")
                .and_then(|s| s.get("log"))
                .and_then(|l| l.get("level"))
                .and_then(|v| v.as_str())
            {
                self.client
                    .log_message(MessageType::INFO, format!("init config level is {}", level))
                    .await;

                match level {
                    "off" => config.log.set_new_spec(LogSpecification::off()),
                    "messages" => config.log.set_new_spec(LogSpecification::error()),
                    "verbose" => config.log.set_new_spec(LogSpecification::trace()),
                    _ => (),
                }
            }
            if let Some(append) = init_config
                .get("settings")
                .and_then(|s| s.get("options"))
                .and_then(|o| o.get("auto_append_suffix"))
                .and_then(|v| v.as_bool())
            {
                config.append_suffix = append;
            }
            if let Some(lang) = init_config.get("locale").and_then(|l| l.as_str()) {
                trace!("client lang is {:?}", lang);
                config.lang = Arc::from(lang);
            }
        }

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
            server_info:  Some(ServerInfo {
                name:    "jsonui support".to_string(),
                version: None,
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::INCREMENTAL),
                    save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        include_text: Some(true),
                    })),
                    ..Default::default()
                })),
                completion_provider: Some(CompletionOptions {
                    resolve_provider:           Some(false),
                    trigger_characters:         Some(vec!["\"".to_string(), ":".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters:      None,
                    completion_item:            Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
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

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
        self.start_file_processor().await;
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        trace!("jsonui-lsp shutdown");
        Ok(())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let r = self.completer.goto_definition(params).await;
        if let Some((r, is_current_file)) = r {
            if !is_current_file {
                self.ignore_semaphore
                    .store(GotoDefSequence::ExpectFirstOpen as u8, Ordering::SeqCst);
            }
            Ok(Some(r))
        } else {
            Ok(None)
        }
    }

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> tower_lsp::jsonrpc::Result<Option<Vec<Location>>> {
        let r = self.completer.references(&params).await;
        Ok(r)
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<CompletionResponse>> {
        let url = params.text_document_position.text_document.uri.clone();
        let r = self.completer.complete(url, self.config.clone(), &params).await;
        if let Some(r) = r {
            Ok(Some(CompletionResponse::Array(r)))
        } else {
            Ok(None)
        }
    }

    async fn document_color(
        &self,
        params: DocumentColorParams,
    ) -> tower_lsp::jsonrpc::Result<Vec<ColorInformation>> {
        let url = params.text_document.uri;
        let r = self.completer.complete_color(url);
        if let Some(r) = r {
            Ok(r)
        } else {
            Ok(vec![])
        }
    }

    async fn color_presentation(
        &self,
        params: ColorPresentationParams,
    ) -> tower_lsp::jsonrpc::Result<Vec<ColorPresentation>> {
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

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let config = self.config.try_lock().ok();
        if let Some(mut config) = config {
            if let Some(level) = params
                .settings
                .get("log")
                .and_then(|l| l.get("level"))
                .and_then(|v| v.as_str())
            {
                self.client
                    .log_message(MessageType::INFO, format!("init config level is {}", level))
                    .await;

                match level {
                    "off" => config.log.set_new_spec(LogSpecification::off()),
                    "messages" => config.log.set_new_spec(LogSpecification::error()),
                    "verbose" => config.log.set_new_spec(LogSpecification::trace()),
                    _ => (),
                }
            }
            if let Some(append) = params
                .settings
                .get("options")
                .and_then(|o| o.get("auto_append_suffix"))
                .and_then(|v| v.as_bool())
            {
                config.append_suffix = append;
            }
            if let Some(lang) = params.settings.get("locale").and_then(|l| l.as_str()) {
                config.lang = Arc::from(lang);
            }
        }
    }

    async fn did_create_files(&self, params: CreateFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                if let Ok(content) = tokio::fs::read_to_string(url.path()).await {
                    self.open_queue
                        .enqueue(OpenFileRequest::Content((url, content)))
                        .await;
                }
            }
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let url = &params.text_document.uri;
        self.completer.did_change(url.clone(), &params).await;
    }

    async fn did_rename_files(&self, params: RenameFilesParams) {
        for i in params.files.iter() {
            if let Ok(o_url) = Url::parse(&i.old_uri)
                && let Ok(n_url) = Url::parse(&i.new_uri)
            {
                self.close_queue.remove_close_request(&o_url).await;
                self.close_queue.remove_close_request(&n_url).await;
                self.completer.did_rename(o_url, n_url).await;
            }
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let params = params.text_document;
        if params.language_id != "json" {
            return;
        }
        let current_state = GotoDefSequence::from_u8(self.ignore_semaphore.load(Ordering::SeqCst));
        match current_state {
            GotoDefSequence::ExpectFirstOpen => {
                self.ignore_semaphore
                    .store(GotoDefSequence::ExpectSecondClose as u8, Ordering::SeqCst);
                trace!("Ignored first open file request after goto_definition!");
                return;
            }
            _ => {
                trace!("Request open file, url({})", params.uri);
                self.close_queue.remove_close_request(&params.uri).await;
                self.open_queue
                    .enqueue(OpenFileRequest::Content((params.uri, params.text)))
                    .await;
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let current_state = GotoDefSequence::from_u8(self.ignore_semaphore.load(Ordering::SeqCst));
        match current_state {
            GotoDefSequence::ExpectSecondClose => {
                self.ignore_semaphore
                    .store(GotoDefSequence::Normal as u8, Ordering::SeqCst);
                trace!("Ignored second close file request after goto_definition!");
                return;
            }
            _ => {
                trace!("Request close file, url({})", params.text_document.uri);
                self.close_queue
                    .enqueue(CloseFileRequest(params.text_document.uri, tokio::time::Instant::now()))
                    .await
            }
        }
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                self.completer.did_close(&url);
            }
        }
    }
}

impl Backend {
    async fn init_workspace(&self, workspace_folders: PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
        {
            let abs_path = tokio::fs::canonicalize(&entry.path())
                .await
                .expect("Failed to get absolute path");
            let url = Url::from_file_path(abs_path).expect("Failed to convert path to URL");

            self.open_queue
                .enqueue(OpenFileRequest::Path((url, entry.into_path())))
                .await;
        }
    }

    pub async fn start_file_processor(&self) {
        let open_queue = self.open_queue.clone();
        let close_queue = self.close_queue.clone();
        let completer = self.completer.clone();
        tokio::spawn(async move {
            tokio::time::sleep(std::time::Duration::from_millis(500)).await;
            loop {
                let request = open_queue.dequeue().await;
                match request {
                    OpenFileRequest::Path((url, path)) => match Self::read_file(&path).await {
                        Ok(content) => {
                            completer.did_open(&url, content.as_str()).await;
                        }
                        Err(e) => {
                            trace!("Failed to process file request {:?}: {}", url, e);
                        }
                    },
                    OpenFileRequest::Content((url, content)) => {
                        completer.did_open(&url, content.as_str()).await;
                    }
                }
            }
        });
        let completer2 = self.completer.clone();
        tokio::spawn(async move {
            loop {
                let request = close_queue.do_clean_close_file().await;
                completer2.did_close(&request.0);
            }
        });
    }

    async fn read_file(path: &PathBuf) -> Result<String, std::io::Error> {
        tokio::fs::read_to_string(path).await
    }
}

pub(crate) fn load_completer() -> Arc<Completer> {
    let pool = StringPool::global();
    let parser = DocumentParser::default(VANILLA_PACK_DEFINE);

    let vanilla_controls_table = {
        let mut result =
            BfastHashMap::<(Arc<str>, Arc<str>), completer::VanillaControlDefine>::default();
        let map = parser.hashmap();
        for (k1, v1) in map {
            if let Value::Object(map2) = v1 {
                for (k2, v2) in map2 {
                    let k1_spur = Arc::from(k1.as_str());
                    let k2_spur = Arc::from(k2.as_str());
                    let tuple = (k1_spur, k2_spur);

                    let spurs = if let Value::Object(map3) = v2 {
                        let type_spur = if let Some(Value::String(v)) = map3.get("type") {
                            pool.get_or_intern(v.as_str())
                        } else {
                            trace!("cant find type for {:?}.{:?}", k1, k2);
                            pool.get_or_intern("")
                        };

                        let variables_spur = if let Some(Value::Array(v)) = map3.get("variables") {
                            let mut r = HashSet::default();
                            for i in v.iter().filter_map(|v| {
                                if let Value::String(s) = v {
                                    Some(s)
                                } else {
                                    None
                                }
                            }) {
                                r.insert(pool.get_or_intern(i.as_str()));
                            }
                            r
                        } else {
                            trace!("variables is empty for {:?}.{:?}", k1, k2);
                            HashSet::default()
                        };
                        (type_spur, variables_spur)
                    } else {
                        trace!("cant find definition for {:?}.{:?}", k1, k2);
                        (pool.get_or_intern(""), HashSet::default())
                    };
                    result.insert(
                        tuple.clone(),
                        VanillaControlDefine {
                            name:      (tuple.0, tuple.1, None),
                            type_n:    spurs.0,
                            variables: spurs.1,
                        },
                    );
                }
            }
        }
        result
    };

    let parser2 = DocumentParser::default(JSONUI_DEFINE);
    let jsonui_define_map: BfastHashMap<String, parser::Value> = parser2.hashmap();
    Arc::new(Completer::new(vanilla_controls_table, jsonui_define_map))
}

#[tokio::main]
async fn main() {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let log = Logger::with(LogSpecification::info()).start().unwrap();

    let completer = load_completer();
    let (service, socket) = LspService::build(|client| Backend {
        client,
        config: Arc::new(TokioMutex::new(Config {
            log:           Arc::new(log),
            lang:          Arc::from("en-us"),
            append_suffix: true,
        })),
        completer,
        open_queue: Arc::new(OpenFileRequestQueue::new()),
        close_queue: Arc::new(CloseFileRequestQueue::new(Duration::from_secs(3))),
        ignore_semaphore: AtomicU8::new(0 as u8),
    })
    .finish();
    info!("starting jsonui_lsp...");
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    #[test]
    pub(crate) fn setup_logger() {
        #[cfg(feature = "debug")]
        {
            use flexi_logger::{FileSpec, LogSpecification, Logger, WriteMode};
            Logger::with(LogSpecification::trace())
                .log_to_file(FileSpec::default().directory("../../logs").basename("debug"))
                .write_mode(WriteMode::Direct)
                .start()
                .unwrap();
        }
    }
}
