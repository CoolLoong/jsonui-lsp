// Import from lib.rs
use jsonui_lsp::completer::Completer;
use jsonui_lsp::config::{Config, ConfigManager};
use jsonui_lsp::document_manager::{DocumentManager, OpenRequest};
use jsonui_lsp::load_vanilla_controls_table;
use jsonui_lsp::museair::BfastHashMap;
use jsonui_lsp::navigation_state::NavigationStateManager;
use jsonui_lsp::parser::DocumentParser;

pub(crate) mod towerlsp {
    pub(crate) use tower_lsp::lsp_types::notification::*;
    pub(crate) use tower_lsp::lsp_types::request::*;
    pub(crate) use tower_lsp::lsp_types::*;
    pub(crate) use tower_lsp::{Client, LanguageServer, LspService, Server, async_trait};
}

use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::OnceLock;
use tower_lsp::lsp_types::InitializeParams;
use towerlsp::*;
use tracing::{info, trace};
use tracing_subscriber::EnvFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use walkdir::WalkDir;

const JSONUI_DEFINE: &str = include_str!("../resources/jsonui_define.json");
#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

struct Backend {
    client: Client,
    config: Arc<ConfigManager>,
    completer: Arc<Completer>,
    document_manager: Arc<DocumentManager>,
    root_path: OnceLock<PathBuf>,
    navigation_state: NavigationStateManager,
    workspace_initialized: Arc<AtomicBool>,
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, param: InitializeParams) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        let init_config = param
            .initialization_options
            .expect("initialization options cant be empty!");

        // Update configuration from client initialization options with dynamic log level reload
        self.config
            .update_with_log_reload(|old_config| {
                let mut new_config = old_config.clone();

                if let Some(level) = init_config
                    .get("settings")
                    .and_then(|s| s.get("log"))
                    .and_then(|l| l.get("level"))
                    .and_then(|v| v.as_str())
                {
                    new_config.log_level = match level {
                        "off" => Arc::from("off"),
                        "messages" => Arc::from("error"),
                        "verbose" => Arc::from("trace"),
                        _ => new_config.log_level.clone(),
                    };
                }

                if let Some(append) = init_config
                    .get("settings")
                    .and_then(|s| s.get("options"))
                    .and_then(|o| o.get("auto_append_suffix"))
                    .and_then(|v| v.as_bool())
                {
                    new_config.append_suffix = append;
                }

                if let Some(lang) = init_config.get("locale").and_then(|l| l.as_str()) {
                    new_config.lang = Arc::from(lang);
                }

                new_config
            })
            .await;

        if let Some(root_url) = param.root_uri
            && let Ok(workspace) = root_url.to_file_path()
        {
            let _ = self.root_path.set(workspace);
        }

        let file_operation_filters = vec![FileOperationFilter {
            scheme: Some("file".to_string()),
            pattern: FileOperationPattern {
                glob: "**/*.json".to_string(),
                matches: None,
                options: None,
            },
        }];
        let registration_options = FileOperationRegistrationOptions {
            filters: file_operation_filters,
        };
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "jsonui-lsp".to_string(),
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
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["\"".to_string(), ":".to_string()]),
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
                    file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                        did_create: Some(registration_options.clone()),
                        will_create: None,
                        did_rename: Some(registration_options.clone()),
                        will_rename: None,
                        did_delete: Some(registration_options.clone()),
                        will_delete: None,
                    }),
                }),
                color_provider: Some(ColorProviderCapability::Options(
                    StaticTextDocumentColorProviderOptions {
                        document_selector: Some(vec![DocumentFilter {
                            language: Some("json".to_string()),
                            scheme: None,
                            pattern: None,
                        }]),
                        id: None,
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;

        if let Some(root_path) = self.root_path.get() {
            self.init_workspace(root_path.clone()).await;
        }

        // Mark workspace as initialized
        self.workspace_initialized.store(true, Ordering::SeqCst);
        self.client
            .log_message(MessageType::INFO, "Workspace initialization completed")
            .await;
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        trace!("jsonui-lsp shutdown");
        Ok(())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<GotoDefinitionResponse>> {
        if !self.workspace_initialized.load(Ordering::SeqCst) {
            return Ok(None);
        }

        let r = self.completer.goto_definition(params).await;
        if let Some((r, is_current_file)) = r {
            if !is_current_file {
                self.navigation_state.start_navigation();
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
        if !self.workspace_initialized.load(Ordering::SeqCst) {
            return Ok(None);
        }

        let r = self.completer.references(&params).await;
        Ok(r)
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<CompletionResponse>> {
        if !self.workspace_initialized.load(Ordering::SeqCst) {
            return Ok(None);
        }

        let url = params.text_document_position.text_document.uri.clone();
        let r = self.completer.complete(url, self.config.get(), &params).await;
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
        if !self.workspace_initialized.load(Ordering::SeqCst) {
            return Ok(vec![]);
        }

        let url = params.text_document.uri;
        let r = self.completer.complete_color(url);
        if let Some(r) = r { Ok(r) } else { Ok(vec![]) }
    }

    async fn color_presentation(
        &self,
        params: ColorPresentationParams,
    ) -> tower_lsp::jsonrpc::Result<Vec<ColorPresentation>> {
        let ColorPresentationParams { color, range, .. } = params;
        let color_presentation = ColorPresentation {
            label: format!(
                "rgba({:.3}, {:.3}, {:.3}, {:.3})",
                color.red, color.green, color.blue, color.alpha
            ),
            text_edit: Some(TextEdit {
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
        // Update configuration from client with dynamic log level reload
        self.config
            .update_with_log_reload(|old_config| {
                let mut new_config = old_config.clone();

                if let Some(level) = params
                    .settings
                    .get("log")
                    .and_then(|l| l.get("level"))
                    .and_then(|v| v.as_str())
                {
                    new_config.log_level = match level {
                        "off" => Arc::from("off"),
                        "messages" => Arc::from("error"),
                        "verbose" => Arc::from("trace"),
                        _ => new_config.log_level.clone(),
                    };
                }

                if let Some(append) = params
                    .settings
                    .get("options")
                    .and_then(|o| o.get("auto_append_suffix"))
                    .and_then(|v| v.as_bool())
                {
                    new_config.append_suffix = append;
                }

                if let Some(lang) = params.settings.get("locale").and_then(|l| l.as_str()) {
                    new_config.lang = Arc::from(lang);
                }

                new_config
            })
            .await;
    }

    async fn did_create_files(&self, params: CreateFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri)
                && let Ok(content) = tokio::fs::read_to_string(url.path()).await {
                    self.document_manager
                        .request_open(OpenRequest::Content(url, content));
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
                self.completer.did_rename(o_url, n_url).await;
            }
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let params = params.text_document;
        if params.language_id != "json" {
            return;
        }
        if self.navigation_state.should_ignore_open() {
            return;
        }

        if !self.is_in_workspace(&params.uri) {
            self.document_manager.cancel_delayed_close(&params.uri);
        }

        self.document_manager
            .request_open(OpenRequest::Content(params.uri, params.text));
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let url = params.text_document.uri;

        if !self.is_in_workspace(&url) {
            self.document_manager.request_delayed_close(url);
        }
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                trace!("File deleted: {}", url);
                self.completer.did_close(&url);
            }
        }
    }
}

impl Backend {
    /// Check if a file URL is within the workspace
    /// Returns true if the file is in workspace, false if it's a standalone file
    fn is_in_workspace(&self, url: &Url) -> bool {
        if let Some(workspace_root) = self.root_path.get() {
            if let Ok(file_path) = url.to_file_path() {
                return file_path.starts_with(workspace_root);
            }
        }
        false
    }

    async fn init_workspace(&self, workspace_folders: PathBuf) {
        let json_files: Vec<_> = WalkDir::new(&workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().is_some_and(|ext| ext == "json"))
            .collect();
        let total_files = json_files.len();
        if total_files == 0 {
            return;
        }

        let token = NumberOrString::String("workspace-indexing".to_string());

        // Send begin progress notification
        if let Err(e) = self
            .client
            .send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                token: token.clone(),
            })
            .await
        {
            trace!("Failed to create progress token: {:?}", e);
        }

        self.client
            .send_notification::<Progress>(ProgressParams {
                token: token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(WorkDoneProgressBegin {
                    title: "Indexing workspace".to_string(),
                    cancellable: Some(false),
                    message: Some(format!("0/{}", total_files)),
                    percentage: Some(0),
                })),
            })
            .await;

        // Process files concurrently with per-file progress updates
        use futures::stream::{self, StreamExt};
        use std::sync::atomic::{AtomicUsize, Ordering};

        let completed = Arc::new(AtomicUsize::new(0));
        let client = self.client.clone();
        let completer = self.completer.clone();
        let token_clone = token.clone();

        // Process up to 8 files concurrently
        stream::iter(json_files.into_iter())
            .map(|entry| {
                let path = entry.into_path();
                let client = client.clone();
                let completer = completer.clone();
                let token = token_clone.clone();
                let completed = completed.clone();

                async move {
                    // Read and canonicalize file path
                    match tokio::fs::read_to_string(&path).await {
                        Ok(content) => {
                            if let Ok(abs_path) = tokio::fs::canonicalize(&path).await
                                && let Ok(url) = Url::from_file_path(abs_path) {
                                    // Index document (this awaits completion)
                                    completer.did_open(&url, &content).await;
                                }
                        }
                        Err(e) => {
                            trace!("Failed to read file {:?}: {}", path, e);
                        }
                    }

                    // Update progress after file is fully indexed
                    let current = completed.fetch_add(1, Ordering::SeqCst) + 1;
                    let percentage = (current as f64 / total_files as f64 * 100.0) as u32;

                    // Send progress update for this file
                    let _ = client
                        .send_notification::<Progress>(ProgressParams {
                            token,
                            value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                                WorkDoneProgressReport {
                                    cancellable: Some(false),
                                    message: Some(format!("{}/{}", current, total_files)),
                                    percentage: Some(percentage),
                                },
                            )),
                        })
                        .await;
                }
            })
            .buffer_unordered(8) // Process up to 8 files concurrently
            .collect::<Vec<_>>()
            .await;

        // Send end progress notification
        self.client
            .send_notification::<Progress>(ProgressParams {
                token: token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(format!("Indexed {} files", total_files)),
                })),
            })
            .await;

        self.client
            .log_message(
                MessageType::INFO,
                format!("Workspace indexing completed: {} files", total_files),
            )
            .await;
    }

    // Note: file processing is now handled by DocumentManager
}

pub(crate) fn load_completer() -> Arc<Completer> {
    let vanilla_controls_table = load_vanilla_controls_table();
    let p = DocumentParser::default(JSONUI_DEFINE);
    let jsonui_define_map: BfastHashMap<String, jsonui_lsp::parser::Value> = p.hashmap();
    Arc::new(Completer::new(vanilla_controls_table, jsonui_define_map))
}

#[tokio::main]
async fn main() {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    // Initialize tracing subscriber with reload capability - MUST write to stderr, not stdout
    // LSP protocol uses stdout for communication
    let directive = "jsonui_lsp=info"
        .parse()
        .expect("Invalid log directive: jsonui_lsp=info");
    let filter = EnvFilter::new("error").add_directive(directive);
    let (filter, reload_handle) = tracing_subscriber::reload::Layer::new(filter);

    tracing_subscriber::registry()
        .with(filter)
        .with(
            tracing_subscriber::fmt::layer()
                .with_target(false)
                .with_ansi(false)
                .with_writer(std::io::stderr),
        )
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let completer = load_completer();
    let document_manager = Arc::new(DocumentManager::new(completer.clone()));

    let config_manager = Arc::new(ConfigManager::new(Config::new("info", "en-us", true)));

    // Set the log reload handle for dynamic log level changes
    config_manager.set_log_reload_handle(reload_handle).await;

    let (service, socket) = LspService::build(|client| Backend {
        client,
        config: config_manager,
        completer: completer.clone(),
        document_manager,
        root_path: OnceLock::new(),
        navigation_state: NavigationStateManager::new(),
        workspace_initialized: Arc::new(AtomicBool::new(false)),
    })
    .finish();
    info!("starting jsonui-lsp...");
    Server::new(stdin, stdout, socket).serve(service).await;
}