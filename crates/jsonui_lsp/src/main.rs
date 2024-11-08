#![feature(let_chains)]

mod complete_helper;
mod completer;
mod document;
mod generator;
mod lexer;
mod tree_ds;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use completer::Completer;
use flexi_logger::{LogSpecification, Logger, LoggerHandle};
use lasso::Rodeo;
use log::{info, set_max_level, trace};
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

type StdMutex<T> = std::sync::Mutex<T>;

const VANILLAPACK_DEFINE: &str = include_str!("resources/vanillapack_define_1.21.40.3.json");
const JSONUI_DEFINE: &str = include_str!("resources/jsonui_define.json");
const SEED: u64 = 32;

#[inline]
fn hash_uri(url: &Url) -> u64 {
    museair::bfast::hash(url.path().as_bytes(), SEED)
}

struct Backend {
    client:               Client,
    log:                  Arc<LoggerHandle>,
    lang:                 Mutex<Arc<str>>,
    pub(crate) completer: Completer,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
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
        trace!("client lang is {:?}", client_lang);
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
            server_info:  Some(ServerInfo {
                name:    "jsonui support".to_string(),
                version: None,
            }),
            capabilities: ServerCapabilities {
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

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if params.text_document.language_id != "json" {
            return;
        }
        trace!("open {:?}", params.text_document.uri);
        let arc_content: Arc<str> = Arc::from(params.text_document.text.as_str());
        let hash_value = hash_uri(&params.text_document.uri);
        self.completer.did_open(hash_value, arc_content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let url = &params.text_document.uri;
        let key = hash_uri(url);
        self.completer.update_document(key, &params).await;
    }

    /// do clean completer for close file
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let url = &params.text_document.uri;
        trace!("close {}", url);
        let hash_value = hash_uri(url);
        self.completer.did_close(hash_value);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let url = &params.text_document_position.text_document.uri;
        let id = hash_uri(url);
        let l = self.lang.lock().await;
        let r = self.completer.complete(id, l.clone(), &params).await;
        if let Some(r) = r {
            Ok(Some(CompletionResponse::Array(r)))
        } else {
            Ok(None)
        }
    }

    async fn document_color(&self, params: DocumentColorParams) -> Result<Vec<ColorInformation>> {
        let url = &params.text_document.uri;
        let id = hash_uri(url);
        let r = self.completer.complete_color(id).await;
        if let Some(r) = r {
            Ok(r)
        } else {
            Ok(vec![])
        }
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

    async fn did_create_files(&self, params: CreateFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                let hash_value = hash_uri(&url);
                if let Ok(content) = std::fs::read_to_string(url.path()) {
                    self.completer
                        .did_open(hash_value, Arc::from(content.as_str()))
                        .await;
                }
            }
        }
    }

    async fn did_rename_files(&self, params: RenameFilesParams) {
        for i in params.files.iter() {
            if let Ok(o_url) = Url::parse(&i.old_uri)
                && let Ok(n_url) = Url::parse(&i.new_uri)
            {
                let ho = hash_uri(&o_url);
                let hn = hash_uri(&n_url);
                self.completer.did_rename(ho, hn);
            }
        }
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                let x = hash_uri(&url);
                self.completer.did_close(x);
            }
        }
    }
}

impl Backend {
    async fn init_workspace(&self, workspace_folders: PathBuf) {
        self.completer.init(&workspace_folders).await;
    }
}

pub(crate) async fn load_completer() -> Completer {
    let mut pool = Rodeo::default();
    let r = lexer::parse_full(VANILLAPACK_DEFINE).await;
    let vanilla_controls_tabel = match r {
        Some((_, r)) => {
            let mut result =
                HashMap::<(lasso::Spur, lasso::Spur), completer::PooledControlDefine>::new();
            let m = lexer::to_map(r);
            for (k1, v) in m {
                if let lexer::Token::Object(_, v) = v {
                    let m = lexer::to_map(v.unwrap());
                    for (k2, v) in m {
                        if let lexer::Token::Object(_, v) = v {
                            let m = lexer::to_map(v.unwrap());
                            let k1_spur = pool.get_or_intern(k1.as_str());
                            let k2_spur = pool.get_or_intern(k2.as_str());
                            let tuple = (k1_spur, k2_spur);
                            let type_spur = if let Some(lexer::Token::Str(_, v)) = m.get("type") {
                                pool.get_or_intern(v.as_str())
                            } else {
                                trace!("cant find type for {:?}", m);
                                pool.get_or_intern("")
                            };
                            let variables = if let Some(v) = m.get("variables")
                                && let lexer::Token::Array(_, v) = v
                            {
                                let mut r = HashSet::new();
                                for i in v.as_ref().unwrap() {
                                    if let lexer::Token::Str(_, v) = i {
                                        r.insert(pool.get_or_intern(v.as_str()));
                                    }
                                }
                                r
                            } else {
                                HashSet::new()
                            };
                            result.insert(
                                tuple,
                                completer::PooledControlDefine {
                                    name: k2_spur,
                                    extend: None,
                                    type_n: Some(type_spur),
                                    variables,
                                },
                            );
                        }
                    }
                }
            }
            result
        }
        None => {
            panic!("error in load vanilla pack definition.")
        }
    };

    let resolver: lasso::RodeoResolver = pool.into_resolver();
    let vanilla_controls_tabel = vanilla_controls_tabel
        .into_iter()
        .map(|(k, v)| {
            let (v1, v2) = k;
            let v = v.to(&resolver);
            ((Arc::from(resolver.resolve(&v1)), Arc::from(resolver.resolve(&v2))), v)
        })
        .collect();

    let jsonui_define = crate::lexer::parse_full(JSONUI_DEFINE)
        .await
        .expect("can parse jsonui_define.json");
    let jsonui_define_map: HashMap<String, lexer::Token> = lexer::to_map(jsonui_define.1);
    Completer::new(vanilla_controls_tabel, jsonui_define_map)
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let log = Logger::with(LogSpecification::info()).start().unwrap();

    let completer = load_completer().await;
    let (service, socket) = LspService::build(|client| Backend {
        client,
        log: Arc::new(log),
        lang: Mutex::new(Arc::from("zh-cn")),
        completer,
    })
    .finish();
    info!("starting jsonui_lsp...");
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    pub(crate) fn setup_logger() {
        #[cfg(feature = "debug-parse")]
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
