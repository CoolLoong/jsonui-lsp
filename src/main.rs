#![feature(let_chains)]

mod chumsky;
mod complete_helper;
mod document;
mod generator;
mod completer;
mod path_info;
mod tree_ds;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use flexi_logger::{LogSpecification, Logger, LoggerHandle};
use log::{info, set_max_level, trace};
use completer::Completer;
use tokio::fs;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

type StdMutex<T> = std::sync::Mutex<T>;

const VANILLAPACK_DEFINE: &str = include_str!("../resources/vanillapack_define_1.21.40.3.json");
const JSONUI_DEFINE: &str = include_str!("../resources/jsonui_define.json");

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
        Ok(())
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let url = &params.text_document.uri;
        let key = hash_uri(url);
        self.completer.update_document(key, &params);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {}

    async fn did_create_files(&self, params: CreateFilesParams) {
        for i in params.files.iter() {
            if let Ok(url) = Url::parse(&i.uri) {
                let hash_value = hash_uri(&url);
                if let Ok(content) = fs::read_to_string(url.path()).await {
                    self.completer.did_open(hash_value, Arc::from(content.as_str()));
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

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if params.text_document.language_id != "json" {
            return;
        }
        trace!("open {:?}", params.text_document.uri);
        let arc_content: Arc<str> = Arc::from(params.text_document.text.as_str());
        let hash_value = hash_uri(&params.text_document.uri);
        self.completer.did_open(hash_value, arc_content);
    }

    /// do clean completer for close file
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let url = &params.text_document.uri;
        trace!("close {}", url);
        let hash_value = hash_uri(url);
        self.completer.did_close(hash_value);
    }

    async fn document_color(&self, params: DocumentColorParams) -> Result<Vec<ColorInformation>> {
        // TODO: REFACTOR
        // let url = &params.text_document.uri;
        // let hash_value = hash_uri(url);

        // let cmp_v = self.completers.get(&hash_value);
        // if let Some(vv) = cmp_v {
        //     if let Some(result) = vv.complete_color(self).await {
        //         return Ok(result);
        //     }
        // }
        Ok(vec![])
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let url = &params.text_document_position.text_document.uri;
        let id = hash_uri(url);
        let l = self.lang.lock().await;
        self.completer.complete(id, l.clone(), &params).await;
        Ok(None)
    }
}

impl Backend {
    async fn init_workspace(&self, workspace_folders: PathBuf) {
        self.completer.init(&workspace_folders);
    }
}

fn extract_keyword_from_json(
    keys: &[&str],
    json_map: &HashMap<String, chumsky::Token>,
) -> HashSet<String> {
    let mut keyword = HashSet::new();
    for key in keys {
        if let Some(value) = json_map.get(*key) {
            if let Some(v) = chumsky::to_array_ref(value) {
                for item in v {
                    keyword.insert(chumsky::to_string_ref(item));
                }
            }
        }
    }
    keyword
}

pub(crate) fn load_completer() -> Completer {
    let r = crate::chumsky::parse(crate::chumsky::parser(), VANILLAPACK_DEFINE);
    let vanilla_controls_tabel = match r {
        Ok((_, r)) => {
            let mut result = HashMap::<(Arc<str>, Arc<str>), completer::ControlDefine>::new();
            let m = chumsky::to_map(r);
            for (k1, v) in m {
                if let chumsky::Token::Object(_, v) = v {
                    let m = chumsky::to_map(v);
                    for (k2, v) in m {
                        if let chumsky::Token::Object(_, v) = v {
                            let m = chumsky::to_map(v);
                            let tuple = (Arc::from(k1.clone().as_ref()), Arc::from(k2.as_ref()));
                            let type_n: Arc<str> = if let Some(chumsky::Token::Str(_, v)) = m.get("type")
                            {
                                Arc::from(*v)
                            } else {
                                trace!("cant find type for {:?}", m);
                                Arc::from("")
                            };
                            let variables = if let Some(v) = m.get("variables")
                                && let chumsky::Token::Array(_, v) = v
                            {
                                chumsky::to_arc_str_hashset(v)
                            } else {
                                HashSet::new()
                            };
                            result.insert(
                                tuple,
                                completer::ControlDefine {
                                    name:      Arc::from(k2.as_ref()),
                                    extend:    None,
                                    type_n:    StdMutex::new(Some(type_n)),
                                    variables: StdMutex::new(variables),
                                },
                            );
                        }
                    }
                }
            }
            result
        }
        Err(e) => {
            panic!("error in load vanilla pack definition {:?}.", e)
        }
    };

    let jsonui_define = crate::chumsky::parse(crate::chumsky::parser(), JSONUI_DEFINE)
        .expect("can parse jsonui_define.json");
    let jsonui_define_map: HashMap<String, chumsky::Token<'_>> = chumsky::to_map(jsonui_define.1);
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
    Completer::new(keyword, vanilla_controls_tabel)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let log: flexi_logger::LoggerHandle = Logger::with(LogSpecification::info()).start().unwrap();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        log: Arc::new(log),
        lang: Mutex::new(Arc::from("zh-cn")),
        completer: load_completer(),
    })
    .finish();
    info!("starting server...");
    Server::new(stdin, stdout, socket).serve(service).await;
}
