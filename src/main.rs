use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        todo!()
    }
    async fn initialized(&self, _: InitializedParams) {
        todo!()
    }

    async fn shutdown(&self) -> Result<()> {
        todo!()
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        todo!()
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        todo!()
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        todo!()
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        todo!()
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        todo!()
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        todo!()
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        todo!()
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        todo!()
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        todo!()
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        todo!()
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        todo!()
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {}

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {}
}

impl Backend {}

#[tokio::main]
async fn main() {
    env_logger::init();
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::build(|client| Backend { client }).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
