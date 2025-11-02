use std::path::PathBuf;
use std::sync::Arc;

use tracing::trace;
use tokio::sync::mpsc;
use tower_lsp::lsp_types::Url;

use crate::completer::Completer;

#[derive(Debug)]
pub enum OpenRequest {
    /// Open file by reading from path
    Path(Url, PathBuf),
    /// Open file with provided content
    Content(Url, String),
}

impl OpenRequest {
    pub fn url(&self) -> &Url {
        match self {
            OpenRequest::Path(url, _) => url,
            OpenRequest::Content(url, _) => url,
        }
    }
}

pub struct DocumentManager {
    /// Channel for file open requests
    open_tx: mpsc::UnboundedSender<OpenRequest>,
}

impl DocumentManager {
    pub fn new(completer: Arc<Completer>) -> Self {
        let (open_tx, open_rx) = mpsc::unbounded_channel();
        tokio::spawn(async move {
            Self::process_open_requests(open_rx, completer).await;
        });
        Self { open_tx }
    }

    /// Request to open a file
    ///
    /// If the file is already in the queue, the old request is replaced.
    pub fn request_open(&self, request: OpenRequest) {
        let _ = self.open_tx.send(request);
    }

    /// Process open file requests
    async fn process_open_requests(
        mut rx: mpsc::UnboundedReceiver<OpenRequest>,
        completer: Arc<Completer>,
    ) {
        while let Some(request) = rx.recv().await {
            let url = request.url().clone();
            // Read file content
            let content = match request {
                OpenRequest::Path(_, ref path) => match tokio::fs::read_to_string(path).await {
                    Ok(content) => content,
                    Err(e) => {
                        trace!("Failed to read file {:?}: {}", path, e);
                        continue;
                    }
                },
                OpenRequest::Content(_, content) => content,
            };
            // Index the document
            completer.did_open(&url, &content).await;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open_request_url() {
        let url = Url::parse("file:///test.json").unwrap();
        let request = OpenRequest::Content(url.clone(), "{}".to_string());
        assert_eq!(request.url(), &url);
    }
}
