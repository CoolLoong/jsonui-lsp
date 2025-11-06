use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use tracing::trace;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tower_lsp::lsp_types::Url;

use crate::completer::Completer;
use crate::museair::BfastDashMap;

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

/// Delayed close request
enum CloseRequest {
    /// Schedule a delayed close for a URL
    Schedule(Url),
    /// Cancel a pending close for a URL
    Cancel(Url),
}

pub struct DocumentManager {
    /// Channel for file open requests
    open_tx: mpsc::UnboundedSender<OpenRequest>,
    /// Channel for delayed close requests
    close_tx: mpsc::UnboundedSender<CloseRequest>,
}

impl DocumentManager {
    pub fn new(completer: Arc<Completer>) -> Self {
        let (open_tx, open_rx) = mpsc::unbounded_channel();
        let (close_tx, close_rx) = mpsc::unbounded_channel();

        // Spawn task to process open requests
        tokio::spawn({
            let completer = completer.clone();
            async move {
                Self::process_open_requests(open_rx, completer).await;
            }
        });

        // Spawn task to process close requests
        tokio::spawn(async move {
            Self::process_close_requests(close_rx, completer).await;
        });

        Self { open_tx, close_tx }
    }

    /// Request to open a file
    ///
    /// If the file is already in the queue, the old request is replaced.
    pub fn request_open(&self, request: OpenRequest) {
        let _ = self.open_tx.send(request);
    }

    /// Request a delayed close for a file
    pub fn request_delayed_close(&self, url: Url) {
        let _ = self.close_tx.send(CloseRequest::Schedule(url));
    }

    /// Cancel a pending delayed close for a file
    pub fn cancel_delayed_close(&self, url: &Url) {
        let _ = self.close_tx.send(CloseRequest::Cancel(url.clone()));
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

    /// Process delayed close requests
    /// Maintains a map of pending close tasks that can be cancelled
    async fn process_close_requests(
        mut rx: mpsc::UnboundedReceiver<CloseRequest>,
        completer: Arc<Completer>,
    ) {
        // Map of URL to pending close task handle
        let pending_closes: Arc<BfastDashMap<Url, JoinHandle<()>>> = Arc::new(BfastDashMap::default());

        while let Some(request) = rx.recv().await {
            match request {
                CloseRequest::Schedule(url) => {
                    // Cancel any existing pending close for this URL
                    if let Some((_, handle)) = pending_closes.remove(&url) {
                        handle.abort();
                    }

                    // Schedule a new delayed close
                    let url_clone = url.clone();
                    let completer_clone = completer.clone();
                    let pending_closes_clone = pending_closes.clone();

                    let handle = tokio::spawn(async move {
                        tokio::time::sleep(Duration::from_secs(5)).await;

                        pending_closes_clone.remove(&url_clone);

                        trace!("Executing delayed close for {}", url_clone);
                        completer_clone.did_close(&url_clone);
                    });

                    pending_closes.insert(url, handle);
                }
                CloseRequest::Cancel(url) => {
                    if let Some((_, handle)) = pending_closes.remove(&url) {
                        handle.abort();
                    }
                }
            }
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
