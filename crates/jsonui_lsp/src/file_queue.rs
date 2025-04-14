use std::collections::VecDeque;
use std::path::PathBuf;
use std::time::Duration;
use std::usize;

use log::trace;
use tokio::sync::{Mutex as TokioMutex, Notify};
use tokio::time;
use tower_lsp::lsp_types::Url;

use crate::museair::BfastHashSet;
use crate::utils::hash_url;

#[derive(Debug)]
pub(crate) enum OpenFileRequest {
    Path((Url, PathBuf)),
    Content((Url, String)),
}
impl OpenFileRequest {
    pub(crate) fn url(&self) -> Url {
        match self {
            OpenFileRequest::Path((url, _)) => url.clone(),
            OpenFileRequest::Content((url, _)) => url.clone(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct CloseFileRequest(pub(crate) Url, pub(crate) tokio::time::Instant);

#[derive(Debug)]
pub(crate) struct OpenFileRequestQueue {
    queue:    TokioMutex<VecDeque<OpenFileRequest>>,
    urls:     TokioMutex<BfastHashSet<u64>>,
    notifier: Notify,
}
impl OpenFileRequestQueue {
    pub(crate) fn new() -> Self {
        Self {
            queue:    TokioMutex::new(VecDeque::new()),
            urls:     TokioMutex::new(BfastHashSet::default()),
            notifier: Notify::new(),
        }
    }

    pub(crate) async fn enqueue(&self, request: OpenFileRequest) {
        // Lock all resources at once
        let mut queue_lock = self.queue.lock().await;
        let mut url_set_lock = self.urls.lock().await;
        // Extract and hash the URL
        let url = hash_url(&request.url());
        // Update the URL set and queue
        if url_set_lock.contains(&url) {
            // Remove existing request with same URL
            if let Some(pos) = queue_lock.iter().position(|req| {
                let now = hash_url(&req.url());
                now == url
            }) {
                queue_lock.remove(pos);
            }
        } else {
            url_set_lock.insert(url);
        }
        queue_lock.push_back(request);
        // Locks are automatically dropped here
        self.notifier.notify_one();
    }

    pub(crate) async fn dequeue(&self) -> OpenFileRequest {
        loop {
            {
                let mut open_queue = self.queue.lock().await;
                let mut urls = self.urls.lock().await;
                if let Some(request) = open_queue.pop_front() {
                    let hash = hash_url(&request.url());
                    urls.remove(&hash);
                    return request;
                }
            }
            self.notifier.notified().await;
        }
    }
}

#[derive(Debug)]
pub(crate) struct CloseFileRequestQueue {
    queue:        TokioMutex<VecDeque<CloseFileRequest>>,
    urls:         TokioMutex<BfastHashSet<u64>>,
    invalid_time: Duration,
}
impl CloseFileRequestQueue {
    pub(crate) fn new(invalid_time: Duration) -> Self {
        Self {
            queue: TokioMutex::new(VecDeque::new()),
            urls: TokioMutex::new(BfastHashSet::default()),
            invalid_time,
        }
    }

    pub(crate) async fn enqueue(&self, request: CloseFileRequest) {
        // Lock all resources at once
        let mut queue_lock = self.queue.lock().await;
        let mut url_set_lock = self.urls.lock().await;
        // Extract and hash the URL
        let url = hash_url(&request.0);
        // Update the URL set and queue
        if url_set_lock.contains(&url) {
            // Remove existing request with same URL
            if let Some(pos) = queue_lock.iter().position(|req| {
                let now = hash_url(&req.0);
                now == url
            }) {
                queue_lock.remove(pos);
            }
        } else {
            url_set_lock.insert(url);
        }
        queue_lock.push_back(request);
    }

    pub(crate) async fn do_clean_close_file(&self) -> CloseFileRequest {
        let mut r: Option<CloseFileRequest> = None;
        while r.is_none() {
            time::sleep(Duration::from_secs(1)).await;
            let now = tokio::time::Instant::now();
            r = {
                let mut close_queue = self.queue.lock().await;
                let mut close_urls = self.urls.lock().await;
                let tasks: Vec<usize> = close_queue
                    .iter()
                    .enumerate()
                    .filter_map(|(k, v)| {
                        trace!("timeout {} {:?}", v.0, now.duration_since(v.1.clone()));
                        if now.duration_since(v.1.clone()) > self.invalid_time {
                            return Some(k);
                        } else {
                            None
                        }
                    })
                    .collect();
                tasks.iter().find_map(|f| {
                    let v = close_queue.remove(*f);
                    if let Some(request) = v {
                        close_urls.remove(&hash_url(&request.0));
                        return Some(request);
                    }
                    None
                })
            };
        }
        return r.unwrap();
    }

    pub(crate) async fn remove_close_request(&self, url: &Url) {
        let mut close_queue = self.queue.lock().await;
        let mut close_urls = self.urls.lock().await;
        let pos = close_queue.iter().position(|f| &f.0 == url);
        if let Some(pos) = pos {
            close_queue.remove(pos);
            let hash_url = hash_url(url);
            close_urls.remove(&hash_url);
        }
    }
}
