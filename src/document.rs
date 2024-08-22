use log::debug;
use serde_json::json;
use std::path::PathBuf;
use std::{
    fs::File,
    io::{BufReader, Read},
    sync::Arc,
};
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Position};

#[derive(Debug)]
pub struct Document {
    pub line_info_cache: Mutex<Vec<LineInfo>>,
    pub content_cache: Mutex<String>,
}

#[derive(Debug)]
pub struct LineInfo {
    pub start_index: usize,
    pub char_count: usize,
}

impl Document {
    pub fn from(str: &String) -> Self {
        let content: String = str.chars().collect();
        Self {
            line_info_cache: Self::init_line_info_cache(&str),
            content_cache: Mutex::new(content),
        }
    }

    fn init_line_info_cache(content: &str) -> Mutex<Vec<LineInfo>> {
        Mutex::new(Self::build_line_info_cache(content))
    }

    fn build_line_info_cache(content: &str) -> Vec<LineInfo> {
        let mut line_info_table = Vec::new();
        let mut start_index = 0;
        let mut char_count = 0;

        let mut chars = content.chars().peekable();
        while let Some(c) = chars.next() {
            char_count += 1;

            if c == '\n' || (c == '\r' && chars.peek() == Some(&'\n')) {
                if c == '\r' {
                    chars.next();
                    char_count += 1;
                }

                line_info_table.push(LineInfo {
                    start_index,
                    char_count,
                });

                start_index += char_count;
                char_count = 0;
            }
        }

        if char_count > 0 {
            line_info_table.push(LineInfo {
                start_index,
                char_count,
            });
        }

        line_info_table
    }

    pub async fn get_content_index(&self, pos: &Position) -> Option<usize> {
        let line = pos.line as usize; // Index starts from 0
        let line_cache = self.line_info_cache.lock().await;

        if line >= line_cache.len() {
            return None;
        }

        let index = pos.character as usize;
        let result = if line == 0 {
            index
        } else {
            let mut result = line_cache[0..line]
                .iter()
                .map(|info| info.char_count)
                .sum::<usize>();
            result += index;
            result
        };
        Some(result)
    }

    pub async fn apply_change(&self, request: &DidChangeTextDocumentParams) {
        for e in request.content_changes.iter() {
            if let Some(v) = e.range {
                let s = &v.start;
                let end = &v.end;
                let start_index = self.get_content_index(s).await;
                let end_index = self.get_content_index(end).await;
                if let Some(si) = start_index
                    && let Some(ei) = end_index
                {
                    let mut line_cache = self.line_info_cache.lock().await;
                    let mut content = self.content_cache.lock().await;
                    content.replace_range(si..ei, e.text.as_str());
                    *line_cache = Self::build_line_info_cache(&content);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tower_lsp::lsp_types::{
        Range, TextDocumentContentChangeEvent, Url, VersionedTextDocumentIdentifier,
    };

    use super::*;

    const EXAMPLE1: &'static str = include_str!("../test/achievement_screen.json");

    #[tokio::test]
    async fn test_apply_change() {
        let document = Document::from(&EXAMPLE1.to_string());
        let request = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: Url::parse("https://example.com").unwrap(),
                version: 1,
            },
            content_changes: vec![
                TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: 24,
                            character: 30,
                        },
                        end: Position {
                            line: 24,
                            character: 30,
                        },
                    }),
                    range_length: None,
                    text: ",".to_string(),
                },
                TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: 24,
                            character: 31,
                        },
                        end: Position {
                            line: 24,
                            character: 31,
                        },
                    }),
                    range_length: None,
                    text: "\n    ".to_string(),
                },
                TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: 25,
                            character: 4,
                        },
                        end: Position {
                            line: 25,
                            character: 4,
                        },
                    }),
                    range_length: None,
                    text: "\"\"".to_string(),
                },
            ],
        };
        document.apply_change(&request).await;
        #[rustfmt::skip]
        assert!(document
            .content_cache
            .lock()
            .await
            .contains(r#""clip_pixelperfect": false,
    """#));
    }

    #[tokio::test]
    async fn test_apply_delete_change() {
        let document = Document::from(&EXAMPLE1.to_string());
        let request = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: Url::parse("https://example.com").unwrap(),
                version: 1,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: Position {
                        line: 24,
                        character: 25,
                    },
                    end: Position {
                        line: 24,
                        character: 30,
                    },
                }),
                range_length: None,
                text: "".to_string(),
            }],
        };
        document.apply_change(&request).await;
        let docs = document.content_cache.lock().await;
        #[rustfmt::skip]
        assert!(docs.contains(
            r#""clip_pixelperfect": 
  },"#));
    }
}
