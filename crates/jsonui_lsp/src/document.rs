use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use parking_lot::Mutex;
use unicode_segmentation::UnicodeSegmentation;

use crate::towerlsp::*;

#[derive(Debug)]
pub struct Document {
    namespace: String,
    line_info_cache: Mutex<Vec<LineInfo>>,
    content_cache: Mutex<String>,
    pub(crate) chars: Mutex<Vec<Arc<str>>>,
    dirty: AtomicBool,
}

#[derive(Debug)]
pub struct LineInfo {
    pub start_index: usize,
    pub char_count:  usize,
}

impl Document {
    pub fn from(str: Arc<str>) -> Self {
        let content_chars: Vec<Arc<str>> = str.graphemes(true).map(Arc::from).collect();
        let namespace = Self::get_namespace(str.clone()).unwrap_or("unknown_namespace".to_string());
        Self {
            namespace,
            line_info_cache: Self::init_line_info_cache(str.as_ref()),
            content_cache: Mutex::new(str.to_string()),
            chars: Mutex::new(content_chars),
            dirty: AtomicBool::new(false),
        }
    }

    pub fn get_content(&self) -> Arc<str> {
        Arc::from(self.content_cache.lock().as_str())
    }

    pub fn get_cache_namespace(&self) -> Arc<str> {
        Arc::from(self.namespace.as_str())
    }

    pub fn is_dirty(&self) -> bool {
        self.dirty.load(std::sync::atomic::Ordering::Acquire)
    }

    pub fn clear_dirty(&self) {
        self.dirty.store(false, std::sync::atomic::Ordering::Release)
    }

    /// position line index start from 0
    pub fn get_index_from_position(&self, pos: Position) -> Option<usize> {
        if let Some(line_cache) = self.line_info_cache.try_lock() {
            let line = pos.line as usize; // Index starts from 0
            if line >= line_cache.len() {
                return None;
            }
            let index = pos.character as usize;
            let result = if line == 0 {
                index
            } else {
                let mut result = line_cache[0..line] // get 0 ~ line-1
                    .iter()
                    .map(|info| info.char_count)
                    .sum::<usize>();
                result += index;
                result
            };
            Some(result)
        } else {
            None
        }
    }

    /// Given a character index, return the corresponding Position in the document.
    /// Index starts from 0.
    pub fn get_position_from_index(&self, index: usize) -> Option<Position> {
        if let Some(line_cache) = self.line_info_cache.try_lock() {
            // Calculate the total number of characters in the document
            let total_chars = line_cache.iter().map(|info| info.char_count).sum::<usize>();
            if index >= total_chars {
                return None; // Index is out of bounds
            }
            let mut current_index = 0;

            for (line_num, info) in line_cache.iter().enumerate() {
                // Check if the index is within this line
                if index < current_index + info.char_count {
                    let character = index - current_index;
                    return Some(Position {
                        line: line_num as u32,
                        character: character as u32,
                    });
                }
                // Update the current index to the start of the next line
                current_index += info.char_count;
            }
        }
        None // Should not reach here if total_chars is correctly calculated
    }

    pub fn apply_change(&self, request: &DidChangeTextDocumentParams) {
        for e in request.content_changes.iter() {
            if let Some(v) = e.range {
                let s = v.start;
                let end = v.end;
                let start_index = self.get_index_from_position(s);
                let end_index = self.get_index_from_position(end);
                if let Some(si) = start_index
                    && let Some(ei) = end_index
                {
                    self.replace_grapheme_range(si, ei, Arc::from(e.text.as_str()));
                    let mut line_cache = self.line_info_cache.lock();
                    let content = self.content_cache.lock();
                    *line_cache = Self::build_line_info_cache(&content);
                }
            }
        }
        self.dirty.store(true, std::sync::atomic::Ordering::Release);
    }

    pub fn get_boundary_indices(&self, index: usize) -> Option<(usize, usize)> {
        if let Some(content) = self.chars.try_lock() {
            let (forward, backward) = content.split_at(index);

            #[cfg(feature = "debug-parse")]
            trace!("before {:?} | after {:?}", forward, backward);
            if forward.is_empty() || backward.is_empty() {
                return None;
            }

            let mut boundary_stacks = vec![vec![], vec![], vec![], vec![]];
            let (mut start_index, mut end_index) = (None, None);
            let mut forward_iter = forward.iter().rev().peekable().enumerate();

            let f_len = forward.len();
            while let Some((_, ch)) = forward_iter.next() {
                let char = ch.as_ref();
                match char {
                    "{" | "}" | "[" | "]" => Self::handle_boundary_char(char, &mut boundary_stacks),
                    ":" => {
                        if !(boundary_stacks[0].len() == 1 || boundary_stacks[2].len() == 1) {
                            continue;
                        }
                        let opt = forward_iter
                            .by_ref()
                            .skip_while(|(_, c)| c.as_ref() != "\"")
                            .skip(1)
                            .skip_while(|(_, c)| c.as_ref() != "\"")
                            .nth(1);
                        if let Some((index, _)) = opt {
                            start_index = Some(f_len - index);
                            break;
                        }
                    }
                    _ => {}
                }
            }
            let start_index_v = start_index?;

            let backend_iter = backward.iter().peekable().enumerate();
            for (index, ch) in backend_iter {
                let char = ch.as_ref();
                match char {
                    "{" | "}" | "[" | "]" => Self::handle_boundary_char(char, &mut boundary_stacks),
                    _ => {}
                }
                if boundary_stacks.iter().all(|stack| stack.is_empty()) {
                    end_index = Some(index + f_len);
                    break;
                }
            }
            let end_index_v = end_index?;
            Some((start_index_v, end_index_v))
        } else {
            None
        }
    }

    pub fn get_char(&self, index: usize) -> Option<Arc<str>> {
        if let Some(chars) = self.chars.try_lock() {
            if index >= chars.len() {
                None
            } else {
                Some(chars[index].clone())
            }
        } else {
            None
        }
    }

    pub fn replace_grapheme_range(&self, start_idx: usize, end_idx: usize, replacement: Arc<str>) {
        if let Some(mut chars) = self.chars.try_lock() {
            if start_idx > end_idx || start_idx > chars.len() || end_idx > chars.len() {
                panic!()
            }

            let (before, after) = if end_idx == start_idx {
                chars.split_at(start_idx)
            } else {
                let (s, _) = chars.split_at(start_idx);
                let (_, e) = chars.split_at(end_idx);
                (s, e)
            };
            let mut result: Vec<Arc<str>> = Vec::new();
            result.extend_from_slice(before);
            result.append(
                &mut replacement
                    .graphemes(true)
                    .map(Arc::from)
                    .collect::<Vec<Arc<str>>>(),
            );
            result.extend_from_slice(after);

            let mut content = self.content_cache.lock();
            *content = result.join("");
            *chars = result;
        }
    }

    fn build_line_info_cache(content: &str) -> Vec<LineInfo> {
        let mut line_info_table = Vec::new();
        let mut start_index = 0;
        let mut char_count = 0;
        let chars = content.graphemes(true).peekable();
        for c in chars {
            char_count += 1;
            if c == "\n" || c == "\r\n" {
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

    fn handle_boundary_char<'a>(char: &'a str, boundary_stacks: &mut [Vec<&'a str>]) {
        let (current_index, opposite_index) = match char {
            "{" => (0, 1),
            "}" => (1, 0),
            "[" => (2, 3),
            "]" => (3, 2),
            _ => unreachable!(),
        };
        if boundary_stacks[opposite_index].is_empty() {
            boundary_stacks[current_index].push(char);
        } else {
            boundary_stacks[opposite_index].pop();
        }
    }

    pub(crate) fn get_namespace(s: Arc<str>) -> Option<String> {
        const NAMESPACE: &str = "namespace\"";
        let mut crs = s.chars().peekable();

        while let Some(ch) = crs.next() {
            if ch == '"' && NAMESPACE.chars().all(|c| Some(c) == crs.next()) {
                let mut skip_w = crs.skip_while(|&c| c != '"');
                skip_w.next();
                let result: String = skip_w.take_while(|&c| c != '"').collect();
                return Some(result);
            }
        }
        None
    }

    fn init_line_info_cache(content: &str) -> Mutex<Vec<LineInfo>> {
        Mutex::new(Self::build_line_info_cache(content))
    }
}

#[cfg(test)]
mod tests {
    use tower_lsp::lsp_types::{
        Range, TextDocumentContentChangeEvent, Url, VersionedTextDocumentIdentifier,
    };

    use super::*;

    const EXAMPLE1: &str = include_str!("../test/achievement.json");

    #[test]
    fn test_apply_change() {
        let document = Document::from(Arc::from(EXAMPLE1));
        let request = DidChangeTextDocumentParams {
            text_document:   VersionedTextDocumentIdentifier {
                uri:     Url::parse("https://example.com").unwrap(),
                version: 1,
            },
            content_changes: vec![
                TextDocumentContentChangeEvent {
                    range:        Some(Range {
                        start: Position {
                            line:      24,
                            character: 30,
                        },
                        end:   Position {
                            line:      24,
                            character: 30,
                        },
                    }),
                    range_length: None,
                    text:         ",".to_string(),
                },
                TextDocumentContentChangeEvent {
                    range:        Some(Range {
                        start: Position {
                            line:      24,
                            character: 31,
                        },
                        end:   Position {
                            line:      24,
                            character: 31,
                        },
                    }),
                    range_length: None,
                    text:         "\n    ".to_string(),
                },
                TextDocumentContentChangeEvent {
                    range:        Some(Range {
                        start: Position {
                            line:      25,
                            character: 4,
                        },
                        end:   Position {
                            line:      25,
                            character: 4,
                        },
                    }),
                    range_length: None,
                    text:         "\"\"".to_string(),
                },
            ],
        };
        document.apply_change(&request);
        #[rustfmt::skip]
        assert!(document
            .content_cache
            .lock()
            .contains(r#""clip_pixelperfect": false,
    """#));
    }

    #[test]
    fn test_apply_delete_change() {
        let document = Document::from(Arc::from(EXAMPLE1));
        let request = DidChangeTextDocumentParams {
            text_document:   VersionedTextDocumentIdentifier {
                uri:     Url::parse("https://example.com").unwrap(),
                version: 1,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range:        Some(Range {
                    start: Position {
                        line:      23,
                        character: 23,
                    },
                    end:   Position {
                        line:      23,
                        character: 27,
                    },
                }),
                range_length: None,
                text:         "".to_string(),
            }],
        };
        document.apply_change(&request);
        let docs = document.content_cache.lock();
        #[rustfmt::skip]
        assert!(docs.contains("\"clip_direction\": \"\""));
    }

    #[test]
    fn test_replace_grapheme_range() {
        let document = Document::from(Arc::from("hello 世界"));
        document.replace_grapheme_range(5, 5, Arc::from("MMM"));
        assert_eq!(*document.content_cache.lock(), "helloMMM 世界");
        document.replace_grapheme_range(5, 8, Arc::from("XXX"));
        assert_eq!(*document.content_cache.lock(), "helloXXX 世界");
        document.replace_grapheme_range(8, 8, Arc::from("D"));
        assert_eq!(*document.content_cache.lock(), "helloXXXD 世界");
    }

    #[test]
    fn test_get_boundary_indices() {
        let document = Document::from(Arc::from(EXAMPLE1));
        let v: usize = document
            .get_index_from_position(Position {
                line:      16,
                character: 6,
            })
            .unwrap();

        let result = document.get_boundary_indices(v);
        let (l, r) = result.unwrap();
        let lc = document.get_char(l).unwrap();
        let rc = document.get_char(r).unwrap();
        assert_eq!((lc.as_ref(), rc.as_ref()), ("\"", "}"));

        let v = document
            .get_index_from_position(Position {
                line:      6,
                character: 3,
            })
            .unwrap();
        let result = document.get_boundary_indices(v);
        assert_eq!(result, None);
    }

    #[test]
    fn test_get_namespace_esay() {
        let result = Document::get_namespace(Arc::from(r#"{"namespace": "test"}"#)).unwrap();
        let expect = "test";
        assert_eq!(result, expect);
    }

    #[test]
    fn test_get_namespace_hard() {
        let result = Document::get_namespace(Arc::from(
            r#"// this is comment{"test_control": {"type": "panel"},"namespace": "test"}"#,
        ))
            .unwrap();
        let expect = "test";
        assert_eq!(result, expect);
    }
}
