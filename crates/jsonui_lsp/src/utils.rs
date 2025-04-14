use std::hash::{BuildHasher, Hash};
use std::sync::OnceLock;

use tower_lsp::lsp_types::Url;

use crate::museair::BfastHash;

static HASHER: OnceLock<BfastHash<true>> = OnceLock::new();
pub(crate) fn hash_url(url: &Url) -> u64 {
    let hasher = HASHER.get_or_init(|| BfastHash::<true>::new());
    let mut r = hasher.build_hasher();
    let path = normalize_url(url);
    path.hash(&mut r);
    r.finish()
}

fn normalize_url(url: &Url) -> String {
    let mut url_str = url.to_string();

    if url_str.starts_with("file:///") {
        if let Some(rest) = url_str.strip_prefix("file:///") {
            if let Some((drive, path)) = rest.split_once('/') {
                let normalized_drive = if drive.contains('%') {
                    drive.to_lowercase().replace("%3a", ":")
                } else {
                    drive.to_lowercase()
                };
                url_str = format!("file:///{}/{}", normalized_drive, path);
            }
        }
    }
    url_str
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_url() {
        let url_str = "file:///C/Users/xxxx/Downloads/7788/json/achievement.json";
        let url = Url::parse(url_str).unwrap();
        let hash1 = hash_url(&url);
        let hash2 = hash_url(&url);
        assert_eq!(hash1, hash2, "Same URL should produce same hash");

        let other_url = Url::parse("file:///other/path.json").unwrap();
        let other_hash = hash_url(&other_url);
        assert_ne!(hash1, other_hash, "Different URLs should produce different hashes");
    }
}
