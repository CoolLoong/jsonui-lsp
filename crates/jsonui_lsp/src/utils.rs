use tower_lsp::lsp_types::Url;

type Hasher = museair::impls::IncrementalHasher<true>;
pub(crate) fn hash_url(url: &Url) -> u64 {
    let path = normalize_url(url);
    let mut hasher = Hasher::default();
    hasher.write(path.as_bytes());
    hasher.finish()
}

fn normalize_url(url: &Url) -> String {
    let url_str = url.to_string();
    if url_str.starts_with("file:///") {
        if let Some(rest) = url_str.strip_prefix("file:///") {
            if let Some((drive, path)) = rest.split_once('/') {
                let normalized_drive = drive.to_lowercase().replace("%3a", ":");
                return format!("file:///{}/{}", normalized_drive, path);
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

    #[test]
    fn test_normalize_url() {
        let url1 = Url::parse("file:///d%3A/Download/ui/test.json").unwrap();
        let normalized1 = normalize_url(&url1);
        let url2 = Url::parse("file:///D:/Download/ui/test.json").unwrap();
        let normalized2 = normalize_url(&url2);
        assert_eq!(normalized1, normalized2, "Both URLs should normalize to the same format");
        assert_eq!(normalized1, "file:///d:/Download/ui/test.json");
    }
}
