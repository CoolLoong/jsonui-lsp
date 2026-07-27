use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::env;
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use jsonc_parser::parse_to_serde_value;
use serde_json::{Value, json};
use walkdir::WalkDir;

fn main() -> io::Result<()> {
    let (path, version) = parse_args(env::args_os())?;
    let namespace_map: Rc<RefCell<HashMap<String, Value>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut result: HashMap<String, HashMap<String, serde_json::Value>> = HashMap::new();

    for entry in WalkDir::new(&path)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "json"))
        .filter(|e| !e.path().ends_with("_global_variables.json"))
        .filter(|e| !e.path().ends_with("_ui_defs.json"))
    {
        if let Err(e) = process_file(entry.path(), &mut namespace_map.borrow_mut()) {
            eprintln!("Error processing file {}: {}", entry.path().display(), e);
        }
    }

    let map: &RefCell<HashMap<String, Value>> = namespace_map.borrow();
    for (k, v) in map.borrow().iter() {
        let mut export_map: HashMap<String, serde_json::Value> = HashMap::new();
        let mut resolving = HashSet::new();
        process_properties(None, k, v, &mut export_map, &map.borrow(), &mut resolving);
        result.insert(k.clone(), export_map);
    }

    let output_path = format!("crates/jsonui_lsp/resources/vanillapack_define_{version}.json");
    let output_dir = Path::new(output_path.as_str())
        .parent()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid output path"))?;
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)?;
    }
    let output_file = match File::create(output_path.clone()) {
        Ok(file) => file,
        Err(e) => {
            println!("Failed to create file: {:?}", e);
            return Err(e);
        }
    };
    if let Err(e) = serde_json::to_writer_pretty(output_file, &result) {
        println!("Failed to write JSON to file: {:?}", e);
    }
    println!("Spawn output path: {}", output_path);
    Ok(())
}

fn parse_args(args: impl IntoIterator<Item = OsString>) -> io::Result<(PathBuf, String)> {
    let mut args = args.into_iter();
    let _program = args.next();
    let path = args.next().ok_or_else(usage_error)?;
    let version = args.next().ok_or_else(usage_error)?;
    if args.next().is_some() {
        return Err(usage_error());
    }

    let path = PathBuf::from(path);
    if !path.is_dir() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("UI path is not a directory: {}", path.display()),
        ));
    }

    let version = version
        .into_string()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "Version must be valid UTF-8"))?;
    if !is_valid_version(&version) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Version must contain three or four numeric components",
        ));
    }

    Ok((path, version))
}

fn usage_error() -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, "Usage: vanillapack_define_gen <ui-directory> <version>")
}

fn is_valid_version(version: &str) -> bool {
    let mut count = 0;
    for component in version.split('.') {
        if component.is_empty() || !component.bytes().all(|byte| byte.is_ascii_digit()) {
            return false;
        }
        count += 1;
    }
    count == 3 || count == 4
}

fn process_file(path: &Path, namespace_map: &mut HashMap<String, Value>) -> io::Result<()> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let value: Value = parse_to_serde_value(content.as_str(), &Default::default())
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Parse error: {:?}", e)))?
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "No value parsed"))?;
    if let Some(namespace) = value.get("namespace").and_then(Value::as_str) {
        namespace_map.insert(namespace.to_string(), value);
    }
    Ok(())
}

fn process_properties(
    name: Option<&str>,
    namespace: &str,
    properties: &Value,
    export_map: &mut HashMap<String, serde_json::Value>,
    namespace_map: &HashMap<String, Value>,
    resolving: &mut HashSet<(String, String)>,
) {
    let Some(properties_obj) = properties.as_object() else {
        return;
    };
    for (key, value) in properties_obj {
        let split_key: Vec<&str> = key.split('@').collect();
        let np = name.unwrap_or(split_key[0]).to_string();

        if let Value::Object(map) = value {
            for map_key in map.keys() {
                update_export_map(export_map, &np, map_key.replace("|default", ""));
            }
        }

        if let Some(type_value) = value.get("type").and_then(Value::as_str) {
            set_export_type(export_map, &np, type_value);
        }

        if key.contains('@') {
            handle_namespace(key, namespace, namespace_map, name, export_map, resolving);
        } else if key == "type" {
            if let Some(type_str) = value.as_str() {
                set_export_type(export_map, &np, type_str);
            }
        } else if key.starts_with('$') {
            update_export_map(export_map, &np, key.replace("|default", ""));
        }
    }
}

fn update_export_map(export_map: &mut HashMap<String, serde_json::Value>, key: &str, variable: String) {
    if variable.starts_with("$") {
        let entry = export_map.entry(key.to_string()).or_insert_with(|| json!({}));
        if let Value::Object(map) = entry
            && let Some(variables) = map.entry("variables").or_insert_with(|| json!([])).as_array_mut()
        {
            variables.push(json!(variable));
        }
    }
}

fn set_export_type(export_map: &mut HashMap<String, serde_json::Value>, key: &str, type_value: &str) {
    let entry = export_map.entry(key.to_string()).or_insert_with(|| json!({}));
    if let Value::Object(map) = entry {
        map.insert("type".to_string(), json!(type_value.to_string()));
    }
}

fn handle_namespace(
    key: &str,
    namespace: &str,
    namespace_map: &HashMap<String, Value>,
    name: Option<&str>,
    export_map: &mut HashMap<String, serde_json::Value>,
    resolving: &mut HashSet<(String, String)>,
) {
    let parts: Vec<&str> = key.split('@').collect();
    if parts.len() == 2 {
        let rest = parts[1];
        let parts_namespace: Vec<&str> = rest.split('.').collect();
        let (np, cn) = if parts_namespace.len() == 2 {
            (parts_namespace[0], parts_namespace[1])
        } else {
            (namespace.as_ref(), parts_namespace[0])
        };
        if let Some(namespace_object) = namespace_map.get(np)
            && let Some(ns_properties) = namespace_object.as_object()
        {
            for (kk, v) in ns_properties {
                if extract_prefix(kk) == cn {
                    let reference = (np.to_owned(), cn.to_owned());
                    if resolving.insert(reference.clone()) {
                        let next_name = name.or(Some(parts[0]));
                        if let Ok(v_str) = serde_json::to_string(v) {
                            let json = format!("{{ \"{}\": {} }}", kk, v_str);
                            if let Ok(target) = serde_json::from_str(json.as_str()) {
                                process_properties(
                                    next_name,
                                    np,
                                    &target,
                                    export_map,
                                    namespace_map,
                                    resolving,
                                );
                            }
                        }
                        resolving.remove(&reference);
                    }
                    break;
                }
            }
        }
    }
}

fn extract_prefix(input: &str) -> &str {
    match input.find('@') {
        Some(index) => &input[..index],
        None => input,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};
    use std::env;
    use std::ffi::OsString;

    use serde_json::json;

    use super::{is_valid_version, parse_args, process_properties};

    #[test]
    fn validates_numeric_versions() {
        assert!(is_valid_version("1.21.120"));
        assert!(is_valid_version("1.21.120.4"));
        assert!(!is_valid_version("v1.21.120.4"));
        assert!(!is_valid_version("1.21"));
        assert!(!is_valid_version("1.21.120.4.1"));
        assert!(!is_valid_version("1.21.preview.4"));
        assert!(!is_valid_version("1.21..4"));
    }

    #[test]
    fn requires_exactly_two_arguments() {
        let path = env::current_dir().unwrap().into_os_string();
        assert!(
            parse_args([
                OsString::from("vanillapack_define_gen"),
                path.clone(),
                OsString::from("1.21.120.4"),
            ])
            .is_ok()
        );
        assert!(parse_args([OsString::from("vanillapack_define_gen")]).is_err());
        assert!(
            parse_args([
                OsString::from("vanillapack_define_gen"),
                path.clone(),
                OsString::from("1.21.120.4"),
                OsString::from("extra"),
            ])
            .is_err()
        );
        assert!(
            parse_args([
                OsString::from("vanillapack_define_gen"),
                path,
                OsString::from("preview"),
            ])
            .is_err()
        );
    }

    #[test]
    fn rejects_non_directory_path() {
        assert!(
            parse_args([
                OsString::from("vanillapack_define_gen"),
                env::current_exe().unwrap().into_os_string(),
                OsString::from("1.21.120.4"),
            ])
            .is_err()
        );
    }

    #[test]
    fn skips_cyclic_namespace_references() {
        let namespace_map = HashMap::from([(
            "test".to_string(),
            json!({
                "namespace": "test",
                "widget": {
                    "type": "panel",
                    "loop@test.widget": {}
                }
            }),
        )]);
        let mut export_map = HashMap::new();
        let mut resolving = HashSet::new();

        process_properties(
            None,
            "test",
            &json!({ "entry@test.widget": {} }),
            &mut export_map,
            &namespace_map,
            &mut resolving,
        );

        assert_eq!(export_map["entry"]["type"], "panel");
    }
}
