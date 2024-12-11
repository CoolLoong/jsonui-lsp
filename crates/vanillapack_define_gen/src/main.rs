use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::Path;
use std::rc::Rc;

use jsonc_parser::parse_to_serde_value;
use serde_json::{json, Value};
use walkdir::WalkDir;

const VERSION: &str = "1.21.50.7";

fn main() -> io::Result<()> {
    let namespace_map: Rc<RefCell<HashMap<String, Value>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut result: HashMap<String, HashMap<String, serde_json::Value>> = HashMap::new();

    print!("please input vanilla pack path: ");
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let path = input.trim();

    if !Path::new(path).is_dir() {
        eprintln!("Err: '{}' not is valid path", path);
        return Ok(());
    }

    for entry in WalkDir::new(path)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
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
        process_properties(None, &k.clone(), v, &mut export_map, &map.borrow());
        result.insert(k.clone(), export_map);
    }

    let output_path = format!("crates/jsonui_lsp/src/resources/vanillapack_define_{}.json", VERSION);
    let output_dir = Path::new(output_path.as_str()).parent().unwrap();
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

fn process_file(path: &Path, namespace_map: &mut HashMap<String, Value>) -> io::Result<()> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let value: Value = parse_to_serde_value(content.as_str(), &Default::default())
        .unwrap()
        .unwrap();
    if let Some(namespace) = value.get("namespace").and_then(Value::as_str) {
        namespace_map.insert(namespace.to_string(), value);
    }
    Ok(())
}

fn process_properties(
    name: Option<&str>,
    namespace: &String,
    properties: &Value,
    export_map: &mut HashMap<String, serde_json::Value>,
    namespace_map: &HashMap<String, Value>,
) {
    for (key, value) in properties.as_object().unwrap() {
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
            handle_namespace(key, namespace, namespace_map, name, export_map);
        } else if key == "type" {
            set_export_type(export_map, &np, value.as_str().unwrap());
        } else if key.starts_with('$') {
            update_export_map(export_map, &np, key.replace("|default", ""));
        }
    }
}

fn update_export_map(export_map: &mut HashMap<String, serde_json::Value>, key: &str, variable: String) {
    if variable.starts_with("$") {
        let entry = export_map.entry(key.to_string()).or_insert_with(|| json!({}));
        if let Value::Object(map) = entry {
            let variables = map
                .entry("variables")
                .or_insert_with(|| json!([]))
                .as_array_mut()
                .unwrap();
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
    namespace: &String,
    namespace_map: &HashMap<String, Value>,
    name: Option<&str>,
    export_map: &mut HashMap<String, serde_json::Value>,
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
        if let Some(namespace_object) = namespace_map.get(np) {
            if let Some(ns_properties) = namespace_object.as_object() {
                for (kk, v) in ns_properties {
                    if extract_prefix(kk) == cn {
                        let next_name = name.or(Some(parts[0]));
                        let json = format!("{{ \"{}\": {} }}", kk, serde_json::to_string(v).unwrap());
                        let target = serde_json::from_str(json.as_str()).unwrap();
                        process_properties(
                            next_name,
                            &np.to_string(),
                            &target,
                            export_map,
                            namespace_map,
                        );
                        break;
                    }
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
