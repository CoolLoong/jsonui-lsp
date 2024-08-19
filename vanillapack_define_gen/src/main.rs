use jsonc_parser::parse_to_serde_value;
use serde_json::Value;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::Path;
use std::rc::Rc;
use walkdir::WalkDir;

const VERSION: &str = "1.21.20.3";

fn main() -> io::Result<()> {
    let namespace_map: Rc<RefCell<HashMap<String, Value>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut result: HashMap<String, HashMap<String, String>> = HashMap::new();

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

    let map: &RefCell<HashMap<String, Value>>  = namespace_map.borrow();
    for (k, v) in map.borrow().iter() {
        let mut type_map: HashMap<String, String> = HashMap::new();
        process_properties(None, &v, &mut type_map, &map.borrow());
        result.insert(k.clone(), type_map);
    }

    let output_path = format!("out/vanillapack_define_{}.json", VERSION);
    let output_dir = Path::new(output_path.as_str()).parent().unwrap();
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)?;
    }
    let output_file = match File::create(output_path.clone()) {
        Ok(file) => file,
        Err(e) => {
            println!("Failed to create file: {:?}", e);
            return Err(e.into());
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
    properties: &Value,
    type_map: &mut HashMap<String, String>,
    namespace_map: &HashMap<String, Value>,
) {
    for (key, value) in properties.as_object().unwrap() {
        let sp: Vec<&str> = key.split('@').collect();
        if let Some(type_value) = value.get("type").and_then(Value::as_str) {
            type_map.insert(name.unwrap_or(sp[0]).to_string(), type_value.to_string());
        } else if key.contains('@') {
            let parts: Vec<&str> = key.split('@').collect();
            if parts.len() == 2 {
                let rest = parts[1];
                let parts: Vec<&str> = rest.split('.').collect();
                if parts.len() == 2 {
                    let namespace = parts[0];
                    let control_name = parts[1];
                    if let Some(namespace_object) = namespace_map.get(namespace) {
                        if let Some(properties) = namespace_object.as_object() {
                            for (kk, vv) in properties {
                                if extract_prefix(kk) == control_name {
                                    let next = if name.is_none() { Some(sp[0]) } else { name };
                                    process_properties(next, vv, type_map, namespace_map);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        } else if key == "type" {
            type_map.insert(
                name.unwrap_or(sp[0]).to_string(),
                value.as_str().unwrap().to_string(),
            );
        }
    }
}

fn extract_prefix(input: &str) -> &str {
    match input.find('@') {
        Some(index) => &input[..index],
        None => input,
    }
}
