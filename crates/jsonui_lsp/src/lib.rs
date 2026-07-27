pub mod completer;
pub mod config;
pub mod document_manager;
pub mod museair;
pub mod navigation_state;
pub mod parser;
pub mod utils;

use std::sync::Arc;
use parser::DocumentParser;
use parser::Value;

/// Load vanilla controls table from embedded resource
pub fn load_vanilla_controls_table()
-> Arc<museair::BfastHashMap<(Arc<str>, Arc<str>), completer::types::VanillaControlDefine>>
{
    const VANILLA_PACK_DEFINE: &str = include_str!("../resources/vanillapack_define_1.26.30.5.json");

    let parser = DocumentParser::default(VANILLA_PACK_DEFINE);

    let mut result = museair::BfastHashMap::<
        (Arc<str>, Arc<str>),
        completer::types::VanillaControlDefine,
    >::default();
    let map = parser.hashmap();

    for (k1, v1) in map {
        if let Value::Object(map2) = v1 {
            for (k2, v2) in map2 {
                let k1_spur = Arc::from(k1.as_str());
                let k2_spur = Arc::from(k2.as_str());
                let tuple = (k1_spur, k2_spur);

                let spurs = if let Value::Object(map3) = v2 {
                    let type_spur = if let Some(Value::String(v)) = map3.get("type") {
                        Arc::from(v.as_str())
                    } else {
                        Arc::from("")
                    };

                    let variables_spur = if let Some(Value::Array(v)) = map3.get("variables") {
                        let mut r = std::collections::HashSet::default();
                        for i in v
                            .iter()
                            .filter_map(|v| if let Value::String(s) = v { Some(s) } else { None })
                        {
                            r.insert(Arc::from(i.as_str()));
                        }
                        r
                    } else {
                        std::collections::HashSet::default()
                    };
                    (type_spur, variables_spur)
                } else {
                    (Arc::from(""), std::collections::HashSet::default())
                };

                result.insert(
                    tuple.clone(),
                    completer::types::VanillaControlDefine {
                        name: (tuple.0, tuple.1, None),
                        type_n: spurs.0,
                        variables: spurs.1,
                    },
                );
            }
        }
    }

    Arc::new(result)
}