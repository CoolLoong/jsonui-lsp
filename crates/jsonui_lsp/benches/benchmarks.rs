use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use jsonui_lsp::completer::indexer::SymbolIndexer;
use jsonui_lsp::load_vanilla_controls_table;
use std::hint::black_box;
use tower_lsp::lsp_types::Url;

fn bench_document_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("document_parsing");

    // Small document
    let small_doc = r#"{
        "namespace": "test",
        "test_control": {
            "type": "panel"
        }
    }"#;

    // Medium document
    let medium_doc = r#"{
        "namespace": "test",
        "control_1": { "type": "panel", "size": [100, 100] },
        "control_2": { "type": "label", "text": "Hello" },
        "control_3": { "type": "button", "controls": [] }
    }"#;

    // Large document
    let large_doc = r##"{
        "namespace": "achievement",
        "main_panel": {
            "type": "panel",
            "size": [400, 300],
            "controls": [
                { "control_1@main_panel.button_panel": {} },
                { "control_2@main_panel.list_panel": {} }
            ]
        },
        "button_panel": {
            "type": "panel",
            "variables": [
                { "requires": "$button_state", "$button_enabled": true },
                { "requires": "$button_visible", "$button_visible": false }
            ],
            "bindings": [
                { "binding_name": "#button_text", "binding_type": "global" }
            ]
        },
        "list_panel": {
            "type": "stack_panel",
            "orientation": "vertical",
            "controls": [
                { "item_1@achievement.item_renderer": {} },
                { "item_2@achievement.item_renderer": {} },
                { "item_3@achievement.item_renderer": {} }
            ]
        },
        "item_renderer": {
            "type": "panel",
            "size": [200, 50],
            "controls": [
                { "icon@common.icon": {} },
                { "label@common.label": {} }
            ],
            "color": [0.5, 0.5, 0.5, 1.0],
            "variables": [
                { "requires": "$item_selected", "$highlight_color": [1.0, 1.0, 0.0, 1.0] }
            ]
        }
    }"##;

    let rt = tokio::runtime::Runtime::new().unwrap();

    let vanilla_table = load_vanilla_controls_table();

    group.bench_with_input(BenchmarkId::new("small", "100 bytes"), &small_doc, |b, doc| {
        let vanilla_table = vanilla_table.clone();
        b.to_async(&rt).iter(|| async {
            let indexer = SymbolIndexer::new(vanilla_table.clone());
            let url = Url::parse("file:///test1.json").unwrap();
            indexer.did_open(&url, doc).await;
            black_box(())
        });
    });

    group.bench_with_input(BenchmarkId::new("medium", "500 bytes"), &medium_doc, |b, doc| {
        let vanilla_table = vanilla_table.clone();
        b.to_async(&rt).iter(|| async {
            let indexer = SymbolIndexer::new(vanilla_table.clone());
            let url = Url::parse("file:///test2.json").unwrap();
            indexer.did_open(&url, doc).await;
            black_box(())
        });
    });

    group.bench_with_input(BenchmarkId::new("large", "1500 bytes"), &large_doc, |b, doc| {
        let vanilla_table = vanilla_table.clone();
        b.to_async(&rt).iter(|| async {
            let indexer = SymbolIndexer::new(vanilla_table.clone());
            let url = Url::parse("file:///test3.json").unwrap();
            indexer.did_open(&url, doc).await;
            black_box(())
        });
    });

    group.finish();
}

criterion_group!(benches, bench_document_parsing);
criterion_main!(benches);
