use std::cell::{Cell, RefCell};
use std::sync::Arc;

use tower_lsp::lsp_types::{CompletionItem, Position};

use crate::completer::{AutoTree, ControlNode};
use crate::document::Document;
use crate::tree_ds::prelude::{AutomatedId, Node};

pub(crate) struct NormalCompletionParam {}

pub(crate) fn normal(
    pos: &Position,
    index: usize,
    char: Arc<str>,
    lang: Arc<str>,
    doc: &Document,
    tree: &AutoTree<ControlNode>,
) -> Option<Vec<CompletionItem>> {
    // let root = tree.get_root_node().expect("cant get root node");
    // let r = find_current_node(tree, &root, index);
    // if let Some(r) = r{
    //     tree.get_node_by_id(r)
    // }
    None
}

fn find_closest_node(
    tree: &AutoTree<ControlNode>,
    n: &Node<AutomatedId, ControlNode>,
    index: usize,
) -> Option<AutomatedId> {
    let mut closest_node = None;
    let mut min_distance = usize::MAX;

    fn search(
        tree: &AutoTree<ControlNode>,
        n: &Node<AutomatedId, ControlNode>,
        index: usize,
        closest_node: &mut Option<AutomatedId>,
        min_distance: &mut usize,
    ) {
        if let Some(ref v) = n.get_value() {
            let (l, r) = v.loc;
            let distance = index.abs_diff(l) + r.abs_diff(index);

            if distance < *min_distance {
                *min_distance = distance;
                *closest_node = Some(n.get_node_id());
            }

            if index >= l {
                for i in n.get_children_ids() {
                    if let Some(child_node) = tree.get_node_by_id(&i) {
                        search(tree, &child_node, index, closest_node, min_distance);
                    }
                }
            }
        }
    }

    search(tree, n, index, &mut closest_node, &mut min_distance);
    closest_node
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    #[cfg(test)]
    #[test]
    fn test_completer_init() {
        use crate::complete_helper::find_closest_node;

        let p = crate::load_completer();
        let path = PathBuf::from("test");
        p.init(&path);
        let trees = p.trees.read().unwrap();
        let r = trees.get("add_external_server");
        let r = r.unwrap();
        println!("{}", r);
        let cr = find_closest_node(r, &r.get_root_node().unwrap(), 1000);
        let cr = cr.expect("cant find closest node");
        println!("{}", r.get_node_by_id(&cr).unwrap());
    }
}
