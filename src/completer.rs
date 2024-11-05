use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, format, Display};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Mutex, RwLock};
use std::{fs, vec};

use dashmap::DashMap;
use log::trace;
use tower_lsp::lsp_types::{CompletionItem, CompletionParams, DidChangeTextDocumentParams};
use walkdir::WalkDir;

use crate::chumsky::prelude::*;
use crate::document::Document;
use crate::tree_ds::prelude::*;

pub(crate) type AutoTree<T> = Tree<AutomatedId, T>;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub(crate) struct ControlNode {
    pub(crate) define: ControlDefine,
    pub(crate) loc:    (usize, usize),
}
impl Display for ControlNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ControlNode {{ define: {}, loc: ({}, {}) }}", self.define, self.loc.0, self.loc.1)
    }
}

#[derive(Debug, Default)]
pub(crate) struct ControlDefine {
    pub(crate) name:      Arc<str>,
    pub(crate) extend:    Option<(Arc<str>, Arc<str>)>,
    pub(crate) type_n:    Mutex<Option<Arc<str>>>,
    pub(crate) variables: Mutex<std::collections::HashSet<Arc<str>>>,
}
impl Clone for ControlDefine {
    fn clone(&self) -> Self {
        ControlDefine {
            name:      Arc::clone(&self.name),
            extend:    Clone::clone(&self.extend),
            type_n:    Mutex::new(self.type_n.lock().unwrap().clone()),
            variables: Mutex::new(self.variables.lock().unwrap().clone()),
        }
    }
}
impl PartialEq for ControlDefine {
    fn eq(&self, other: &Self) -> bool {
        if self.name != other.name {
            return false;
        }
        if self.extend != other.extend {
            return false;
        }
        true
    }
}
impl Eq for ControlDefine {}

impl fmt::Display for ControlDefine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_n = self.type_n.lock().expect("Failed to lock type_n");
        let variables = self.variables.lock().expect("Failed to lock variables");
        let variables_str: String = format!("{:?}", variables);
        if variables_str.len() > 50 {
            write!(
                f,
                "ControlDefine({}, {:?}, {:?}, [{} ...])",
                self.name,
                self.extend,
                *type_n,
                &variables_str.as_str()[0..50]
            )
        } else {
            write!(
                f,
                "ControlDefine({}, {:?}, {:?}, [{}])",
                self.name, self.extend, *type_n, variables_str
            )
        }
    }
}
pub struct Completer {
    pub(crate) trees:       RwLock<HashMap<Arc<str>, AutoTree<ControlNode>>>,
    documents:              DashMap<u64, Document>,
    keyword:                HashSet<String>,
    vanilla_controls_tabel: HashMap<(Arc<str>, Arc<str>), ControlDefine>,
}

#[derive(Debug)]
pub(crate) struct BuildTreeContext {
    namespace:    Rc<str>,
    control_name: RefCell<Rc<str>>,
    last_node:    Cell<Option<AutomatedId>>,
    loc:          RefCell<(usize, usize)>,
    layer:        AtomicUsize,
}

#[derive(Debug)]
pub(crate) struct RecursiveSearchContext {
    type_n:    RefCell<Option<Arc<str>>>,
    variables: RefCell<std::collections::HashSet<Arc<str>>>,
    layer:     AtomicUsize,
}

impl RecursiveSearchContext {
    pub fn new() -> Self {
        RecursiveSearchContext {
            type_n:    RefCell::new(None),
            variables: RefCell::new(HashSet::new()),
            layer:     AtomicUsize::new(0),
        }
    }
}

fn find_namespace(tokens: &Vec<Token<'_>>) -> Option<String> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, token)| matches!(token, Token::Str(_, "namespace")))
        .and_then(|(index, _)| match tokens.get(index + 2) {
            Some(Token::Str(_, s)) => Some(String::from(*s)),
            _ => None,
        })
}

fn find_type(tokens: &Vec<Token<'_>>) -> Option<String> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, token)| matches!(token, Token::Str(_, "namespace")))
        .and_then(|(index, _)| match tokens.get(index + 2) {
            Some(Token::Str(_, s)) => Some(String::from(*s)),
            _ => None,
        })
}

fn split_control_name(name: &str, def_namespace: &str) -> (Rc<str>, Rc<str>, Rc<str>) {
    let mut part1 = "";
    let mut part2 = "";
    let mut part3 = "";
    let parts: Vec<&str> = name.split('@').collect();
    let namespace_parts = match parts.len() {
        2 => {
            part1 = parts[0];
            parts[1].split('.').collect()
        }
        1 => {
            part1 = parts[0];
            vec![]
        }
        _ => {
            vec![]
        }
    };
    match namespace_parts.len() {
        2 => {
            part2 = namespace_parts[0];
            part3 = namespace_parts[1];
        }
        1 => {
            part2 = def_namespace.as_ref();
            part3 = namespace_parts[0];
        }
        _ => {}
    }
    (Rc::from(part1), Rc::from(part2), Rc::from(part3))
}

fn join(arcs: &[Arc<str>]) -> String {
    arcs.iter().map(|s| s.as_ref()).collect::<Vec<&str>>().join("")
}

fn extract_prefix(input: &str) -> &str {
    match input.find('@') {
        Some(index) => &input[..index],
        None => input,
    }
}

impl Completer {
    pub fn new(
        keyword: HashSet<String>,
        vanilla_controls_tabel: HashMap<(Arc<str>, Arc<str>), ControlDefine>,
    ) -> Self {
        Completer {
            trees: RwLock::new(HashMap::new()),
            documents: DashMap::with_shard_amount(2),
            keyword,
            vanilla_controls_tabel,
        }
    }

    pub(crate) fn init(&self, workspace_folders: &PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                let tokenizer = crate::chumsky::parser::<'_>();
                let r = crate::chumsky::parse(tokenizer, content.as_ref());
                match r {
                    Ok(r) => {
                        if let Some((k, v)) = Self::handle_tokens(r) {
                            self.add_tree(k.as_str(), v);
                        }
                    }
                    Err(e) => {
                        trace!("error in new parser{:?}", e)
                    }
                }
            }
        }

        let guard = self.trees.read().unwrap();
        for (_, v) in guard.iter() {
            self.update_control_tree(v);
        }
    }

    pub(crate) async fn complete(
        &self,
        id: u64,
        lang: Arc<str>,
        param: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let doc = self.documents.get(&id);
        if let Some(doc) = doc {
            let input = doc.get_content().await;
            self.parse(input.as_ref());

            let pos = param.text_document_position.position;
            let index = doc.get_index_from_position(pos).await;

            let index_value;
            if let Some(index_v) = index {
                index_value = index_v;
            } else {
                trace!("cant get_index_from_position {:?}", pos);
                return None;
            }

            let char;
            {
                let input_char_index = index_value - 1;
                if let Some(cr) = doc.get_char(input_char_index).await {
                    char = cr;
                } else {
                    return None;
                }
            }

            let tree = self.trees.read().expect("get read lock in complete");
            if let Some(tr) = tree.get(doc.get_cache_namespace().as_ref()) {
                // crate::complete_helper::normal(&pos, index_value, char, lang, &doc, &tr)
            }
        }
        None
    }

    pub(crate) fn update_document(&self, id: u64, param: &DidChangeTextDocumentParams) {
        let doc = self.documents.get(&id);
        if let Some(doc) = doc {
            doc.apply_change(param);
        }
    }

    pub(crate) fn parse<'a>(&self, input: &'a str) {
        let tokenizer = crate::chumsky::parser::<'_>();
        let r = crate::chumsky::parse(tokenizer, input);
        match r {
            Ok(r) => {
                if let Some((k, v)) = Self::handle_tokens(r) {
                    self.update_control_tree(&v);
                    self.add_tree(k.as_str(), v);
                }
            }
            Err(e) => {
                trace!("error in new parser{:?}", e)
            }
        }
    }

    pub(crate) async fn did_open(&self, id: u64, content: Arc<str>) {
        let np = Document::get_namespace(content.clone());
        if let Some(np) = np {
            let r = self
                .documents
                .entry(id)
                .or_insert_with(|| Document::from(content));
            if !self.contain_tree(np.as_str()) {
                let input = r.get_content().await;
                self.parse(input.as_ref());
            }
        } else {
            trace!("cant find namespace when did_open.")
        }
    }

    pub(crate) fn did_rename(&self, o_id: u64, n_id: u64) {
        if let Some((_, v)) = self.documents.remove(&o_id) {
            self.documents.insert(n_id, v);
        }
    }

    pub(crate) fn did_close(&self, id: u64) {
        let r = self.documents.remove(&id);
        if let Some((_, v)) = r {
            self.del_tree(&v.get_cache_namespace());
        }
    }

    // pub(crate) async fn input(pos: (usize, usize), doc: &Document) -> String {
    //     let chars = doc.content_chars.lock().await;
    //     let (l, r) = pos;
    //     join(&chars[l..r + 1])
    // }

    pub(crate) fn contain_tree(&self, name: &str) -> bool {
        self.trees
            .read()
            .expect("cant get read lock in contain_tree")
            .contains_key(name)
    }

    pub(crate) fn add_tree(&self, k: &str, v: Tree<AutomatedId, ControlNode>) {
        let mut t_lock = self.trees.write().expect("cant get write lock in add_tree");
        t_lock.insert(Arc::from(k), v);
    }

    pub(crate) fn del_tree(&self, k: &str) {
        let mut t_lock = self.trees.write().expect("cant get write lock in add_tree");
        t_lock.remove(k);
    }

    fn handle_tokens(
        result: ((usize, usize), Vec<Token<'_>>),
    ) -> Option<(String, Tree<AutomatedId, ControlNode>)> {
        let (root_span, tokens) = result;
        let np = find_namespace(&tokens);
        if let Some(np) = np {
            let mut tr: Tree<AutomatedId, ControlNode> =
                AutoTree::<ControlNode>::new(Option::Some(np.as_str()));
            let ctx = BuildTreeContext {
                namespace:    Rc::from(np.as_ref()),
                control_name: RefCell::new(Rc::from("()")),
                last_node:    Cell::new(None),
                loc:          RefCell::new((0, 0)),
                layer:        AtomicUsize::new(0),
            };
            let root = ControlNode {
                define: ControlDefine {
                    name:      Arc::from("root"),
                    extend:    None,
                    type_n:    Mutex::new(None),
                    variables: Mutex::new(std::collections::HashSet::<Arc<str>>::new()),
                },
                loc:    root_span,
            };
            Self::add_tree_node(&mut tr, &ctx, root, None);
            Self::build_control_tree(&mut tr, &tokens, &ctx);
            Some((np, tr))
        } else {
            trace!("cant find namespace from tokens {:?}", &format!("{:?}", tokens)[..100]);
            None
        }
    }

    fn build_control_tree(
        tr: &mut Tree<AutomatedId, ControlNode>,
        tokens: &Vec<Token<'_>>,
        ctx: &BuildTreeContext,
    ) {
        let layer = ctx.layer.load(std::sync::atomic::Ordering::Acquire);
        if layer > 50 {
            panic!("too deep layer {} for build_control_tree, more than 50", layer);
        }
        let m = to_map_with_span_ref(tokens);
        if ctx.control_name.borrow().as_ref() != "()" {
            let (name, np, extend) =
                split_control_name(ctx.control_name.borrow().as_ref(), ctx.namespace.as_ref());
            let mut type_n = None;
            let mut variables = std::collections::HashSet::<Arc<str>>::new();
            for (k, v) in m {
                if k == "type" {
                    type_n = Some(to_string_ref(v.1));
                } else if k.starts_with("$") {
                    variables.insert(Arc::from(k.replace("|default", "").as_str()));
                }
            }
            let type_n = type_n.unwrap_or("".to_string());
            let node = ControlNode {
                define: ControlDefine {
                    name:      Arc::from(name.as_ref()),
                    extend:    Some((Arc::from(np.as_ref()), Arc::from(extend.as_ref()))),
                    type_n:    Mutex::new(Some(Arc::from(type_n.as_ref()))),
                    variables: Mutex::new(variables),
                },
                loc:    ctx.loc.take(),
            };
            Self::add_tree_node(tr, ctx, node, ctx.last_node.get().as_ref());
            ctx.control_name.replace(Rc::from("()"));
        } else {
            for (k, v) in m {
                match v.1 {
                    Token::Object(_, value) => {
                        ctx.control_name.replace(Rc::from(k));
                        ctx.loc.replace(v.0);
                        ctx.layer.fetch_add(1, std::sync::atomic::Ordering::Acquire);
                        Self::build_control_tree(tr, value, ctx);
                        Self::pop_tree(tr, ctx);
                    }
                    Token::Array(_, value) => {
                        for i in value {
                            if let Token::Object(_, value) = i {
                                ctx.control_name.replace(Rc::from("()"));
                                ctx.layer.fetch_add(1, std::sync::atomic::Ordering::Acquire);
                                Self::build_control_tree(tr, value, ctx);
                                Self::pop_tree(tr, ctx);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn recursive_search(&self, extend: &(Arc<str>, Arc<str>), ctx: &RecursiveSearchContext) {
        let layer = ctx.layer.load(std::sync::atomic::Ordering::Acquire);
        if layer > 50 {
            panic!("too deep recursive layer {}, more than 50", layer);
        }
        let trees = self.trees.read().unwrap();
        let parent = trees.get(&extend.0);
        if let Some(tr) = parent {
            let root = tr.get_root_node().unwrap();
            for i in root.get_children_ids() {
                let n = tr.get_node_by_id(&i).unwrap();
                let v = n.get_value().unwrap();
                if v.define.name != extend.1 {
                    continue;
                }
                {
                    let type_n = v.define.type_n.lock().expect("type_n lock error");
                    if ctx.type_n.borrow().is_none() && type_n.is_some() {
                        ctx.type_n.replace(type_n.clone());
                    }
                }
                {
                    let variables = v.define.variables.lock().expect("type_n lock error");
                    if !variables.is_empty() {
                        ctx.variables.borrow_mut().extend(variables.clone());
                    }
                }
                if let Some(ref extend) = v.define.extend {
                    ctx.layer.fetch_add(1, std::sync::atomic::Ordering::Acquire);
                    self.recursive_search(extend, ctx);
                }
            }
        } else {
            let r = self.vanilla_controls_tabel.get(&extend);
            if let Some(r) = r {
                {
                    let type_n = r.type_n.lock().expect("type_n lock error");
                    if ctx.type_n.borrow().is_none() && type_n.is_some() {
                        ctx.type_n.replace(type_n.clone());
                    }
                }
                {
                    let variables = r.variables.lock().expect("type_n lock error");
                    if !variables.is_empty() {
                        ctx.variables.borrow_mut().extend(variables.clone());
                    }
                }
            }
        }
    }

    fn update_control_tree(&self, tr: &Tree<AutomatedId, ControlNode>) {
        let root = tr.get_root_node().expect("cant find root node!");
        let r = tr.traverse(&root.get_node_id(), TraversalStrategy::PreOrder);
        match r {
            Ok(r) => {
                for i in r {
                    let n = tr.get_node_by_id(&i);
                    if let Some(n) = n {
                        let v = n.get_value().expect("the value of tree node is empty.");
                        let def = &v.define;
                        if let Some(ref extend) = def.extend {
                            let ctx = RecursiveSearchContext::new();
                            self.recursive_search(extend, &ctx);
                            if let Some(type_n) = ctx.type_n.take() {
                                let mut now_type_n = def.type_n.lock().expect("cant lock type_n");
                                *now_type_n = Some(type_n);
                            }
                            {
                                let mut now_vars = def.variables.lock().expect("cant lock variables");
                                now_vars.extend(ctx.variables.take());
                            }
                        }
                        n.set_value(Some(v));
                    }
                }
            }
            Err(e) => {
                trace!("error in update_control_tree detail {}", e);
            }
        }
    }

    fn pop_tree(tr: &mut Tree<AutomatedId, ControlNode>, ctx: &BuildTreeContext) {
        if let Some(last_node) = ctx.last_node.get() {
            let r = tr.get_node_by_id(&last_node);
            if let Some(r) = r {
                ctx.last_node.replace(r.get_parent_id());
            }
        }
    }

    fn add_tree_node(
        tr: &mut Tree<AutomatedId, ControlNode>,
        ctx: &BuildTreeContext,
        node: ControlNode,
        id: Option<&AutomatedId>,
    ) {
        let r = tr.add_node(Node::<AutomatedId, ControlNode>::new_auto(Some(node)), id);
        match r {
            Ok(r) => {
                ctx.last_node.replace(Some(r));
            }
            Err(e) => {
                println!("error in add node for tree,detail {}", e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_control_name() {
        let r = split_control_name("empty_progress_bar_icon", "achievement");
        assert_eq!((Rc::from("empty_progress_bar_icon"), Rc::from(""), Rc::from("")), r);
        let r = split_control_name("empty_progress_bar_icon@test.cc", "achievement");
        assert_eq!((Rc::from("empty_progress_bar_icon"), Rc::from("test"), Rc::from("cc")), r);
        let r = split_control_name("empty_progress_bar_icon@cc", "achievement");
        assert_eq!((Rc::from("empty_progress_bar_icon"), Rc::from("achievement"), Rc::from("cc")), r);
    }

    #[cfg(test)]
    #[test]
    fn test_completer_init() {
        let p = crate::load_completer();
        let path = PathBuf::from("test");
        p.init(&path);
        assert!(p.contain_tree("achievement"));
        assert!(p.contain_tree("add_external_server"));
        let trees = p.trees.read().unwrap();
        for (_, v) in trees.iter() {
            println!("tree {} \n------------", v);
        }
    }
}
