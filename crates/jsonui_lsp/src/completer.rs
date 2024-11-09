use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Mutex, RwLock};
use std::{fs, vec};

use dashmap::DashMap;
use lasso::Spur;
use log::trace;
use walkdir::WalkDir;

use crate::document::Document;
use crate::lexer::prelude::*;
use crate::museair::{BfastDashMap, BfastHashMap, BfastHashSet};
use crate::tower_lsp::*;
use crate::tree_ds::prelude::*;
use crate::StdMutex;

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
impl ControlNode {
    pub(crate) fn get_type(&self) -> Option<String> {
        let lock = self.define.type_n.lock().unwrap();
        lock.as_ref().map(|f| f.to_string())
    }
}

pub(crate) struct PooledControlDefine {
    pub(crate) name:      Spur,
    pub(crate) extend:    Option<(Spur, Spur)>,
    pub(crate) type_n:    Option<Spur>,
    pub(crate) variables: BfastHashSet<Spur>,
}
impl PooledControlDefine {
    pub(crate) fn to(&self, resolver: &lasso::RodeoResolver) -> ControlDefine {
        let name = Arc::from(resolver.resolve(&self.name));
        let extend = if let Some((v1, v2)) = &self.extend {
            Some((Arc::from(resolver.resolve(v1)), Arc::from(resolver.resolve(v2))))
        } else {
            None
        };
        let type_n = self.type_n.as_ref().map(|v| Arc::from(resolver.resolve(v)));
        let variables: BfastHashSet<Arc<str>> = self
            .variables
            .iter()
            .map(|f| Arc::from(resolver.resolve(f)))
            .collect();
        ControlDefine {
            name,
            extend,
            type_n: StdMutex::new(type_n),
            variables: StdMutex::new(variables),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct ControlDefine {
    pub(crate) name:      Arc<str>,
    pub(crate) extend:    Option<(Arc<str>, Arc<str>)>,
    pub(crate) type_n:    Mutex<Option<Arc<str>>>,
    pub(crate) variables: Mutex<BfastHashSet<Arc<str>>>,
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
impl Display for ControlDefine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_n = self.type_n.lock().expect("Failed to lock type_n");
        let variables = self.variables.lock().expect("Failed to lock variables");
        let variables_str: String = format!("{:?}", variables);
        if variables_str.len() > 50 {
            write!(
                f,
                "ControlDefine(name: {}, {:?}, type: {:?}, [{} ...])",
                self.name,
                self.extend,
                *type_n,
                &variables_str.as_str()[0..50]
            )
        } else {
            write!(
                f,
                "ControlDefine(name: {}, {:?}, type: {:?}, [{}])",
                self.name, self.extend, *type_n, variables_str
            )
        }
    }
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
    variables: RefCell<BfastHashSet<Arc<str>>>,
    layer:     AtomicUsize,
}
impl RecursiveSearchContext {
    pub fn new() -> Self {
        RecursiveSearchContext {
            type_n:    RefCell::new(None),
            variables: RefCell::new(BfastHashSet::default()),
            layer:     AtomicUsize::new(0),
        }
    }
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    name: Arc<str>,
    definition: Location,
    references: Vec<Location>,
}

pub struct Completer {
    pub(crate) trees: RwLock<BfastHashMap<Arc<str>, AutoTree<ControlNode>>>,
    documents: BfastDashMap<Url, Document>,
    urls: BfastDashMap<Arc<str>, Url>,
    symbol_table: BfastDashMap<Arc<str>, SymbolInfo>,
    vanilla_controls_table: BfastHashMap<(Arc<str>, Arc<str>), ControlDefine>,
    jsonui_define: BfastHashMap<String, Token>,
}
impl Completer {
    pub fn new(
        vanilla_controls_table: BfastHashMap<(Arc<str>, Arc<str>), ControlDefine>,
        jsonui_define: BfastHashMap<String, Token>,
    ) -> Self {
        Completer {
            trees: RwLock::new(BfastHashMap::default()),
            documents: DashMap::with_hasher_and_shard_amount(
                crate::museair::BfastHash::<true>::new(),
                2,
            ),
            urls: DashMap::with_hasher_and_shard_amount(crate::museair::BfastHash::<true>::new(), 2),
            symbol_table: DashMap::with_hasher_and_shard_amount(
                crate::museair::BfastHash::<true>::new(),
                2,
            ),
            vanilla_controls_table,
            jsonui_define,
        }
    }

    pub(crate) async fn init(&self, workspace_folders: &PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                let r = parse_full(content.as_ref()).await;
                match r {
                    Some(r) => {
                        if let Some((k, v)) = Self::build_tree(&r) {
                            self.add_tree(k.as_str(), v);
                        } else {
                            trace!("Failed to build tree: {:?}", entry.path())
                        }
                    }
                    None => {
                        trace!("error in new parser")
                    }
                }
            } else {
                trace!("Failed to read content {:?}", entry.path());
            }
        }
    }

    pub(crate) async fn complete(
        &self,
        url: &Url,
        lang: Arc<str>,
        param: &CompletionParams,
    ) -> Option<Vec<CompletionItem>> {
        let doc = self.documents.get(url);
        if let Some(doc) = doc {
            let input = doc.get_content().await;
            let pos = param.text_document_position.position;
            let r = parse_full(input.as_ref()).await;
            match r {
                Some(r) => {
                    if let Some((k, v)) = Self::build_tree(&r) {
                        let index = doc.get_index_from_position(pos).await;
                        let index_value;
                        if let Some(index_v) = index {
                            index_value = index_v;
                        } else {
                            trace!("cant get_index_from_position {:?}", pos);
                            return None;
                        }

                        let boundary_indices;
                        if let Some(bd) = doc.get_boundary_indices(index_value).await {
                            boundary_indices = bd;
                        } else {
                            trace!("cant get_boundary_indices {:?}", pos);
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
                        let tokens = r.1;
                        let r = crate::complete_helper::normal(
                            self,
                            index_value,
                            boundary_indices,
                            pos,
                            char,
                            lang,
                            &tokens,
                            &v,
                            &self.jsonui_define,
                        );
                        self.add_tree(k.as_str(), v);
                        return r;
                    }
                }
                None => {
                    trace!("error in new parser")
                }
            }
        }
        None
    }

    pub async fn complete_color(&self, url: &Url) -> Option<Vec<ColorInformation>> {
        let doc = self.documents.get(url);
        if let Some(doc) = doc {
            let input = doc.get_content().await;
            let r = parse_full(input.as_ref()).await;
            if let Some(r) = r {
                return crate::complete_helper::color(doc.borrow(), &r.1).await;
            }
        }
        None
    }

    pub(crate) async fn update_document(&self, url: &Url, param: &DidChangeTextDocumentParams) {
        let doc = self.documents.get(url);
        if let Some(doc) = doc {
            doc.apply_change(param).await;
        }
    }

    pub(crate) async fn did_open(&self, url: Url, content: Arc<str>) {
        let np = Document::get_namespace(content.clone());
        if let Some(np) = np {
            let r = self
                .documents
                .entry(url.clone())
                .or_insert_with(|| Document::from(content));
            self.urls.entry(Arc::from(np.as_str())).or_insert(url);
            if !self.contain_tree(np.as_str()) {
                let input = r.get_content().await;
                let r = parse_full(input.as_ref()).await;
                match r {
                    Some(r) => {
                        if let Some((k, v)) = Self::build_tree(&r) {
                            self.add_tree(k.as_str(), v);
                        }
                    }
                    None => {
                        trace!("error in new parser")
                    }
                }
            }
        } else {
            trace!("cant find namespace when did_open.")
        }
    }

    pub(crate) fn did_rename(&self, o_url: &Url, n_url: Url) {
        if let Some((_, v)) = self.documents.remove(o_url) {
            let namespace = &v.get_cache_namespace();
            self.urls.remove(namespace);
            self.urls.insert(namespace.clone(), n_url.clone());
            self.documents.insert(n_url, v);
        }
    }

    pub(crate) fn did_close(&self, url: &Url) {
        let r = self.documents.remove(url);
        if let Some((_, v)) = r {
            let namespace = &v.get_cache_namespace();
            self.urls.remove(namespace);
            self.del_tree(namespace);
        }
    }

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

    fn build_tree(
        result: &((usize, usize), Vec<Token>),
    ) -> Option<(String, Tree<AutomatedId, ControlNode>)> {
        let (root_span, ref tokens) = result;
        let np = find_namespace(tokens);
        if let Some(np) = np {
            let mut tr: Tree<AutomatedId, ControlNode> = AutoTree::<ControlNode>::new(Some(np.as_str()));
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
                    variables: Mutex::new(BfastHashSet::<Arc<str>>::default()),
                },
                loc:    *root_span,
            };
            Self::add_tree_node(&mut tr, &ctx, root, None);
            Self::build_control_tree(&mut tr, tokens, &ctx);
            Some((np, tr))
        } else {
            None
        }
    }

    fn build_control_tree(
        tr: &mut Tree<AutomatedId, ControlNode>,
        tokens: &Vec<Token>,
        ctx: &BuildTreeContext,
    ) {
        let layer = ctx.layer.load(std::sync::atomic::Ordering::Acquire);
        if layer > 100 {
            panic!("too deep layer {} for build_control_tree, more than 100", layer);
        }

        let m = to_map_ref(tokens);
        if ctx.control_name.borrow().as_ref() != "()" {
            let (name, np, extend) =
                split_control_name(ctx.control_name.borrow().as_ref(), ctx.namespace.as_ref());
            let mut type_n = None;
            let mut variables = BfastHashSet::<Arc<str>>::default();
            let mut arrays = vec![];

            for (k, v) in m {
                if k == "type" {
                    type_n = Some(to_string_ref(v));
                } else if k.starts_with("$") {
                    variables.insert(Arc::from(k.replace("|default", "").as_str()));
                }
                if matches!(v, Token::Array(_, _)) {
                    arrays.push(v);
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
            for i in arrays {
                if let Token::Array(_, vec) = i {
                    for i in vec.as_ref().unwrap() {
                        if let Token::Object(_, value) = i {
                            ctx.control_name.replace(Rc::from("()"));
                            ctx.layer.fetch_add(1, std::sync::atomic::Ordering::Acquire);
                            Self::build_control_tree(tr, value.as_ref().unwrap(), ctx);
                        }
                    }
                }
            }
            ctx.control_name.replace(Rc::from("()"));
        } else {
            for (k, v) in m {
                if let Token::Object(r, value) = v {
                    ctx.control_name.replace(Rc::from(k));
                    ctx.loc.replace(*r);
                    ctx.layer.fetch_add(1, std::sync::atomic::Ordering::Acquire);
                    Self::build_control_tree(tr, value.as_ref().unwrap(), ctx);
                    Self::pop_tree(tr, ctx);
                }
            }
        }
    }

    fn recursive_search(&self, extend: &(Arc<str>, Arc<str>), ctx: &RecursiveSearchContext) {
        let layer = ctx.layer.load(std::sync::atomic::Ordering::Acquire);
        if layer > 100 {
            panic!("too deep recursive layer {}, more than 100", layer);
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
            let r = self.vanilla_controls_table.get(extend);
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

    pub(crate) fn find_extend_value(
        &self,
        extend: &(Arc<str>, Arc<str>),
    ) -> Option<(Arc<str>, BfastHashSet<Arc<str>>)> {
        let ctx = RecursiveSearchContext::new();
        self.recursive_search(extend, &ctx);
        if let Some(type_n) = ctx.type_n.take() {
            Some((type_n, ctx.variables.take()))
        } else {
            None
        }
    }

    fn pop_tree(tr: &mut Tree<AutomatedId, ControlNode>, ctx: &BuildTreeContext) {
        if let Some(last_node) = ctx.last_node.get() {
            let r = tr.get_node_by_id(&last_node);
            if let Some(r) = r {
                ctx.last_node.replace(r.get_parent_id());
            }
        }
        ctx.layer.fetch_sub(1, std::sync::atomic::Ordering::Acquire);
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
                trace!("error in add node for tree,detail {}", e);
            }
        }
    }
}

fn find_namespace(tokens: &[Token]) -> Option<String> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, token)| {
            if let Token::Str(_, v) = token {
                v.as_str() == "namespace"
            } else {
                false
            }
        })
        .and_then(|(index, _)| match tokens.get(index + 2) {
            Some(Token::Str(_, s)) => Some(s.clone()),
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
            part2 = def_namespace;
            part3 = namespace_parts[0];
        }
        _ => {}
    }
    (Rc::from(part1), Rc::from(part2), Rc::from(part3))
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

    #[tokio::test]
    async fn test_completer_init() {
        let p = crate::load_completer().await;
        let path = PathBuf::from("test");
        assert!(path.exists());
        crate::tests::setup_logger();
        p.init(&path).await;
        assert!(p.contain_tree("achievement"));
        assert!(p.contain_tree("add_external_server"));
        let trees = p.trees.read().unwrap();
        for (_, v) in trees.iter() {
            println!("tree {} \n------------", v);
        }
    }
}
