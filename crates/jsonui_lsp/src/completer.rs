use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{self, Display};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, RwLock};
use std::{fs, vec};

use dashmap::DashMap;
use lasso::Spur;
use log::trace;
use tokio::sync::Mutex;
use walkdir::WalkDir;

use crate::document::Document;
use crate::lexer::prelude::*;
use crate::museair::{BfastDashMap, BfastHash, BfastHashMap, BfastHashSet};
use crate::tower_lsp::*;
use crate::tree_ds::prelude::*;
use crate::StdMutex;

pub(crate) type AutoTree<T> = Tree<AutomatedId, T>;
pub(crate) type ControlName = (Arc<str>, Option<(Arc<str>, Arc<str>)>);

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
        let lock = self.define.type_n.lock().expect("Failed to lock type_n");
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
            name: (name, extend),
            type_n: StdMutex::new(type_n),
            variables: StdMutex::new(variables),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct ControlDefine {
    pub(crate) name: ControlName,
    pub(crate) type_n: StdMutex<Option<Arc<str>>>,
    pub(crate) variables: StdMutex<BfastHashSet<Arc<str>>>,
}
impl Clone for ControlDefine {
    fn clone(&self) -> Self {
        ControlDefine {
            name: Clone::clone(&self.name),
            type_n: StdMutex::new(self.type_n.lock().expect("Failed to lock type_n").clone()),
            variables: StdMutex::new(self.variables.lock().expect("Failed to lock variables").clone()),
        }
    }
}
impl PartialEq for ControlDefine {
    fn eq(&self, other: &Self) -> bool {
        if self.name != other.name {
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
                self.name.0,
                self.name.1,
                *type_n,
                &variables_str.as_str()[0..50]
            )
        } else {
            write!(
                f,
                "ControlDefine(name: {}, {:?}, type: {:?}, [{}])",
                self.name.0, self.name.1, *type_n, variables_str
            )
        }
    }
}

#[derive(Debug)]
pub(crate) struct BuildTreeContext {
    url: Url,
    document: Arc<Document>,
    tree: Mutex<Tree<AutomatedId, ControlNode>>,
    symbol_table: Mutex<BfastHashMap<ControlNameSymbol, Location>>,
    control_name: Mutex<Option<Arc<str>>>,
    last_node: Mutex<Option<AutomatedId>>,
    loc: Mutex<(usize, usize)>,
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ControlNameSymbol((Arc<str>, Arc<str>));
impl ControlNameSymbol {
    pub(crate) fn new(name: (Arc<str>, Arc<str>)) -> Self {
        ControlNameSymbol(name)
    }
}

pub struct Completer {
    pub(crate) trees: RwLock<BfastHashMap<Arc<str>, AutoTree<ControlNode>>>,
    documents: BfastDashMap<Url, Arc<Document>>,
    caches: BfastDashMap<Arc<str>, Arc<Vec<Token>>>,
    urls: BfastDashMap<Arc<str>, Url>,
    symbol_table: BfastDashMap<ControlNameSymbol, Location>,
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
            documents: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
            caches: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
            urls: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
            symbol_table: DashMap::with_hasher_and_shard_amount(BfastHash::<true>::new(), 2),
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
                let content = Arc::from(content.as_str());
                let abs_path = fs::canonicalize(entry.path()).expect("Failed to get absolute path");
                let url = Url::from_file_path(abs_path).expect("Failed to convert path to URL");
                trace!("init url {}", &url);
                self.did_open(url, content).await;
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
            let r = doc.deref().clone();
            if let Some((k, v, tokens, symbols)) = Self::build_tree(url, r).await {
                let pos = param.text_document_position.position;
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
                )
                    .await;

                self.add_tree(k.as_ref(), v);
                self.caches.insert(k.clone(), Arc::new(tokens));
                symbols.into_iter().for_each(|(k, v)| {
                    self.symbol_table.insert(k, v);
                });
                doc.clear_dirty();
                return r;
            } else {
                trace!("Failed to build_tree {:?}", url);
            }
        }
        None
    }

    pub async fn complete_color(&self, url: &Url) -> Option<Vec<ColorInformation>> {
        let doc = self.documents.get(url);
        if let Some(doc) = doc {
            if !doc.is_dirty() {
                let tokens = self.caches.get(&doc.get_cache_namespace());
                if let Some(tokens) = tokens {
                    return crate::complete_helper::color(doc.borrow(), &tokens).await;
                }
            }
            let input = doc.get_content().await;
            let r = parse_full(input.as_ref()).await;
            if let Some(r) = r {
                return crate::complete_helper::color(doc.borrow(), &r.1).await;
            }
        }
        None
    }

    pub async fn goto_definition(
        &self,
        params: &GotoDefinitionParams,
    ) -> Option<GotoDefinitionResponse> {
        let param = &params.text_document_position_params;
        let url = &param.text_document.uri;
        let pos = &param.position;
        let doc = self.documents.get(url);
        if let Some(doc) = doc {
            let index = doc.get_index_from_position(*pos).await;
            let index = index?;
            let namespace = doc.get_cache_namespace();

            if !doc.is_dirty() {
                if let Some(v) = self.caches.get(&namespace) {
                    return crate::complete_helper::goto_definition(self, namespace, v.clone(), index);
                }
            }

            if let Some((k, v, tokens, symbols)) = Self::build_tree(url, doc.clone()).await {
                let tokens = Arc::new(tokens);
                self.add_tree(k.as_ref(), v);
                self.caches.insert(k.clone(), tokens.clone());
                symbols.into_iter().for_each(|(k, v)| {
                    self.symbol_table.insert(k, v);
                });
                doc.clear_dirty();
                return crate::complete_helper::goto_definition(self, k.clone(), tokens, index);
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
            let doc: Arc<Document> = Arc::new(Document::from(content));

            self.documents.insert(url.clone(), doc.clone());
            if let Some((k, v, tokens, symbols)) = Self::build_tree(&url, doc).await {
                self.urls.insert(Arc::from(np.as_str()), url.clone());
                self.add_tree(k.as_ref(), v);
                self.caches.insert(k.clone(), Arc::new(tokens));
                symbols.into_iter().for_each(|(k, v)| {
                    self.symbol_table.insert(k, v);
                });
            } else {
                trace!("Failed to build_tree {:?}", np);
            }
        } else {
            trace!("cant find namespace when did_open.")
        }
    }

    pub(crate) fn did_rename(&self, o_url: &Url, n_url: Url) {
        if let Some((_, v)) = self.documents.remove(o_url) {
            let namespace = &v.get_cache_namespace();
            self.urls.insert(namespace.clone(), n_url.clone());
            self.documents.insert(n_url, v);
        }
    }

    pub(crate) fn did_close(&self, url: &Url) {
        let r = self.documents.remove(url);
        if let Some((_, v)) = r {
            let namespace = &v.get_cache_namespace();
            self.urls.remove(namespace);
            self.caches.remove(namespace);
            self.del_tree(namespace);
        }
    }

    pub(crate) fn get_url(&self, namespace: &Arc<str>) -> Option<Url> {
        let r = self.urls.get(namespace);
        r.map(|r| r.clone())
    }

    pub(crate) fn get_namespace(&self, url: &Url) -> Option<Arc<str>> {
        let r = self.documents.get(url);
        r.map(|r| r.get_cache_namespace())
    }

    pub(crate) fn find_symbol(&self, key: &ControlNameSymbol) -> Option<Location> {
        self.symbol_table.get(key).map(|f| f.clone())
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

    async fn build_tree(
        url: &Url,
        document: Arc<Document>,
    ) -> Option<(
        Arc<str>,
        Tree<AutomatedId, ControlNode>,
        Vec<Token>,
        BfastHashMap<ControlNameSymbol, Location>,
    )> {
        let input = document.get_content().await;
        let r = parse_full(input.as_ref()).await;
        match r {
            Some((range, tokens)) => {
                let np = document.get_cache_namespace();
                let tr: Tree<AutomatedId, ControlNode> = AutoTree::<ControlNode>::new(Some(np.as_ref()));
                let ctx = BuildTreeContext {
                    url: url.clone(),
                    document,
                    tree: Mutex::new(tr),
                    symbol_table: Mutex::new(BfastHashMap::default()),
                    control_name: Mutex::new(None),
                    last_node: Mutex::new(None),
                    loc: Mutex::new((0, 0)),
                    layer: AtomicUsize::new(0),
                };
                let root = ControlNode {
                    define: ControlDefine {
                        name: (Arc::from("root"), None),
                        type_n: StdMutex::new(None),
                        variables: StdMutex::new(BfastHashSet::default()),
                    },
                    loc: range,
                };
                Self::add_tree_node(&ctx, root, None).await;
                Self::build_control_tree(&tokens, &ctx).await;
                Some((np, ctx.tree.into_inner(), tokens, ctx.symbol_table.into_inner()))
            }
            None => {
                trace!("error in new parser");
                None
            }
        }
    }

    async fn build_control_tree<'a>(tokens: &'a Vec<Token>, ctx: &'a BuildTreeContext) {
        // recursion layer check
        let layer = ctx.layer.load(std::sync::atomic::Ordering::SeqCst);
        if layer > 100 {
            panic!("too deep layer {} for build_control_tree, more than 100", layer);
        }

        let m = to_map_with_span_ref(tokens);
        let v = {
            ctx.control_name
                .try_lock()
                .expect("cant get control_name lock")
                .take()
        };
        let np = ctx.document.get_cache_namespace();
        if let Some(v) = v {
            let (name, extend) = split_control_name(v.as_ref(), np.as_ref());
            let mut type_n = None;
            let mut variables = BfastHashSet::default();
            let mut arrays = vec![];

            for ((_, k), v) in m {
                if matches!(v, Token::Array(_, _)) {
                    arrays.push(v);
                }
                if k == "type" {
                    type_n = Some(to_string_ref(v));
                } else if k.starts_with("$") {
                    let str = k.replace("|default", "");
                    let var: Arc<str> = Arc::from(str.as_str());
                    variables.insert(var.clone());
                }
            }

            let type_n = type_n.map(|f| Arc::from(f.as_str()));
            let node = ControlNode {
                define: ControlDefine {
                    name: (name, extend),
                    type_n: StdMutex::new(type_n),
                    variables: StdMutex::new(variables),
                },
                loc: { *ctx.loc.try_lock().expect("cant get loc lock") },
            };

            let option = { ctx.last_node.try_lock().expect("cant get last_node lock").take() };
            let option = option.as_ref();
            Self::add_tree_node(ctx, node, option).await;

            for i in arrays {
                if let Token::Array(_, vec) = i {
                    for i in vec.as_ref().unwrap() {
                        if let Token::Object(_, value) = i {
                            {
                                *ctx.control_name.try_lock().expect("cant get control_name lock") = None;
                            }
                            Box::pin(Self::build_control_tree(value.as_ref().unwrap(), ctx)).await;
                        }
                    }
                }
            }
            {
                *ctx.control_name.try_lock().expect("cant get control_name lock") = None;
            }
        } else {
            for ((range, k), v) in m {
                if let Token::Object(r, value) = v {
                    // ControlName Symbol build
                    if layer == 0 {
                        let pos_l = { ctx.document.get_position_from_index(range.0).await };
                        let pos_r = { ctx.document.get_position_from_index(range.1).await };
                        if let (Some(pos_l), Some(pos_r)) = (pos_l, pos_r) {
                            let name = split_control_name(k.as_ref(), np.as_ref());
                            let symbol_key = ControlNameSymbol::new((np.clone(), name.0));
                            {
                                let location = Location::new(ctx.url.clone(), Range::new(pos_l, pos_r));
                                ctx.symbol_table
                                    .try_lock()
                                    .expect("cant get symbol_table lock")
                                    .insert(symbol_key, location);
                            }
                        }
                    }
                    {
                        *ctx.control_name.try_lock().expect("cant get control_name lock") =
                            Some(Arc::from(k));
                    }
                    {
                        *ctx.loc.try_lock().expect("cant get loc lock") = *r;
                    }
                    ctx.layer.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    Box::pin(Self::build_control_tree(value.as_ref().unwrap(), ctx)).await;
                    Self::pop_tree(ctx).await;
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
                if v.define.name.0 != extend.1 {
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
                if let Some(ref extend) = v.define.name.1 {
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

    async fn pop_tree(ctx: &BuildTreeContext) {
        let last_node = { ctx.last_node.lock().await.take() };
        if let Some(last_node) = last_node {
            let r = ctx.tree.lock().await.borrow().get_node_by_id(&last_node);
            if let Some(r) = r {
                *ctx.last_node.lock().await = r.get_parent_id();
            }
        }
        ctx.layer.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
    }

    async fn add_tree_node(ctx: &BuildTreeContext, node: ControlNode, id: Option<&AutomatedId>) {
        let mut tr = ctx.tree.lock().await;
        let r = tr.add_node(Node::<AutomatedId, ControlNode>::new_auto(Some(node)), id);
        match r {
            Ok(r) => {
                *ctx.last_node.lock().await = Some(r);
            }
            Err(e) => {
                trace!("error in add node for tree,detail {}", e);
            }
        }
    }
}

pub(crate) fn split_control_name(name: &str, def_namespace: &str) -> ControlName {
    let mut part1 = "";
    let mut part2 = None;
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
            part2 = Some((Arc::from(namespace_parts[0]), Arc::from(namespace_parts[1])));
        }
        1 => {
            part2 = Some((Arc::from(def_namespace), Arc::from(namespace_parts[0])));
        }
        _ => {}
    }
    (Arc::from(part1), part2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_control_name() {
        let r = split_control_name("empty_progress_bar_icon", "achievement");
        assert_eq!((Arc::from("empty_progress_bar_icon"), None), r);
        let r = split_control_name("empty_progress_bar_icon@test.cc", "achievement");
        assert_eq!(
            (Arc::from("empty_progress_bar_icon"), Some((Arc::from("test"), Arc::from("cc")))),
            r
        );
        let r = split_control_name("empty_progress_bar_icon@cc", "achievement");
        assert_eq!(
            (Arc::from("empty_progress_bar_icon"), Some((Arc::from("achievement"), Arc::from("cc")))),
            r
        );
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
