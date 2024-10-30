use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::{fs};

use chumsky::prelude::*;
use dashmap::DashMap;
use log::{error, trace};
use walkdir::WalkDir;

use crate::document::Document;
use crate::chumsky::prelude::*;
use crate::tree_ds::prelude::*;

fn extract_prefix(input: &str) -> &str {
    match input.find('@') {
        Some(index) => &input[..index],
        None => input,
    }
}

type ChumskyParser<'a> = Boxed<'a, 'a, &'a str, Vec<Token<'a>>, extra::Err<Rich<'a, char>>>;
type AutoTree<T> = Tree<AutomatedId, T>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct ControlNode {
    define: ControlDefine,
    loc:    (usize, usize),
}

#[derive(Debug)]
pub(crate) struct ControlDefine {
    pub(crate) name:      Arc<str>,
    pub(crate) extend:    Arc<str>,
    pub(crate) type_n:    Mutex<Arc<str>>,
    pub(crate) variables: Mutex<std::collections::HashSet<Arc<str>>>,
}

impl Clone for ControlDefine {
    fn clone(&self) -> Self {
        ControlDefine {
            name:      Arc::clone(&self.name),
            extend:    Arc::clone(&self.extend),
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

pub struct Parser {
    trees:                  DashMap<String, AutoTree<ControlNode>>,
    keyword:                HashSet<String>,
    vanilla_controls_tabel: HashMap<String, ControlDefine>,
}

#[derive(Debug)]
pub(crate) struct BuildTreeContext {
    namespace:    Rc<str>,
    control_name: RefCell<Rc<str>>,
    last_node:    Cell<Option<AutomatedId>>,
    loc:          RefCell<(usize, usize)>,
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
        1 => parts[0].split('.').collect(),
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

impl Parser {
    pub fn new(
        keyword: HashSet<String>,
        vanilla_controls_tabel: HashMap<String, ControlDefine>,
    ) -> Self {
        Parser {
            trees: DashMap::with_shard_amount(2),
            keyword,
            vanilla_controls_tabel,
        }
    }

    pub(crate) fn init(&self, workspace_folders: &PathBuf) {
        for entry in WalkDir::new(workspace_folders)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "json"))
            .filter(|e| !e.path().ends_with("_global_variables.json"))
            .filter(|e| !e.path().ends_with("_ui_defs.json"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                let tokenizer = crate::chumsky::parser::<'_>();
                let r = crate::chumsky::parse(tokenizer, content.as_ref());
                match r {
                    Ok(r) => Self::handle_tokens(self, r),
                    Err(e) => {
                        trace!("error in new parser{:?}", e)
                    }
                }
            }
        }
    }

    fn handle_tokens(parser: &Parser, tokens: Vec<Token<'_>>) {
        let np = find_namespace(&tokens);
        if let Some(np) = np {
            let mut tr: Tree<AutomatedId, ControlNode> =
                AutoTree::<ControlNode>::new(Option::Some(np.as_str()));
            let ctx = BuildTreeContext {
                namespace:    Rc::from(np.as_ref()),
                control_name: RefCell::new(Rc::from("()")),
                last_node:    Cell::new(None),
                loc:          RefCell::new((0, 0)),
            };
            Self::build_control_tree(&mut tr, &tokens, &ctx);
        } else {
            trace!("cant find namespace from tokens {:?}", tokens);
        }
    }

    fn build_control_tree(
        tr: &mut Tree<AutomatedId, ControlNode>,
        tokens: &Vec<Token<'_>>,
        ctx: &BuildTreeContext,
    ) {
        let m = to_map_with_span_ref(tokens);
        if ctx.control_name.borrow().as_ref() != "()" {
            let (name, np, extend) =
                split_control_name(ctx.control_name.borrow().as_ref(), ctx.namespace.as_ref());
            let mut type_n = None;
            let mut variables = std::collections::HashSet::<Arc<str>>::new();
            for (k, v) in m {
                if k == "type" {
                    type_n = Some(to_string_ref(v.1));
                }
                if k.starts_with("$") {
                    variables.insert(Arc::from(to_string_ref(v.1).as_ref()));
                }
            }
            let extend = np.to_string() + "." + extend.as_ref();
            let type_n = type_n.unwrap_or("".to_string());
            let node = ControlNode {
                define: ControlDefine {
                    name:      Arc::from(name.as_ref()),
                    extend:    Arc::from(extend.as_ref()),
                    type_n:    Mutex::new(Arc::from(type_n.as_ref())),
                    variables: Mutex::new(variables),
                },
                loc:    ctx.loc.take(),
            };
            if let Some(last_node) = ctx.last_node.get() {
                let r = tr.get_node_by_id(&last_node);
                if let Some(r) = r {
                    let node = Node::<AutomatedId, ControlNode>::new_auto(Some(node));
                    let new_node_id = node.get_node_id();
                    r.add_child(node);
                    ctx.last_node.replace(Some(new_node_id));
                } else {
                    unreachable!()
                }
            } else {
                let r = tr.add_node(Node::<AutomatedId, ControlNode>::new_auto(Some(node)), None);
                match r {
                    Ok(r) => {
                        ctx.last_node.replace(Some(r));
                    }
                    Err(e) => {
                        error!("error in add node for tree, detail {}", e);
                    }
                }
            }
            ctx.control_name.replace(Rc::from("()"));
        } else {
            for (k, v) in m {
                match v.1 {
                    Token::Controls(_, value) => {
                        ctx.control_name.replace(Rc::from(k));
                        ctx.loc.replace(v.0);
                        Self::build_control_tree(tr, value, ctx);
                    }
                    Token::Array(_, value) => {
                        for i in value {
                            if let Token::Controls(_, value) = i {
                                ctx.control_name.replace(Rc::from("()"));
                                Self::build_control_tree(tr, value, ctx);
                            }
                        }
                    }
                    _ => {}
                }
            }
            if let Some(last_node) = ctx.last_node.get() {
                let r = tr.get_node_by_id(&last_node);
                if let Some(r) = r {
                    ctx.last_node.replace(r.get_parent_id());
                }
            }
        }
    }

    pub(crate) async fn input(pos: (usize, usize), doc: &Document) -> String {
        let chars = doc.content_chars.lock().await;
        let (l, r) = pos;
        join(&chars[l..r + 1])
    }
}
