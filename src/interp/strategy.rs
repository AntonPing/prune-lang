use itertools::Itertools;

use super::*;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct PathNode {
    rule_idx: usize,
    call_idx: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path(Vec<PathNode>);

impl Path {
    pub fn new() -> Path {
        Path(Vec::new())
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push(&mut self, rule_idx: usize, call_idx: usize) {
        self.0.push(PathNode { rule_idx, call_idx });
    }
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.0.len().cmp(&other.0.len()) {
            std::cmp::Ordering::Less => {
                for (node1, node2) in self.0.iter().zip(other.0.iter()) {
                    if node1 != node2 {
                        return None;
                    }
                }
                Some(std::cmp::Ordering::Less)
            }
            std::cmp::Ordering::Equal => {
                if self.0 == other.0 {
                    Some(std::cmp::Ordering::Equal)
                } else {
                    None
                }
            }
            std::cmp::Ordering::Greater => {
                for (node1, node2) in self.0.iter().zip(other.0.iter()) {
                    if node1 != node2 {
                        return None;
                    }
                }
                Some(std::cmp::Ordering::Greater)
            }
        }
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nodes = self.0.iter().format_with(", ", |node, f| {
            f(&format_args!("({}, {})", node.rule_idx, node.call_idx))
        });
        write!(f, "[{}]", nodes)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct HistoryNode {
    pred: Ident,
    args_size: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct History(Vec<HistoryNode>);

impl History {
    pub fn new() -> History {
        History(Vec::new())
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push(&mut self, pred: Ident, args_size: Vec<usize>) {
        self.0.push(HistoryNode { pred, args_size });
    }

    pub fn left_biased_strategy_pred(&self) -> bool {
        true
    }

    pub fn naive_strategy_pred(&self, n: usize) -> bool {
        self.0.len() < n
    }

    pub fn struct_recur_strategy_pred(&self, pred: Ident, args: &Vec<TermVal<IdentCtx>>) -> bool {
        let args_size: Vec<usize> = args.iter().map(|arg| arg.height()).collect();

        for node in self.0.iter() {
            if node.pred == pred {
                if node
                    .args_size
                    .iter()
                    .zip(args_size.iter())
                    .all(|(arg0, arg)| arg0 <= arg)
                {
                    return false;
                }
            }
        }

        true
    }
}

#[derive(Clone, Debug)]
pub struct CallInfo {
    // these two vector should have the same length, they are seperated for convenience.
    pub history: History,
    pub path: Path,
}

impl CallInfo {
    pub fn new() -> CallInfo {
        CallInfo {
            history: History::new(),
            path: Path::new(),
        }
    }
}

pub struct ConflitCache {
    max_size: usize,
    cache: Vec<Path>,
    cursor: usize,
}

impl ConflitCache {
    pub fn new(max_size: usize) -> ConflitCache {
        ConflitCache {
            max_size,
            cache: std::iter::repeat(Path::new()).take(max_size).collect(),
            cursor: 0,
        }
    }

    pub fn lookup(&self, path: &Path) -> bool {
        // println!("check: {}", path);
        // for path in self.cache.iter() {
        //     println!("{}", path);
        // }
        let res = self.cache.iter().any(|path2| path <= path2);
        // println!("result: {}", res);
        res
    }

    pub fn update(&mut self, path: &Path) {
        if !self.lookup(path) {
            self.cache[self.cursor] = path.clone();
            self.cursor = (self.cursor + 1) % self.max_size;
        }
    }
}
