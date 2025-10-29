use std::ops::Deref;
use std::rc::Rc;

use radix_trie::Trie;

use super::*;

#[derive(Clone, Debug)]
pub struct Path {
    pub pred: Ident,
    pub idx: usize,
    pub ctx: usize,
    link: PathLink,
}

impl Path {
    pub fn new(pred: Ident) -> Path {
        Path {
            pred,
            idx: 0,
            ctx: 0,
            link: PathLink::new(pred),
        }
    }

    pub fn jump(&self, idx: usize) -> Path {
        let mut res = self.clone();
        res.idx = idx;
        res.link = res.link.link(self.pred, idx);
        res
    }

    pub fn call(&self, pred: Ident, ctx: usize) -> Path {
        let mut res = self.clone();
        res.pred = pred;
        res.idx = 0;
        res.ctx = ctx;
        res.link = res.link.link(pred, 0);
        res
    }
}

#[derive(Clone, Debug)]
enum PathLinkData {
    Nil,
    Cons {
        link: Rc<PathLinkData>,
        pred: Ident,
        idx: usize,
    },
}

#[derive(Clone, Debug)]
struct PathLink(Rc<PathLinkData>);

impl PathLink {
    fn new(pred: Ident) -> PathLink {
        PathLink(Rc::new(PathLinkData::Cons {
            link: Rc::new(PathLinkData::Nil),
            pred,
            idx: 0,
        }))
    }

    fn link(&self, pred: Ident, idx: usize) -> PathLink {
        PathLink(Rc::new(PathLinkData::Cons {
            link: Rc::new(PathLinkData::Nil),
            pred,
            idx,
        }))
    }

    fn to_usize_vec(&self) -> Vec<usize> {
        let mut vec: Vec<usize> = Vec::new();
        let mut path = self.0.clone();

        while let PathLinkData::Cons { link, pred, idx } = path.deref() {
            vec.push(*idx);
            vec.push(pred.index);
            path = link.clone();
        }

        vec.reverse();
        vec
    }
}

#[derive(Debug, Clone)]
struct PathInfo {
    counter: usize,
    last_conflit: usize,
    last_branch: usize,
}

impl PathInfo {
    fn new(tmsp: usize) -> PathInfo {
        PathInfo {
            counter: 0,
            last_conflit: tmsp,
            last_branch: tmsp,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PathTree {
    time_stamp: usize,
    map: Trie<Vec<usize>, PathInfo>,
}

impl PathTree {
    pub fn new() -> PathTree {
        PathTree {
            time_stamp: 0,
            map: Trie::new(),
        }
    }

    pub fn update_path_inc(&mut self, path: &Path) {
        let mut vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        let mut new_counter = 4;

        while !vec.is_empty() {
            if let Some(info) = self.map.get_mut(&vec) {
                if info.counter < new_counter {
                    info.counter = new_counter;
                }
                info.last_conflit = self.time_stamp;
            } else {
                let mut info = PathInfo::new(self.time_stamp);
                info.counter = new_counter;
                self.map.insert(vec.clone(), info);
            }

            vec.pop().unwrap();
            vec.pop().unwrap();

            if new_counter == 1 {
                break;
            } else {
                new_counter /= 2;
            }
        }
    }

    pub fn update_path_dec(&mut self, path: &Path) {
        let vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        if let Some(info) = self.map.get_mut(&vec) {
            if info.counter > 0 {
                info.counter -= 1;
            }
            info.last_branch = self.time_stamp;
        } else {
            let info = PathInfo::new(self.time_stamp);
            self.map.insert(vec, info);
        }

        self.time_stamp += 1;
    }

    pub fn get_counter(&self, path: &Path) -> isize {
        self.map
            .get(&path.link.to_usize_vec())
            .map(|info| info.counter)
            .unwrap_or(0) as isize
    }

    pub fn get_last_conflit(&self, path: &Path) -> isize {
        self.map
            .get(&path.link.to_usize_vec())
            .map(|info| info.last_conflit)
            .unwrap_or(0) as isize
    }

    pub fn get_neg_last_branch(&self, path: &Path) -> isize {
        let res = self
            .map
            .get(&path.link.to_usize_vec())
            .map(|info| info.last_branch)
            .unwrap_or(0) as isize;
        -res
    }

    pub fn get_last_conflict_branch_diff(&self, path: &Path) -> isize {
        self.map
            .get(&path.link.to_usize_vec())
            .map(|info| {
                let l_c = (self.time_stamp - info.last_conflit) as isize;
                let l_b = (self.time_stamp - info.last_branch) as isize;
                l_b - l_c
            })
            .unwrap_or(0)
    }
}
