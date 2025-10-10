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
pub struct PathTree {
    map: Trie<Vec<usize>, usize>,
}

impl PathTree {
    pub fn new() -> PathTree {
        let map = Trie::new();
        PathTree { map }
    }

    pub fn update_path_inc(&mut self, path: &Path) {
        let mut vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        let mut count = 4;

        while !vec.is_empty() {
            self.map.map_with_default(
                vec.clone(),
                |val| {
                    if *val < count {
                        *val = count
                    }
                },
                count,
            );

            vec.pop().unwrap();
            vec.pop().unwrap();

            if count == 1 {
                break;
            } else {
                count /= 2;
            }
        }
    }

    pub fn update_path_dec(&mut self, path: &Path) {
        let vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        self.map.map_with_default(
            vec,
            |val| {
                if *val > 0 {
                    *val -= 1;
                }
            },
            0,
        );
    }

    pub fn get(&self, path: &Path) -> usize {
        self.map
            .get(&path.link.to_usize_vec())
            .cloned()
            .unwrap_or(0)
    }
}
