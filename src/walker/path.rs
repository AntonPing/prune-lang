use std::ops::Deref;
use std::rc::Rc;

use radix_trie::Trie;

use super::*;

#[derive(Clone, Debug)]
pub struct Path {
    pub pred: PredIdent,
    pub idx: usize,
    pub ctx: usize,
    link: PathLink,
}

impl Path {
    pub fn new(pred: PredIdent) -> Path {
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

    pub fn call(&self, pred: PredIdent, ctx: usize) -> Path {
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
        pred: PredIdent,
        idx: usize,
    },
}

#[derive(Clone, Debug)]
struct PathLink(Rc<PathLinkData>);

impl PathLink {
    fn new(pred: PredIdent) -> PathLink {
        PathLink(Rc::new(PathLinkData::Cons {
            link: Rc::new(PathLinkData::Nil),
            pred,
            idx: 0,
        }))
    }

    fn link(&self, pred: PredIdent, idx: usize) -> PathLink {
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
            let PredIdent::Pred(pred) = pred;
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
    count: usize,
}

impl PathTree {
    pub fn new() -> PathTree {
        let map = Trie::new();
        PathTree { map, count: 0 }
    }

    pub fn update_path(&mut self, path: &Path) {
        let mut vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        self.count += 1;

        while !vec.is_empty() {
            self.map.insert(vec.clone(), self.count);

            vec.pop().unwrap();
            vec.pop().unwrap();
        }
    }

    pub fn get(&self, path: &Path) -> usize {
        self.map
            .get(&path.link.to_usize_vec())
            .cloned()
            .unwrap_or(0)
    }
}
