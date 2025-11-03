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
    vsids_score: isize,
    vsids_tmsp: usize,
}

const ALPHA: f32 = 0.95;

impl PathInfo {
    fn new(tmsp: usize) -> PathInfo {
        PathInfo {
            counter: 0,
            vsids_score: 0,
            vsids_tmsp: tmsp,
        }
    }

    fn bump_score(&mut self, tmsp: usize, inc: isize) {
        self.decay_update(tmsp);
        self.vsids_score += inc;
    }

    fn decay_update(&mut self, tmsp: usize) {
        let powered = ALPHA.powi((tmsp - self.vsids_tmsp) as i32);
        self.vsids_score = ((self.vsids_score as f32) * powered) as isize;
        self.vsids_tmsp = tmsp;
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

    pub fn conflict_update(&mut self, path: &Path) {
        let mut vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        let mut new_counter = 4;

        while !vec.is_empty() {
            let info = if let Some(info) = self.map.get_mut(&vec) {
                info
            } else {
                let info = PathInfo::new(self.time_stamp);
                self.map.insert(vec.clone(), info);
                self.map.get_mut(&vec).unwrap()
            };

            info.counter = std::cmp::max(info.counter, new_counter);
            info.bump_score(self.time_stamp, new_counter as isize * 100);

            vec.pop().unwrap();
            vec.pop().unwrap();

            if new_counter == 1 {
                break;
            } else {
                new_counter /= 2;
            }
        }
    }

    pub fn branch_update(&mut self, path: &Path) {
        let vec = path.link.to_usize_vec();
        assert_eq!(vec.len() % 2, 0);

        let info = if let Some(info) = self.map.get_mut(&vec) {
            info
        } else {
            let info = PathInfo::new(self.time_stamp);
            self.map.insert(vec.clone(), info);
            self.map.get_mut(&vec).unwrap()
        };

        info.counter = info.counter.saturating_sub(1);
        info.bump_score(self.time_stamp, -200);

        self.time_stamp += 1;
    }

    pub fn get_counter(&self, path: &Path) -> isize {
        self.map
            .get(&path.link.to_usize_vec())
            .map(|info| info.counter)
            .unwrap_or(0) as isize
    }

    pub fn get_mixed_vsids_score(&mut self, path: &Path) -> isize {
        self.map
            .get_mut(&path.link.to_usize_vec())
            .map(|info| {
                info.decay_update(self.time_stamp);
                info.vsids_score
            })
            .unwrap_or(0)
    }
}
