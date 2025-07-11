use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Priority {
    tag: usize,
    tmsp: usize,
    weight: usize,
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.tag.partial_cmp(&other.tag) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        assert_eq!(self.tmsp, other.tmsp);
        self.weight.partial_cmp(&other.weight)
    }
}

impl Ord for Priority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.tag.cmp(&other.tag) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        assert_eq!(self.tmsp, other.tmsp);
        self.weight.cmp(&other.weight)
    }
}

impl Priority {
    pub fn new(tag: usize, tmsp_cnt: usize) -> Priority {
        Priority {
            tag,
            tmsp: tmsp_cnt,
            weight: 0,
        }
    }

    pub fn update_decay(&mut self, tmsp_cnt: usize) {
        for _ in self.tmsp..tmsp_cnt {
            self.weight *= 31;
            self.weight /= 32;
        }
        self.tmsp = tmsp_cnt;
    }

    pub fn update_bump(&mut self, tmsp_cnt: usize) {
        self.update_decay(tmsp_cnt);
        self.tmsp += 1;
        self.weight += 1000;
    }
}

impl fmt::Display for Priority {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.tag, self.tmsp, self.weight)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct PointInfo {
    pub addr: usize,
    pub idx: usize,
    pub prior: Priority,
    pub pred: Option<Point>,
}

#[derive(Clone, Debug)]
pub struct Point(pub Rc<RefCell<PointInfo>>);

impl Point {
    pub fn new(addr: usize, idx: usize, prior: Priority, pred: Option<Point>) -> Point {
        let pnt_info = PointInfo {
            addr,
            idx,
            prior,
            pred,
        };
        Point(Rc::new(RefCell::new(pnt_info)))
    }
    pub fn get_addr_idx(&self) -> (usize, usize) {
        let ptr = self.0.borrow();
        (ptr.addr, ptr.idx)
    }

    pub fn get_prior(&self) -> Priority {
        let ptr = self.0.borrow();
        ptr.prior.clone()
    }

    pub fn update_decay(&self, tmsp_cnt: usize) {
        let mut ptr = self.0.borrow_mut();
        ptr.prior.update_decay(tmsp_cnt);
    }

    pub fn update_bump_upward(&self, tmsp_cnt: usize) {
        let mut ptr = self.0.borrow_mut();
        ptr.prior.update_bump(tmsp_cnt);
        if let Some(pred) = ptr.pred.clone() {
            pred.update_bump_upward(tmsp_cnt);
        }
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (addr, idx) = self.get_addr_idx();
        let prior = &self.0.borrow().prior;
        write!(f, "({}, {}, {})", addr, idx, prior)?;
        Ok(())
    }
}
