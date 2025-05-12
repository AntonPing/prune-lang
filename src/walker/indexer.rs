use super::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdentIdx {
    ident: Ident,
    idx: usize,
}

impl IdentIdx {
    pub fn new(ident: Ident, idx: usize) -> IdentIdx {
        IdentIdx { ident, idx }
    }
}

impl std::fmt::Display for IdentIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_idx{}", self.ident, self.idx)
    }
}

#[derive(Clone, Debug)]
pub struct Indexer {
    counter: usize,
    idx: usize,
    stack: Vec<usize>,
    saves: Vec<Vec<usize>>,
}

impl Indexer {
    pub fn new() -> Indexer {
        Indexer {
            counter: 0,
            idx: 0,
            stack: Vec::new(),
            saves: Vec::new(),
        }
    }

    pub fn add_ctx(&self, ident: &Ident) -> IdentIdx {
        IdentIdx {
            ident: *ident,
            idx: self.idx,
        }
    }

    pub fn add_next_ctx(&self, ident: &Ident) -> IdentIdx {
        IdentIdx {
            ident: *ident,
            idx: self.counter + 1,
        }
    }

    pub fn stack_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn push(&mut self) {
        self.stack.push(self.idx);
        self.counter += 1;
        self.idx = self.counter;
    }

    pub fn pop(&mut self) {
        self.idx = self.stack.pop().unwrap();
    }

    pub fn savepoint(&mut self) {
        self.saves.push(self.stack.clone());
        self.saves.last_mut().unwrap().push(self.idx);
    }

    pub fn backtrack(&mut self) {
        self.stack = self.saves.pop().unwrap();
        self.idx = self.stack.pop().unwrap();
    }
}
