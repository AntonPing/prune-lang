use super::*;

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

    pub fn is_empty(&self) -> bool {
        self.saves.is_empty()
    }

    pub fn reset(&mut self) {
        self.counter = 0;
        self.idx = 0;
        self.stack.clear();
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

impl Indexer {
    pub fn add_idx(&self, ident: &Ident) -> IdentCtx {
        ident.tag_ctx(self.idx)
    }

    pub fn add_next_idx(&self, ident: &Ident) -> IdentCtx {
        ident.tag_ctx(self.counter + 1)
    }

    pub fn push(&mut self) {
        self.stack.push(self.idx);
        self.counter += 1;
        self.idx = self.counter;
    }

    pub fn pop(&mut self) {
        self.idx = self.stack.pop().unwrap();
    }
}
