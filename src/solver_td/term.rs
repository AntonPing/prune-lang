use super::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdentCtx {
    ident: Ident,
    ctx: usize,
}

impl IdentCtx {
    pub fn new(ident: Ident, ctx: usize) -> IdentCtx {
        IdentCtx { ident, ctx }
    }
}

impl std::fmt::Display for IdentCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_ctx{}", self.ident, self.ctx)
    }
}

pub type TermCtx = Term<IdentCtx>;

#[derive(Debug)]
pub struct CtxAlloc {
    counter: usize,
    ctx: usize,
    stack: Vec<usize>,
    save: Vec<Vec<usize>>,
}

impl std::fmt::Display for CtxAlloc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "counter: {}", self.counter)?;
        writeln!(f, "ctx: {}", self.ctx)?;
        writeln!(f, "stack: {:?}", self.stack)?;
        writeln!(f, "save: {:?}", self.save)?;
        Ok(())
    }
}

impl CtxAlloc {
    pub fn new() -> CtxAlloc {
        CtxAlloc {
            counter: 0,
            ctx: 0,
            stack: Vec::new(),
            save: Vec::new(),
        }
    }

    pub fn add_ctx(&self, ident: &Ident) -> IdentCtx {
        IdentCtx {
            ident: *ident,
            ctx: self.ctx,
        }
    }

    pub fn add_next_ctx(&self, ident: &Ident) -> IdentCtx {
        IdentCtx {
            ident: *ident,
            ctx: self.counter + 1,
        }
    }

    pub fn stack_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn push(&mut self) {
        self.stack.push(self.ctx);
        self.counter += 1;
        self.ctx = self.counter;
    }

    pub fn pop(&mut self) {
        self.ctx = self.stack.pop().unwrap();
    }

    pub fn savepoint(&mut self) {
        self.save.push(self.stack.clone());
        self.save.last_mut().unwrap().push(self.ctx);
    }

    pub fn backtrack(&mut self) {
        self.stack = self.save.pop().unwrap();
        self.ctx = self.stack.pop().unwrap();
    }
}
