use super::*;

use super::compile::{BlockCtx, PredBlock};
use crate::logic::ast::PredIdent;
use crate::solver::solver::Solver;
use std::collections::VecDeque;
use std::io;

#[derive(Clone, Debug)]
struct State<'blk> {
    depth: usize,
    curr_blk: BlockCtx<'blk>,
    queue: VecDeque<Vec<BlockCtx<'blk>>>,
}

impl<'blk> State<'blk> {
    fn new(curr_blk: BlockCtx<'blk>) -> State<'blk> {
        State {
            depth: 0,
            curr_blk,
            queue: VecDeque::new(),
        }
    }
}

#[derive(Debug)]
pub struct Walker<'blk, 'log, Log: io::Write> {
    dict: &'blk HashMap<PredIdent, PredBlock>,
    stack: Vec<State<'blk>>,
    ctx_cnt: usize,
    step_cnt: usize,
    step_cnt_la: usize,
    sol: Solver,
    log: &'log mut Log,
}

impl<'blk, 'log, Log: io::Write> Walker<'blk, 'log, Log> {
    pub fn new(
        dict: &'blk HashMap<PredIdent, PredBlock>,
        log: &'log mut Log,
    ) -> Walker<'blk, 'log, Log> {
        Walker {
            dict,
            stack: Vec::new(),
            ctx_cnt: 0,
            step_cnt: 0,
            step_cnt_la: 0,
            sol: Solver::new(),
            log,
        }
    }

    fn reset(&mut self) {
        self.ctx_cnt = 0;
        self.step_cnt = 0;
        self.step_cnt_la = 0;
        self.sol.reset();
    }

    fn push_state(&mut self, state: State<'blk>) {
        self.sol.savepoint();
        self.stack.push(state);
    }

    fn pop_state(&mut self) -> State<'blk> {
        self.sol.backtrack();
        self.stack.pop().unwrap()
    }

    pub fn write_stack(&mut self, stack: &Vec<BlockCtx>) -> io::Result<()> {
        let stack = stack.iter().format(&", ");
        writeln!(self.log, "stack = [{}]", stack)?;
        Ok(())
    }

    pub fn write_saves(&mut self, saves: &Vec<Vec<BlockCtx>>) -> io::Result<()> {
        for (i, stack) in saves.iter().rev().enumerate() {
            let stack = stack.iter().format(&", ");
            writeln!(self.log, "save{} = [{}]", i, stack)?;
        }
        Ok(())
    }

    pub fn write_solver(&mut self) -> io::Result<()> {
        writeln!(self.log, "{}", self.sol)
    }

    fn run_stack_loop(&mut self, depth: usize) -> bool {
        while !self.stack.is_empty() {
            let mut state = self.pop_state();
            if self.run_state_loop(depth, &mut state) {
                return true;
            }
        }
        false
    }

    fn run_state_loop(&mut self, depth: usize, state: &mut State<'blk>) -> bool {
        loop {
            if state.depth + state.queue.len() > depth {
                return false;
            }
            self.step_cnt += 1;
            if !self.run_block(state) {
                return false;
            }
            if state.queue.is_empty() {
                return true;
            } else {
                self.split_branch(state);
                return false;
            }
        }
    }

    fn split_branch(&mut self, state: &mut State<'blk>) {
        assert!(!state.queue.is_empty());

        // DFS branching strategy
        // let brchs = state.queue.pop_back().unwrap();

        // BFS branching strategy
        // let brchs = state.queue.pop_front().unwrap();

        // look-ahead branching heuristic
        let brchs = self.look_ahead(state);

        for brch in brchs {
            let mut new_state = state.clone();
            new_state.curr_blk = brch;
            self.push_state(new_state);
        }
    }

    fn look_ahead(&mut self, state: &mut State<'blk>) -> Vec<BlockCtx<'blk>> {
        assert!(!state.queue.is_empty());

        // look-ahead for the first point with branching factor 0 or 1
        let mut vec = Vec::new();
        while let Some(brchs) = state.queue.pop_front() {
            let new_brchs = self.look_ahead_filter(state, brchs);
            if new_brchs.len() <= 1 {
                for brchs in vec.into_iter() {
                    state.queue.push_back(brchs);
                }
                return new_brchs;
            } else {
                vec.push(new_brchs);
            }
        }

        for brchs in vec.into_iter() {
            state.queue.push_back(brchs);
        }

        state.queue.pop_front().unwrap()
    }

    fn look_ahead_filter(
        &mut self,
        state: &State<'blk>,
        brchs: Vec<BlockCtx<'blk>>,
    ) -> Vec<BlockCtx<'blk>> {
        brchs
            .into_iter()
            .filter(|brch| {
                self.sol.savepoint();

                let mut new_state = state.clone();
                new_state.curr_blk = *brch;
                self.step_cnt_la += 1;
                let res = self.run_block(&mut new_state);

                self.sol.backtrack();

                res
            })
            .collect()
    }

    fn run_block(&mut self, state: &mut State<'blk>) -> bool {
        state.depth += 1;

        let curr_blk = state.curr_blk.blk;
        let curr_ctx = state.curr_blk.ctx;

        for (var, atom) in curr_blk.eqs.iter() {
            let var = var.tag_ctx(curr_ctx);
            let atom = atom.tag_ctx(curr_ctx);
            let res = self.sol.bind(var, atom.to_term()).is_ok();
            if !res {
                return false;
            }
        }

        for (var, cons, flds) in curr_blk.cons.iter() {
            let var = var.tag_ctx(curr_ctx);
            let flds = flds
                .iter()
                .map(|fld| fld.tag_ctx(curr_ctx).to_term())
                .collect();
            let res = self.sol.bind(var, Term::Cons((), *cons, flds)).is_ok();
            if !res {
                return false;
            }
        }

        for (prim, args) in curr_blk.prims.iter() {
            let args = args.iter().map(|arg| arg.tag_ctx(curr_ctx)).collect();
            let res = self.sol.solve(*prim, args).is_ok();
            if !res {
                return false;
            }
        }

        for (pred, args) in curr_blk.calls.iter() {
            let callee = &self.dict[&pred];
            assert_eq!(callee.name, *pred);
            let (pars, vars) = (callee.pars.clone(), callee.vars.clone());
            self.ctx_cnt += 1;
            assert_eq!(pars.len(), args.len());
            for ((par, _par_ty), arg) in pars.iter().zip(args.iter()) {
                self.sol.declare(&par.tag_ctx(self.ctx_cnt));
                let par = par.tag_ctx(self.ctx_cnt);
                let arg = arg.tag_ctx(curr_ctx);
                self.sol.bind(par, arg.to_term()).unwrap(); // unify with a fresh variable cannot fail
            }
            for (var, _var_ty) in vars {
                self.sol.declare(&var.tag_ctx(self.ctx_cnt));
            }
            state.curr_blk = callee.blk.tag_ctx(self.ctx_cnt);
            let res = self.run_block(state);
            if !res {
                return false;
            }
        }

        for brchs in curr_blk.brchss.iter() {
            let mut vec = Vec::new();
            for brch in brchs {
                vec.push(brch.tag_ctx(curr_ctx));
            }
            state.queue.push_back(vec);
        }

        true
    }

    pub fn run_loop(&mut self, entry: PredIdent, start: usize, end: usize, step: usize) -> bool {
        // use std::io::Read;
        // let mut stdin = io::stdin();
        // self.write_code().unwrap();

        let mut acc_total: usize = 0;

        for depth in (start..end).into_iter().step_by(step) {
            write!(self.log, "try depth = {}...", depth).unwrap();
            self.log.flush().unwrap();

            self.reset();

            let callee = &self.dict[&entry];
            for (par, _par_ty) in &callee.pars {
                self.sol.declare(&par.tag_ctx(self.ctx_cnt));
            }
            for (var, _var_ty) in &callee.vars {
                self.sol.declare(&var.tag_ctx(self.ctx_cnt));
            }
            let state = State::new(callee.blk.tag_ctx(self.ctx_cnt));
            self.push_state(state);

            let res = self.run_stack_loop(depth);

            let total = self.step_cnt + self.step_cnt_la;
            acc_total += total;

            if res {
                writeln!(self.log, "success! depth = {}", depth).unwrap();
                return true;
            } else {
                writeln!(
                    self.log,
                    "fail. step = {}, step_la = {}(ratio {}), total = {}, acc_total = {} ",
                    self.step_cnt,
                    self.step_cnt_la,
                    (self.step_cnt_la as f32) / (self.step_cnt as f32 + 0.001),
                    total,
                    acc_total
                )
                .unwrap();
            }
        }
        false
    }
}

#[test]
fn test_walker() {
    use crate::logic::ast::*;
    use crate::utils::ident::Ident;

    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if @icmpeq(head, x) then true else is_elem(tail, x)
    | Nil => false
    end
end

predicate is_elem_after_append(xs: IntList, x: Int)
begin
    is_elem(append(xs, x), x) = false
end
    "#;

    let (prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let prog = crate::logic::transform::logic_translation(&prog);
    // println!("{:#?}", prog);

    let map = crate::tych::elab::elab_pass(&prog);
    // println!("{:?}", map);

    let dict = crate::walker::compile::compile_dict(&prog, &map);

    let mut log = std::io::empty();
    let mut wlk = Walker::new(&dict, &mut log);
    let entry = PredIdent::Pos(Ident::dummy(&"is_elem_after_append"));
    assert!(!wlk.run_loop(entry, 10, 100, 10))
}
