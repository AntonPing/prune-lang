use super::compile::{BlockCtx, PredDef};
use super::config::WalkerStat;
use super::*;

use crate::solver::solver::Solver;
use crate::utils::ident::IdentCtx;
use crate::walker::config::WalkerConfig;

use std::collections::VecDeque;

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
pub struct Walker<'blk> {
    dict: &'blk HashMap<PredIdent, PredDef>,
    config: WalkerConfig,
    stats: WalkerStat,
    stack: Vec<State<'blk>>,
    ansr_cnt: usize,
    ctx_cnt: usize,
    sol: Solver,
}

impl<'blk> Walker<'blk> {
    pub fn new(dict: &'blk HashMap<PredIdent, PredDef>) -> Walker<'blk> {
        Walker {
            dict,
            config: WalkerConfig::new(),
            stats: WalkerStat::new(),
            stack: Vec::new(),
            ansr_cnt: 0,
            ctx_cnt: 0,
            sol: Solver::new(),
        }
    }

    pub fn config_reset_default(&mut self) {
        self.config.reset_default();
    }

    pub fn config_set_param(&mut self, param: &QueryParam) {
        self.config.set_param(param);
    }

    fn reset(&mut self) {
        self.stats.reset();
        assert!(self.stack.is_empty());
        self.ctx_cnt = 0;
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

    fn run_stack_loop(&mut self, depth_last: usize, depth: usize, pars: Vec<IdentCtx>) {
        while !self.stack.is_empty() {
            let mut state = self.pop_state();
            if self.run_state_loop(depth, &mut state)
                && state.depth >= depth_last
                && state.depth < depth
            {
                println!("[ANSWER]: (depth = {})", state.depth);
                for par in pars.iter() {
                    println!("{} = {}", par.ident, self.sol.get_value(*par));
                }
                self.ansr_cnt += 1;
                if self.ansr_cnt == self.config.answer_limit {
                    break;
                }
            }
        }
    }

    fn run_state_loop(&mut self, depth: usize, state: &mut State<'blk>) -> bool {
        loop {
            if state.depth + state.queue.len() >= depth {
                return false;
            }
            self.stats.step();
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

        // random branching heuristic
        // let brchs = self.random_branching(state);

        // look-ahead branching heuristic
        let brchs = self.look_ahead_branching(state);

        for brch in brchs {
            let mut new_state = state.clone();
            new_state.curr_blk = brch;
            self.push_state(new_state);
        }
    }

    #[allow(dead_code)]
    fn random_branching(&mut self, state: &mut State<'blk>) -> Vec<BlockCtx<'blk>> {
        assert!(!state.queue.is_empty());
        let idx = rand::random::<u32>().rem_euclid(state.queue.len() as u32);
        state.queue.remove(idx as usize).unwrap()
    }

    #[allow(dead_code)]
    fn look_ahead_branching(&mut self, state: &mut State<'blk>) -> Vec<BlockCtx<'blk>> {
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
                self.stats.step_la();
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
            for ((par, par_ty), arg) in pars.iter().zip(args.iter()) {
                let par = par.tag_ctx(self.ctx_cnt);
                self.sol.declare(&par, par_ty);
                let arg = arg.tag_ctx(curr_ctx);
                self.sol.bind(par, arg.to_term()).unwrap(); // unify with a fresh variable cannot fail
            }
            for (var, var_ty) in vars.iter() {
                self.sol.declare(&var.tag_ctx(self.ctx_cnt), var_ty);
            }
            state.curr_blk = callee.blks[0].tag_ctx(self.ctx_cnt);
            let res = self.run_block(state);
            if !res {
                return false;
            }
        }

        for brchs in curr_blk.brchss.iter() {
            let mut vec = Vec::new();
            for brch in brchs {
                let blk = self.dict[&curr_blk.pred.0].blks[*brch].tag_ctx(curr_ctx);
                vec.push(blk);
            }
            state.queue.push_back(vec);
        }

        true
    }

    pub fn run_loop(&mut self, entry: PredIdent) -> usize {
        for depth in (self.config.depth_step..self.config.depth_limit)
            .into_iter()
            .step_by(self.config.depth_step)
        {
            eprintln!(
                "[RUN]: try depth = {}... (found answer: {})",
                depth, self.ansr_cnt
            );

            self.reset();

            let callee = &self.dict[&entry];
            for (par, par_ty) in callee.pars.iter() {
                self.sol.declare(&par.tag_ctx(0), par_ty);
            }
            for (var, var_ty) in callee.vars.iter() {
                self.sol.declare(&var.tag_ctx(0), var_ty);
            }
            let state = State::new(callee.blks[0].tag_ctx(0));
            self.push_state(state);

            let pars: Vec<IdentCtx> = callee
                .pars
                .iter()
                .map(|(par, _par_ty)| par.tag_ctx(0))
                .collect();

            self.run_stack_loop(depth - self.config.depth_step, depth, pars);

            let stat_res = self.stats.print_stat();

            eprintln!("{}", stat_res);

            if self.ansr_cnt >= self.config.answer_limit {
                return self.ansr_cnt;
            }
        }
        return self.ansr_cnt;
    }
}

#[test]
fn test_walker() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> IntList
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Nil
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

query is_elem_after_append(depth_step=5, depth_limit=1000, answer_limit=1)
    "#;

    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let errs = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    let errs = crate::tych::check::check_pass(&prog);
    assert!(errs.is_empty());

    let prog = crate::logic::transform::logic_translation(&prog);
    // println!("{:#?}", prog);

    let map = crate::tych::elab::elab_pass(&prog);
    // println!("{:?}", map);

    let dict = crate::walker::compile::compile_dict(&prog, &map);
    // println!("{:#?}", dict);

    let mut wlk = Walker::new(&dict);
    let query = &prog.querys[0];

    for param in query.params.iter() {
        wlk.config_set_param(param);
    }
    assert!(wlk.run_loop(query.entry) > 0)
}
