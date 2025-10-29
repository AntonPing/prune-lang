use super::config::{WalkerConfig, WalkerStat};
use super::path::{Path, PathTree};
use super::*;

use crate::block::ast::*;
use crate::driver::cli::PipeIO;
use crate::solver::solver::Solver;
use crate::utils::ident::IdentCtx;

use std::collections::VecDeque;

#[derive(Clone, Debug)]
struct State {
    depth: usize,
    path: Path,
    queue: VecDeque<Vec<Path>>,
}

impl State {
    fn new(path: Path) -> State {
        State {
            depth: 0,
            path,
            queue: VecDeque::new(),
        }
    }
}

pub struct Walker<'blk, 'io> {
    pipe_io: &'io mut PipeIO,
    dict: &'blk HashMap<Ident, PredDef>,
    path_tree: PathTree,
    config: WalkerConfig,
    stats: WalkerStat,
    stack: Vec<State>,
    ansr_cnt: usize,
    ctx_cnt: usize,
    sol: Solver<crate::solver::incr_smt::IncrSmtSolver>,
}

impl<'blk, 'io> Walker<'blk, 'io> {
    pub fn new(dict: &'blk HashMap<Ident, PredDef>, pipe: &'io mut PipeIO) -> Walker<'blk, 'io> {
        Walker {
            dict,
            pipe_io: pipe,
            path_tree: PathTree::new(),
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

    fn get_block(&self, pred: &Ident, idx: usize) -> &'blk Block {
        &self.dict[pred].blks[idx]
    }

    fn reset(&mut self) {
        self.stats.reset();
        assert!(self.stack.is_empty());
        self.ctx_cnt = 0;
        self.sol.reset();
    }

    fn push_state(&mut self, state: State) {
        self.sol.savepoint();
        self.stack.push(state);
    }

    fn pop_state(&mut self) -> State {
        self.sol.backtrack();
        self.stack.pop().unwrap()
    }

    fn run_stack_loop(&mut self, depth_last: usize, depth: usize, pars: Vec<IdentCtx>) {
        while !self.stack.is_empty() {
            let mut state = self.pop_state();
            if !self.run_state_loop(depth, &mut state) {
                continue;
            }
            if self.sol.check_sound() {
                if state.depth > depth_last && state.depth <= depth {
                    writeln!(self.pipe_io.output, "[ANSWER]: (depth = {})", state.depth).unwrap();
                    let val = self.sol.get_value(&pars);
                    for (par, val) in pars.iter().zip(val.iter()) {
                        writeln!(self.pipe_io.output, "{} = {}", par.ident, val).unwrap();
                    }
                    self.ansr_cnt += 1;
                    if self.ansr_cnt == self.config.answer_limit {
                        break;
                    }
                }
            }
        }
    }

    fn run_state_loop(&mut self, depth: usize, state: &mut State) -> bool {
        loop {
            if state.depth + state.queue.len() > depth {
                return false;
            }
            self.stats.step();
            if !self.run_block(state) {
                // conflict-driven update weight
                self.path_tree.update_path_inc(&state.path);
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

    fn split_branch(&mut self, state: &mut State) {
        assert!(!state.queue.is_empty());

        // DFS branching strategy
        // let brchs = state.queue.pop_back().unwrap();

        // BFS branching strategy
        // let brchs = state.queue.pop_front().unwrap();

        // random branching heuristic
        // let brchs = self.random_branching(state);

        // conflict-driven branching heuristic
        // let brchs = self.conflict_driven_branching(state);

        // conflict-driven branching heuristic with exploit-explore balancing
        let brchs = self.exploit_explore_conflict_driven_branching(state);

        // look-ahead branching heuristic
        // let brchs = self.look_ahead_branching(state);

        for brch in brchs {
            let mut new_state = state.clone();
            new_state.path = brch;
            self.push_state(new_state);
        }
    }

    #[allow(dead_code)]
    fn random_branching(&mut self, state: &mut State) -> Vec<Path> {
        assert!(!state.queue.is_empty());
        let idx = rand::random::<u32>().rem_euclid(state.queue.len() as u32);
        state.queue.remove(idx as usize).unwrap()
    }

    #[allow(dead_code)]
    fn conflict_driven_branching(&mut self, state: &mut State) -> Vec<Path> {
        assert!(!state.queue.is_empty());

        let mut len_vec = Vec::new();
        for paths in state.queue.iter() {
            len_vec.push(paths.len());
        }
        assert!(len_vec.iter().all(|len| *len > 1));

        let conj_vals: Vec<(usize, isize)> = state
            .queue
            .iter()
            .enumerate()
            .map(|(conj, paths)| {
                let mut vec: Vec<isize> = Vec::new();
                for path in paths {
                    vec.push(self.path_tree.get_counter(path));
                }
                vec.iter().map(|val| (conj, *val)).collect::<Vec<_>>()
            })
            .flatten()
            .collect();

        let (conj, _val) = conj_vals
            .iter()
            .rev()
            .max_by_key(|(_conj, val)| if *val > 0 { 1 } else { 0 })
            .unwrap();

        let paths = state.queue.remove(*conj).unwrap();
        for path in paths.iter() {
            self.path_tree.update_path_dec(&path);
        }

        paths
    }

    #[allow(dead_code)]
    fn exploit_explore_conflict_driven_branching(&mut self, state: &mut State) -> Vec<Path> {
        assert!(!state.queue.is_empty());

        let mut len_vec = Vec::new();
        for paths in state.queue.iter() {
            len_vec.push(paths.len());
        }
        assert!(len_vec.iter().all(|len| *len > 1));

        let conj_vals: Vec<(usize, isize)> = state
            .queue
            .iter()
            .enumerate()
            .map(|(conj, paths)| {
                let mut vec: Vec<isize> = Vec::new();
                for path in paths {
                    vec.push(self.path_tree.get_last_conflict_branch_diff(path));
                }
                vec.iter().map(|val| (conj, *val)).collect::<Vec<_>>()
            })
            .flatten()
            .collect();

        let (conj, _val) = conj_vals
            .iter()
            .rev()
            .max_by_key(|(_conj, val)| *val)
            .unwrap();

        let paths = state.queue.remove(*conj).unwrap();
        for path in paths.iter() {
            self.path_tree.update_path_dec(&path);
        }

        paths
    }

    #[allow(dead_code)]
    fn look_ahead_branching(&mut self, state: &mut State) -> Vec<Path> {
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

    fn look_ahead_filter(&mut self, state: &State, brchs: Vec<Path>) -> Vec<Path> {
        brchs
            .into_iter()
            .filter(|brch| {
                self.sol.savepoint();

                let mut new_state = state.clone();
                new_state.path = brch.clone();
                let res = self.run_block(&mut new_state);

                self.sol.backtrack();

                if res {
                    self.stats.step_la();
                } else {
                    self.stats.step();
                }

                res
            })
            .collect()
    }

    fn run_block(&mut self, state: &mut State) -> bool {
        state.depth += 1;

        let curr_blk = self.get_block(&state.path.pred, state.path.idx);
        let curr_ctx = state.path.ctx;

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

        for brchs in curr_blk.brchss.iter() {
            let mut paths = Vec::new();
            for brch in brchs {
                paths.push(state.path.jump(*brch));
            }
            state.queue.push_back(paths);
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

            let curr_path = state.path.clone();
            let call_path = state.path.call(*pred, self.ctx_cnt);

            state.path = call_path;
            let res = self.run_block(state);
            state.path = curr_path;

            if !res {
                return false;
            }
        }

        true
    }

    pub fn run_loop(&mut self, entry: Ident) -> usize {
        for depth in (self.config.depth_step..=self.config.depth_limit)
            .into_iter()
            .step_by(self.config.depth_step)
        {
            writeln!(
                self.pipe_io.stat_log,
                "[RUN]: try depth = {}... (found answer: {})",
                depth, self.ansr_cnt
            )
            .unwrap();

            self.reset();

            let callee = &self.dict[&entry];
            for (par, par_ty) in callee.pars.iter() {
                self.sol.declare(&par.tag_ctx(0), par_ty);
            }
            for (var, var_ty) in callee.vars.iter() {
                self.sol.declare(&var.tag_ctx(0), var_ty);
            }

            self.push_state(State::new(Path::new(entry)));

            let pars: Vec<IdentCtx> = callee
                .pars
                .iter()
                .map(|(par, _par_ty)| par.tag_ctx(0))
                .collect();

            self.run_stack_loop(depth - self.config.depth_step, depth, pars);

            let stat_res = self.stats.print_stat();

            writeln!(self.pipe_io.stat_log, "{}", stat_res).unwrap();

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
    | Cons(head, tail) => if head == x then true else is_elem(tail, x) 
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

    let prog = crate::block::compile::compile_dict(&prog);
    // println!("{:#?}", dict);

    let mut pipe_io = PipeIO::empty();
    let mut wlk = Walker::new(&prog.preds, &mut pipe_io);
    let query = &prog.querys[0];

    for param in query.params.iter() {
        wlk.config_set_param(param);
    }
    assert!(wlk.run_loop(query.entry) > 0)
}
