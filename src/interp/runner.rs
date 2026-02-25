use super::config::{RunnerConfig, RunnerStats};
use super::solver;
use super::strategy::*;
use super::*;
use crate::cli::args::{self, Heuristic};
use crate::cli::pipeline::PipeIO;

pub struct RunnerState<'prog, 'io> {
    prog: &'prog Program,
    pipe_io: &'io mut PipeIO,
    config: RunnerConfig,
    stats: RunnerStats,
    ctx_cnt: usize,
    ansr_cnt: usize,
    stack: Vec<Branch>,
    solver: Box<dyn solver::common::PrimSolver>,
    heuristic: args::Heuristic,
}

impl<'prog, 'io> RunnerState<'prog, 'io> {
    pub fn new(
        prog: &'prog Program,
        pipe: &'io mut PipeIO,
        solver: args::Solver,
        heuristic: args::Heuristic,
    ) -> RunnerState<'prog, 'io> {
        let solver: Box<dyn solver::common::PrimSolver> = match solver {
            args::Solver::Z3 => Box::new(super::solver::smtlib::SmtLibSolver::new(
                super::solver::smtlib::SolverBackend::Z3,
            )),
            args::Solver::CVC5 => Box::new(super::solver::smtlib::SmtLibSolver::new(
                super::solver::smtlib::SolverBackend::CVC5,
            )),
            args::Solver::NoSmt => Box::new(super::solver::no_smt::NoSmtSolver::new()),
        };

        RunnerState {
            prog,
            pipe_io: pipe,
            config: RunnerConfig::new(),
            stats: RunnerStats::new(),
            ctx_cnt: 0,
            ansr_cnt: 0,
            stack: Vec::new(),
            solver,
            heuristic,
        }
    }

    pub fn config_set_param(&mut self, param: &QueryParam) {
        self.config.set_param(param);
    }

    fn reset(&mut self) {
        self.stats.reset();
        assert!(self.stack.is_empty());
        self.ctx_cnt = 0;
    }

    pub fn run_dfs_with_depth(&mut self, pred: Ident, depth_start: usize, depth_end: usize) {
        // predicate for query can not be polymorphic!
        assert!(self.prog.preds[&pred].polys.is_empty());

        self.ctx_cnt = 0;
        let pars: Vec<Ident> = self.prog.preds[&pred]
            .pars
            .iter()
            .map(|(par, _typ)| *par)
            .collect();

        let rules = &self.prog.preds[&pred].rules;
        let mut call = PredCall {
            pred,
            polys: Vec::new(),
            args: pars.iter().map(|par| Term::Var(par.tag_ctx(0))).collect(),
            looks: (0..rules.len()).collect(),
            history: History::new(),
        };

        if self.heuristic == Heuristic::LookAhead {
            self.stats.step_la();
            call.lookahead_update(&rules);
        }

        let brch = Branch {
            depth: 0,
            answers: pars
                .iter()
                .map(|par| (*par, Term::Var(par.tag_ctx(0))))
                .collect(),
            prims: Vec::new(),
            calls: vec![call],
        };

        self.stack.push(brch);

        while let Some(mut brch) = self.stack.pop() {
            // write!(self.pipe_io.tree, "{}", brch).unwrap();
            if self.ansr_cnt >= self.config.answer_limit {
                return;
            }
            assert!(brch.depth <= depth_end);
            if brch.calls.is_empty() {
                if brch.depth >= depth_start {
                    self.solve_answer(&brch);
                }
            } else if brch.depth + brch.calls.len() <= depth_end {
                self.run_branch_step(&mut brch);
            }
        }
    }

    fn solve_answer(&mut self, brch: &Branch) {
        let start = std::time::Instant::now();

        if let Some(map) = self.solver.check_sat(&brch.prims) {
            let duration = start.elapsed();
            writeln!(
                self.pipe_io.output,
                "[ANSWER]: depth = {}, solving time = {:?}",
                brch.depth, duration
            )
            .unwrap();

            let map = map
                .into_iter()
                .map(|(var, lit)| (var, Term::Lit(lit)))
                .collect();

            for (par, val) in brch.answers.iter() {
                writeln!(self.pipe_io.output, "{} = {}", par, val.substitute(&map)).unwrap();
            }
            self.ansr_cnt += 1;
        }
    }

    fn run_branch_step(&mut self, brch: &mut Branch) {
        let call_idx = match self.heuristic {
            args::Heuristic::LeftBiased => brch.left_biased_strategy(),
            args::Heuristic::Interleave => brch.naive_strategy(1),
            args::Heuristic::StructRecur => brch.struct_recur_strategy(),
            args::Heuristic::LookAhead => brch.lookahead_strategy(),
            args::Heuristic::Random => brch.random_strategy(),
        };

        for rule_idx in brch.calls[call_idx].looks.iter().rev() {
            self.stats.step();
            self.ctx_cnt += 1;
            if let Ok(new_brch) = self.apply_rule(&brch, call_idx, *rule_idx) {
                self.stack.push(new_brch);
            }
        }
    }

    fn apply_rule(
        &mut self,
        brch: &Branch,
        call_idx: usize,
        rule_idx: usize,
    ) -> Result<Branch, ()> {
        let rules = &self.prog.preds[&brch.calls[call_idx].pred].rules;
        let rule_ctx = rules[rule_idx].tag_ctx(self.ctx_cnt);

        let call = &brch.calls[call_idx];
        assert_eq!(rule_ctx.head.len(), call.args.len());

        let mut unifier: Unifier<IdentCtx, LitVal, OptCons<Ident>> = Unifier::new();
        for (par, arg) in rule_ctx.head.iter().zip(call.args.iter()) {
            if unifier.unify(par, arg).is_err() {
                return Err(());
            }
        }

        let mut new_brch = brch.clone();
        new_brch.depth += 1;
        new_brch.remove(call_idx);

        for (prim, args) in rule_ctx.prims.iter() {
            new_brch.prims.push((*prim, args.clone()));
        }

        if super::progagate::propagate_unify(&mut new_brch.prims, &mut unifier).is_err() {
            return Err(());
        }

        let mut new_history = call.history.clone();
        new_history.push(
            call.pred,
            call.args.iter().map(|arg| arg.height()).collect(),
        );

        for (pred, polys, args) in rule_ctx.calls.iter().rev() {
            let mut new_call = PredCall {
                pred: *pred,
                polys: polys.clone(),
                args: args.clone(),
                looks: (0..self.prog.preds[pred].rules.len()).collect(),
                history: new_history.clone(),
            };

            if self.heuristic == Heuristic::LookAhead {
                self.stats.step_la();
                new_call.lookahead_update(&self.prog.preds[pred].rules);
            }

            new_brch.insert(call_idx, new_call);
        }

        for call in new_brch.calls.iter_mut() {
            let mut dirty_flag = false;
            for arg in call.args.iter_mut() {
                if let Some(new_arg) = unifier.subst_opt(arg) {
                    *arg = new_arg;
                    dirty_flag = true;
                }
            }
            // update lookahead information if any information is propagated
            if dirty_flag && self.heuristic == Heuristic::LookAhead {
                self.stats.step_la();
                call.lookahead_update(&self.prog.preds[&call.pred].rules);
            }
        }

        for (_par, val) in new_brch.answers.iter_mut() {
            *val = unifier.subst(val);
        }

        Ok(new_brch)
    }

    pub fn run_iddfs_loop(&mut self, entry: Ident) -> usize {
        for depth_limit in
            (self.config.depth_step..=self.config.depth_limit).step_by(self.config.depth_step)
        {
            writeln!(
                self.pipe_io.stat,
                "[RUN]: try depth = {}... (found answer: {})",
                depth_limit, self.ansr_cnt
            )
            .unwrap();

            self.reset();

            self.run_dfs_with_depth(entry, depth_limit - self.config.depth_step + 1, depth_limit);

            let stat_res = self.stats.print_stat();
            writeln!(self.pipe_io.stat, "{}", stat_res).unwrap();

            if self.ansr_cnt >= self.config.answer_limit {
                return self.ansr_cnt;
            }
        }
        self.ansr_cnt
    }
}

#[test]
fn test_runner() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> IntList
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if head == x then true else is_elem(tail, x) 
    | Nil => false
    end
end

function is_elem_after_append(xs: IntList, x: Int) -> Bool
begin
    guard !is_elem(append(xs, x), x);
    true
end

query is_elem_after_append(depth_step=5, depth_limit=50, answer_limit=100)
    "#;

    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let errs = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    let errs = crate::tych::check::check_pass(&prog);
    assert!(errs.is_empty());

    let mut prog = crate::logic::compile::compile_pass(&prog);
    crate::logic::elaborate::elaborate_pass(&mut prog);

    // println!("{:#?}", prog);

    let mut pipe_io = PipeIO::empty();
    let mut runner = RunnerState::new(
        &prog,
        &mut pipe_io,
        args::Solver::Z3,
        args::Heuristic::Interleave,
    );
    let query = &prog.querys[0];

    for param in query.params.iter() {
        runner.config_set_param(param);
    }
    runner.run_iddfs_loop(query.entry);
}
