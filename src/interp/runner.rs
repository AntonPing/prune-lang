use super::*;
use crate::cli::pipeline::PipeIO;
use crate::interp::config::{RunnerConfig, RunnerStats};
use crate::interp::smt_solver::{SmtBackend, SmtSolver};
use crate::utils::unify::Unifier;

#[derive(Clone, Debug)]
struct Branch {
    depth: usize,
    answers: Vec<(Ident, TermVal<IdentCtx>)>,
    prims: Vec<(Prim, Vec<AtomVal<IdentCtx>>)>,
    calls: Vec<PredCall>,
    cursor: usize,
}

impl Branch {
    fn next_cursor(&mut self) {
        assert!(!self.calls.is_empty());

        if self.cursor >= self.calls.len() {
            self.cursor = 0;
            for call in self.calls.iter_mut() {
                call.history.clear();
            }
        }

        if !self.calls[self.cursor].naive_strategy_pred(1) {
            self.cursor += 1;
        }

        if self.cursor >= self.calls.len() {
            self.cursor = 0;
            for call in self.calls.iter_mut() {
                call.history.clear();
            }
        }
    }
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
struct PredCall {
    pred: Ident,
    polys: Vec<TermType>,
    args: Vec<TermVal<IdentCtx>>,
    history: Vec<(Ident, Vec<usize>)>,
}

impl PredCall {
    #[allow(dead_code)]
    fn left_biased_strategy_pred(&self) -> bool {
        true
    }

    #[allow(dead_code)]
    fn naive_strategy_pred(&self, n: usize) -> bool {
        self.history.len() < n
    }

    #[allow(dead_code)]
    fn struct_recur_strategy_pred(&self) -> bool {
        let args: Vec<usize> = self.args.iter().map(|arg| arg.height()).collect();

        for (pred2, args2) in self.history.iter() {
            if self.pred == *pred2 {
                if !args2.iter().zip(args.iter()).all(|(arg2, arg)| arg2 <= arg) {
                    return false;
                }
            }
        }

        true
    }
}

pub struct RunnerState<'prog, 'io> {
    prog: &'prog Program,
    pipe_io: &'io mut PipeIO,
    config: RunnerConfig,
    stats: RunnerStats,
    ctx_cnt: usize,
    ansr_cnt: usize,
    stack: Vec<Branch>,
    smt_solver: SmtSolver,
}

impl<'prog, 'io> RunnerState<'prog, 'io> {
    pub fn new(
        prog: &'prog Program,
        pipe: &'io mut PipeIO,
        backend: SmtBackend,
    ) -> RunnerState<'prog, 'io> {
        RunnerState {
            prog,
            pipe_io: pipe,
            config: RunnerConfig::new(),
            stats: RunnerStats::new(),
            ctx_cnt: 0,
            ansr_cnt: 0,
            stack: Vec::new(),
            smt_solver: SmtSolver::new(backend),
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

    pub fn run_dfs_with_depth(&mut self, entry: Ident, depth_start: usize, depth_end: usize) {
        self.ctx_cnt = 0;

        let answers: Vec<(Ident, TermVal<IdentCtx>)> = self.prog.preds[&entry]
            .pars
            .iter()
            .map(|(par, _typ)| (*par, Term::Var(par.tag_ctx(self.ctx_cnt))))
            .collect();

        let args: Vec<TermVal<IdentCtx>> = self.prog.preds[&entry]
            .pars
            .iter()
            .map(|(par, _typ)| Term::Var(par.tag_ctx(self.ctx_cnt)))
            .collect();

        // predicate for query can not be polymorphic!
        assert!(self.prog.preds[&entry].polys.is_empty());

        let call = PredCall {
            pred: entry,
            polys: Vec::new(),
            args,
            history: Vec::new(),
        };

        let brch = Branch {
            depth: 0,
            answers,
            prims: Vec::new(),
            calls: vec![call],
            cursor: 0,
        };

        self.stack.push(brch);

        while let Some(brch) = self.stack.pop() {
            if self.ansr_cnt >= self.config.answer_limit {
                return;
            }

            assert!(brch.depth <= depth_end);

            // for (par, val) in brch.answers.iter() {
            //     writeln!(self.pipe_io.output, "{} = {}", par, val).unwrap();
            // }

            // writeln!(self.pipe_io.output, "cursor = {}", brch.cursor).unwrap();

            // for (prim, args) in brch.prims.iter() {
            //     writeln!(self.pipe_io.output, "{:?}({:?})", prim, args).unwrap();
            // }

            // for call in brch.calls.iter() {
            //     writeln!(self.pipe_io.output, "{:?}({:?})", call.pred, call.args,).unwrap();
            // }

            if brch.calls.is_empty() {
                if brch.depth >= depth_start {
                    self.solve_answer(&brch);
                }
            } else if brch.depth + brch.calls.len() <= depth_end {
                self.run_branch_step(brch);
            }
        }
    }

    fn solve_answer(&mut self, brch: &Branch) {
        let start = std::time::Instant::now();

        if let Some(map) = self.smt_solver.check_sat(&brch.prims) {
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

    fn run_branch_step(&mut self, mut brch: Branch) {
        brch.next_cursor();
        let call = brch.calls.remove(brch.cursor);

        let rules = &self.prog.preds[&call.pred].rules.clone();

        for rule in rules.iter().rev() {
            assert_eq!(rule.head.len(), call.args.len());

            self.emit_branch(&brch, rule, &call);
        }
    }

    fn propagate_prims(
        &mut self,
        unifier: &mut Unifier<IdentCtx, LitVal, OptCons<Ident>>,
        prims: &mut Vec<(Prim, Vec<AtomVal<IdentCtx>>)>,
    ) -> Result<(), ()> {
        let mut skip_flags: Vec<bool> = prims.iter().map(|_| false).collect();
        let mut dirty_flag: bool = true;

        while dirty_flag {
            dirty_flag = false;

            for ((prim, args), skip_flag) in prims.iter_mut().zip(skip_flags.iter_mut()) {
                if *skip_flag {
                    continue;
                }

                for arg in args.iter_mut() {
                    *arg = unifier.merge(&arg.to_term()).to_atom().unwrap();
                }

                match super::progagate::propagate_prims(*prim, args) {
                    progagate::PropagateResult::Skip => {
                        // skip, do nothing
                    }
                    progagate::PropagateResult::Propagate(subst) => {
                        for (lhs, rhs) in subst.iter() {
                            unifier
                                .unify(&lhs.to_term(), &rhs.to_term())
                                .map_err(|_| ())?;
                        }
                        *skip_flag = true;
                        if !subst.is_empty() {
                            dirty_flag = true;
                        }
                    }
                    progagate::PropagateResult::Conflit => return Err(()),
                }
            }
        }

        let filtered_prims: Vec<(Prim, Vec<AtomVal<IdentCtx>>)> = prims
            .iter()
            .zip(skip_flags.into_iter())
            .filter_map(|(prim, flag)| if !flag { Some(prim.clone()) } else { None })
            .collect();

        *prims = filtered_prims;

        Ok(())
    }

    fn emit_branch(&mut self, brch: &Branch, rule: &Rule, call: &PredCall) {
        assert_eq!(rule.head.len(), call.args.len());

        self.stats.step();
        self.ctx_cnt += 1;
        let rule_ctx = rule.tag_ctx(self.ctx_cnt);

        let mut unifier: Unifier<IdentCtx, LitVal, OptCons<Ident>> = Unifier::new();
        for (par, arg) in rule_ctx.head.iter().zip(call.args.iter()) {
            if unifier.unify(par, arg).is_err() {
                return;
            }
        }

        let mut new_brch = brch.clone();
        new_brch.depth += 1;

        for (prim, args) in rule_ctx.prims.iter() {
            new_brch.prims.push((*prim, args.clone()));
        }

        let mut new_history = call.history.clone();
        new_history.push((
            call.pred,
            call.args.iter().map(|arg| arg.height()).collect(),
        ));

        for (pred, polys, args) in rule_ctx.calls.iter().rev() {
            let new_call = PredCall {
                pred: *pred,
                polys: polys.clone(),
                args: args.clone(),
                history: new_history.clone(),
            };

            new_brch.calls.insert(new_brch.cursor, new_call);
        }

        if self
            .propagate_prims(&mut unifier, &mut new_brch.prims)
            .is_err()
        {
            return;
        }

        for (_par, val) in new_brch.answers.iter_mut() {
            *val = unifier.merge(val);
        }

        // for (_prim, args) in new_brch.prims.iter_mut() {
        //     for arg in args.iter_mut() {
        //         *arg = unifier.merge(&arg.to_term()).to_atom().unwrap();
        //     }
        // }

        for call in new_brch.calls.iter_mut() {
            for arg in call.args.iter_mut() {
                *arg = unifier.merge(&arg);
            }
        }

        self.stack.push(new_brch);
    }

    pub fn run_iddfs_loop(&mut self, entry: Ident) -> usize {
        for depth_limit in
            (self.config.depth_step..=self.config.depth_limit).step_by(self.config.depth_step)
        {
            writeln!(
                self.pipe_io.stat_log,
                "[RUN]: try depth = {}... (found answer: {})",
                depth_limit, self.ansr_cnt
            )
            .unwrap();

            self.reset();

            self.run_dfs_with_depth(entry, depth_limit - self.config.depth_step + 1, depth_limit);

            let stat_res = self.stats.print_stat();
            writeln!(self.pipe_io.stat_log, "{}", stat_res).unwrap();

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
    let mut runner = RunnerState::new(&prog, &mut pipe_io, SmtBackend::Z3);
    let query = &prog.querys[0];

    for param in query.params.iter() {
        runner.config_set_param(param);
    }
    runner.run_iddfs_loop(query.entry);
}
