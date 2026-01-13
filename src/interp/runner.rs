use super::*;
use crate::cli::pipeline::PipeIO;
use crate::interp::config::{RunnerConfig, RunnerStats};
use crate::interp::smt_solver::SmtSolver;
use crate::logic::ast::*;
use crate::utils::unify::Unifier;

#[derive(Clone, Debug)]
struct Branch {
    depth: usize,
    answers: Vec<(Ident, TermCtx)>,
    prims: Vec<(Prim, Vec<AtomCtx>)>,
    calls: Vec<(Ident, Vec<TypeId>, Vec<TermCtx>)>,
}

pub struct RunnerState<'prog, 'io> {
    prog: &'prog Program,
    pipe_io: &'io mut PipeIO,
    config: RunnerConfig,
    stats: RunnerStats,
    ctx_cnt: usize,
    ansr_cnt: usize,
    stack: Vec<Branch>,
}

impl<'prog, 'io> RunnerState<'prog, 'io> {
    pub fn new(prog: &'prog Program, pipe: &'io mut PipeIO) -> RunnerState<'prog, 'io> {
        RunnerState {
            prog,
            pipe_io: pipe,
            config: RunnerConfig::new(),
            stats: RunnerStats::new(),
            ctx_cnt: 0,
            ansr_cnt: 0,
            stack: Vec::new(),
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

    fn term_add_ctx<L: Copy, C: Copy>(&mut self, term: &Term<Ident, L, C>) -> Term<IdentCtx, L, C> {
        match term {
            Term::Var(var) => Term::Var(var.tag_ctx(self.ctx_cnt)),
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(cons, args) => {
                let args = args.iter().map(|arg| self.term_add_ctx(arg)).collect();
                Term::Cons(*cons, args)
            }
        }
    }

    pub fn run_dfs_with_depth(&mut self, entry: Ident, depth_start: usize, depth_end: usize) {
        self.ctx_cnt = 0;

        let answers: Vec<(Ident, TermCtx)> = self.prog.preds[&entry]
            .pars
            .iter()
            .map(|(par, _typ)| (*par, Term::Var(par.tag_ctx(self.ctx_cnt))))
            .collect();

        let args: Vec<TermCtx> = self.prog.preds[&entry]
            .pars
            .iter()
            .map(|(par, _typ)| Term::Var(par.tag_ctx(self.ctx_cnt)))
            .collect();

        // predicate for query can not be polymorphic!
        assert!(self.prog.preds[&entry].polys.is_empty());

        let brch = Branch {
            depth: 0,
            answers,
            prims: Vec::new(),
            calls: vec![(entry, Vec::new(), args)],
        };

        self.stack.push(brch);

        while let Some(brch) = self.stack.pop() {
            assert!(brch.depth <= depth_end);

            // for (par, val) in brch.answers.iter() {
            //     writeln!(self.pipe_io.output, "{} = {}", par, val).unwrap();
            // }

            // for (prim, args) in brch.prims.iter() {
            //     writeln!(self.pipe_io.output, "{:?}({:?})", prim, args).unwrap();
            // }

            // for (pred, _polys, args) in brch.calls.iter() {
            //     writeln!(self.pipe_io.output, "{:?}({:?})", pred, args).unwrap();
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
        writeln!(self.pipe_io.output, "[ANSWER]: depth = {}", brch.depth).unwrap();

        let mut solver = SmtSolver::new(smt_solver::SmtBackend::Z3);

        if let Some(map) = solver.check_sat(&brch.prims) {
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

    fn run_branch_step(&mut self, brch: Branch) {
        let mut brch = brch;
        let (pred, _polys, args) = brch.calls.remove(0);

        let rules = &self.prog.preds[&pred].rules.clone();

        for rule in rules.iter().rev() {
            assert_eq!(rule.head.len(), args.len());

            self.emit_branch(&brch, rule, &args);
        }
    }

    fn propagate_prims(
        &mut self,
        unifier: &mut Unifier<IdentCtx, LitVal, OptCons<Ident>>,
        prims: &mut Vec<(Prim, Vec<AtomCtx>)>,
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

        Ok(())
    }

    fn emit_branch(&mut self, brch: &Branch, rule: &Rule, args: &Vec<TermCtx>) {
        assert_eq!(rule.head.len(), args.len());

        self.stats.step();
        self.ctx_cnt += 1;

        let pars: Vec<TermCtx> = rule.head.iter().map(|par| self.term_add_ctx(par)).collect();

        let new_prims: Vec<(Prim, Vec<AtomCtx>)> = rule
            .prims
            .iter()
            .map(|(prim, args)| {
                (
                    *prim,
                    args.iter().map(|arg| self.term_add_ctx(arg)).collect(),
                )
            })
            .collect();

        let new_calls: Vec<(Ident, Vec<TypeId>, Vec<TermCtx>)> = rule
            .calls
            .iter()
            .map(|(pred, poly, args)| {
                (
                    *pred,
                    poly.clone(),
                    args.iter().map(|arg| self.term_add_ctx(arg)).collect(),
                )
            })
            .collect();

        let mut unifier: Unifier<IdentCtx, LitVal, OptCons<Ident>> = Unifier::new();
        for (par, arg) in pars.iter().zip(args.iter()) {
            if unifier.unify(par, arg).is_err() {
                return;
            }
        }

        let mut new_brch = brch.clone();
        new_brch.depth += 1;

        for (prim, args) in new_prims.iter() {
            new_brch.prims.push((*prim, args.clone()));
        }

        for (pred, _polys, args) in new_calls.iter() {
            new_brch.calls.push((*pred, Vec::new(), args.clone()));
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

        for (_pred, _polys, args) in new_brch.calls.iter_mut() {
            for arg in args.iter_mut() {
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

    let mut prog = crate::logic::transform::logic_translation(&prog);
    crate::logic::elab::elab_pass(&mut prog);
    crate::logic::normalize::normalize_pass(&mut prog);
    // println!("{:#?}", prog);

    let mut pipe_io = PipeIO::empty();
    let mut runner = RunnerState::new(&prog, &mut pipe_io);
    let query = &prog.querys[0];

    for param in query.params.iter() {
        runner.config_set_param(param);
    }
    runner.run_iddfs_loop(query.entry);
}
