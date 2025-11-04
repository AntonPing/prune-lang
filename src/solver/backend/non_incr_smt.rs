use super::*;

use easy_smt::{Context, ContextBuilder, SExpr};

pub struct NonIncrSmtSolver {
    ctx: Context,
    vars_vec: Vec<(IdentCtx, LitType)>,
    cons_vec: Vec<(Prim, Vec<AtomCtx>)>,
    eq_vec: Vec<(IdentCtx, AtomCtx)>,
    saves: Vec<(usize, usize, usize)>,
}

impl fmt::Debug for NonIncrSmtSolver {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl NonIncrSmtSolver {
    pub fn new(backend: SmtBackend) -> Self {
        let mut ctx_bld = ContextBuilder::new();
        match backend {
            SmtBackend::Z3 => {
                ctx_bld.solver("z3").solver_args(["-smt2", "-in", "-v:0"]);
            }
            SmtBackend::CVC5 => {
                ctx_bld
                    .solver("cvc5")
                    .solver_args(["--quiet", "--lang=smt2", "--incremental"]);
            }
        }

        // ctx_bld.replay_file(Some(std::fs::File::create("replay.smt2").unwrap()));
        let mut ctx = ctx_bld.build().unwrap();

        match backend {
            SmtBackend::Z3 => {
                ctx.set_option(":timeout", ctx.numeral(5000)).unwrap();
            }
            SmtBackend::CVC5 => {
                ctx.set_option(":tlimit-per", ctx.numeral(5000)).unwrap();
            }
        }

        NonIncrSmtSolver {
            ctx,
            vars_vec: Vec::new(),
            cons_vec: Vec::new(),
            eq_vec: Vec::new(),
            saves: Vec::new(),
        }
    }
}

impl SmtSolver for NonIncrSmtSolver {
    fn is_empty(&self) -> bool {
        self.vars_vec.is_empty()
            && self.cons_vec.is_empty()
            && self.eq_vec.is_empty()
            && self.saves.is_empty()
    }

    fn reset(&mut self) {
        self.vars_vec.clear();
        self.cons_vec.clear();
        self.eq_vec.clear();
        self.saves.clear();
    }

    fn savepoint(&mut self) {
        self.saves
            .push((self.vars_vec.len(), self.cons_vec.len(), self.eq_vec.len()));
    }

    fn backtrack(&mut self) {
        let (len1, len2, len3) = self.saves.pop().unwrap();
        for _ in 0..(self.vars_vec.len() - len1) {
            self.vars_vec.pop().unwrap();
        }
        for _ in 0..(self.cons_vec.len() - len2) {
            self.cons_vec.pop().unwrap();
        }
        for _ in 0..(self.eq_vec.len() - len3) {
            self.eq_vec.pop().unwrap();
        }
    }

    fn declare_var(&mut self, var: &IdentCtx, typ: &LitType) {
        self.vars_vec.push((*var, *typ));
    }

    fn push_cons(&mut self, prim: Prim, args: Vec<AtomCtx>) {
        self.cons_vec.push((prim, args));
    }

    fn push_eq(&mut self, x: IdentCtx, atom: AtomCtx) {
        self.eq_vec.push((x, atom));
    }

    fn check_complete(&mut self) -> bool {
        self.solve_constraints();
        let result = self.ctx.check().unwrap();
        match result {
            easy_smt::Response::Sat => true,
            easy_smt::Response::Unsat => false,
            easy_smt::Response::Unknown => true,
        }
    }

    fn check_sound(&mut self) -> bool {
        self.solve_constraints();
        let result = self.ctx.check().unwrap();
        match result {
            easy_smt::Response::Sat => true,
            easy_smt::Response::Unsat => false,
            easy_smt::Response::Unknown => false,
        }
    }

    fn get_value(&mut self, vars: &Vec<IdentCtx>) -> HashMap<IdentCtx, LitVal> {
        let map = self.solve_constraints();
        let result = self.ctx.check().unwrap();
        assert_eq!(result, easy_smt::Response::Sat);
        let vars_sexp = vars.iter().map(|var| map[var]).collect();
        let map_sexp = self.ctx.get_value(vars_sexp).unwrap();
        let map: HashMap<IdentCtx, LitVal> = vars
            .iter()
            .cloned()
            .zip(
                map_sexp
                    .iter()
                    .map(|(_var, val)| self.sexp_to_lit_val(*val).unwrap()),
            )
            .collect();
        map
    }
}

impl NonIncrSmtSolver {
    fn solve_constraints(&mut self) -> HashMap<IdentCtx, SExpr> {
        // reset solver state
        self.ctx
            .raw_send(self.ctx.list(vec![self.ctx.atom("reset")]))
            .unwrap();
        self.ctx.raw_recv().unwrap();

        let (_vars_vec, cons_vec) = self.propagate_eqs();

        let map: HashMap<IdentCtx, SExpr> = self
            .vars_vec
            .iter()
            .map(|(var, typ)| {
                let sort = match typ {
                    LitType::TyInt => self.ctx.int_sort(),
                    LitType::TyFloat => self.ctx.real_sort(),
                    LitType::TyBool => self.ctx.bool_sort(),
                    LitType::TyChar => todo!(),
                    LitType::TyUnit => self.ctx.bool_sort(),
                };
                let sexp = self.ctx.declare_const(format!("{:?}", var), sort).unwrap();
                (*var, sexp)
            })
            .collect();

        for (prim, args) in cons_vec.iter() {
            let args: Vec<SExpr> = args
                .iter()
                .map(|arg| self.atom_to_sexp(arg, &map))
                .collect();

            match (prim, &args[..]) {
                (
                    Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem,
                    &[arg1, arg2, arg3],
                ) => {
                    let res = match prim {
                        Prim::IAdd => self.ctx.plus(arg1, arg2),
                        Prim::ISub => self.ctx.sub(arg1, arg2),
                        Prim::IMul => self.ctx.times(arg1, arg2),
                        Prim::IDiv => self.ctx.div(arg1, arg2),
                        Prim::IRem => self.ctx.rem(arg1, arg2),
                        _ => unreachable!(),
                    };
                    self.ctx.assert(self.ctx.eq(res, arg3)).unwrap();
                }
                (Prim::INeg, &[arg1, arg2]) => {
                    let res = self.ctx.negate(arg1);
                    self.ctx.assert(self.ctx.eq(res, arg2)).unwrap();
                }
                (Prim::ICmp(cmp), &[arg1, arg2, arg3]) => {
                    let res = match cmp {
                        Compare::Lt => self.ctx.lt(arg1, arg2),
                        Compare::Le => self.ctx.lte(arg1, arg2),
                        Compare::Eq => self.ctx.eq(arg1, arg2),
                        Compare::Ge => self.ctx.gte(arg1, arg2),
                        Compare::Gt => self.ctx.gt(arg1, arg2),
                        Compare::Ne => self.ctx.not(self.ctx.eq(arg1, arg2)),
                    };
                    self.ctx.assert(self.ctx.eq(res, arg3)).unwrap();
                }
                (Prim::BAnd | Prim::BOr, &[arg1, arg2, arg3]) => {
                    let res = match prim {
                        Prim::BAnd => self.ctx.and(arg1, arg2),
                        Prim::BOr => self.ctx.or(arg1, arg2),
                        _ => unreachable!(),
                    };
                    self.ctx.assert(self.ctx.eq(res, arg3)).unwrap();
                }
                (Prim::BNot, &[arg1, arg2]) => {
                    let res = self.ctx.not(arg1);
                    self.ctx.assert(self.ctx.eq(res, arg2)).unwrap();
                }
                _ => {
                    panic!("wrong arity of primitives!");
                }
            }
        }

        map
    }

    fn propagate_eqs(&self) -> (Vec<(IdentCtx, LitType)>, Vec<(Prim, Vec<AtomCtx>)>) {
        let eq_map: HashMap<IdentCtx, AtomCtx> = self.eq_vec.iter().cloned().collect();
        let mut subst: HashMap<IdentCtx, AtomCtx> = HashMap::new();

        for (key, value) in self.eq_vec.iter() {
            let mut value = value;
            loop {
                if let Term::Var(var) = value {
                    if let Some(value2) = eq_map.get(&var) {
                        value = value2;
                        continue;
                    } else {
                        subst.insert(*key, value.clone());
                        break;
                    }
                } else {
                    subst.insert(*key, value.clone());
                    break;
                }
            }
        }

        let cons_vec: Vec<(Prim, Vec<AtomCtx>)> = self
            .cons_vec
            .iter()
            .map(|(prim, args)| {
                let args = args
                    .iter()
                    .map(|arg| {
                        if let Term::Var(key) = arg {
                            subst.get(key).unwrap_or(arg)
                        } else {
                            arg
                        }
                    })
                    .cloned()
                    .collect();
                (*prim, args)
            })
            .collect();

        let set: std::collections::HashSet<IdentCtx> = cons_vec
            .iter()
            .map(|(_prim, args)| {
                args.iter().filter_map(|arg| {
                    if let Term::Var(key) = arg {
                        Some(*key)
                    } else {
                        None
                    }
                })
            })
            .flatten()
            .collect();

        let vars_vec: Vec<(IdentCtx, LitType)> = self
            .vars_vec
            .iter()
            .filter(|(var, _typ)| set.contains(var))
            .cloned()
            .collect();

        (vars_vec, cons_vec)
    }

    fn atom_to_sexp(&self, atom: &AtomCtx, map: &HashMap<IdentCtx, SExpr>) -> SExpr {
        match atom {
            Term::Var(var) => map[var].clone(),
            Term::Lit(LitVal::Int(x)) => self.ctx.numeral(*x),
            Term::Lit(LitVal::Float(x)) => self.ctx.decimal(*x),
            Term::Lit(LitVal::Bool(x)) => {
                if *x {
                    self.ctx.true_()
                } else {
                    self.ctx.false_()
                }
            }
            Term::Lit(LitVal::Char(_x)) => todo!(),
            Term::Lit(LitVal::Unit) => self.ctx.true_(),
            Term::Cons(_, _ident, _terms) => unreachable!(),
        }
    }

    fn sexp_to_lit_val(&self, sexpr: SExpr) -> Option<LitVal> {
        if let Some(res) = self.ctx.get_i64(sexpr) {
            return Some(LitVal::Int(res));
        }
        if let Some(res) = self.ctx.get_f64(sexpr) {
            return Some(LitVal::Float(res));
        }
        if let Some(res) = self.ctx.get_atom(sexpr) {
            match res {
                "true" => {
                    return Some(LitVal::Bool(true));
                }
                "false" => {
                    return Some(LitVal::Bool(false));
                }
                _ => {
                    return None;
                }
            }
        }

        // todo: basic type `Char``

        None
    }
}
