use super::*;

pub struct IncrSmtSolver {
    pub ctx: Context,
    pub level: usize,
    pub map: EnvMap<IdentCtx, SExpr>,
}

impl fmt::Debug for IncrSmtSolver {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl IncrSmtSolver {
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
        ctx.set_logic("QF_NIA").unwrap();
        match backend {
            SmtBackend::Z3 => {
                ctx.set_option(":timeout", ctx.numeral(100)).unwrap();
            }
            SmtBackend::CVC5 => {
                ctx.set_option(":tlimit-per", ctx.numeral(100)).unwrap();
            }
        }

        // push an empty context for reset
        ctx.push().unwrap();
        IncrSmtSolver {
            ctx,
            level: 1,
            map: EnvMap::new(),
        }
    }
}

impl SmtSolver for IncrSmtSolver {
    fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    fn reset(&mut self) {
        self.ctx.pop_many(self.level).unwrap();
        self.level = 1;
        // push an empty context for reset
        self.ctx.push().unwrap();
        self.map.clear();
    }

    fn savepoint(&mut self) {
        self.ctx.push().unwrap();
        self.level += 1;
        self.map.enter_scope();
    }

    fn backtrack(&mut self) {
        self.ctx.pop().unwrap();
        assert_ne!(self.level, 0);
        self.level -= 1;
        self.map.leave_scope();
    }

    fn declare_var(&mut self, var: &IdentCtx, typ: &LitType) {
        assert!(!self.map.contains_key(var));
        let sort = match typ {
            LitType::TyInt => self.ctx.int_sort(),
            LitType::TyFloat => self.ctx.real_sort(),
            LitType::TyBool => self.ctx.bool_sort(),
            LitType::TyChar => todo!(),
        };
        let sexp = self.ctx.declare_const(format!("{:?}", var), sort).unwrap();
        self.map.insert(*var, sexp);
    }

    fn push_cons(&mut self, prim: Prim, args: Vec<AtomCtx>) {
        let args: Vec<SExpr> = args.iter().map(|arg| self.atom_to_sexp(arg)).collect();
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

    fn push_eq(&mut self, x: IdentCtx, atom: AtomCtx) {
        let lhs = self.map[&x];
        let rhs = self.atom_to_sexp(&atom);
        self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
    }

    fn check_complete(&mut self) -> bool {
        let result = self.ctx.check().unwrap();
        match result {
            easy_smt::Response::Sat => true,
            easy_smt::Response::Unsat => false,
            easy_smt::Response::Unknown => true,
        }
    }

    fn check_sound(&mut self) -> bool {
        let result = self.ctx.check().unwrap();
        match result {
            easy_smt::Response::Sat => true,
            easy_smt::Response::Unsat => false,
            easy_smt::Response::Unknown => false,
        }
    }

    fn get_value(&mut self, vars: &[IdentCtx]) -> HashMap<IdentCtx, LitVal> {
        let vars_sexp = vars.iter().map(|var| self.map[var]).collect();
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

impl IncrSmtSolver {
    fn atom_to_sexp(&self, atom: &AtomCtx) -> SExpr {
        match atom {
            Term::Var(var) => self.map[var],
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
            Term::Cons(_cons, _flds) => unreachable!(),
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
