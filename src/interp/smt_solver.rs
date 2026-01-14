use super::*;
use easy_smt::{Context, ContextBuilder, SExpr};

use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SmtBackend {
    Z3,
    CVC5,
}

pub struct SmtSolver {
    ctx: Context,
}

impl SmtSolver {
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
                ctx.set_option(":timeout", ctx.numeral(1000)).unwrap();
            }
            SmtBackend::CVC5 => {
                ctx.set_option(":tlimit-per", ctx.numeral(1000)).unwrap();
            }
        }

        // push an empty context for reset
        ctx.push().unwrap();

        SmtSolver { ctx }
    }

    pub fn check_sat(
        &mut self,
        prims: &Vec<(Prim, Vec<AtomVal<IdentCtx>>)>,
    ) -> Option<HashMap<IdentCtx, LitVal>> {
        // fast path for empty solver query
        if prims.is_empty() {
            return Some(HashMap::new());
        }

        // reset solver state
        self.ctx.pop().unwrap();
        self.ctx.push().unwrap();

        let ty_map: HashMap<IdentCtx, LitType> = self.infer_type(prims);
        let sexp_map = self.solve_constraints(prims, &ty_map);

        let check_res = self.ctx.check().unwrap();
        if check_res == easy_smt::Response::Sat {
            let vars: Vec<IdentCtx> = ty_map.iter().map(|(var, _typ)| *var).collect();
            let res = vars
                .iter()
                .cloned()
                .zip(
                    self.ctx
                        .get_value(vars.iter().map(|var| sexp_map[var]).collect())
                        .unwrap()
                        .iter()
                        .map(|(_var, val)| self.sexp_to_lit_val(*val).unwrap()),
                )
                .collect();

            Some(res)
        } else {
            None
        }
    }

    pub fn get_value(
        &mut self,
        vars: &[IdentCtx],
        map: &HashMap<IdentCtx, SExpr>,
    ) -> HashMap<IdentCtx, LitVal> {
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

    fn infer_type(
        &mut self,
        prims: &Vec<(Prim, Vec<AtomVal<IdentCtx>>)>,
    ) -> HashMap<IdentCtx, LitType> {
        let mut map = HashMap::new();

        for (prim, args) in prims.iter() {
            for (arg, typ) in args.iter().zip(prim.get_typ().iter()) {
                match arg {
                    Term::Var(var) => {
                        if let Some(res) = map.get(var) {
                            assert_eq!(*res, *typ);
                        } else {
                            map.insert(*var, *typ);
                        }
                    }
                    Term::Lit(lit) => {
                        assert_eq!(lit.get_typ(), *typ);
                    }
                    Term::Cons(_, _) => unreachable!(),
                }
            }
        }

        map
    }

    fn solve_constraints(
        &mut self,
        prims: &Vec<(Prim, Vec<AtomVal<IdentCtx>>)>,
        ty_map: &HashMap<IdentCtx, LitType>,
    ) -> HashMap<IdentCtx, SExpr> {
        let sexp_map: HashMap<IdentCtx, SExpr> = ty_map
            .iter()
            .map(|(var, typ)| {
                let sort = match typ {
                    LitType::TyInt => self.ctx.int_sort(),
                    LitType::TyFloat => self.ctx.real_sort(),
                    LitType::TyBool => self.ctx.bool_sort(),
                    LitType::TyChar => todo!(),
                };
                let sexp = self.ctx.declare_const(format!("{:?}", var), sort).unwrap();
                (*var, sexp)
            })
            .collect();

        for (prim, args) in prims.iter() {
            let args: Vec<SExpr> = args
                .iter()
                .map(|arg| self.atom_to_sexp(arg, &sexp_map))
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

        sexp_map
    }

    fn atom_to_sexp(&self, atom: &AtomVal<IdentCtx>, map: &HashMap<IdentCtx, SExpr>) -> SExpr {
        match atom {
            Term::Var(var) => map[var],
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
