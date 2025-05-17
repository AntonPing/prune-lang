use super::*;

use easy_smt::{Context, ContextBuilder, Response, SExpr};
use std::collections::HashMap;

pub struct Constr {
    pub ctx: Context,
    pub map: HashMap<Ident, LitType>,
    pub vars: Vec<(IdentCtx, SExpr)>,
    pub saves: Vec<usize>,
}

impl fmt::Debug for Constr {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Constr {
    pub fn new(map: HashMap<Ident, LitType>) -> Constr {
        let mut ctx = ContextBuilder::new()
            .solver("z3")
            .solver_args(["-smt2", "-in"])
            .build()
            .unwrap();
        // push an empty context for reset
        ctx.push().unwrap();
        Constr {
            ctx,
            map,
            vars: Vec::new(),
            saves: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.saves.is_empty()
    }

    pub fn reset(&mut self) {
        self.ctx.pop().unwrap();
        assert!(self.ctx.pop().is_err());
        // push an empty context for reset
        self.ctx.push().unwrap();

        self.vars.clear();
        self.saves.clear();
    }

    pub fn savepoint(&mut self) {
        self.ctx.push().unwrap();
        self.saves.push(self.vars.len());
    }

    pub fn backtrack(&mut self) {
        self.ctx.pop().unwrap();
        let len = self.saves.pop().unwrap();
        for _ in 0..(self.vars.len() - len) {
            self.vars.pop().unwrap();
        }
    }
}

impl Constr {
    pub fn get_int(&mut self, term: &Term<IdentCtx>) -> Option<SExpr> {
        match term {
            Term::Var(x) => {
                if let Some(sexp) = self
                    .vars
                    .iter()
                    .find_map(|(k, v)| if k == x { Some(v) } else { None })
                {
                    Some(*sexp)
                } else {
                    let sexp = self
                        .ctx
                        .declare_const(format!("{}", x).as_str(), self.ctx.int_sort())
                        .unwrap();
                    self.vars.push((*x, sexp));
                    Some(sexp)
                }
            }
            Term::Lit(LitVal::Int(x)) => Some(self.ctx.numeral(*x)),
            _ => None,
        }
    }

    pub fn get_bool(&mut self, term: &Term<IdentCtx>) -> Option<SExpr> {
        match term {
            Term::Var(x) => {
                if let Some(sexp) = self
                    .vars
                    .iter()
                    .find_map(|(k, v)| if k == x { Some(v) } else { None })
                {
                    Some(*sexp)
                } else {
                    let sexp = self
                        .ctx
                        .declare_const(format!("{}", x).as_str(), self.ctx.bool_sort())
                        .unwrap();
                    self.vars.push((*x, sexp));
                    Some(sexp)
                }
            }
            Term::Lit(LitVal::Bool(true)) => Some(self.ctx.true_()),
            Term::Lit(LitVal::Bool(false)) => Some(self.ctx.false_()),
            _ => None,
        }
    }

    pub fn push_cons(&mut self, prim: Prim, args: Vec<Term<IdentCtx>>) {
        match (prim, &args[..]) {
            (
                Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem,
                [arg1, arg2, arg3],
            ) => {
                let arg1 = self.get_int(arg1).unwrap();
                let arg2 = self.get_int(arg2).unwrap();
                let arg3 = self.get_int(arg3).unwrap();
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
            (Prim::INeg, [arg1, arg2]) => {
                let arg1 = self.get_int(arg1).unwrap();
                let arg2 = self.get_int(arg2).unwrap();
                let res = self.ctx.negate(arg1);
                self.ctx.assert(self.ctx.eq(res, arg2)).unwrap();
            }
            (Prim::ICmp(cmp), [arg1, arg2, arg3]) => {
                let arg1 = self.get_int(arg1).unwrap();
                let arg2 = self.get_int(arg2).unwrap();
                let arg3 = self.get_bool(arg3).unwrap();
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
            (Prim::BAnd | Prim::BOr, [arg1, arg2, arg3]) => {
                let arg1 = self.get_bool(arg1).unwrap();
                let arg2 = self.get_bool(arg2).unwrap();
                let arg3 = self.get_bool(arg3).unwrap();
                let res = match prim {
                    Prim::BAnd => self.ctx.and(arg1, arg2),
                    Prim::BOr => self.ctx.or(arg1, arg2),
                    _ => unreachable!(),
                };
                self.ctx.assert(self.ctx.eq(res, arg3)).unwrap();
            }
            (Prim::BNot, [arg1, arg2]) => {
                let arg1 = self.get_bool(arg1).unwrap();
                let arg2 = self.get_bool(arg2).unwrap();
                let res = self.ctx.not(arg1);
                self.ctx.assert(self.ctx.eq(res, arg2)).unwrap();
            }
            _ => {
                panic!("wrong arity of primitives!");
            }
        }
    }

    pub fn push_eq(&mut self, x: IdentCtx, term: Term<IdentCtx>) {
        if let Some(typ) = self.map.get(&x.ident) {
            match *typ {
                LitType::TyInt => {
                    let x = self.get_int(&Term::Var(x)).unwrap();
                    let term = self.get_int(&term).unwrap();
                    self.ctx.assert(self.ctx.eq(x, term)).unwrap();
                }
                LitType::TyFloat => todo!(),
                LitType::TyBool => {
                    let x = self.get_bool(&Term::Var(x)).unwrap();
                    let term = self.get_bool(&term).unwrap();
                    self.ctx.assert(self.ctx.eq(x, term)).unwrap();
                }
                LitType::TyChar => todo!(),
            }
        }
    }

    pub fn solve(&mut self) -> bool {
        let result = self.ctx.check().unwrap();
        match result {
            Response::Sat => true,
            Response::Unsat => false,
            Response::Unknown => panic!("smt solver returns unknown!"),
        }
    }
}
