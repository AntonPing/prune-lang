use std::collections::HashMap;

use super::*;
use crate::utils::env_map::EnvMap;
use easy_smt::{Context, ContextBuilder, Response, SExpr};

pub struct Constr {
    pub ctx: Context,
    pub map: EnvMap<IdentCtx, SExpr>,
}

impl fmt::Debug for Constr {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Constr {
    pub fn new() -> Constr {
        let mut ctx = ContextBuilder::new()
            .solver("z3")
            .solver_args(["-smt2", "-in"])
            .build()
            .unwrap();
        // push an empty context for reset
        ctx.push().unwrap();
        Constr {
            ctx,
            map: EnvMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn reset(&mut self) {
        self.ctx.pop().unwrap();
        assert!(self.ctx.pop().is_err());
        // push an empty context for reset
        self.ctx.push().unwrap();
        self.map.clear();
    }

    pub fn savepoint(&mut self) {
        self.ctx.push().unwrap();
        self.map.enter_scope();
    }

    pub fn backtrack(&mut self) {
        self.ctx.pop().unwrap();
        self.map.leave_scope();
    }
}

impl Constr {
    pub fn declare_var(&mut self, var: &IdentCtx, typ: &LitType) {
        match typ {
            LitType::TyInt => self.declare_int(var),
            LitType::TyFloat => unimplemented!(),
            LitType::TyBool => self.declare_bool(var),
            LitType::TyChar => unimplemented!(),
        }
    }

    pub fn declare_int(&mut self, var: &IdentCtx) {
        assert!(!self.map.contains_key(var));
        let sexp = self
            .ctx
            .declare_const(var.to_string(), self.ctx.int_sort())
            .unwrap();
        self.map.insert(*var, sexp);
    }

    pub fn declare_bool(&mut self, var: &IdentCtx) {
        assert!(!self.map.contains_key(var));
        let sexp = self
            .ctx
            .declare_const(var.to_string(), self.ctx.bool_sort())
            .unwrap();
        self.map.insert(*var, sexp);
    }

    pub fn get_int(&mut self, atom: &AtomCtx) -> SExpr {
        match atom {
            Term::Var(x) => {
                let res = self
                    .map
                    .iter()
                    .find_map(|(k, v)| if k == x { Some(*v) } else { None })
                    .expect(format!("integer variable {} not declared!", x).as_str());
                res
            }
            Term::Lit(LitVal::Int(x)) => self.ctx.numeral(*x),
            _ => panic!("atom is not an integer!"),
        }
    }

    pub fn get_bool(&mut self, atom: &AtomCtx) -> SExpr {
        match atom {
            Term::Var(x) => {
                let res = self
                    .map
                    .iter()
                    .find_map(|(k, v)| if k == x { Some(*v) } else { None })
                    .expect(format!("boolean variable {} not declared!", x).as_str());
                res
            }
            Term::Lit(LitVal::Bool(true)) => self.ctx.true_(),
            Term::Lit(LitVal::Bool(false)) => self.ctx.false_(),
            _ => panic!("atom is not a boolean!"),
        }
    }

    pub fn push_cons(&mut self, prim: Prim, args: Vec<AtomCtx>) {
        match (prim, &args[..]) {
            (
                Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem,
                [arg1, arg2, arg3],
            ) => {
                let arg1 = self.get_int(arg1);
                let arg2 = self.get_int(arg2);
                let arg3 = self.get_int(arg3);
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
                let arg1 = self.get_int(arg1);
                let arg2 = self.get_int(arg2);
                let res = self.ctx.negate(arg1);
                self.ctx.assert(self.ctx.eq(res, arg2)).unwrap();
            }
            (Prim::ICmp(cmp), [arg1, arg2, arg3]) => {
                let arg1 = self.get_int(arg1);
                let arg2 = self.get_int(arg2);
                let arg3 = self.get_bool(arg3);
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
                let arg1 = self.get_bool(arg1);
                let arg2 = self.get_bool(arg2);
                let arg3 = self.get_bool(arg3);
                let res = match prim {
                    Prim::BAnd => self.ctx.and(arg1, arg2),
                    Prim::BOr => self.ctx.or(arg1, arg2),
                    _ => unreachable!(),
                };
                self.ctx.assert(self.ctx.eq(res, arg3)).unwrap();
            }
            (Prim::BNot, [arg1, arg2]) => {
                let arg1 = self.get_bool(arg1);
                let arg2 = self.get_bool(arg2);
                let res = self.ctx.not(arg1);
                self.ctx.assert(self.ctx.eq(res, arg2)).unwrap();
            }
            _ => {
                panic!("wrong arity of primitives!");
            }
        }
    }

    pub fn get_var(&mut self, var: &IdentCtx) -> Option<SExpr> {
        self.map
            .iter()
            .find_map(|(k, v)| if k == var { Some(*v) } else { None })
    }

    pub fn push_eq(&mut self, x: IdentCtx, atom: AtomCtx) -> Option<()> {
        let lhs = self.get_var(&x)?;
        match atom {
            Term::Var(y) => {
                let rhs = self.get_var(&y).unwrap();
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
            }
            Term::Lit(LitVal::Int(_)) => {
                let rhs = self.get_int(&atom);
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
            }
            Term::Lit(LitVal::Bool(_)) => {
                let rhs = self.get_bool(&atom);
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
            }
            Term::Lit(LitVal::Float(_)) => {
                unimplemented!()
            }
            Term::Lit(LitVal::Char(_)) => {
                unimplemented!()
            }
        }
        Some(())
    }

    pub fn check(&mut self) -> bool {
        let result = self.ctx.check().unwrap();
        match result {
            Response::Sat => true,
            Response::Unsat => false,
            Response::Unknown => panic!("smt solver returns unknown!"),
        }
    }

    pub fn get_value(&mut self, vars: &Vec<IdentCtx>) -> Option<HashMap<IdentCtx, LitVal>> {
        let vars_sexp = vars.iter().map(|var| self.get_var(var).unwrap()).collect();
        let map_sexp = self.ctx.get_value(vars_sexp).ok()?;
        let map: HashMap<IdentCtx, LitVal> = vars
            .iter()
            .cloned()
            .zip(map_sexp.iter().map(|(_var, val)| {
                self.ctx
                    .display(*val)
                    .to_string()
                    .as_str()
                    .parse::<LitVal>()
                    .unwrap()
            }))
            .collect();

        Some(map)
    }
}
