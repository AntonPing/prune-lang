use super::*;

use easy_smt::{Context, ContextBuilder, Response, SExpr};

pub struct Constr {
    pub ctx: Context,
    pub int_vars: Vec<(IdentCtx, SExpr)>,
    pub bool_vars: Vec<(IdentCtx, SExpr)>,
    pub saves: Vec<(usize, usize)>,
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
            int_vars: Vec::new(),
            bool_vars: Vec::new(),
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

        self.int_vars.clear();
        self.bool_vars.clear();
        self.saves.clear();
    }

    pub fn savepoint(&mut self) {
        self.ctx.push().unwrap();
        self.saves.push((self.int_vars.len(), self.bool_vars.len()));
    }

    pub fn backtrack(&mut self) {
        self.ctx.pop().unwrap();
        let (int_len, bool_len) = self.saves.pop().unwrap();
        for _ in 0..(self.int_vars.len() - int_len) {
            self.int_vars.pop().unwrap();
        }
        for _ in 0..(self.bool_vars.len() - bool_len) {
            self.bool_vars.pop().unwrap();
        }
    }
}

impl Constr {
    pub fn declare_int(&mut self, var: &IdentCtx) {
        let sexp = self
            .ctx
            .declare_const(format!("{}_i", var).as_str(), self.ctx.int_sort())
            .unwrap();
        self.int_vars.push((*var, sexp));
    }

    pub fn declare_bool(&mut self, var: &IdentCtx) {
        let sexp = self
            .ctx
            .declare_const(format!("{}_b", var).as_str(), self.ctx.bool_sort())
            .unwrap();
        self.bool_vars.push((*var, sexp));
    }

    pub fn declare_var(&mut self, var: &IdentCtx) {
        self.declare_int(var);
        self.declare_bool(var);
    }

    pub fn get_int(&mut self, atom: &Atom) -> SExpr {
        match atom {
            Atom::Var(x) => {
                let res = self
                    .int_vars
                    .iter()
                    .find_map(|(k, v)| if k == x { Some(*v) } else { None })
                    .expect(format!("integer variable {} not declared!", x).as_str());
                res
            }
            Atom::Lit(LitVal::Int(x)) => self.ctx.numeral(*x),
            _ => panic!("atom is not an integer!"),
        }
    }

    pub fn get_bool(&mut self, atom: &Atom) -> SExpr {
        match atom {
            Atom::Var(x) => {
                let res = self
                    .bool_vars
                    .iter()
                    .find_map(|(k, v)| if k == x { Some(*v) } else { None })
                    .expect(format!("boolean variable {} not declared!", x).as_str());
                res
            }
            Atom::Lit(LitVal::Bool(true)) => self.ctx.true_(),
            Atom::Lit(LitVal::Bool(false)) => self.ctx.false_(),
            _ => panic!("atom is not a boolean!"),
        }
    }

    pub fn push_cons(&mut self, prim: Prim, args: Vec<Atom>) {
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

    pub fn push_eq(&mut self, x: IdentCtx, atom: Atom) {
        match atom {
            Atom::Var(_) => {
                let lhs: SExpr = self.get_int(&Atom::Var(x));
                let rhs = self.get_int(&atom);
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();

                let lhs = self.get_bool(&Atom::Var(x));
                let rhs = self.get_bool(&atom);
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
            }
            Atom::Lit(LitVal::Int(_)) => {
                let lhs = self.get_int(&Atom::Var(x));
                let rhs = self.get_int(&atom);
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
            }
            Atom::Lit(LitVal::Bool(_)) => {
                let lhs = self.get_bool(&Atom::Var(x));
                let rhs = self.get_bool(&atom);
                self.ctx.assert(self.ctx.eq(lhs, rhs)).unwrap();
            }
            Atom::Lit(LitVal::Float(_)) => {
                todo!()
            }
            Atom::Lit(LitVal::Char(_)) => {
                todo!()
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
