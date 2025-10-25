use super::*;

use constr::ConstrSolver;
use easy_smt::{Context, ContextBuilder, SExpr};

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

impl ConstrSolver for IncrSmtSolver {
    fn new() -> Self {
        let mut ctx = ContextBuilder::new()
            .with_z3_defaults()
            // .replay_file(Some(std::fs::File::create("replay.smt2").unwrap()))
            .build()
            .unwrap();
        ctx.set_option(":timeout", ctx.numeral(10)).unwrap();
        // push an empty context for reset
        ctx.push().unwrap();
        IncrSmtSolver {
            ctx,
            level: 1,
            map: EnvMap::new(),
        }
    }

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
        match typ {
            LitType::TyInt => self.declare_int(var),
            LitType::TyFloat => unimplemented!(),
            LitType::TyBool => self.declare_bool(var),
            LitType::TyChar => unimplemented!(),
            LitType::TyUnit => self.declare_unit(var),
        }
    }

    fn push_cons(&mut self, prim: Prim, args: Vec<AtomCtx>) {
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

    fn push_eq(&mut self, x: IdentCtx, atom: AtomCtx) {
        let lhs = self.map[&x].clone();
        match atom {
            Term::Var(y) => {
                let rhs = self.map[&y].clone();
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
            Term::Lit(LitVal::Unit) => {
                // always sat, nothing to do
            }
        }
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

    fn get_value(&mut self, vars: &Vec<IdentCtx>) -> HashMap<IdentCtx, LitVal> {
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
    fn declare_int(&mut self, var: &IdentCtx) {
        assert!(!self.map.contains_key(var));
        let sexp = self
            .ctx
            .declare_const(format!("{:?}", var), self.ctx.int_sort())
            .unwrap();
        self.map.insert(*var, sexp);
    }

    fn declare_bool(&mut self, var: &IdentCtx) {
        assert!(!self.map.contains_key(var));
        let sexp = self
            .ctx
            .declare_const(format!("{:?}", var), self.ctx.bool_sort())
            .unwrap();
        self.map.insert(*var, sexp);
    }

    fn declare_unit(&mut self, var: &IdentCtx) {
        assert!(!self.map.contains_key(var));
        self.map.insert(*var, self.ctx.true_());
    }

    fn get_int(&mut self, atom: &AtomCtx) -> SExpr {
        match atom {
            Term::Var(x) => self
                .map
                .get(x)
                .expect(format!("integer variable {} not declared!", x).as_str())
                .clone(),
            Term::Lit(LitVal::Int(x)) => self.ctx.numeral(*x),
            _ => panic!("atom is not an integer!"),
        }
    }

    fn get_bool(&mut self, atom: &AtomCtx) -> SExpr {
        match atom {
            Term::Var(x) => self
                .map
                .get(x)
                .expect(format!("boolean variable {} not declared!", x).as_str())
                .clone(),
            Term::Lit(LitVal::Bool(true)) => self.ctx.true_(),
            Term::Lit(LitVal::Bool(false)) => self.ctx.false_(),
            _ => panic!("atom is not a boolean!"),
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
