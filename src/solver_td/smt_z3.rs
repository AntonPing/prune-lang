use easy_smt::{Context, ContextBuilder, Response, SExpr};

use super::*;

pub struct Constr {
    pub ctx: Context,
    pub vars: Vec<(IdentCtx, SExpr)>,
    pub prims: Vec<(Prim, Vec<TermCtx>)>,
    pub save: Vec<(usize, usize)>,
}

impl std::fmt::Debug for Constr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (prim, args) in self.prims.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "{prim:?}({args:?})")?;
        }
        let save = self.save.iter().format(&", ");
        writeln!(f, "constr save:[{save:?}]")?;
        Ok(())
    }
}

impl std::fmt::Display for Constr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (prim, args) in self.prims.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "{prim:?}({args})")?;
        }
        let save = self.save.iter().format(&", ");
        writeln!(f, "constr save:[{save:?}]")?;
        Ok(())
    }
}

impl Constr {
    pub fn new() -> Constr {
        let ctx = ContextBuilder::new()
            .solver("z3")
            .solver_args(["-smt2", "-in"])
            .build()
            .unwrap();
        Constr {
            ctx,
            vars: Vec::new(),
            prims: Vec::new(),
            save: Vec::new(),
        }
    }

    pub fn get_int(&mut self, term: &TermCtx) -> Option<SExpr> {
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

    pub fn get_bool(&mut self, term: &TermCtx) -> Option<SExpr> {
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

    pub fn push_cons(&mut self, prim: Prim, args: Vec<TermCtx>) {
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
        self.prims.push((prim, args));
    }

    pub fn push_eq(&mut self, x: IdentCtx, term: TermCtx) {
        match term {
            Term::Var(_) => {
                let x = self.get_bool(&Term::Var(x)).unwrap();
                let term = self.get_bool(&term).unwrap();
                self.ctx.assert(self.ctx.eq(x, term)).unwrap();
            }
            Term::Lit(LitVal::Int(_)) => {
                let x = self.get_int(&Term::Var(x)).unwrap();
                let term = self.get_int(&term).unwrap();
                self.ctx.assert(self.ctx.eq(x, term)).unwrap();
            }
            Term::Lit(LitVal::Bool(_)) => {
                let x = self.get_bool(&Term::Var(x)).unwrap();
                let term = self.get_bool(&term).unwrap();
                self.ctx.assert(self.ctx.eq(x, term)).unwrap();
            }
            Term::Lit(LitVal::Float(_)) => {
                todo!()
            }
            Term::Lit(LitVal::Char(_)) => {
                todo!()
            }
            Term::Cons(_, _) => {
                panic!("only atom terms for eq in Constr!")
            }
        }
    }

    pub fn solve(&mut self) -> bool {
        let result = self.ctx.check().unwrap();
        match result {
            Response::Sat => true,
            Response::Unsat => false,
            Response::Unknown => true,
        }
    }

    pub fn savepoint(&mut self) {
        self.ctx.push().unwrap();
        self.save.push((self.vars.len(), self.prims.len()))
    }

    pub fn backtrack(&mut self) {
        self.ctx.pop().unwrap();
        let (vars_len, prims_len) = self.save.pop().unwrap();
        for _ in 0..(self.vars.len() - vars_len) {
            self.vars.pop().unwrap();
        }
        for _ in 0..(self.prims.len() - prims_len) {
            self.prims.pop().unwrap();
        }
    }
}

// #[test]
// fn test_smt_z3() -> io::Result<()> {
//     let mut ctx = ContextBuilder::new()
//         .solver("z3")
//         .solver_args(["-smt2", "-in"])
//         .build()?;

//     // Declare `x` and `y` variables that are bitvectors of width 32.
//     let bv32 = ctx.bit_vec_sort(ctx.numeral(32));
//     let x = ctx.declare_const("x", bv32)?;
//     let y = ctx.declare_const("y", bv32)?;

//     // Assert that `x * y = 18`.
//     ctx.assert(ctx.eq(ctx.bvmul(x, y), ctx.binary(32, 18)))?;

//     // And assert that neither `x` nor `y` is 1.
//     ctx.assert(ctx.not(ctx.eq(x, ctx.binary(32, 1))))?;
//     ctx.assert(ctx.not(ctx.eq(y, ctx.binary(32, 1))))?;

//     // Check whether the assertions are satisfiable. They should be in this example.
//     assert_eq!(ctx.check()?, Response::Sat);

//     // Print the solution!
//     let solution = ctx.get_value(vec![x, y])?;
//     for (variable, value) in solution {
//         println!("{} = {}", ctx.display(variable), ctx.display(value));
//     }
//     // There are many solutions, but the one I get from Z3 is:
//     //
//     //     x = #x10000012
//     //     y = #x38000001
//     //
//     // Solvers are great at finding edge cases and surprising-to-humans results! In
//     // this case, I would have naively expected something like `x = 2, y = 9` or
//     // `x = 3, y = 6`, but the solver found a solution where the multiplication
//     // wraps around. Neat!
//     Ok(())
// }
