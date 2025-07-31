use super::*;

use super::unify::{UnifySolver, UnifyType};
use crate::syntax::ast::*;
use crate::utils::prim::Prim;

type CheckResult<T> = Result<T, ()>;

struct Checker {
    val_ctx: HashMap<Ident, UnifyType>,
    func_ctx: HashMap<Ident, (Vec<UnifyType>, UnifyType)>,
    pred_ctx: HashMap<Ident, Vec<UnifyType>>,
    cons_ctx: HashMap<Ident, (Vec<UnifyType>, UnifyType)>,
    data_ctx: HashMap<Ident, Vec<Ident>>,
    solver: UnifySolver,
}

impl Checker {
    pub fn new() -> Checker {
        Checker {
            val_ctx: HashMap::new(),
            func_ctx: HashMap::new(),
            pred_ctx: HashMap::new(),
            cons_ctx: HashMap::new(),
            data_ctx: HashMap::new(),
            solver: UnifySolver::new(),
        }
    }

    fn fresh(&mut self) -> UnifyType {
        UnifyType::Cell(self.solver.new_cell())
    }

    fn unify(&mut self, typ1: &UnifyType, typ2: &UnifyType) -> CheckResult<()> {
        match self.solver.unify(typ1, typ2) {
            Ok(()) => {}
            Err(err) => match err {
                unify::UnifyError::VecDiffLen(_typs1, _typs2) => {
                    // self.diags.push(Diagnostic::error(
                    //     "cannot unify varibles with different length!",
                    // ));
                }
                unify::UnifyError::CannotUnify(_typ1, _typ2) => {
                    // self.diags.push(Diagnostic::error("cannot unify types!"));
                }
                unify::UnifyError::OccurCheckFailed(_var, _typ) => {
                    // self.diags.push(Diagnostic::error("occur check failed!"));
                }
            },
        }
        Ok(())
    }

    fn unify_vec(&mut self, vec1: &Vec<UnifyType>, vec2: &Vec<UnifyType>) -> CheckResult<()> {
        if vec1.len() != vec2.len() {
            Err(())
        } else {
            for (typ1, typ2) in vec1.iter().zip(vec2.iter()) {
                self.unify(typ1, typ2)?;
            }
            Ok(())
        }
    }

    fn check_prim(&mut self, prim: &Prim, args: &Vec<Expr>) -> CheckResult<UnifyType> {
        let args = args
            .iter()
            .map(|arg| self.check_expr(arg))
            .collect::<CheckResult<Vec<_>>>()?;

        match prim {
            Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem => {
                self.unify_vec(
                    &vec![
                        UnifyType::Lit(LitType::TyInt),
                        UnifyType::Lit(LitType::TyInt),
                    ],
                    &args,
                )?;
                Ok(UnifyType::Lit(LitType::TyInt))
            }
            Prim::INeg => {
                self.unify_vec(&vec![UnifyType::Lit(LitType::TyInt)], &args)?;
                Ok(UnifyType::Lit(LitType::TyInt))
            }
            Prim::ICmp(_) => {
                self.unify_vec(
                    &vec![
                        UnifyType::Lit(LitType::TyInt),
                        UnifyType::Lit(LitType::TyInt),
                    ],
                    &args,
                )?;
                Ok(UnifyType::Lit(LitType::TyInt))
            }
            Prim::BAnd | Prim::BOr => {
                self.unify_vec(
                    &vec![
                        UnifyType::Lit(LitType::TyBool),
                        UnifyType::Lit(LitType::TyBool),
                    ],
                    &args,
                )?;
                Ok(UnifyType::Lit(LitType::TyBool))
            }
            Prim::BNot => {
                self.unify_vec(&vec![UnifyType::Lit(LitType::TyBool)], &args)?;
                Ok(UnifyType::Lit(LitType::TyBool))
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> CheckResult<UnifyType> {
        match expr {
            Expr::Lit { lit, span: _ } => Ok(UnifyType::Lit(lit.get_typ())),
            Expr::Var { var, span: _ } => Ok(self.val_ctx[var].clone()),
            Expr::Prim {
                prim,
                args,
                span: _,
            } => self.check_prim(prim, args),
            Expr::Cons {
                name,
                flds,
                span: _,
            } => {
                let flds = flds
                    .iter()
                    .map(|fld| self.check_expr(fld))
                    .collect::<CheckResult<Vec<_>>>()?;
                let (pars, res) = self.cons_ctx[name].clone();
                self.unify_vec(&pars, &flds)?;
                Ok(res)
            }
            Expr::Match {
                expr,
                brchs,
                span: _,
            } => {
                let expr = self.check_expr(expr)?;
                let res = self.fresh();
                for (lhs, rhs) in brchs.iter() {
                    let lhs = self.check_patn(lhs)?;
                    self.unify(&expr, &lhs)?;
                    let rhs = self.check_expr(rhs)?;
                    self.unify(&res, &rhs)?;
                }
                Ok(res)
            }
            Expr::Let {
                bind,
                expr,
                cont,
                span: _,
            } => {
                let expr = self.check_expr(expr)?;
                self.val_ctx.insert(*bind, expr);
                self.check_expr(&cont)
            }
            Expr::App {
                func,
                args,
                span: _,
            } => {
                let (pars, res) = self.func_ctx[func].clone();
                let args = args
                    .iter()
                    .map(|arg| self.check_expr(arg))
                    .collect::<CheckResult<Vec<_>>>()?;
                self.unify_vec(&pars, &args)?;
                Ok(res)
            }
            Expr::Ifte {
                cond,
                then,
                els,
                span: _,
            } => {
                let cond = self.check_expr(cond)?;
                let then = self.check_expr(then)?;
                let els = self.check_expr(els)?;
                self.unify(&UnifyType::Lit(LitType::TyBool), &cond)?;
                self.unify(&then, &els)?;
                Ok(then)
            }
        }
    }

    fn check_goal(&mut self, goal: &Goal) -> CheckResult<()> {
        match goal {
            Goal::Fresh {
                vars,
                body,
                span: _,
            } => {
                for var in vars {
                    let cell = self.fresh();
                    self.val_ctx.insert(*var, cell);
                }
                self.check_goal(&body)?;
                Ok(())
            }
            Goal::Eq { lhs, rhs, span: _ } => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                self.unify(&lhs, &rhs)?;
                Ok(())
            }
            Goal::Pred {
                pred,
                args,
                span: _,
            } => {
                let pars = self.pred_ctx[pred].clone();
                let args = args
                    .iter()
                    .map(|arg| self.check_expr(arg))
                    .collect::<CheckResult<Vec<_>>>()?;
                self.unify_vec(&pars, &args)?;
                Ok(())
            }
            Goal::And { goals, span: _ } => {
                for goal in goals {
                    self.check_goal(goal)?;
                }
                Ok(())
            }
            Goal::Or { goals, span: _ } => {
                for goal in goals {
                    self.check_goal(goal)?;
                }
                Ok(())
            }
        }
    }

    fn check_patn(&mut self, patn: &Pattern) -> CheckResult<UnifyType> {
        let (pars, res) = self.cons_ctx[&patn.name].clone();

        pars.iter().zip(patn.flds.iter()).for_each(|(par, fld)| {
            self.val_ctx.insert(*fld, par.clone());
        });

        let flds = patn.flds.iter().map(|fld| UnifyType::Var(*fld)).collect();
        self.unify_vec(&pars, &flds)?;

        Ok(res)
    }

    fn scan_data_decl_head(&mut self, data_decl: &DataDecl) {
        let cons_names = data_decl.cons.iter().map(|cons| cons.name).collect();
        self.data_ctx.insert(data_decl.name, cons_names);

        for cons in data_decl.cons.iter() {
            let flds = cons.flds.iter().map(|fld| fld.into()).collect();
            self.cons_ctx.insert(
                cons.name,
                (flds, UnifyType::Cons(data_decl.name, Vec::new())),
            );
        }
    }

    fn scan_func_decl_head(&mut self, func_decl: &FuncDecl) {
        let pars = func_decl
            .pars
            .iter()
            .map(|(_par, typ)| typ.into())
            .collect();
        let res = (&func_decl.res).into();
        self.func_ctx.insert(func_decl.name, (pars, res));
    }

    fn scan_pred_decl_head(&mut self, pred_decl: &PredDecl) {
        let pars = pred_decl
            .pars
            .iter()
            .map(|(_par, typ)| typ.into())
            .collect();
        self.pred_ctx.insert(pred_decl.name, pars);
    }

    fn check_func_decl(&mut self, func_decl: &FuncDecl) -> CheckResult<()> {
        let (pars_ty, res_ty) = self.func_ctx[&func_decl.name].clone();

        for ((par, _), par_ty) in func_decl.pars.iter().zip(pars_ty) {
            self.val_ctx.insert(*par, par_ty);
        }

        let body_ty = self.check_expr(&func_decl.body)?;
        self.unify(&res_ty, &body_ty)?;

        Ok(())
    }

    fn check_pred_decl(&mut self, pred_decl: &PredDecl) -> CheckResult<()> {
        let pars_ty = self.pred_ctx[&pred_decl.name].clone();

        for ((par, _), par_ty) in pred_decl.pars.iter().zip(pars_ty) {
            self.val_ctx.insert(*par, par_ty);
        }

        self.check_goal(&pred_decl.body)?;
        Ok(())
    }

    fn check_prog(&mut self, prog: &Program) -> CheckResult<()> {
        for data_decl in prog.datas.iter() {
            self.scan_data_decl_head(&data_decl);
        }

        for func_decl in prog.funcs.iter() {
            self.scan_func_decl_head(&func_decl);
        }

        for pred_decl in prog.preds.iter() {
            self.scan_pred_decl_head(&pred_decl);
        }

        for func_decl in prog.funcs.iter() {
            self.check_func_decl(&func_decl)?;
        }

        for pred_decl in prog.preds.iter() {
            self.check_pred_decl(&pred_decl)?;
        }

        Ok(())
    }
}

pub fn check_pass(prog: &Program) -> Result<HashMap<Ident, UnifyType>, ()> {
    let mut pass = Checker::new();
    pass.check_prog(prog)?;
    Ok(pass.val_ctx)
}

#[test]
#[ignore = "just to see result"]
fn check_test() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if @icmpeq(head, x) then true else is_elem(tail, x) 
    | Nil => false
    end
end

predicate is_elem_after_append(xs: IntList, x: Int)
begin
    fresh(ys) (
        and(
            ys = append(xs, x),
            is_elem(ys, x) = false,
        )
    )
end
"#;
    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let (_map, errs) = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    // println!("{:#?}", prog);

    let map = check_pass(&prog).unwrap();

    println!("{:?}", map);

    // println!("{:#?}", prog);
    // println!("{:#?}", errs);
}
