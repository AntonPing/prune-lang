use super::*;

use super::unify::{UnifySolver, UnifyType};
use crate::syntax::ast::*;
use crate::tych::unify::UnifyError;
use crate::utils::prim::Prim;

struct Checker {
    val_ctx: HashMap<Ident, UnifyType>,
    func_ctx: HashMap<Ident, (Vec<UnifyType>, UnifyType)>,
    cons_ctx: HashMap<Ident, (Vec<UnifyType>, UnifyType)>,
    data_ctx: HashMap<Ident, Vec<Ident>>,
    solver: UnifySolver,
    diag: Vec<UnifyError>,
}

impl Checker {
    pub fn new() -> Checker {
        Checker {
            val_ctx: HashMap::new(),
            func_ctx: HashMap::new(),
            cons_ctx: HashMap::new(),
            data_ctx: HashMap::new(),
            solver: UnifySolver::new(),
            diag: Vec::new(),
        }
    }

    fn fresh(&mut self) -> UnifyType {
        UnifyType::Cell(self.solver.new_cell())
    }

    fn unify(&mut self, typ1: &UnifyType, typ2: &UnifyType) {
        match self.solver.unify(typ1, typ2) {
            Ok(()) => {}
            Err(err) => {
                self.diag.push(err);
            }
        }
    }

    fn unify_many(&mut self, typs1: &[UnifyType], typs2: &[UnifyType]) {
        match self.solver.unify_many(typs1, typs2) {
            Ok(()) => {}
            Err(err) => {
                self.diag.push(err);
            }
        }
    }

    fn check_prim(&mut self, prim: &Prim, args: &[Expr]) -> UnifyType {
        let args: Vec<_> = args.iter().map(|arg| self.check_expr(arg)).collect();

        match prim {
            Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem => {
                self.unify_many(
                    &[
                        UnifyType::Lit(LitType::TyInt),
                        UnifyType::Lit(LitType::TyInt),
                    ],
                    &args,
                );
                UnifyType::Lit(LitType::TyInt)
            }
            Prim::INeg => {
                self.unify_many(&[UnifyType::Lit(LitType::TyInt)], &args);
                UnifyType::Lit(LitType::TyInt)
            }
            Prim::ICmp(_) => {
                self.unify_many(
                    &[
                        UnifyType::Lit(LitType::TyInt),
                        UnifyType::Lit(LitType::TyInt),
                    ],
                    &args,
                );
                UnifyType::Lit(LitType::TyBool)
            }
            Prim::BAnd | Prim::BOr => {
                self.unify_many(
                    &[
                        UnifyType::Lit(LitType::TyBool),
                        UnifyType::Lit(LitType::TyBool),
                    ],
                    &args,
                );
                UnifyType::Lit(LitType::TyBool)
            }
            Prim::BNot => {
                self.unify_many(&[UnifyType::Lit(LitType::TyBool)], &args);
                UnifyType::Lit(LitType::TyBool)
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> UnifyType {
        match expr {
            Expr::Lit { lit, span: _ } => UnifyType::Lit(lit.get_typ()),
            Expr::Var { var, span: _ } => self.val_ctx[&var.ident].clone(),
            Expr::Prim {
                prim,
                args,
                span: _,
            } => self.check_prim(prim, args),
            Expr::Cons {
                cons,
                flds,
                span: _,
            } => {
                let flds: Vec<_> = flds.iter().map(|fld| self.check_expr(fld)).collect();
                let (pars, res) = self.cons_ctx[&cons.ident].clone();
                self.unify_many(&pars, &flds);
                res
            }
            Expr::Tuple { flds, span: _ } => {
                let flds = flds.iter().map(|fld| self.check_expr(fld)).collect();
                UnifyType::Cons(None, flds)
            }
            Expr::Match {
                expr,
                brchs,
                span: _,
            } => {
                let expr = self.check_expr(expr);
                let res = self.fresh();
                for (patn, cont) in brchs.iter() {
                    let patn = self.check_patn(patn);
                    self.unify(&patn, &expr);
                    let cont = self.check_expr(cont);
                    self.unify(&res, &cont);
                }
                res
            }
            Expr::Let {
                patn,
                expr,
                cont,
                span: _,
            } => {
                let expr = self.check_expr(expr);
                let patn = self.check_patn(patn);
                self.unify(&patn, &expr);
                self.check_expr(cont)
            }
            Expr::App {
                func,
                args,
                span: _,
            } => {
                let (pars, res) = self.func_ctx[&func.ident].clone();
                let args: Vec<_> = args.iter().map(|arg| self.check_expr(arg)).collect();
                self.unify_many(&pars, &args);
                res
            }
            Expr::Ifte {
                cond,
                then,
                els,
                span: _,
            } => {
                let cond = self.check_expr(cond);
                self.unify(&cond, &UnifyType::Lit(LitType::TyBool));
                let then = self.check_expr(then);
                let els = self.check_expr(els);
                self.unify(&then, &els);
                then
            }
            Expr::Cond { brchs, span: _ } => {
                let res = self.fresh();
                for (cond, body) in brchs {
                    let cond = self.check_expr(cond);
                    let body = self.check_expr(body);
                    self.unify(&cond, &UnifyType::Lit(LitType::TyBool));
                    self.unify(&body, &res);
                }
                res
            }
            Expr::Alter { brchs, span: _ } => {
                let res = self.fresh();
                for body in brchs {
                    let body = self.check_expr(body);
                    self.unify(&body, &res);
                }
                res
            }
            Expr::Fresh {
                vars,
                cont,
                span: _,
            } => {
                for var in vars {
                    let cell = self.fresh();
                    self.val_ctx.insert(var.ident, cell);
                }
                self.check_expr(cont)
            }
            Expr::Guard {
                lhs,
                rhs,
                cont,
                span: _,
            } => {
                let lhs = self.check_expr(lhs);
                if let Some(rhs) = rhs {
                    let rhs = self.check_expr(rhs);
                    self.unify(&lhs, &rhs);
                } else {
                    self.unify(&lhs, &UnifyType::Lit(LitType::TyBool));
                }
                self.check_expr(cont)
            }
            Expr::Undefined { span: _ } => self.fresh(),
        }
    }

    fn check_patn(&mut self, patn: &Pattern) -> UnifyType {
        match patn {
            Pattern::Lit { lit, span: _ } => UnifyType::Lit(lit.get_typ()),
            Pattern::Var { var, span: _ } => {
                let ty = self.fresh();
                self.val_ctx.insert(var.ident, ty.clone());
                ty
            }
            Pattern::Cons {
                cons,
                flds,
                span: _,
            } => {
                let (pars, res) = self.cons_ctx[&cons.ident].clone();
                for (par, fld) in pars.iter().zip(flds.iter()) {
                    let ty = self.check_patn(fld);
                    self.unify(par, &ty);
                }
                res
            }
            Pattern::Tuple { flds, span: _ } => {
                let typs = flds.iter().map(|fld| self.check_patn(fld)).collect();
                UnifyType::Cons(None, typs)
            }
        }
    }

    fn scan_data_decl_head(&mut self, data_decl: &DataDecl) {
        let cons_names = data_decl.cons.iter().map(|cons| cons.name.ident).collect();
        self.data_ctx.insert(data_decl.name.ident, cons_names);

        for cons in data_decl.cons.iter() {
            let flds = cons.flds.iter().map(|fld| fld.into()).collect();
            self.cons_ctx.insert(
                cons.name.ident,
                (
                    flds,
                    UnifyType::Cons(Some(data_decl.name.ident), Vec::new()),
                ),
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
        self.func_ctx.insert(func_decl.name.ident, (pars, res));
    }

    fn check_func_decl(&mut self, func_decl: &FuncDecl) {
        let (pars_ty, res_ty) = self.func_ctx[&func_decl.name.ident].clone();

        for ((par, _), par_ty) in func_decl.pars.iter().zip(pars_ty) {
            self.val_ctx.insert(par.ident, par_ty);
        }

        let body_ty = self.check_expr(&func_decl.body);
        self.unify(&res_ty, &body_ty);
    }

    fn check_prog(&mut self, prog: &Program) {
        for data_decl in prog.datas.iter() {
            self.scan_data_decl_head(data_decl);
        }

        for func_decl in prog.funcs.iter() {
            self.scan_func_decl_head(func_decl);
        }

        for func_decl in prog.funcs.iter() {
            self.check_func_decl(func_decl);
        }
    }
}

pub fn check_pass(prog: &Program) -> Vec<UnifyError> {
    let mut pass = Checker::new();
    pass.check_prog(prog);
    pass.diag
}

#[test]
#[ignore = "just to see result"]
fn check_test() {
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

    let errs = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    // println!("{:#?}", prog);

    let errs = check_pass(&prog);
    assert!(errs.is_empty());

    // println!("{:#?}", errs);
    // println!("{:?}", map);

    // println!("{:#?}", prog);
    // println!("{:#?}", errs);
}
