use super::*;

use crate::syntax::{self, ast::*};
use crate::utils::prim::Prim;
use crate::utils::unify::*;

#[derive(Clone, Debug)]
struct FuncType {
    polys: Vec<Ident>,
    pars: Vec<TypeId>,
    res: TypeId,
}

#[derive(Clone, Debug)]
struct ConsType {
    polys: Vec<Ident>,
    flds: Vec<TypeId>,
    res: TypeId,
}

struct Checker {
    val_ctx: HashMap<Ident, TypeId>,
    func_ctx: HashMap<Ident, FuncType>,
    cons_ctx: HashMap<Ident, ConsType>,
    data_ctx: HashMap<Ident, Vec<Ident>>,
    unifier: Unifier<Ident, LitType, Option<Ident>>,
    diag: Vec<UnifyError<Ident, LitType, Option<Ident>>>,
}

impl Checker {
    pub fn new() -> Checker {
        Checker {
            val_ctx: HashMap::new(),
            func_ctx: HashMap::new(),
            cons_ctx: HashMap::new(),
            data_ctx: HashMap::new(),
            unifier: Unifier::new(),
            diag: Vec::new(),
        }
    }

    fn fresh(&mut self) -> TypeId {
        TypeId::Var(Ident::fresh(&"t"))
    }

    fn inst_func(&mut self, name: Ident) -> (Vec<TypeId>, TypeId) {
        let func_typ = &self.func_ctx[&name];
        let mut typs = func_typ.pars.clone();
        typs.push(func_typ.res.clone());
        instantiate(&func_typ.polys, &mut typs);
        let res = typs.pop().unwrap();
        (typs, res)
    }

    fn inst_cons(&mut self, name: Ident) -> (Vec<TypeId>, TypeId) {
        let cons_typ = &self.cons_ctx[&name];
        let mut typs = cons_typ.flds.clone();
        typs.push(cons_typ.res.clone());
        instantiate(&cons_typ.polys, &mut typs);
        let res = typs.pop().unwrap();
        (typs, res)
    }

    fn unify(&mut self, typ1: &TypeId, typ2: &TypeId) {
        match self.unifier.unify(typ1, typ2) {
            Ok(()) => {}
            Err(err) => {
                self.diag.push(err);
            }
        }
    }

    fn unify_many(&mut self, typs1: &[TypeId], typs2: &[TypeId]) {
        match self.unifier.unify_many(typs1, typs2) {
            Ok(()) => {}
            Err(err) => {
                self.diag.push(err);
            }
        }
    }

    fn check_prim(&mut self, prim: &Prim, args: &[Expr]) -> TypeId {
        let args: Vec<_> = args.iter().map(|arg| self.check_expr(arg)).collect();

        match prim {
            Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem => {
                self.unify_many(
                    &[TypeId::Lit(LitType::TyInt), TypeId::Lit(LitType::TyInt)],
                    &args,
                );
                TypeId::Lit(LitType::TyInt)
            }
            Prim::INeg => {
                self.unify_many(&[TypeId::Lit(LitType::TyInt)], &args);
                TypeId::Lit(LitType::TyInt)
            }
            Prim::ICmp(_) => {
                self.unify_many(
                    &[TypeId::Lit(LitType::TyInt), TypeId::Lit(LitType::TyInt)],
                    &args,
                );
                TypeId::Lit(LitType::TyBool)
            }
            Prim::BAnd | Prim::BOr => {
                self.unify_many(
                    &[TypeId::Lit(LitType::TyBool), TypeId::Lit(LitType::TyBool)],
                    &args,
                );
                TypeId::Lit(LitType::TyBool)
            }
            Prim::BNot => {
                self.unify_many(&[TypeId::Lit(LitType::TyBool)], &args);
                TypeId::Lit(LitType::TyBool)
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> TypeId {
        match expr {
            Expr::Lit { lit, span: _ } => TypeId::Lit(lit.get_typ()),
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
                let (pars, res) = self.inst_cons(cons.ident).clone();
                self.unify_many(&pars, &flds);
                res
            }
            Expr::Tuple { flds, span: _ } => {
                let flds = flds.iter().map(|fld| self.check_expr(fld)).collect();
                TypeId::Cons(None, flds)
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
                let (pars, res) = self.inst_func(func.ident);
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
                self.unify(&cond, &TypeId::Lit(LitType::TyBool));
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
                    self.unify(&cond, &TypeId::Lit(LitType::TyBool));
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
                    self.unify(&lhs, &TypeId::Lit(LitType::TyBool));
                }
                self.check_expr(cont)
            }
            Expr::Undefined { span: _ } => self.fresh(),
        }
    }

    fn check_patn(&mut self, patn: &Pattern) -> TypeId {
        match patn {
            Pattern::Lit { lit, span: _ } => TypeId::Lit(lit.get_typ()),
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
                let (pars, res) = self.inst_cons(cons.ident).clone();
                let flds: Vec<TypeId> = flds.iter().map(|fld| self.check_patn(fld)).collect();
                self.unify_many(&pars, &flds);
                res
            }
            Pattern::Tuple { flds, span: _ } => {
                let typs = flds.iter().map(|fld| self.check_patn(fld)).collect();
                TypeId::Cons(None, typs)
            }
        }
    }

    fn scan_data_decl_head(&mut self, data_decl: &DataDecl) {
        let cons_names = data_decl.cons.iter().map(|cons| cons.name.ident).collect();
        self.data_ctx.insert(data_decl.name.ident, cons_names);

        let polys: Vec<Ident> = data_decl.polys.iter().map(|poly| poly.ident).collect();
        let res = TypeId::Cons(
            Some(data_decl.name.ident),
            polys.iter().map(|poly| TypeId::Var(*poly)).collect(),
        );

        for cons in data_decl.cons.iter() {
            let flds = cons.flds.iter().map(|fld| into_term(fld)).collect();
            let cons_typ = ConsType {
                polys: polys.clone(),
                flds,
                res: res.clone(),
            };
            self.cons_ctx.insert(cons.name.ident, cons_typ);
        }
    }

    fn scan_func_decl_head(&mut self, func_decl: &FuncDecl) {
        let polys = func_decl.polys.iter().map(|poly| poly.ident).collect();
        let pars = func_decl
            .pars
            .iter()
            .map(|(_par, typ)| into_term(typ))
            .collect();
        let res = into_term(&func_decl.res);
        let func_typ = FuncType { polys, pars, res };
        self.func_ctx.insert(func_decl.name.ident, func_typ);
    }

    fn check_func_decl(&mut self, func_decl: &FuncDecl) {
        let func_typ = self.func_ctx[&func_decl.name.ident].clone();
        for ((par, _), par_ty) in func_decl.pars.iter().zip(func_typ.pars.iter()) {
            self.val_ctx.insert(par.ident, par_ty.clone());
        }
        let body_ty = self.check_expr(&func_decl.body);
        self.unify(&func_typ.res, &body_ty);
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

fn into_term(value: &syntax::ast::Type) -> TypeId {
    match value {
        Type::Lit { lit, span: _ } => Term::Lit(*lit),
        Type::Var { var, span: _ } => Term::Var(var.ident),
        Type::Cons {
            cons,
            flds,
            span: _,
        } => {
            let flds = flds.iter().map(|fld| into_term(fld)).collect();
            Term::Cons(Some(cons.ident), flds)
        }
        Type::Tuple { flds, span: _ } => {
            let flds = flds.iter().map(|fld| into_term(fld)).collect();
            Term::Cons(None, flds)
        }
    }
}

fn instantiate(polys: &Vec<Ident>, typs: &mut Vec<TypeId>) {
    let map: HashMap<Ident, TypeId> = polys
        .iter()
        .map(|poly| (*poly, Term::Var(poly.uniquify())))
        .collect();

    for typ in typs {
        *typ = typ.substitute(&map);
    }
}

pub fn check_pass(prog: &Program) -> Vec<UnifyError<Ident, LitType, Option<Ident>>> {
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
    | Cons(head, tail) => if head == x then true else is_elem(tail, x) 
    | Nil => false
    end
end

function is_elem_after_append(xs: IntList, x: Int)
begin
    guard !is_elem(append(xs, x), x);
end

query is_elem_after_append(depth_step=5, depth_limit=50, answer_limit=1)
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
