use std::collections::HashMap;

use super::*;

use crate::syntax::ast::*;
use crate::utils::env_map::EnvMap;

struct Renamer {
    /// map a dummy identifier to an unique Identifier
    val_map: EnvMap<Ident, Ident>,
    typ_map: EnvMap<Ident, Ident>,
    cons_map: EnvMap<Ident, Ident>,
    error: Vec<RenameError>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RenameError {
    UnboundedValueVariable(Ident),
    UnboundedTypeVariable(Ident),
    UnboundedConstructorVariable(Ident),
    MultipuleValueDefinition(Ident),
    MultipuleTypeDefinition(Ident),
    MultipuleConstructorDefinition(Ident),
}

impl Renamer {
    fn new() -> Renamer {
        Renamer {
            val_map: EnvMap::new(),
            typ_map: EnvMap::new(),
            cons_map: EnvMap::new(),
            error: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.val_map.enter_scope();
        self.typ_map.enter_scope();
        self.cons_map.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.val_map.leave_scope();
        self.typ_map.leave_scope();
        self.cons_map.leave_scope();
    }

    fn intro_val_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.val_map.insert(*var, new_var) {
            self.error.push(RenameError::MultipuleValueDefinition(*var));
        }
        *var = new_var
    }

    fn intro_typ_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.typ_map.insert(*var, new_var) {
            self.error.push(RenameError::MultipuleTypeDefinition(*var));
        }
        *var = new_var
    }

    fn intro_cons_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.cons_map.insert(*var, new_var) {
            self.error
                .push(RenameError::MultipuleConstructorDefinition(*var));
        }
        *var = new_var
    }

    fn update_val_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.val_map.get(var).copied().unwrap_or_else(|| {
            self.error.push(RenameError::UnboundedValueVariable(*var));
            *var
        });
        *var = new_var;
    }

    fn update_typ_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.typ_map.get(var).copied().unwrap_or_else(|| {
            self.error.push(RenameError::UnboundedTypeVariable(*var));
            *var
        });
        *var = new_var;
    }

    fn update_cons_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.cons_map.get(var).copied().unwrap_or_else(|| {
            self.error
                .push(RenameError::UnboundedConstructorVariable(*var));
            *var
        });
        *var = new_var;
    }

    fn visit_type(&mut self, typ: &mut Type) {
        match typ {
            Type::Lit(_) => {}
            Type::Data(var) => self.update_typ_var(var),
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Lit { lit: _ } => {}
            Expr::Var { var } => {
                self.update_val_var(var);
            }
            Expr::Prim { prim: _, args } => {
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::App { func, args } => {
                self.update_val_var(func);
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::Cons { name, flds } => {
                self.update_cons_var(name);
                flds.iter_mut().for_each(|fld| self.visit_expr(fld));
            }
            Expr::Match { expr, brchs } => {
                self.visit_expr(expr);
                brchs.iter_mut().for_each(|(patn, expr)| {
                    self.enter_scope();
                    self.update_cons_var(&mut patn.name);
                    patn.flds.iter_mut().for_each(|fld| self.intro_val_var(fld));
                    self.visit_expr(expr);
                    self.leave_scope();
                });
            }
            Expr::Let { bind, expr, cont } => {
                self.visit_expr(expr);
                self.enter_scope();
                self.intro_val_var(bind);
                self.visit_expr(cont);
                self.leave_scope();
            }
            Expr::Ifte { cond, then, els } => {
                self.visit_expr(cond);
                self.visit_expr(then);
                self.visit_expr(els);
            }
            Expr::Assert { expr, cont } => {
                self.visit_expr(expr);
                self.visit_expr(cont);
            }
        }
    }

    fn visit_goal(&mut self, goal: &mut Goal) {
        match goal {
            Goal::Fresh { vars, body } => {
                self.enter_scope();
                vars.iter_mut().for_each(|var| self.intro_val_var(var));
                self.visit_goal(body);
                self.leave_scope();
            }
            Goal::Eq { lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Goal::Fail { expr } => {
                self.visit_expr(expr);
            }
            Goal::Pred { pred, args } => {
                self.update_val_var(pred);
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Goal::And { goals } => {
                goals.iter_mut().for_each(|goal| self.visit_goal(goal));
            }
            Goal::Or { goals } => {
                goals.iter_mut().for_each(|goal| self.visit_goal(goal));
            }
        }
    }

    fn visit_func_decl_head(&mut self, func_decl: &mut FuncDecl) {
        self.intro_val_var(&mut func_decl.name);
    }

    fn visit_func_decl(&mut self, func_decl: &mut FuncDecl) {
        self.enter_scope();
        func_decl.pars.iter_mut().for_each(|(par, typ)| {
            self.intro_val_var(par);
            self.visit_type(typ);
        });
        self.visit_expr(&mut func_decl.body);
        self.leave_scope();
    }

    fn visit_pred_decl_head(&mut self, pred_decl: &mut PredDecl) {
        self.intro_val_var(&mut pred_decl.name);
    }

    fn visit_pred_decl(&mut self, pred_decl: &mut PredDecl) {
        self.enter_scope();
        pred_decl.pars.iter_mut().for_each(|(par, typ)| {
            self.intro_val_var(par);
            self.visit_type(typ);
        });
        self.visit_goal(&mut pred_decl.body);
        self.leave_scope();
    }

    fn visit_data_decl_head(&mut self, data_decl: &mut DataDecl) {
        self.intro_typ_var(&mut data_decl.name);
        data_decl.cons.iter_mut().for_each(|cons| {
            self.intro_cons_var(&mut cons.name);
        });
    }

    fn visit_data_decl(&mut self, data_decl: &mut DataDecl) {
        self.enter_scope();
        data_decl.cons.iter_mut().for_each(|cons| {
            cons.flds.iter_mut().for_each(|fld| self.visit_type(fld));
        });
        self.leave_scope();
    }

    fn visit_prog(&mut self, prog: &mut Program) {
        // first iteration: visit heads
        prog.datas
            .iter_mut()
            .for_each(|data_decl| self.visit_data_decl_head(data_decl));
        prog.funcs
            .iter_mut()
            .for_each(|func_decl| self.visit_func_decl_head(func_decl));
        prog.preds
            .iter_mut()
            .for_each(|pred_decl| self.visit_pred_decl_head(pred_decl));

        // second iteration: visit body
        prog.datas
            .iter_mut()
            .for_each(|data_decl| self.visit_data_decl(data_decl));
        prog.funcs
            .iter_mut()
            .for_each(|func_decl| self.visit_func_decl(func_decl));
        prog.preds
            .iter_mut()
            .for_each(|pred_decl| self.visit_pred_decl(pred_decl));
    }
}

pub fn rename_pass(prog: &mut Program) -> Result<HashMap<Ident, Ident>, Vec<RenameError>> {
    let mut pass = Renamer::new();
    pass.visit_prog(prog);
    if pass.error.is_empty() {
        Ok(pass.val_map.iter().map(|x| (*x.0, *x.1)).collect())
    } else {
        Err(pass.error)
    }
}

#[test]
fn renamer_test() {
    let p1: &'static str = r#"
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
        ys = append(xs, x);
        is_elem(ys, x) = false;
    )
end
"#;
    let mut prog = crate::syntax::parser_gen::parser::ProgramParser::new()
        .parse(p1)
        .unwrap();

    // println!("{:#?}", prog);

    rename_pass(&mut prog).unwrap();

    // println!("{:#?}", prog);
    // println!("{:#?}", errs);
}
