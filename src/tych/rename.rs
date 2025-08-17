use super::*;

use crate::syntax::ast::*;
use crate::utils::env_map::EnvMap;

struct Renamer {
    /// map a dummy identifier to an unique Identifier
    val_map: EnvMap<Ident, Ident>,
    func_pred_map: EnvMap<Ident, Ident>,
    typ_map: EnvMap<Ident, Ident>,
    cons_map: EnvMap<Ident, Ident>,
    errors: Vec<RenameError>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RenameError {
    UnboundedValueVariable(Ident),
    UnboundedFuncPredVariable(Ident),
    UnboundedTypeVariable(Ident),
    UnboundedConstrVariable(Ident),
    MultipleValueDefinition(Ident),
    MultipleFuncPredDefinition(Ident),
    MultipleTypeDefinition(Ident),
    MultipleConstrDefinition(Ident),
}

use crate::driver::diagnostic::Diagnostic;
impl Into<Diagnostic> for RenameError {
    fn into(self) -> Diagnostic {
        match self {
            RenameError::UnboundedValueVariable(var) => {
                Diagnostic::error(format!("unbounded value variable {var}!"))
            }
            RenameError::UnboundedFuncPredVariable(var) => {
                Diagnostic::error(format!("unbounded func-pred variable {var}!"))
            }
            RenameError::UnboundedTypeVariable(var) => {
                Diagnostic::error(format!("unbounded type variable {var}!"))
            }
            RenameError::UnboundedConstrVariable(var) => {
                Diagnostic::error(format!("unbounded constructor variable {var}!"))
            }
            RenameError::MultipleValueDefinition(var) => {
                Diagnostic::error(format!("unbounded value variable {var}!"))
            }
            RenameError::MultipleFuncPredDefinition(var) => {
                Diagnostic::error(format!("unbounded func-pred variable {var}!"))
            }
            RenameError::MultipleTypeDefinition(var) => {
                Diagnostic::error(format!("unbounded type variable {var}!"))
            }
            RenameError::MultipleConstrDefinition(var) => {
                Diagnostic::error(format!("unbounded constructor variable {var}!"))
            }
        }
    }
}

impl Renamer {
    fn new() -> Renamer {
        Renamer {
            val_map: EnvMap::new(),
            func_pred_map: EnvMap::new(),
            typ_map: EnvMap::new(),
            cons_map: EnvMap::new(),
            errors: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.val_map.enter_scope();
        self.func_pred_map.enter_scope();
        self.typ_map.enter_scope();
        self.cons_map.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.val_map.leave_scope();
        self.func_pred_map.leave_scope();
        self.typ_map.leave_scope();
        self.cons_map.leave_scope();
    }

    fn intro_val_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.val_map.insert(*var, new_var) {
            self.errors.push(RenameError::MultipleValueDefinition(*var));
        }
        *var = new_var
    }

    fn intro_func_pred_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.func_pred_map.insert(*var, new_var) {
            self.errors
                .push(RenameError::MultipleFuncPredDefinition(*var));
        }
        *var = new_var
    }

    fn intro_typ_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.typ_map.insert(*var, new_var) {
            self.errors.push(RenameError::MultipleTypeDefinition(*var));
        }
        *var = new_var
    }

    fn intro_cons_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = var.uniquify();
        if self.cons_map.insert(*var, new_var) {
            self.errors
                .push(RenameError::MultipleConstrDefinition(*var));
        }
        *var = new_var
    }

    fn update_val_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.val_map.get(var).copied().unwrap_or_else(|| {
            self.errors.push(RenameError::UnboundedValueVariable(*var));
            *var
        });
        *var = new_var;
    }

    fn update_func_pred_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.func_pred_map.get(var).copied().unwrap_or_else(|| {
            self.errors
                .push(RenameError::UnboundedFuncPredVariable(*var));
            *var
        });
        *var = new_var;
    }

    fn update_typ_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.typ_map.get(var).copied().unwrap_or_else(|| {
            self.errors.push(RenameError::UnboundedTypeVariable(*var));
            *var
        });
        *var = new_var;
    }

    fn update_cons_var(&mut self, var: &mut Ident) {
        assert!(var.is_dummy());
        let new_var = self.cons_map.get(var).copied().unwrap_or_else(|| {
            self.errors.push(RenameError::UnboundedConstrVariable(*var));
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
            Expr::Lit { lit: _, span: _ } => {}
            Expr::Var { var, span: _ } => {
                self.update_val_var(var);
            }
            Expr::Prim {
                prim: _,
                args,
                span: _,
            } => {
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::App {
                func,
                args,
                span: _,
            } => {
                self.update_func_pred_var(func);
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::Cons {
                cons: name,
                flds,
                span: _,
            } => {
                self.update_cons_var(name);
                flds.iter_mut().for_each(|fld| self.visit_expr(fld));
            }
            Expr::Match {
                expr,
                brchs,
                span: _,
            } => {
                self.visit_expr(expr);
                brchs.iter_mut().for_each(|(patn, expr)| {
                    self.enter_scope();
                    self.update_cons_var(&mut patn.cons);
                    patn.flds.iter_mut().for_each(|fld| self.intro_val_var(fld));
                    self.visit_expr(expr);
                    self.leave_scope();
                });
            }
            Expr::Let {
                bind,
                expr,
                cont,
                span: _,
            } => {
                self.visit_expr(expr);
                self.enter_scope();
                self.intro_val_var(bind);
                self.visit_expr(cont);
                self.leave_scope();
            }
            Expr::Ifte {
                cond,
                then,
                els,
                span: _,
            } => {
                self.visit_expr(cond);
                self.visit_expr(then);
                self.visit_expr(els);
            }
        }
    }

    fn visit_goal(&mut self, goal: &mut Goal) {
        match goal {
            Goal::Fresh {
                vars,
                body,
                span: _,
            } => {
                self.enter_scope();
                vars.iter_mut().for_each(|var| self.intro_val_var(var));
                self.visit_goal(body);
                self.leave_scope();
            }
            Goal::Eq { lhs, rhs, span: _ } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Goal::Pred {
                pred,
                args,
                span: _,
            } => {
                self.update_func_pred_var(pred);
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Goal::And { goals, span: _ } => {
                goals.iter_mut().for_each(|goal| self.visit_goal(goal));
            }
            Goal::Or { goals, span: _ } => {
                goals.iter_mut().for_each(|goal| self.visit_goal(goal));
            }
        }
    }

    fn visit_func_decl_head(&mut self, func_decl: &mut FuncDecl) {
        self.intro_func_pred_var(&mut func_decl.name);
    }

    fn visit_func_decl(&mut self, func_decl: &mut FuncDecl) {
        self.enter_scope();
        func_decl.pars.iter_mut().for_each(|(par, typ)| {
            self.intro_val_var(par);
            self.visit_type(typ);
        });
        self.visit_type(&mut func_decl.res);
        self.visit_expr(&mut func_decl.body);
        self.leave_scope();
    }

    fn visit_pred_decl_head(&mut self, pred_decl: &mut PredDecl) {
        self.intro_func_pred_var(&mut pred_decl.name);
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

    fn visit_entry_decl(&mut self, entry_decl: &mut EntryDecl) {
        self.update_func_pred_var(&mut entry_decl.entry);
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
        prog.entrys
            .iter_mut()
            .for_each(|entry_decl| self.visit_entry_decl(entry_decl));
    }
}

pub fn rename_pass(prog: &mut Program) -> (HashMap<Ident, Ident>, Vec<RenameError>) {
    let mut pass = Renamer::new();
    pass.visit_prog(prog);
    (
        pass.func_pred_map.iter().map(|(k, v)| (*k, *v)).collect(),
        pass.errors,
    )
}

#[test]
fn renamer_test() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

entry is_elem_after_append(10, 20, 5)

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

    let (_map, errs) = rename_pass(&mut prog);
    assert!(errs.is_empty());

    // println!("{:#?}", prog);
    // println!("{:#?}", errs);
}
