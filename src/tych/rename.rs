use logos::Span;

use super::*;

use crate::syntax::ast::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum VarType {
    Value,
    Function,
    DataType,
    Constructor,
    TypeVar,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct VarInfo {
    new_id: Ident,
    span: Span,
    used: bool,
}

struct Renamer {
    scopes: Vec<HashMap<(Ident, VarType), VarInfo>>,
    errors: Vec<RenameError>,
}

impl VarType {
    pub fn get_name(&self) -> &'static str {
        match self {
            VarType::Value => "value",
            VarType::Function => "function",
            VarType::DataType => "datatype",
            VarType::Constructor => "constructor",
            VarType::TypeVar => "type variable",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RenameError {
    UnboundedVariable {
        ident: Ident,
        span: Span,
        var_ty: VarType,
    },
    MultipleDefinition {
        ident: Ident,
        span1: Span,
        span2: Span,
        var_ty: VarType,
    },
    UnusedVarible {
        ident: Ident,
        span: Span,
        var_ty: VarType,
    },
}

use crate::cli::diagnostic::Diagnostic;
impl From<RenameError> for Diagnostic {
    fn from(val: RenameError) -> Self {
        match val {
            RenameError::UnboundedVariable {
                ident,
                span,
                var_ty,
            } => {
                let var_ty = var_ty.get_name();
                Diagnostic::error(format!("unbounded {var_ty} varible!")).line_span(
                    span.clone(),
                    format!("the identifier \"{}\" is not defined", ident),
                )
            }
            RenameError::MultipleDefinition {
                ident,
                span1,
                span2,
                var_ty,
            } => {
                let var_ty = var_ty.get_name();
                Diagnostic::error(format!("multipile {var_ty} varible definition!"))
                    .line_span(
                        span1.clone(),
                        format!("the identifier {} is defined here", ident),
                    )
                    .line_span(span2.clone(), "and it is defined here again".to_string())
            }
            RenameError::UnusedVarible {
                ident,
                span,
                var_ty,
            } => {
                let var_ty = var_ty.get_name();
                Diagnostic::warn(format!("unused {var_ty} varible!")).line_span(
                    span.clone(),
                    format!("the identifier {} is defined here, but never used", ident),
                )
            }
        }
    }
}

impl Renamer {
    fn new() -> Renamer {
        Renamer {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn leave_scope(&mut self) {
        let scope = self.scopes.pop().unwrap();
        for ((id, var_ty), info) in scope.into_iter() {
            if !info.used && !id.as_str().starts_with('_') {
                self.errors.push(RenameError::UnusedVarible {
                    ident: info.new_id,
                    span: info.span,
                    var_ty,
                });
            }
        }
    }

    fn intro_var(&mut self, var: &mut Var, ty: VarType) {
        if let Some(info) = self.scopes.last().unwrap().get(&(var.ident, ty)) {
            self.errors.push(RenameError::MultipleDefinition {
                ident: var.ident,
                span1: info.span.clone(),
                span2: var.span.clone(),
                var_ty: ty,
            });
        }
        let new_id = var.ident.uniquify();
        let info = VarInfo {
            new_id,
            span: var.span.clone(),
            used: false,
        };
        self.scopes
            .last_mut()
            .unwrap()
            .insert((var.ident, ty), info);
        var.ident = new_id
    }

    fn update_var(&mut self, var: &mut Var, ty: VarType) {
        for map in self.scopes.iter_mut().rev() {
            if let Some(info) = map.get_mut(&(var.ident, ty)) {
                info.used = true;
                var.ident = info.new_id;
                return;
            }
        }
        self.errors.push(RenameError::UnboundedVariable {
            ident: var.ident,
            span: var.span.clone(),
            var_ty: ty,
        });
        var.ident = var.ident.uniquify();
    }

    fn visit_type(&mut self, typ: &mut Type) {
        match typ {
            Type::Lit { lit: _, span: _ } => {}
            Type::Var { var, span: _ } => {
                self.update_var(var, VarType::TypeVar);
            }
            Type::Cons {
                cons,
                flds,
                span: _,
            } => {
                self.update_var(cons, VarType::DataType);

                for fld in flds.iter_mut() {
                    self.visit_type(fld);
                }
            }
            Type::Tuple { flds, span: _ } => {
                for fld in flds.iter_mut() {
                    self.visit_type(fld);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Lit { lit: _, span: _ } => {}
            Expr::Var { var, span: _ } => {
                self.update_var(var, VarType::Value);
            }
            Expr::Prim {
                prim: _,
                args,
                span: _,
            } => {
                for arg in args.iter_mut() {
                    self.visit_expr(arg);
                }
            }
            Expr::App {
                func,
                args,
                span: _,
            } => {
                self.update_var(func, VarType::Function);

                for arg in args.iter_mut() {
                    self.visit_expr(arg);
                }
            }
            Expr::Cons {
                cons: name,
                flds,
                span: _,
            } => {
                self.update_var(name, VarType::Constructor);

                for fld in flds.iter_mut() {
                    self.visit_expr(fld);
                }
            }
            Expr::Tuple { flds, span: _ } => {
                for fld in flds.iter_mut() {
                    self.visit_expr(fld);
                }
            }
            Expr::Match {
                expr,
                brchs,
                span: _,
            } => {
                self.visit_expr(expr);
                for (patn, expr) in brchs.iter_mut() {
                    self.enter_scope();
                    self.visit_pattern(patn);
                    self.visit_expr(expr);
                    self.leave_scope();
                }
            }
            Expr::Let {
                patn,
                expr,
                cont,
                span: _,
            } => {
                self.visit_expr(expr);
                self.enter_scope();
                self.visit_pattern(patn);
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
            Expr::Cond { brchs, span: _ } => {
                for (cond, body) in brchs {
                    self.visit_expr(cond);
                    self.visit_expr(body);
                }
            }
            Expr::Alter { brchs, span: _ } => {
                for body in brchs {
                    self.visit_expr(body);
                }
            }
            Expr::Fresh {
                vars,
                cont,
                span: _,
            } => {
                self.enter_scope();
                for var in vars.iter_mut() {
                    self.intro_var(var, VarType::Value);
                }
                self.visit_expr(cont);
                self.leave_scope();
            }
            Expr::Guard {
                lhs,
                rhs,
                cont,
                span: _,
            } => {
                self.visit_expr(lhs);
                if let Some(rhs) = rhs {
                    self.visit_expr(rhs);
                }
                self.visit_expr(cont);
            }
            Expr::Undefined { span: _ } => {}
        }
    }

    fn visit_pattern(&mut self, patn: &mut Pattern) {
        match patn {
            Pattern::Lit { lit: _, span: _ } => {
                // do nothing
            }
            Pattern::Var { var, span: _ } => {
                self.intro_var(var, VarType::Value);
            }
            Pattern::Cons {
                cons,
                flds,
                span: _,
            } => {
                self.update_var(cons, VarType::Constructor);
                for fld in flds.iter_mut() {
                    self.visit_pattern(fld);
                }
            }
            Pattern::Tuple { flds, span: _ } => {
                for fld in flds.iter_mut() {
                    self.visit_pattern(fld);
                }
            }
        }
    }

    fn visit_func_decl_head(&mut self, func_decl: &mut FuncDecl) {
        self.intro_var(&mut func_decl.name, VarType::Function);
    }

    fn visit_func_decl(&mut self, func_decl: &mut FuncDecl) {
        self.enter_scope();

        for poly in func_decl.polys.iter_mut() {
            self.intro_var(poly, VarType::TypeVar);
        }

        for (par, typ) in func_decl.pars.iter_mut() {
            self.intro_var(par, VarType::Value);
            self.visit_type(typ);
        }

        self.visit_type(&mut func_decl.res);
        self.visit_expr(&mut func_decl.body);
        self.leave_scope();
    }

    fn visit_data_decl_head(&mut self, data_decl: &mut DataDecl) {
        self.intro_var(&mut data_decl.name, VarType::DataType);

        for cons in data_decl.cons.iter_mut() {
            self.intro_var(&mut cons.name, VarType::Constructor);
        }
    }

    fn visit_data_decl(&mut self, data_decl: &mut DataDecl) {
        self.enter_scope();

        for poly in data_decl.polys.iter_mut() {
            self.intro_var(poly, VarType::TypeVar);
        }

        for cons in data_decl.cons.iter_mut() {
            for fld in cons.flds.iter_mut() {
                self.visit_type(fld);
            }
        }

        self.leave_scope();
    }

    fn visit_query_decl(&mut self, query_decl: &mut QueryDecl) {
        self.update_var(&mut query_decl.entry, VarType::Function);
    }

    fn visit_prog(&mut self, prog: &mut Program) {
        // first iteration: visit heads
        for data_decl in prog.datas.iter_mut() {
            self.visit_data_decl_head(data_decl);
        }
        for func_decl in prog.funcs.iter_mut() {
            self.visit_func_decl_head(func_decl);
        }
        // second iteration: visit body
        for data_decl in prog.datas.iter_mut() {
            self.visit_data_decl(data_decl);
        }
        for func_decl in prog.funcs.iter_mut() {
            self.visit_func_decl(func_decl);
        }
        for query_decl in prog.querys.iter_mut() {
            self.visit_query_decl(query_decl);
        }
    }
}

pub fn rename_pass(prog: &mut Program) -> Vec<RenameError> {
    let mut pass = Renamer::new();
    pass.visit_prog(prog);
    pass.errors
}

#[test]
fn renamer_test() {
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

function is_elem_after_append(xs: IntList, x: Int) -> Bool
begin
    guard !is_elem(append(xs, x), x);
    true
end

query is_elem_after_append(depth_step=5, depth_limit=50, answer_limit=1)
"#;

    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let errs = rename_pass(&mut prog);
    assert!(errs.is_empty());

    // println!("{:#?}", prog);
    // println!("{:#?}", errs);
}
