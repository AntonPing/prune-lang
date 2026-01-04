use crate::logic;
use crate::syntax::ast;

use super::optimize;
use super::*;

pub struct Translater {
    vars: Vec<(Ident, TypeId)>,
}

impl Translater {
    fn new() -> Translater {
        Translater { vars: Vec::new() }
    }

    fn translate_data_decl(&self, data: &ast::DataDecl) -> logic::ast::DataDecl {
        let name = data.name.ident;
        let polys = data.polys.iter().map(|poly| poly.ident).collect();
        let cons = data
            .cons
            .iter()
            .map(|cons| self.translate_constructor(cons))
            .collect();
        logic::ast::DataDecl { name, polys, cons }
    }

    fn translate_constructor(&self, cons: &ast::Constructor) -> logic::ast::Constructor {
        let name = cons.name.ident;
        let flds = cons
            .flds
            .iter()
            .map(|fld| self.translate_type(fld))
            .collect();
        Constructor { name, flds }
    }

    fn translate_type(&self, typ: &ast::Type) -> TypeId {
        match typ {
            ast::Type::Lit { lit, span: _ } => Term::Lit(*lit),
            ast::Type::Var { var, span: _ } => Term::Var(var.ident),
            ast::Type::Cons {
                cons,
                flds,
                span: _,
            } => {
                let flds = flds.iter().map(|fld| self.translate_type(fld)).collect();
                Term::Cons(OptCons::Some(cons.ident), flds)
            }
            ast::Type::Tuple { flds, span: _ } => {
                let flds: Vec<TypeId> = flds.iter().map(|fld| self.translate_type(fld)).collect();
                Term::Cons(OptCons::None, flds)
            }
        }
    }

    fn fresh_var(&mut self) -> Ident {
        let var = Ident::fresh(&"x");
        let typ = TypeId::Var(Ident::fresh(&"a"));
        self.vars.push((var, typ));
        var
    }

    fn translate_func(&mut self, func: &ast::FuncDecl) -> PredDecl {
        self.vars = Vec::new();
        let (term, goal) = self.translate_expr(&func.body);
        let name = func.name.ident;
        let polys = func.polys.iter().map(|poly| poly.ident).collect();
        let mut pars: Vec<(Ident, TypeId)> = func
            .pars
            .iter()
            .map(|(var, typ)| (var.ident, self.translate_type(typ)))
            .collect();
        let res = Ident::fresh(&"res");
        pars.push((res, self.translate_type(&func.res)));
        let goal = Goal::And(vec![Goal::Eq(Term::Var(res), term), goal]);
        PredDecl {
            name,
            polys,
            pars,
            vars: self.vars.clone(),
            goal: optimize::goal_optimize(goal),
        }
    }

    fn translate_expr(&mut self, expr: &ast::Expr) -> (TermId, Goal) {
        match expr {
            ast::Expr::Lit { lit, span: _ } => (Term::Lit(*lit), Goal::Lit(true)),
            ast::Expr::Var { var, span: _ } => (Term::Var(var.ident), Goal::Lit(true)),
            ast::Expr::Prim {
                prim,
                args,
                span: _,
            } => {
                let x = self.fresh_var();
                let (mut terms, mut goals): (Vec<TermId>, Vec<Goal>) =
                    args.iter().map(|arg| self.translate_expr(arg)).unzip();
                terms.push(Term::Var(x));
                let terms = terms
                    .into_iter()
                    .map(|term| term.to_atom().unwrap())
                    .collect();
                goals.push(Goal::Prim(*prim, terms));
                (Term::Var(x), Goal::And(goals))
            }
            ast::Expr::Cons {
                cons,
                flds,
                span: _,
            } => {
                let (flds, goals): (Vec<TermId>, Vec<Goal>) =
                    flds.iter().map(|fld| self.translate_expr(fld)).unzip();
                (
                    Term::Cons(OptCons::Some(cons.ident), flds),
                    Goal::And(goals),
                )
            }
            ast::Expr::Tuple { flds, span: _ } => {
                let (flds, goals): (Vec<TermId>, Vec<Goal>) =
                    flds.iter().map(|fld| self.translate_expr(fld)).unzip();
                (Term::Cons(OptCons::None, flds), Goal::And(goals))
            }
            ast::Expr::Match {
                expr,
                brchs,
                span: _,
            } => {
                let x = self.fresh_var();
                let (term0, goal0) = self.translate_expr(expr);
                let mut goals = Vec::new();
                for (patn, expr) in brchs {
                    let patn_term = self.patn_to_term(patn);
                    let (term1, goal1) = self.translate_expr(expr);
                    goals.push(Goal::And(vec![
                        Goal::Eq(term0.clone(), patn_term),
                        goal1,
                        Goal::Eq(Term::Var(x), term1),
                    ]));
                }
                (Term::Var(x), Goal::And(vec![goal0, Goal::Or(goals)]))
            }
            ast::Expr::Let {
                patn,
                expr,
                cont,
                span: _,
            } => {
                let (term0, goal0) = self.translate_expr(expr);
                let patn_term = self.patn_to_term(patn);
                let (term1, goal1) = self.translate_expr(cont);
                (
                    term1,
                    Goal::And(vec![goal0, Goal::Eq(term0, patn_term), goal1]),
                )
            }
            ast::Expr::App {
                func,
                args,
                span: _,
            } => {
                let x = self.fresh_var();
                let (mut terms, mut goals): (Vec<TermId>, Vec<Goal>) =
                    args.iter().map(|arg| self.translate_expr(arg)).unzip();
                terms.push(Term::Var(x));
                goals.push(Goal::Call(func.ident, Vec::new(), terms));
                (Term::Var(x), Goal::And(goals))
            }
            ast::Expr::Ifte {
                cond,
                then,
                els,
                span: _,
            } => {
                let x = self.fresh_var();
                let (term0, goal0) = self.translate_expr(cond);
                let (term1, goal1) = self.translate_expr(then);
                let (term2, goal2) = self.translate_expr(els);
                match term0 {
                    Term::Var(var) => {
                        let goal = Goal::And(vec![
                            goal0,
                            Goal::Or(vec![
                                Goal::And(vec![
                                    Goal::Eq(Term::Var(var), Term::Lit(LitVal::Bool(true))),
                                    goal1,
                                    Goal::Eq(Term::Var(x), term1),
                                ]),
                                Goal::And(vec![
                                    Goal::Eq(Term::Var(var), Term::Lit(LitVal::Bool(false))),
                                    goal2,
                                    Goal::Eq(Term::Var(x), term2),
                                ]),
                            ]),
                        ]);
                        (Term::Var(x), goal)
                    }
                    Term::Lit(LitVal::Bool(true)) => (term1, Goal::And(vec![goal0, goal1])),
                    Term::Lit(LitVal::Bool(false)) => (term2, Goal::And(vec![goal0, goal2])),
                    _ => {
                        unreachable!();
                    }
                }
            }
            ast::Expr::Cond { brchs, span: _ } => {
                let x = self.fresh_var();
                let mut goals = Vec::new();
                for (cond, body) in brchs {
                    let (term0, goal0) = self.translate_expr(cond);
                    let (term1, goal1) = self.translate_expr(body);
                    match term0 {
                        Term::Var(var) => {
                            let goal = Goal::And(vec![
                                goal0,
                                Goal::Eq(Term::Var(var), Term::Lit(LitVal::Bool(true))),
                                goal1,
                                Goal::Eq(Term::Var(x), term1),
                            ]);
                            goals.push(goal);
                        }
                        Term::Lit(LitVal::Bool(true)) => {
                            let goal = Goal::And(vec![goal0, goal1, Goal::Eq(Term::Var(x), term1)]);
                            goals.push(goal);
                        }
                        Term::Lit(LitVal::Bool(false)) => {}
                        _ => {
                            unreachable!();
                        }
                    }
                }
                (Term::Var(x), Goal::Or(goals))
            }
            ast::Expr::Alter { brchs, span: _ } => {
                let x = self.fresh_var();
                let mut goals = Vec::new();
                for body in brchs {
                    let (term, goal) = self.translate_expr(body);
                    let goal = Goal::And(vec![goal, Goal::Eq(Term::Var(x), term)]);
                    goals.push(goal);
                }
                (Term::Var(x), Goal::Or(goals))
            }
            ast::Expr::Fresh {
                vars: new_vars,
                cont,
                span: _,
            } => {
                let new_vars: Vec<Ident> = new_vars.iter().map(|var| var.ident).collect();
                let vec: Vec<(Ident, TypeId)> = new_vars
                    .iter()
                    .map(|var| (*var, TypeId::Var(Ident::fresh(&"a"))))
                    .collect();
                self.vars.extend_from_slice(&vec[..]);
                self.translate_expr(cont)
            }
            ast::Expr::Guard {
                lhs,
                rhs,
                cont,
                span: _,
            } => {
                let (term1, goal1) = self.translate_expr(lhs);
                let (term2, goal2) =
                    self.translate_expr(rhs.as_deref().unwrap_or(&Box::new(ast::Expr::Lit {
                        lit: LitVal::Bool(true),
                        span: logos::Span { start: 0, end: 0 },
                    })));
                let (term3, goal3) = self.translate_expr(cont);
                (
                    term3,
                    Goal::And(vec![goal1, goal2, Goal::Eq(term1, term2), goal3]),
                )
            }
            ast::Expr::Undefined { span: _ } => {
                (Term::Var(Ident::dummy(&"@placeholder")), Goal::Lit(false))
            }
        }
    }

    fn patn_to_term(&mut self, patn: &ast::Pattern) -> TermId {
        match patn {
            ast::Pattern::Lit { lit, span: _ } => TermId::Lit(*lit),
            ast::Pattern::Var { var, span: _ } => {
                self.vars.push((var.ident, TypeId::Var(Ident::fresh(&"a"))));
                TermId::Var(var.ident)
            }
            ast::Pattern::Cons {
                cons,
                flds,
                span: _,
            } => {
                let flds = flds.iter().map(|fld| self.patn_to_term(fld)).collect();
                TermId::Cons(OptCons::Some(cons.ident), flds)
            }
            ast::Pattern::Tuple { flds, span: _ } => {
                let flds: Vec<TermId> = flds.iter().map(|fld| self.patn_to_term(fld)).collect();
                TermId::Cons(OptCons::None, flds)
            }
        }
    }

    fn translate_query(&mut self, query: &ast::QueryDecl) -> logic::ast::QueryDecl {
        logic::ast::QueryDecl {
            entry: query.entry.ident,
            params: query
                .params
                .iter()
                .map(|(param, _span)| self.translate_query_param(param))
                .collect(),
        }
    }

    fn translate_query_param(&mut self, param: &ast::QueryParam) -> logic::ast::QueryParam {
        match param {
            ast::QueryParam::DepthStep(x) => logic::ast::QueryParam::DepthStep(*x),
            ast::QueryParam::DepthLimit(x) => logic::ast::QueryParam::DepthLimit(*x),
            ast::QueryParam::AnswerLimit(x) => logic::ast::QueryParam::AnswerLimit(*x),
            ast::QueryParam::AnswerPause(x) => logic::ast::QueryParam::AnswerPause(*x),
        }
    }
}

pub fn logic_translation(prog: &ast::Program) -> logic::ast::Program {
    let mut pass = Translater::new();

    let mut datas: HashMap<Ident, DataDecl> = HashMap::new();
    for data in prog.datas.iter() {
        let res = pass.translate_data_decl(data);
        datas.insert(data.name.ident, res);
    }

    let mut preds = HashMap::new();
    for func in prog.funcs.iter() {
        let res = pass.translate_func(func);
        preds.insert(func.name.ident, res);
    }

    let mut querys = Vec::new();
    for query in prog.querys.iter() {
        let res = pass.translate_query(query);
        querys.push(res);
    }
    logic::ast::Program {
        datas,
        preds,
        querys,
    }
}

#[test]
#[ignore = "just to see result"]
fn prog_to_pred_test() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) =>
        Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end
"#;
    let (prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let prog = logic_translation(&prog);
    println!("{:#?}", prog);
}
