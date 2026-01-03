use crate::logic;
use crate::syntax::ast;

use super::optimize;
use super::*;

fn translate_data_decl(data: &ast::DataDecl) -> logic::ast::DataDecl {
    let name = data.name.ident;
    let polys = data.polys.iter().map(|poly| poly.ident).collect();
    let cons = data.cons.iter().map(translate_constructor).collect();
    logic::ast::DataDecl { name, polys, cons }
}

fn translate_constructor(cons: &ast::Constructor) -> logic::ast::Constructor {
    let name = cons.name.ident;
    let flds = cons.flds.iter().map(translate_type).collect();
    Constructor { name, flds }
}

fn translate_type(typ: &ast::Type) -> TypeId {
    match typ {
        ast::Type::Lit { lit, span: _ } => Term::Lit(*lit),
        ast::Type::Var { var, span: _ } => Term::Var(var.ident),
        ast::Type::Cons {
            cons,
            flds,
            span: _,
        } => {
            let flds = flds.iter().map(translate_type).collect();
            Term::Cons(OptCons::Some(cons.ident), flds)
        }
        ast::Type::Tuple { flds, span: _ } => {
            let flds: Vec<TypeId> = flds.iter().map(translate_type).collect();
            Term::Cons(OptCons::None, flds)
        }
    }
}

fn translate_func(func: &ast::FuncDecl) -> PredDecl {
    let mut vars = Vec::new();
    let (atom, goal) = translate_expr(&mut vars, &func.body);
    let name = func.name.ident;
    let polys = func.polys.iter().map(|poly| poly.ident).collect();
    let mut pars: Vec<(Ident, TypeId)> = func
        .pars
        .iter()
        .map(|(var, typ)| (var.ident, translate_type(typ)))
        .collect();
    let x = Ident::fresh(&"res_func");
    pars.push((x, translate_type(&func.res)));
    let vars = vars
        .iter()
        .map(|var| (*var, TypeId::Var(Ident::fresh(&"res_func"))))
        .collect();
    let goal = Goal::And(vec![Goal::Eq(Term::Var(x), atom.to_term()), goal]);
    PredDecl {
        name,
        polys,
        pars,
        vars,
        goal: optimize::goal_optimize(goal),
    }
}

fn translate_expr(vars: &mut Vec<Ident>, expr: &ast::Expr) -> (AtomId, Goal) {
    match expr {
        ast::Expr::Lit { lit, span: _ } => (Term::Lit(*lit), Goal::Lit(true)),
        ast::Expr::Var { var, span: _ } => (Term::Var(var.ident), Goal::Lit(true)),
        ast::Expr::Prim {
            prim,
            args,
            span: _,
        } => {
            let x = Ident::fresh(&"res_prim");
            vars.push(x);
            let (mut atoms, mut goals): (Vec<AtomId>, Vec<Goal>) =
                args.iter().map(|arg| translate_expr(vars, arg)).unzip();
            atoms.push(Term::Var(x));
            goals.push(Goal::Prim(*prim, atoms));
            (Term::Var(x), Goal::And(goals))
        }
        ast::Expr::Cons {
            cons,
            flds,
            span: _,
        } => {
            let x = Ident::fresh(&"res_cons");
            vars.push(x);
            let (flds, mut goals): (Vec<AtomId>, Vec<Goal>) =
                flds.iter().map(|fld| translate_expr(vars, fld)).unzip();
            goals.push(Goal::Eq(
                Term::Var(x),
                Term::Cons(
                    OptCons::Some(cons.ident),
                    flds.into_iter().map(|fld| fld.to_term()).collect(),
                ),
            ));
            (Term::Var(x), Goal::And(goals))
        }
        ast::Expr::Tuple { flds, span: _ } => {
            let x = Ident::fresh(&"res_tuple");
            vars.push(x);
            let (flds, mut goals): (Vec<AtomId>, Vec<Goal>) =
                flds.iter().map(|fld| translate_expr(vars, fld)).unzip();
            goals.push(Goal::Eq(
                Term::Var(x),
                Term::Cons(
                    OptCons::None,
                    flds.into_iter().map(|fld| fld.to_term()).collect(),
                ),
            ));
            (Term::Var(x), Goal::And(goals))
        }
        ast::Expr::Match {
            expr,
            brchs,
            span: _,
        } => {
            let x = Ident::fresh(&"res_match");
            vars.push(x);
            let (atom0, goal0) = translate_expr(vars, expr);
            let mut goals = Vec::new();
            for (patn, expr) in brchs {
                let patn_term = patn_to_term(vars, patn);
                let (atom1, goal1) = translate_expr(vars, expr);
                goals.push(Goal::And(vec![
                    Goal::Eq(atom0.to_term(), patn_term),
                    goal1,
                    Goal::Eq(Term::Var(x), atom1.to_term()),
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
            let (atom0, goal0) = translate_expr(vars, expr);
            let patn_term = patn_to_term(vars, patn);
            let (atom2, goal2) = translate_expr(vars, cont);
            let goal = Goal::And(vec![goal0, Goal::Eq(atom0.to_term(), patn_term), goal2]);
            (atom2, goal)
        }
        ast::Expr::App {
            func,
            args,
            span: _,
        } => {
            let x = Ident::fresh(&"res_app");
            vars.push(x);
            let (mut atoms, mut goals): (Vec<AtomId>, Vec<Goal>) =
                args.iter().map(|arg| translate_expr(vars, arg)).unzip();
            atoms.push(Term::Var(x));
            goals.push(Goal::Call(
                func.ident,
                Vec::new(),
                atoms.into_iter().map(|atom| atom.to_term()).collect(),
            ));
            (Term::Var(x), Goal::And(goals))
        }
        ast::Expr::Ifte {
            cond,
            then,
            els,
            span: _,
        } => {
            let x = Ident::fresh(&"res_if");
            vars.push(x);
            let (atom0, goal0) = translate_expr(vars, cond);
            let (atom1, goal1) = translate_expr(vars, then);
            let (atom2, goal2) = translate_expr(vars, els);
            match atom0 {
                Term::Var(var) => {
                    let goal = Goal::And(vec![
                        goal0,
                        Goal::Or(vec![
                            Goal::And(vec![
                                Goal::Eq(Term::Var(var), Term::Lit(LitVal::Bool(true))),
                                goal1,
                                Goal::Eq(Term::Var(x), atom1.to_term()),
                            ]),
                            Goal::And(vec![
                                Goal::Eq(Term::Var(var), Term::Lit(LitVal::Bool(false))),
                                goal2,
                                Goal::Eq(Term::Var(x), atom2.to_term()),
                            ]),
                        ]),
                    ]);
                    (Term::Var(x), goal)
                }
                Term::Lit(LitVal::Bool(true)) => {
                    let goal = Goal::And(vec![goal0, goal1]);
                    (atom1, goal)
                }
                Term::Lit(LitVal::Bool(false)) => {
                    let goal = Goal::And(vec![goal0, goal2]);
                    (atom2, goal)
                }
                _ => {
                    unreachable!();
                }
            }
        }
        ast::Expr::Cond { brchs, span: _ } => {
            let x = Ident::fresh(&"res_cond");
            vars.push(x);

            let mut goals = Vec::new();
            for (cond, body) in brchs {
                let (atom0, goal0) = translate_expr(vars, cond);
                let (atom1, goal1) = translate_expr(vars, body);
                match atom0 {
                    Term::Var(var) => {
                        let goal = Goal::And(vec![
                            goal0,
                            Goal::Eq(Term::Var(var), Term::Lit(LitVal::Bool(true))),
                            goal1,
                            Goal::Eq(Term::Var(x), atom1.to_term()),
                        ]);
                        goals.push(goal);
                    }
                    Term::Lit(LitVal::Bool(true)) => {
                        let goal =
                            Goal::And(vec![goal0, goal1, Goal::Eq(Term::Var(x), atom1.to_term())]);
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
            let x = Ident::fresh(&"res_alter");
            vars.push(x);
            let mut goals = Vec::new();
            for body in brchs {
                let (atom, goal) = translate_expr(vars, body);
                let goal = Goal::And(vec![goal, Goal::Eq(Term::Var(x), atom.to_term())]);
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
            vars.extend_from_slice(&new_vars[..]);
            translate_expr(vars, cont)
        }
        ast::Expr::Guard {
            lhs,
            rhs,
            cont,
            span: _,
        } => {
            let (atom1, goal1) = translate_expr(vars, lhs);
            let (atom2, goal2) = translate_expr(
                vars,
                rhs.as_deref().unwrap_or(&Box::new(ast::Expr::Lit {
                    lit: LitVal::Bool(true),
                    span: logos::Span { start: 0, end: 0 },
                })),
            );
            let (atom3, goal3) = translate_expr(vars, cont);
            (
                atom3,
                Goal::And(vec![
                    goal1,
                    goal2,
                    Goal::Eq(atom1.to_term(), atom2.to_term()),
                    goal3,
                ]),
            )
        }
        ast::Expr::Undefined { span: _ } => (Term::Var(Ident::dummy(&"@phoney")), Goal::Lit(false)),
    }
}

fn patn_to_term(vars: &mut Vec<Ident>, patn: &ast::Pattern) -> TermId {
    match patn {
        ast::Pattern::Lit { lit, span: _ } => TermId::Lit(*lit),
        ast::Pattern::Var { var, span: _ } => {
            vars.push(var.ident);
            TermId::Var(var.ident)
        }
        ast::Pattern::Cons {
            cons,
            flds,
            span: _,
        } => {
            let flds = flds.iter().map(|fld| patn_to_term(vars, fld)).collect();
            TermId::Cons(OptCons::Some(cons.ident), flds)
        }
        ast::Pattern::Tuple { flds, span: _ } => {
            let flds: Vec<TermId> = flds.iter().map(|fld| patn_to_term(vars, fld)).collect();
            TermId::Cons(OptCons::None, flds)
        }
    }
}

fn translate_query(query: &ast::QueryDecl) -> logic::ast::QueryDecl {
    logic::ast::QueryDecl {
        entry: query.entry.ident,
        params: query
            .params
            .iter()
            .map(|(param, _span)| translate_query_param(param))
            .collect(),
    }
}

fn translate_query_param(param: &ast::QueryParam) -> logic::ast::QueryParam {
    match param {
        ast::QueryParam::DepthStep(x) => logic::ast::QueryParam::DepthStep(*x),
        ast::QueryParam::DepthLimit(x) => logic::ast::QueryParam::DepthLimit(*x),
        ast::QueryParam::AnswerLimit(x) => logic::ast::QueryParam::AnswerLimit(*x),
        ast::QueryParam::AnswerPause(x) => logic::ast::QueryParam::AnswerPause(*x),
    }
}

pub fn logic_translation(prog: &ast::Program) -> logic::ast::Program {
    let mut datas: HashMap<Ident, DataDecl> = HashMap::new();
    for data in prog.datas.iter() {
        let res = translate_data_decl(data);
        datas.insert(data.name.ident, res);
    }

    let mut preds = HashMap::new();
    for func in prog.funcs.iter() {
        let res = translate_func(func);
        preds.insert(func.name.ident, res);
    }

    let mut querys = Vec::new();
    for query in prog.querys.iter() {
        let res = translate_query(query);
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
