use crate::logic;
use crate::syntax::ast;

use super::optimize;
use super::*;

fn unify_decompose(vars: &mut Vec<Ident>, lhs: TermId, rhs: TermId) -> Goal {
    let mut vec: Vec<Goal> = Vec::new();
    unify_decompose_help(vars, &mut vec, lhs, rhs);
    Goal::And(vec)
}

fn unify_decompose_help(vars: &mut Vec<Ident>, vec: &mut Vec<Goal>, lhs: TermId, rhs: TermId) {
    match (lhs, rhs) {
        (Term::Var(var1), Term::Var(var2)) if var1 == var2 => {}
        (Term::Var(lhs), rhs) | (rhs, Term::Var(lhs)) => match rhs {
            Term::Var(var) => vec.push(Goal::Eq(lhs, Term::Var(var))),
            Term::Lit(lit) => vec.push(Goal::Eq(lhs, Term::Lit(lit))),
            Term::Cons(_, cons, flds) => {
                let mut atoms: Vec<AtomId> = Vec::new();
                for fld in flds {
                    match fld {
                        Term::Var(_) | Term::Lit(_) => {
                            let atom = fld.to_atom().unwrap();
                            atoms.push(atom);
                        }
                        Term::Cons(_, _, _) => {
                            let x = Ident::fresh(&"x_fld");
                            vars.push(x);
                            atoms.push(Term::Var(x));
                            unify_decompose_help(vars, vec, Term::Var(x), fld);
                        }
                    }
                }
                vec.push(Goal::Cons(lhs, cons, atoms));
            }
        },
        (Term::Lit(lit1), Term::Lit(lit2)) => {
            if lit1 != lit2 {
                vec.push(Goal::Lit(false));
            }
        }
        (Term::Cons(_, cons1, flds1), Term::Cons(_, cons2, flds2)) => {
            if cons1 == cons2 {
                assert_eq!(flds1.len(), flds2.len());
                for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                    unify_decompose_help(vars, vec, fld1, fld2);
                }
            } else {
                vec.push(Goal::Lit(false));
            }
        }
        (_, _) => {
            panic!("unify simple and complex type!")
        }
    }
}

fn translate_data_decl(data: &ast::DataDecl) -> logic::ast::DataDecl {
    let name = data.name.ident;
    let cons = data
        .cons
        .iter()
        .map(|cons| translate_constructor(cons))
        .collect();
    logic::ast::DataDecl { name, cons }
}

fn translate_constructor(cons: &ast::Constructor) -> logic::ast::Constructor {
    let name = cons.name.ident;
    let flds = cons.flds.iter().map(|fld| translate_type(fld)).collect();
    Constructor { name, flds }
}

fn translate_type(typ: &ast::Type) -> TypeId {
    match typ {
        ast::Type::Lit { lit, span: _ } => Term::Lit((*lit).into()),
        ast::Type::Cons {
            cons,
            flds,
            span: _,
        } => {
            let flds = flds.iter().map(|fld| translate_type(fld)).collect();
            Term::Cons((), cons.ident, flds)
        }
        ast::Type::Tuple { flds, span: _ } => {
            let flds = flds.iter().map(|fld| translate_type(fld)).collect();
            Term::Cons((), Ident::dummy(&"#"), flds)
        }
    }
}

fn translate_func(func: &ast::FuncDecl) -> PredDecl {
    let mut vars = Vec::new();
    let (term, goal) = translate_expr(&mut vars, &func.body);
    let name = func.name.ident;
    let mut pars: Vec<Ident> = func.pars.iter().map(|(var, _typ)| var.ident).collect();
    let x = Ident::fresh(&"res_func");
    pars.push(x);
    let goal = Goal::And(vec![Goal::Eq(x, term), goal]);
    PredDecl {
        name,
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
            goals.push(Goal::Cons(x, cons.ident, flds));
            (Term::Var(x), Goal::And(goals))
        }
        ast::Expr::Tuple { flds, span: _ } => {
            let x = Ident::fresh(&"res_tuple");
            vars.push(x);
            let (flds, mut goals): (Vec<AtomId>, Vec<Goal>) =
                flds.iter().map(|fld| translate_expr(vars, fld)).unzip();
            goals.push(Goal::Cons(x, Ident::dummy(&"#"), flds));
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
            let goals = brchs
                .iter()
                .map(|(patn, expr)| {
                    let patn_term = patn_to_term(vars, patn);
                    let goal1 = unify_decompose(vars, atom0.to_term(), patn_term);
                    let (atom2, goal2) = translate_expr(vars, expr);
                    Goal::And(vec![goal1, goal2, Goal::Eq(x, atom2)])
                })
                .collect();
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
            let goal1 = unify_decompose(vars, atom0.to_term(), patn_term);
            let (atom2, goal2) = translate_expr(vars, cont);
            let goal = Goal::And(vec![goal0, goal1, goal2]);
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
            goals.push(Goal::Call(func.ident, atoms));
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
                                Goal::Eq(var, Term::Lit(LitVal::Bool(true))),
                                goal1,
                                Goal::Eq(x, atom1),
                            ]),
                            Goal::And(vec![
                                Goal::Eq(var, Term::Lit(LitVal::Bool(false))),
                                goal2,
                                Goal::Eq(x, atom2),
                            ]),
                        ]),
                    ]);
                    (Term::Var(x), goal)
                }
                Term::Lit(LitVal::Bool(true)) => {
                    let goal = Goal::And(vec![goal0, goal1, Goal::Eq(x, atom1)]);
                    (Term::Var(x), goal)
                }
                Term::Lit(LitVal::Bool(false)) => {
                    let goal = Goal::And(vec![goal0, goal2, Goal::Eq(x, atom2)]);
                    (Term::Var(x), goal)
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
                            Goal::Eq(var, Term::Lit(LitVal::Bool(true))),
                            goal1,
                            Goal::Eq(x, atom1),
                        ]);
                        goals.push(goal);
                    }
                    Term::Lit(LitVal::Bool(true)) => {
                        let goal = Goal::And(vec![goal0, goal1, Goal::Eq(x, atom1)]);
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
                let goal = Goal::And(vec![goal, Goal::Eq(x, atom)]);
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
            let (atom2, goal2) = translate_expr(vars, rhs);
            let (atom3, goal3) = translate_expr(vars, cont);
            (
                atom3,
                Goal::And(vec![
                    goal1,
                    goal2,
                    unify_decompose(vars, atom1.to_term(), atom2.to_term()),
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
            TermId::Cons((), cons.ident, flds)
        }
        ast::Pattern::Tuple { flds, span: _ } => {
            let flds = flds.iter().map(|fld| patn_to_term(vars, fld)).collect();
            TermId::Cons((), Ident::dummy(&"#"), flds)
        }
    }
}

fn translate_pred(pred: &ast::PredDecl) -> PredDecl {
    let mut vars = Vec::new();
    let body = translate_goal(&mut vars, &pred.body);
    let pars: Vec<Ident> = pred.pars.iter().map(|(var, _typ)| var.ident).collect();
    PredDecl {
        name: pred.name.ident,
        pars,
        vars,
        goal: optimize::goal_optimize(body),
    }
}

fn translate_goal(vars: &mut Vec<Ident>, goal: &ast::Goal) -> Goal {
    match goal {
        ast::Goal::Eq { lhs, rhs, span: _ } => {
            let (term1, goal1) = translate_expr(vars, lhs);
            let (term2, goal2) = translate_expr(vars, rhs);
            Goal::And(vec![
                goal1,
                goal2,
                unify_decompose(vars, term1.to_term(), term2.to_term()),
            ])
        }
        ast::Goal::Pred {
            pred,
            args,
            span: _,
        } => {
            let (args, mut goals): (Vec<AtomId>, Vec<Goal>) =
                args.iter().map(|arg| translate_expr(vars, arg)).unzip();
            goals.push(Goal::Call(pred.ident, args));
            Goal::And(goals)
        }
        ast::Goal::Fresh {
            vars: new_vars,
            body,
            span: _,
        } => {
            let new_vars: Vec<Ident> = new_vars.iter().map(|var| var.ident).collect();
            vars.extend_from_slice(&new_vars[..]);
            translate_goal(vars, body)
        }
        ast::Goal::And { goals, span: _ } => {
            let goals = goals
                .iter()
                .map(|goal| translate_goal(vars, goal))
                .collect();
            Goal::And(goals)
        }
        ast::Goal::Or { goals, span: _ } => {
            let goals = goals
                .iter()
                .map(|goal| translate_goal(vars, goal))
                .collect();
            Goal::Or(goals)
        }
        ast::Goal::Lit { val, span: _ } => Goal::Lit(*val),
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
    for pred in prog.preds.iter() {
        let res = translate_pred(pred);
        preds.insert(pred.name.ident, res);
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
