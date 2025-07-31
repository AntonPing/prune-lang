use crate::syntax::ast::{self, Expr};

use super::optimize::goal_optimize;
use super::*;

fn unify_decompose(lhs: Term<Ident>, rhs: Term<Ident>) -> Goal {
    let mut vec: Vec<Goal> = Vec::new();
    unify_decompose_help(&mut vec, lhs, rhs);
    Goal::And(vec)
}

fn unify_decompose_help(vec: &mut Vec<Goal>, lhs: Term<Ident>, rhs: Term<Ident>) {
    match (lhs, rhs) {
        (Term::Var(x1), Term::Var(x2)) if x1 == x2 => {}
        (Term::Var(var), term) | (term, Term::Var(var)) => {
            vec.push(Goal::Eq(var, term));
        }
        (Term::Lit(lit1), Term::Lit(lit2)) => {
            if lit1 != lit2 {
                vec.push(Goal::Const(false));
            }
        }
        (Term::Cons(cons1, flds1), Term::Cons(cons2, flds2)) => {
            if cons1 == cons2 {
                assert_eq!(flds1.len(), flds2.len());
                for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                    unify_decompose_help(vec, fld1, fld2);
                }
            } else {
                vec.push(Goal::Const(false));
            }
        }
        (_, _) => {
            panic!("unify simple and complex type!")
        }
    }
}

struct Transformer {
    vars: Vec<Ident>,
}

impl Transformer {
    pub fn new() -> Transformer {
        Transformer { vars: Vec::new() }
    }

    fn fresh_var<S: AsRef<str>>(&mut self, s: S) -> Ident {
        let var = Ident::fresh(&s);
        self.vars.push(var);
        var
    }

    fn translate_func(&mut self, func: &ast::FuncDecl) -> Predicate {
        assert!(self.vars.is_empty());
        let (term, goal) = self.translate_expr(&func.body);
        let name = func.name;
        let mut pars: Vec<Ident> = func.pars.iter().map(|(id, _typ)| *id).collect();
        let x = Ident::fresh(&"res_func");
        pars.push(x);
        let vars = self.vars.drain(..).collect();
        let goal = Goal::And(vec![Goal::Eq(x, term), goal]);
        Predicate {
            name: PredIdent::Pos(name),
            pars,
            vars,
            goal: goal_optimize(goal),
        }
    }

    fn translate_expr(&mut self, expr: &Expr) -> (Term<Ident>, Goal) {
        match expr {
            Expr::Lit { lit, span: _ } => (Term::Lit(*lit), Goal::Const(true)),
            Expr::Var { var, span: _ } => (Term::Var(*var), Goal::Const(true)),
            Expr::Prim {
                prim,
                args,
                span: _,
            } => {
                let x = self.fresh_var("res_prim");
                let (mut terms, mut goals): (Vec<Term<Ident>>, Vec<Goal>) =
                    args.iter().map(|arg| self.translate_expr(arg)).unzip();
                terms.push(Term::Var(x));
                goals.push(Goal::Prim(*prim, terms));
                (Term::Var(x), Goal::And(goals))
            }
            Expr::Cons {
                name,
                flds,
                span: _,
            } => {
                let (terms, goals): (Vec<Term<Ident>>, Vec<Goal>) =
                    flds.iter().map(|fld| self.translate_expr(fld)).unzip();
                (Term::Cons(*name, terms), Goal::And(goals))
            }
            Expr::Match {
                expr,
                brchs,
                span: _,
            } => {
                let x = self.fresh_var("res_match");
                let (term, goal) = self.translate_expr(expr);
                let goals = brchs
                    .iter()
                    .map(|(patn, expr)| {
                        let goal1 = unify_decompose(
                            Term::Cons(
                                patn.name,
                                patn.flds
                                    .iter()
                                    .map(|fld| {
                                        self.vars.push(*fld);
                                        Term::Var(*fld)
                                    })
                                    .collect(),
                            ),
                            term.clone(),
                        );
                        let (term2, goal2) = self.translate_expr(expr);
                        let goal3 = Goal::Eq(x, term2);
                        Goal::And(vec![goal1, goal2, goal3])
                    })
                    .collect();
                (Term::Var(x), Goal::And(vec![goal, Goal::Or(goals)]))
            }
            Expr::Let {
                bind,
                expr,
                cont,
                span: _,
            } => {
                let (term1, goal1) = self.translate_expr(expr);
                let (term2, goal2) = self.translate_expr(cont);
                self.vars.push(*bind);
                let goal = Goal::And(vec![goal1, Goal::Eq(*bind, term1), goal2]);
                (term2, goal)
            }
            Expr::App {
                func,
                args,
                span: _,
            } => {
                let x = self.fresh_var("res_app");
                let (mut terms, mut goals): (Vec<Term<Ident>>, Vec<Goal>) =
                    args.iter().map(|arg| self.translate_expr(arg)).unzip();
                terms.push(Term::Var(x));
                goals.push(Goal::PredCall(PredIdent::Pos(*func), terms));
                (Term::Var(x), Goal::And(goals))
            }
            Expr::Ifte {
                cond,
                then,
                els,
                span: _,
            } => {
                let x = self.fresh_var("res_if");
                let (term0, goal0) = self.translate_expr(cond);
                let (term1, goal1) = self.translate_expr(then);
                let (term2, goal2) = self.translate_expr(els);
                let goal = Goal::And(vec![
                    goal0,
                    Goal::Or(vec![
                        Goal::And(vec![
                            unify_decompose(term0.clone(), Term::Lit(LitVal::Bool(true))),
                            goal1,
                            Goal::Eq(x, term1),
                        ]),
                        Goal::And(vec![
                            unify_decompose(term0, Term::Lit(LitVal::Bool(false))),
                            goal2,
                            Goal::Eq(x, term2),
                        ]),
                    ]),
                ]);
                (Term::Var(x), goal)
            }
            Expr::Assert {
                expr,
                cont,
                span: _,
            } => {
                let (term1, goal1) = self.translate_expr(expr);
                let (term2, goal2) = self.translate_expr(cont);
                let goal = Goal::And(vec![
                    goal1,
                    unify_decompose(term1, Term::Lit(LitVal::Bool(true))),
                    goal2,
                ]);
                (term2, goal)
            }
        }
    }

    fn compile_pred(&mut self, pred: &ast::PredDecl) -> Predicate {
        assert!(self.vars.is_empty());
        let body = self.compile_goal(&pred.body);
        let pars: Vec<Ident> = pred.pars.iter().map(|(id, _typ)| *id).collect();
        let vars = self.vars.drain(..).collect();
        Predicate {
            name: PredIdent::Pos(pred.name),
            pars,
            vars,
            goal: goal_optimize(body),
        }
    }

    fn compile_goal(&mut self, goal: &ast::Goal) -> Goal {
        match goal {
            ast::Goal::Eq { lhs, rhs, span: _ } => {
                let (term1, goal1) = self.translate_expr(lhs);
                let (term2, goal2) = self.translate_expr(rhs);
                Goal::And(vec![goal1, goal2, unify_decompose(term1, term2)])
            }
            ast::Goal::Fail { expr: _, span: _ } => todo!(),
            ast::Goal::Pred {
                pred,
                args,
                span: _,
            } => {
                let (args, mut goals): (Vec<Term<Ident>>, Vec<Goal>) =
                    args.iter().map(|arg| self.translate_expr(arg)).unzip();
                if goals.is_empty() {
                    Goal::PredCall(PredIdent::Pos(*pred), args)
                } else {
                    goals.push(Goal::PredCall(PredIdent::Pos(*pred), args));
                    Goal::And(goals)
                }
            }
            ast::Goal::Fresh {
                vars,
                body,
                span: _,
            } => {
                self.vars.extend_from_slice(&vars[..]);
                self.compile_goal(body)
            }
            ast::Goal::And { goals, span: _ } => {
                let goals = goals.iter().map(|goal| self.compile_goal(goal)).collect();
                Goal::And(goals)
            }
            ast::Goal::Or { goals, span: _ } => {
                let goals = goals.iter().map(|goal| self.compile_goal(goal)).collect();
                Goal::Or(goals)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DnfGoal {
    pub eqs: Vec<(Term<Ident>, Term<Ident>)>,
    pub prims: Vec<(Prim, Vec<Term<Ident>>)>,
    pub preds: Vec<(PredIdent, Vec<Term<Ident>>)>,
}

impl DnfGoal {
    pub fn new() -> DnfGoal {
        DnfGoal {
            eqs: Vec::new(),
            prims: Vec::new(),
            preds: Vec::new(),
        }
    }

    pub fn free_vars(&self, vars: &mut Vec<Ident>) {
        for (lhs, rhs) in &self.eqs {
            lhs.free_vars(vars);
            rhs.free_vars(vars);
        }

        for (_prim, args) in &self.prims {
            args.iter().for_each(|arg| arg.free_vars(vars));
        }

        for (_prim, args) in &self.preds {
            args.iter().for_each(|arg| arg.free_vars(vars));
        }
    }
}

pub fn prog_to_dict(prog: &ast::Program) -> HashMap<PredIdent, Predicate> {
    let mut pass = Transformer::new();
    let mut dict = HashMap::new();
    for func in &prog.funcs {
        let pred = pass.translate_func(func);
        dict.insert(PredIdent::Pos(func.name), pred);
    }
    for pred in &prog.preds {
        let check_pred = pass.compile_pred(pred);
        dict.insert(PredIdent::Pos(pred.name), check_pred);
    }
    dict
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct DnfPredicate {
//     pub name: PredIdent,
//     pub pars: Vec<Ident>,
//     pub goals: Vec<DnfGoal>,
// }

// impl DnfPredicate {
//     pub fn free_vars(&self, vars: &mut Vec<Ident>) {
//         assert!(vars.is_empty());
//         for par in &self.pars {
//             vars.push(*par);
//         }
//         for goal in &self.goals {
//             goal.free_vars(vars);
//         }
//     }
// }

// pub fn dnf_pred_dict(dict: &HashMap<PredIdent, Predicate>) -> HashMap<PredIdent, DnfPredicate> {
//     let mut res: HashMap<_, DnfPredicate> = HashMap::new();
//     for (name, pred) in dict {
//         let pred = dnf_trans_predicate(pred);
//         res.insert(*name, pred);
//     }
//     res
// }

// fn dnf_trans_predicate(pred: &Predicate) -> DnfPredicate {
//     let mut goals = Vec::new();
//     goals.push(DnfGoal::new());
//     dnf_trans_goal(&mut goals, &pred.goal);
//     DnfPredicate {
//         name: pred.name,
//         pars: pred.pars.clone(),
//         goals,
//     }
// }

// fn dnf_trans_goal(vec: &mut Vec<DnfGoal>, goal: &Goal) {
//     match goal {
//         Goal::Const(true) => {}
//         Goal::Const(false) => {
//             vec.clear();
//         }
//         Goal::Eq(term1, term2) => {
//             for goal in vec.iter_mut() {
//                 goal.eqs.push((Term::Var(*term1), term2.clone()));
//             }
//         }
//         Goal::Prim(prim, args) => {
//             for goal in vec.iter_mut() {
//                 goal.prims.push((*prim, args.clone()));
//             }
//         }
//         Goal::And(goals) => {
//             for goal in goals {
//                 dnf_trans_goal(vec, goal);
//             }
//         }
//         Goal::Or(goals) => {
//             let mut save = Vec::new();
//             std::mem::swap(vec, &mut save);
//             for goal in goals {
//                 let mut new_vec = save.clone();
//                 dnf_trans_goal(&mut new_vec, goal);
//                 vec.append(&mut new_vec);
//             }
//         }
//         Goal::PredCall(name, args) => {
//             for goal in vec.iter_mut() {
//                 goal.preds.push((*name, args.clone()));
//             }
//         }
//     }
// }

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

    let dict = prog_to_dict(&prog);
    println!("{:#?}", dict);
    // let dict = dnf_pred_dict(&dict);
    // println!("{:#?}", dict);
}
