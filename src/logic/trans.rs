use crate::syntax::ast::{self, Expr};

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Goal {
    Const(bool),
    Eq(Ident, Term<Ident>),
    Prim(Prim, Vec<Term<Ident>>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    PredCall(PredIdent, Vec<Term<Ident>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    pub name: PredIdent,
    pub pars: Vec<Ident>,
    pub goal: Goal,
}

pub fn goal_flatten(goal: Goal) -> Goal {
    match goal {
        Goal::And(goals) => {
            let mut vec = Vec::new();
            for goal in goals {
                let goal = goal_flatten(goal);
                match goal {
                    Goal::Const(true) => {}
                    Goal::Const(false) => return Goal::Const(false),
                    Goal::And(mut goals) => vec.append(&mut goals),
                    goal => vec.push(goal),
                }
            }
            match vec.len() {
                0 => Goal::Const(true),
                1 => vec.into_iter().next().unwrap(),
                _ => Goal::And(vec),
            }
        }
        Goal::Or(goals) => {
            let mut vec = Vec::new();
            for goal in goals {
                let goal = goal_flatten(goal);
                match goal {
                    Goal::Const(false) => {}
                    Goal::Const(true) => return Goal::Const(true),
                    Goal::Or(mut goals) => vec.append(&mut goals),
                    goal => vec.push(goal),
                }
            }
            match vec.len() {
                0 => Goal::Const(false),
                1 => vec.into_iter().next().unwrap(),
                _ => Goal::Or(vec),
            }
        }
        goal => goal,
    }
}

pub fn goal_reorder(goal: Goal) -> Goal {
    goal_reorder_help(goal).0
}

fn goal_reorder_help(goal: Goal) -> (Goal, usize) {
    match goal {
        Goal::Const(_) => (goal, 0),
        Goal::Eq(_, _) => (goal, 100),
        Goal::Prim(_, _) => (goal, 500),
        Goal::And(goals) => {
            let (goals, priors): (Vec<Goal>, Vec<usize>) = goals
                .into_iter()
                .map(|goal| goal_reorder_help(goal))
                .sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
                .unzip();

            (Goal::And(goals), priors.iter().sum())
        }
        Goal::Or(goals) => {
            let (goals, priors): (Vec<Goal>, Vec<usize>) = goals
                .into_iter()
                .map(|goal| goal_reorder_help(goal))
                .sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
                .unzip();

            (Goal::Or(goals), priors.iter().max().unwrap_or(&0) + 1000)
        }
        Goal::PredCall(_, _) => (goal, 10000),
    }
}

pub fn goal_optimize(goal: Goal) -> Goal {
    let goal = goal_flatten(goal);
    let goal = goal_reorder(goal);
    goal
}

fn func_to_predicate(func: &ast::FuncDecl) -> (Predicate, Predicate) {
    let name = func.name;
    let fail_pars: Vec<Ident> = func.pars.iter().map(|(id, _typ)| *id).collect();
    let mut succ_pars = fail_pars.clone();
    let (term, succ_goal) = expr_to_succ_goal(&func.body);
    let fail_goal = expr_to_fail_goal(&func.body);

    if let Term::Var(x) = term {
        succ_pars.push(x);
        let succ_pred = Predicate {
            name: PredIdent::Succ(name),
            pars: succ_pars,
            goal: goal_optimize(succ_goal),
        };
        let fail_pred = Predicate {
            name: PredIdent::Fail(name),
            pars: fail_pars,
            goal: goal_optimize(fail_goal),
        };
        (succ_pred, fail_pred)
    } else {
        let x = Ident::fresh(&"res");
        succ_pars.push(x);
        let succ_goal = Goal::And(vec![Goal::Eq(x, term), succ_goal]);
        let succ_pred = Predicate {
            name: PredIdent::Succ(name),
            pars: succ_pars,
            goal: goal_optimize(succ_goal),
        };
        let fail_pred = Predicate {
            name: PredIdent::Fail(name),
            pars: fail_pars,
            goal: goal_optimize(fail_goal),
        };
        (succ_pred, fail_pred)
    }
}

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

fn expr_to_succ_goal(expr: &Expr) -> (Term<Ident>, Goal) {
    match expr {
        Expr::Lit { lit } => (Term::Lit(*lit), Goal::Const(true)),
        Expr::Var { var } => (Term::Var(*var), Goal::Const(true)),
        Expr::Prim { prim, args } => {
            let x = Ident::fresh(&"res");
            let (mut terms, mut goals): (Vec<Term<Ident>>, Vec<Goal>) =
                args.iter().map(|arg| expr_to_succ_goal(arg)).unzip();
            terms.push(Term::Var(x));
            goals.push(Goal::Prim(*prim, terms));
            (Term::Var(x), Goal::And(goals))
        }
        Expr::Cons { name, flds } => {
            let (terms, goals): (Vec<Term<Ident>>, Vec<Goal>) =
                flds.iter().map(|fld| expr_to_succ_goal(fld)).unzip();
            (Term::Cons(*name, terms), Goal::And(goals))
        }
        Expr::Match { expr, brchs } => {
            let x = Ident::fresh(&"res");
            let (term, goal) = expr_to_succ_goal(expr);
            let goals = brchs
                .iter()
                .map(|(patn, expr)| {
                    let goal1 = unify_decompose(
                        Term::Cons(
                            patn.name,
                            patn.flds.iter().map(|fld| Term::Var(*fld)).collect(),
                        ),
                        term.clone(),
                    );
                    let (term2, goal2) = expr_to_succ_goal(expr);
                    let goal3 = Goal::Eq(x, term2);
                    Goal::And(vec![goal1, goal2, goal3])
                })
                .collect();
            (Term::Var(x), Goal::And(vec![goal, Goal::Or(goals)]))
        }
        Expr::Let { bind, expr, cont } => {
            let (term1, goal1) = expr_to_succ_goal(expr);
            let (term2, goal2) = expr_to_succ_goal(cont);
            let goal = Goal::And(vec![goal1, Goal::Eq(*bind, term1), goal2]);
            (term2, goal)
        }
        Expr::App { func, args } => {
            let x = Ident::fresh(&"res");
            let (mut terms, mut goals): (Vec<Term<Ident>>, Vec<Goal>) =
                args.iter().map(|arg| expr_to_succ_goal(arg)).unzip();
            terms.push(Term::Var(x));
            goals.push(Goal::PredCall(PredIdent::Succ(*func), terms));
            (Term::Var(x), Goal::And(goals))
        }
        Expr::Ifte { cond, then, els } => {
            let x = Ident::fresh(&"res");
            let (term0, goal0) = expr_to_succ_goal(cond);
            let (term1, goal1) = expr_to_succ_goal(then);
            let (term2, goal2) = expr_to_succ_goal(els);
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
        Expr::Assert { expr, cont } => {
            let (term1, goal1) = expr_to_succ_goal(expr);
            let (term2, goal2) = expr_to_succ_goal(cont);
            let goal = Goal::And(vec![
                goal1,
                unify_decompose(term1, Term::Lit(LitVal::Bool(true))),
                goal2,
            ]);
            (term2, goal)
        }
    }
}

fn expr_to_fail_goal(expr: &Expr) -> Goal {
    match expr {
        Expr::Lit { lit: _ } => Goal::Const(false),
        Expr::Var { var: _ } => Goal::Const(false),
        Expr::Prim { prim: _, args } => {
            let goals = args.iter().map(|expr| expr_to_fail_goal(expr)).collect();
            Goal::Or(goals)
        }
        Expr::Cons { name: _, flds } => {
            let goals = flds.iter().map(|expr| expr_to_fail_goal(expr)).collect();
            Goal::Or(goals)
        }
        Expr::Match { expr, brchs } => {
            let fail_goal = expr_to_fail_goal(expr);
            let (term, succ_goal) = expr_to_succ_goal(expr);
            let goals = brchs
                .iter()
                .map(|(patn, expr)| {
                    let goal1 = unify_decompose(
                        Term::Cons(
                            patn.name,
                            patn.flds.iter().map(|fld| Term::Var(*fld)).collect(),
                        ),
                        term.clone(),
                    );
                    let goal2 = expr_to_fail_goal(expr);
                    Goal::And(vec![goal1, goal2])
                })
                .collect();
            Goal::Or(vec![fail_goal, Goal::And(vec![succ_goal, Goal::Or(goals)])])
        }
        Expr::Let { bind, expr, cont } => {
            let fail_goal1 = expr_to_fail_goal(expr);
            let (term1, succ_goal1) = expr_to_succ_goal(expr);
            let fail_goal2 = expr_to_fail_goal(cont);
            Goal::Or(vec![
                fail_goal1,
                Goal::And(vec![succ_goal1, Goal::Eq(*bind, term1), fail_goal2]),
            ])
        }
        Expr::App { func, args } => {
            let fail_goals = args.iter().map(|arg| expr_to_fail_goal(arg)).collect();

            let (terms, goals): (Vec<Term<Ident>>, Vec<Goal>) =
                args.iter().map(|arg| expr_to_succ_goal(arg)).unzip();

            Goal::Or(vec![
                Goal::Or(fail_goals),
                Goal::And(vec![
                    Goal::And(goals),
                    Goal::PredCall(PredIdent::Fail(*func), terms),
                ]),
            ])
        }
        Expr::Ifte { cond, then, els } => {
            let fail_goal0 = expr_to_fail_goal(cond);
            let (term0, succ_goal0) = expr_to_succ_goal(cond);
            let fail_goal1 = expr_to_fail_goal(then);
            let fail_goal2 = expr_to_fail_goal(els);
            Goal::Or(vec![
                fail_goal0,
                Goal::And(vec![
                    succ_goal0.clone(),
                    unify_decompose(term0.clone(), Term::Lit(LitVal::Bool(true))),
                    fail_goal1,
                ]),
                Goal::And(vec![
                    succ_goal0,
                    unify_decompose(term0.clone(), Term::Lit(LitVal::Bool(false))),
                    fail_goal2,
                ]),
            ])
        }
        Expr::Assert { expr, cont } => {
            let fail_goal1 = expr_to_fail_goal(expr);
            let (term1, succ_goal1) = expr_to_succ_goal(expr);
            let fail_goal2 = expr_to_fail_goal(cont);

            Goal::Or(vec![
                fail_goal1,
                Goal::And(vec![
                    succ_goal1.clone(),
                    unify_decompose(term1.clone(), Term::Lit(LitVal::Bool(false))),
                ]),
                Goal::And(vec![
                    succ_goal1,
                    unify_decompose(term1, Term::Lit(LitVal::Bool(true))),
                    fail_goal2,
                ]),
            ])
        }
    }
}

fn compile_pred(pred: &ast::PredDecl) -> Predicate {
    let pars: Vec<Ident> = pred.pars.iter().map(|(id, _typ)| *id).collect();
    Predicate {
        name: PredIdent::Check(pred.name),
        pars,
        goal: goal_optimize(compile_goal(&pred.body)),
    }
}

fn compile_goal(goal: &ast::Goal) -> Goal {
    match goal {
        ast::Goal::Eq(lhs, rhs) => {
            let (term1, goal1) = expr_to_succ_goal(lhs);
            let (term2, goal2) = expr_to_succ_goal(rhs);
            Goal::And(vec![goal1, goal2, unify_decompose(term1, term2)])
        }
        ast::Goal::Fail(expr) => expr_to_fail_goal(expr),
        ast::Goal::And(goals) => {
            let goals = goals.iter().map(|goal| compile_goal(goal)).collect();
            Goal::And(goals)
        }
        ast::Goal::Or(goals) => {
            let goals = goals.iter().map(|goal| compile_goal(goal)).collect();
            Goal::Or(goals)
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PredIdent {
    Succ(Ident),
    Fail(Ident),
    Check(Ident),
}

impl PredIdent {
    pub fn is_succ(&self) -> bool {
        matches!(self, PredIdent::Succ(_))
    }

    pub fn is_fail(&self) -> bool {
        matches!(self, PredIdent::Fail(_))
    }

    pub fn is_check(&self) -> bool {
        matches!(self, PredIdent::Check(_))
    }
}

impl std::fmt::Display for PredIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredIdent::Succ(ident) => write!(f, "(succ){}", ident),
            PredIdent::Fail(ident) => write!(f, "(fail){}", ident),
            PredIdent::Check(ident) => write!(f, "(check){}", ident),
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

#[derive(Clone, Debug, PartialEq)]
pub struct DnfPredicate {
    pub name: PredIdent,
    pub pars: Vec<Ident>,
    pub goals: Vec<DnfGoal>,
}

impl DnfPredicate {
    pub fn free_vars(&self, vars: &mut Vec<Ident>) {
        assert!(vars.is_empty());
        for par in &self.pars {
            vars.push(*par);
        }
        for goal in &self.goals {
            goal.free_vars(vars);
        }
    }
}

pub fn dnf_pred_dict(dict: &HashMap<PredIdent, Predicate>) -> HashMap<PredIdent, DnfPredicate> {
    let mut res: HashMap<_, DnfPredicate> = HashMap::new();
    for (name, pred) in dict {
        let pred = dnf_trans_predicate(pred);
        res.insert(*name, pred);
    }
    res
}

fn dnf_trans_predicate(pred: &Predicate) -> DnfPredicate {
    let mut goals = Vec::new();
    goals.push(DnfGoal::new());
    dnf_trans_goal(&mut goals, &pred.goal);
    DnfPredicate {
        name: pred.name,
        pars: pred.pars.clone(),
        goals,
    }
}

fn dnf_trans_goal(vec: &mut Vec<DnfGoal>, goal: &Goal) {
    match goal {
        Goal::Const(true) => {}
        Goal::Const(false) => {
            vec.clear();
        }
        Goal::Eq(term1, term2) => {
            for goal in vec.iter_mut() {
                goal.eqs.push((Term::Var(*term1), term2.clone()));
            }
        }
        Goal::Prim(prim, args) => {
            for goal in vec.iter_mut() {
                goal.prims.push((*prim, args.clone()));
            }
        }
        Goal::And(goals) => {
            for goal in goals {
                dnf_trans_goal(vec, goal);
            }
        }
        Goal::Or(goals) => {
            let mut save = Vec::new();
            std::mem::swap(vec, &mut save);
            for goal in goals {
                let mut new_vec = save.clone();
                dnf_trans_goal(&mut new_vec, goal);
                vec.append(&mut new_vec);
            }
        }
        Goal::PredCall(name, args) => {
            for goal in vec.iter_mut() {
                goal.preds.push((*name, args.clone()));
            }
        }
    }
}

pub fn prog_to_dict(prog: &ast::Program) -> HashMap<PredIdent, Predicate> {
    let mut dict = HashMap::new();

    for func in &prog.funcs {
        let (succ_pred, fail_pred) = func_to_predicate(func);
        dict.insert(PredIdent::Succ(func.name), succ_pred);
        dict.insert(PredIdent::Fail(func.name), fail_pred);
    }
    for pred in &prog.preds {
        let check_pred = compile_pred(pred);
        dict.insert(PredIdent::Check(pred.name), check_pred);
    }
    dict
}

#[test]
fn prog_to_pred_test() {
    let p1: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => 
        assert head;
        assert tail;
        Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end
"#;
    let prog = crate::syntax::parser::parser::ProgramParser::new()
        .parse(p1)
        .unwrap();

    let dict = prog_to_dict(&prog);
    println!("{:#?}", dict);
    let dict = dnf_pred_dict(&dict);
    println!("{:#?}", dict);
}
