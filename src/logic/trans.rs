use std::collections::HashMap;

use crate::syntax::ast::*;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Formula {
    Const(bool),
    Eq(Ident, Term<Ident>),
    And(Vec<Formula>),
    Or(Vec<Formula>),
    Prim(Prim, Vec<Term<Ident>>),
    PredCall(PredIdent, Vec<Term<Ident>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    pub name: PredIdent,
    pub pars: Vec<Ident>,
    pub form: Formula,
}

pub fn formula_flatten(form: Formula) -> Formula {
    match form {
        Formula::And(forms) => {
            let mut vec = Vec::new();
            for form in forms {
                let form = formula_flatten(form);
                match form {
                    Formula::Const(true) => {}
                    Formula::Const(false) => return Formula::Const(false),
                    Formula::And(mut forms) => vec.append(&mut forms),
                    form => vec.push(form),
                }
            }
            match vec.len() {
                0 => Formula::Const(true),
                1 => vec.into_iter().next().unwrap(),
                _ => Formula::And(vec),
            }
        }
        Formula::Or(forms) => {
            let mut vec = Vec::new();
            for form in forms {
                let form = formula_flatten(form);
                match form {
                    Formula::Const(false) => {}
                    Formula::Const(true) => return Formula::Const(true),
                    Formula::Or(mut forms) => vec.append(&mut forms),
                    form => vec.push(form),
                }
            }
            match vec.len() {
                0 => Formula::Const(false),
                1 => vec.into_iter().next().unwrap(),
                _ => Formula::Or(vec),
            }
        }
        form => form,
    }
}

fn func_to_predicate(func: &FuncDecl) -> (Predicate, Predicate) {
    let name = func.name;
    let fail_pars: Vec<Ident> = func.pars.iter().map(|(id, _typ)| *id).collect();
    let mut succ_pars = fail_pars.clone();
    let (term, succ_form) = expr_to_succ_form(&func.body);
    let fail_form = expr_to_fail_form(&func.body);

    if let Term::Var(x) = term {
        succ_pars.push(x);
        let succ_pred = Predicate {
            name: PredIdent::Succ(name),
            pars: succ_pars,
            form: formula_flatten(succ_form),
        };
        let fail_pred = Predicate {
            name: PredIdent::Fail(name),
            pars: fail_pars,
            form: formula_flatten(fail_form),
        };
        (succ_pred, fail_pred)
    } else {
        let x = Ident::fresh(&"res");
        succ_pars.push(x);
        let succ_form = Formula::And(vec![Formula::Eq(x, term), succ_form]);
        let succ_pred = Predicate {
            name: PredIdent::Succ(name),
            pars: succ_pars,
            form: formula_flatten(succ_form),
        };
        let fail_pred = Predicate {
            name: PredIdent::Fail(name),
            pars: fail_pars,
            form: formula_flatten(fail_form),
        };
        (succ_pred, fail_pred)
    }
}

fn unify_decompose(lhs: Term<Ident>, rhs: Term<Ident>) -> Formula {
    let mut vec: Vec<Formula> = Vec::new();
    unify_decompose_help(&mut vec, lhs, rhs);
    Formula::And(vec)
}

fn unify_decompose_help(vec: &mut Vec<Formula>, lhs: Term<Ident>, rhs: Term<Ident>) {
    match (lhs, rhs) {
        (Term::Var(x1), Term::Var(x2)) if x1 == x2 => {}
        (Term::Var(var), term) | (term, Term::Var(var)) => {
            vec.push(Formula::Eq(var, term));
        }
        (Term::Lit(lit1), Term::Lit(lit2)) => {
            if lit1 != lit2 {
                vec.clear();
            }
        }
        (Term::Cons(cons1, flds1), Term::Cons(cons2, flds2)) => {
            if cons1 == cons2 {
                assert_eq!(flds1.len(), flds2.len());
                for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                    unify_decompose_help(vec, fld1, fld2)
                }
            } else {
                vec.clear();
            }
        }
        (_, _) => {
            panic!("unify simple and complex type!")
        }
    }
}

fn expr_to_succ_form(expr: &Expr) -> (Term<Ident>, Formula) {
    match expr {
        Expr::Lit { lit } => (Term::Lit(*lit), Formula::Const(true)),
        Expr::Var { var } => (Term::Var(*var), Formula::Const(true)),
        Expr::Prim { prim, args } => {
            let x = Ident::fresh(&"res");
            let (mut terms, mut forms): (Vec<Term<Ident>>, Vec<Formula>) =
                args.iter().map(|arg| expr_to_succ_form(arg)).unzip();
            terms.push(Term::Var(x));
            forms.push(Formula::Prim(*prim, terms));
            (Term::Var(x), Formula::And(forms))
        }
        Expr::Cons { name, flds } => {
            let (terms, forms): (Vec<Term<Ident>>, Vec<Formula>) =
                flds.iter().map(|fld| expr_to_succ_form(fld)).unzip();
            (Term::Cons(*name, terms), Formula::And(forms))
        }
        Expr::Match { expr, brchs } => {
            let x = Ident::fresh(&"res");
            let (term, form) = expr_to_succ_form(expr);
            let forms = brchs
                .iter()
                .map(|(patn, expr)| {
                    let form1 = unify_decompose(
                        Term::Cons(
                            patn.name,
                            patn.flds.iter().map(|fld| Term::Var(*fld)).collect(),
                        ),
                        term.clone(),
                    );
                    let (term2, form2) = expr_to_succ_form(expr);
                    let form3 = Formula::Eq(x, term2);
                    Formula::And(vec![form1, form2, form3])
                })
                .collect();
            (Term::Var(x), Formula::And(vec![form, Formula::Or(forms)]))
        }
        Expr::Let { bind, expr, cont } => {
            let (term1, form1) = expr_to_succ_form(expr);
            let (term2, form2) = expr_to_succ_form(cont);
            let form = Formula::And(vec![form1, Formula::Eq(*bind, term1), form2]);
            (term2, form)
        }
        Expr::App { func, args } => {
            let x = Ident::fresh(&"res");
            let (mut terms, mut forms): (Vec<Term<Ident>>, Vec<Formula>) =
                args.iter().map(|arg| expr_to_succ_form(arg)).unzip();
            terms.push(Term::Var(x));
            forms.push(Formula::PredCall(PredIdent::Succ(*func), terms));
            (Term::Var(x), Formula::And(forms))
        }
        Expr::Ifte { cond, then, els } => {
            let x = Ident::fresh(&"res");
            let (term0, form0) = expr_to_succ_form(cond);
            let (term1, form1) = expr_to_succ_form(then);
            let (term2, form2) = expr_to_succ_form(els);
            let form = Formula::And(vec![
                form0,
                Formula::Or(vec![
                    Formula::And(vec![
                        unify_decompose(term0.clone(), Term::Lit(LitVal::Bool(true))),
                        form1,
                        Formula::Eq(x, term1),
                    ]),
                    Formula::And(vec![
                        unify_decompose(term0, Term::Lit(LitVal::Bool(false))),
                        form2,
                        Formula::Eq(x, term2),
                    ]),
                ]),
            ]);
            (Term::Var(x), form)
        }
        Expr::Assert { expr, cont } => {
            let (term1, form1) = expr_to_succ_form(expr);
            let (term2, form2) = expr_to_succ_form(cont);
            let form = Formula::And(vec![
                form1,
                unify_decompose(term1, Term::Lit(LitVal::Bool(true))),
                form2,
            ]);
            (term2, form)
        }
    }
}

fn expr_to_fail_form(expr: &Expr) -> Formula {
    match expr {
        Expr::Lit { lit: _ } => Formula::Const(false),
        Expr::Var { var: _ } => Formula::Const(false),
        Expr::Prim { prim: _, args } => {
            let forms = args.iter().map(|expr| expr_to_fail_form(expr)).collect();
            Formula::Or(forms)
        }
        Expr::Cons { name: _, flds } => {
            let forms = flds.iter().map(|expr| expr_to_fail_form(expr)).collect();
            Formula::Or(forms)
        }
        Expr::Match { expr, brchs } => {
            let fail_form = expr_to_fail_form(expr);
            let (term, succ_form) = expr_to_succ_form(expr);
            let forms = brchs
                .iter()
                .map(|(patn, expr)| {
                    let form1 = unify_decompose(
                        Term::Cons(
                            patn.name,
                            patn.flds.iter().map(|fld| Term::Var(*fld)).collect(),
                        ),
                        term.clone(),
                    );
                    let form2 = expr_to_fail_form(expr);
                    Formula::And(vec![form1, form2])
                })
                .collect();
            Formula::Or(vec![
                fail_form,
                Formula::And(vec![succ_form, Formula::Or(forms)]),
            ])
        }
        Expr::Let { bind, expr, cont } => {
            let fail_form1 = expr_to_fail_form(expr);
            let (term1, succ_form1) = expr_to_succ_form(expr);
            let fail_form2 = expr_to_fail_form(cont);
            Formula::Or(vec![
                fail_form1,
                Formula::And(vec![succ_form1, Formula::Eq(*bind, term1), fail_form2]),
            ])
        }
        Expr::App { func, args } => {
            let fail_forms = args.iter().map(|arg| expr_to_fail_form(arg)).collect();

            let (terms, forms): (Vec<Term<Ident>>, Vec<Formula>) =
                args.iter().map(|arg| expr_to_succ_form(arg)).unzip();

            Formula::Or(vec![
                Formula::Or(fail_forms),
                Formula::And(vec![
                    Formula::And(forms),
                    Formula::PredCall(PredIdent::Fail(*func), terms),
                ]),
            ])
        }
        Expr::Ifte { cond, then, els } => {
            let fail_form0 = expr_to_fail_form(cond);
            let (term0, succ_form0) = expr_to_succ_form(cond);
            let fail_form1 = expr_to_fail_form(then);
            let fail_form2 = expr_to_fail_form(els);
            Formula::Or(vec![
                fail_form0,
                Formula::And(vec![
                    succ_form0.clone(),
                    unify_decompose(term0.clone(), Term::Lit(LitVal::Bool(true))),
                    fail_form1,
                ]),
                Formula::And(vec![
                    succ_form0,
                    unify_decompose(term0.clone(), Term::Lit(LitVal::Bool(false))),
                    fail_form2,
                ]),
            ])
        }
        Expr::Assert { expr, cont } => {
            let fail_form1 = expr_to_fail_form(expr);
            let (term1, succ_form1) = expr_to_succ_form(expr);
            let fail_form2 = expr_to_fail_form(cont);

            Formula::Or(vec![
                fail_form1,
                Formula::And(vec![
                    succ_form1.clone(),
                    unify_decompose(term1.clone(), Term::Lit(LitVal::Bool(false))),
                ]),
                Formula::And(vec![
                    succ_form1,
                    unify_decompose(term1, Term::Lit(LitVal::Bool(true))),
                    fail_form2,
                ]),
            ])
        }
    }
}

fn compile_pred(pred: &PredDecl) -> Predicate {
    let pars: Vec<Ident> = pred.pars.iter().map(|(id, _typ)| *id).collect();
    Predicate {
        name: PredIdent::Check(pred.name),
        pars,
        form: formula_flatten(compile_form(&pred.body)),
    }
}

fn compile_form(form: &Form) -> Formula {
    match form {
        Form::Eq(lhs, rhs) => {
            let (term1, form1) = expr_to_succ_form(lhs);
            let (term2, form2) = expr_to_succ_form(rhs);
            Formula::And(vec![form1, form2, unify_decompose(term1, term2)])
        }
        Form::Fail(expr) => expr_to_fail_form(expr),
        Form::And(forms) => {
            let forms = forms.iter().map(|form| compile_form(form)).collect();
            Formula::And(forms)
        }
        Form::Or(forms) => {
            let forms = forms.iter().map(|form| compile_form(form)).collect();
            Formula::Or(forms)
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
pub struct DnfFormula {
    pub eqs: Vec<(Term<Ident>, Term<Ident>)>,
    pub prims: Vec<(Prim, Vec<Term<Ident>>)>,
    pub preds: Vec<(PredIdent, Vec<Term<Ident>>)>,
}

impl DnfFormula {
    pub fn new() -> DnfFormula {
        DnfFormula {
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
    pub forms: Vec<DnfFormula>,
}

impl DnfPredicate {
    pub fn free_vars(&self, vars: &mut Vec<Ident>) {
        assert!(vars.is_empty());
        for par in &self.pars {
            vars.push(*par);
        }
        for form in &self.forms {
            form.free_vars(vars);
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
    let mut forms = Vec::new();
    forms.push(DnfFormula::new());
    dnf_trans_formula(&mut forms, &pred.form);
    DnfPredicate {
        name: pred.name,
        pars: pred.pars.clone(),
        forms,
    }
}

fn dnf_trans_formula(vec: &mut Vec<DnfFormula>, form: &Formula) {
    match form {
        Formula::Const(true) => {}
        Formula::Const(false) => {
            vec.clear();
        }
        Formula::Eq(term1, term2) => {
            for form in vec.iter_mut() {
                form.eqs.push((Term::Var(*term1), term2.clone()));
            }
        }
        Formula::And(forms) => {
            for form in forms {
                dnf_trans_formula(vec, form);
            }
        }
        Formula::Or(forms) => {
            let mut save = Vec::new();
            std::mem::swap(vec, &mut save);
            for form in forms {
                let mut new_vec = save.clone();
                dnf_trans_formula(&mut new_vec, form);
                vec.append(&mut new_vec);
            }
        }
        Formula::Prim(prim, args) => {
            for form in vec.iter_mut() {
                form.prims.push((*prim, args.clone()));
            }
        }
        Formula::PredCall(name, args) => {
            for form in vec.iter_mut() {
                form.preds.push((*name, args.clone()));
            }
        }
    }
}

pub fn prog_to_dict(prog: &Program) -> HashMap<PredIdent, Predicate> {
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
