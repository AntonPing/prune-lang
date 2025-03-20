use crate::syntax::ast::*;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Formula {
    Const(bool),
    Eq(Term, Term),
    And(Vec<Formula>),
    Or(Vec<Formula>),
    Prim(Prim, Vec<Term>),
    PredSucc(Ident, Vec<Term>),
    PredFail(Ident, Vec<Term>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    name: Ident,
    pars: Vec<Ident>,
    form: Formula,
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
            name,
            pars: succ_pars,
            form: formula_flatten(succ_form),
        };
        let fail_pred = Predicate {
            name,
            pars: fail_pars,
            form: formula_flatten(fail_form),
        };
        (succ_pred, fail_pred)
    } else {
        let x = Ident::fresh(&"x");
        succ_pars.push(x);
        let succ_form = Formula::And(vec![Formula::Eq(Term::Var(x), term), succ_form]);
        let succ_pred = Predicate {
            name,
            pars: succ_pars,
            form: formula_flatten(succ_form),
        };
        let fail_pred = Predicate {
            name,
            pars: fail_pars,
            form: formula_flatten(fail_form),
        };
        (succ_pred, fail_pred)
    }
}

fn expr_to_succ_form(expr: &Expr) -> (Term, Formula) {
    match expr {
        Expr::Lit { lit } => (Term::Lit(*lit), Formula::Const(true)),
        Expr::Var { var } => (Term::Var(*var), Formula::Const(true)),
        Expr::Prim { prim, args } => {
            let x = Ident::fresh(&"x");
            let (mut terms, mut forms): (Vec<Term>, Vec<Formula>) =
                args.iter().map(|arg| expr_to_succ_form(arg)).unzip();
            terms.push(Term::Var(x));
            forms.push(Formula::Prim(*prim, terms));
            (Term::Var(x), Formula::And(forms))
        }
        Expr::Cons { name, flds } => {
            let (terms, forms): (Vec<Term>, Vec<Formula>) =
                flds.iter().map(|fld| expr_to_succ_form(fld)).unzip();
            (Term::Cons(*name, terms), Formula::And(forms))
        }
        Expr::Match { expr, brchs } => {
            let x = Ident::fresh(&"x");
            let (term, form) = expr_to_succ_form(expr);
            let forms = brchs
                .iter()
                .map(|(patn, expr)| {
                    let form1 = Formula::Eq(
                        Term::Cons(
                            patn.name,
                            patn.flds.iter().map(|fld| Term::Var(*fld)).collect(),
                        ),
                        term.clone(),
                    );
                    let (term2, form2) = expr_to_succ_form(expr);
                    let form3 = Formula::Eq(Term::Var(x), term2);
                    Formula::And(vec![form1, form2, form3])
                })
                .collect();
            (Term::Var(x), Formula::And(vec![form, Formula::Or(forms)]))
        }
        Expr::Let { bind, expr, cont } => {
            let (term1, form1) = expr_to_succ_form(expr);
            let (term2, form2) = expr_to_succ_form(cont);
            let form = Formula::And(vec![form1, Formula::Eq(Term::Var(*bind), term1), form2]);
            (term2, form)
        }
        Expr::App { func, args } => {
            let x = Ident::fresh(&"x");
            let (mut terms, mut forms): (Vec<Term>, Vec<Formula>) =
                args.iter().map(|arg| expr_to_succ_form(arg)).unzip();
            terms.push(Term::Var(x));
            forms.push(Formula::PredSucc(*func, terms));
            (Term::Var(x), Formula::And(forms))
        }
        Expr::Ifte { cond, then, els } => {
            let x = Ident::fresh(&"x");
            let (term0, form0) = expr_to_succ_form(cond);
            let (term1, form1) = expr_to_succ_form(then);
            let (term2, form2) = expr_to_succ_form(els);
            let form = Formula::And(vec![
                form0,
                Formula::Or(vec![
                    Formula::And(vec![
                        Formula::Eq(term0.clone(), Term::Lit(LitVal::Bool(true))),
                        form1,
                        Formula::Eq(Term::Var(x), term1),
                    ]),
                    Formula::And(vec![
                        Formula::Eq(term0, Term::Lit(LitVal::Bool(false))),
                        form2,
                        Formula::Eq(Term::Var(x), term2),
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
                Formula::Eq(term1, Term::Lit(LitVal::Bool(true))),
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
                    let form1 = Formula::Eq(
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
                Formula::And(vec![
                    succ_form1,
                    Formula::Eq(Term::Var(*bind), term1),
                    fail_form2,
                ]),
            ])
        }
        Expr::App { func, args } => {
            let fail_forms = args.iter().map(|arg| expr_to_fail_form(arg)).collect();

            let (terms, forms): (Vec<Term>, Vec<Formula>) =
                args.iter().map(|arg| expr_to_succ_form(arg)).unzip();

            Formula::Or(vec![
                Formula::Or(fail_forms),
                Formula::And(vec![Formula::And(forms), Formula::PredFail(*func, terms)]),
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
                    Formula::Eq(term0.clone(), Term::Lit(LitVal::Bool(true))),
                    fail_form1,
                ]),
                Formula::And(vec![
                    succ_form0,
                    Formula::Eq(term0.clone(), Term::Lit(LitVal::Bool(false))),
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
                    Formula::Eq(term1.clone(), Term::Lit(LitVal::Bool(false))),
                ]),
                Formula::And(vec![
                    succ_form1,
                    Formula::Eq(term1, Term::Lit(LitVal::Bool(true))),
                    fail_form2,
                ]),
            ])
        }
    }
}

fn compile_pred(pred: &PredDecl) -> Predicate {
    let name = pred.name;
    let pars: Vec<Ident> = pred.pars.iter().map(|(id, _typ)| *id).collect();
    Predicate {
        name,
        pars,
        form: compile_form(&pred.body),
    }
}

fn compile_form(form: &Form) -> Formula {
    match form {
        Form::Eq(lhs, rhs) => {
            let (term1, form1) = expr_to_succ_form(lhs);
            let (term2, form2) = expr_to_succ_form(rhs);
            Formula::And(vec![form1, form2, Formula::Eq(term1, term2)])
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

#[derive(Clone, Debug, PartialEq)]
pub struct DnfFormula {
    pub eqs: Vec<(Term, Term)>,
    pub prims: Vec<(Prim, Vec<Term>)>,
    pub succ_preds: Vec<(Ident, Vec<Term>)>,
    pub fail_preds: Vec<(Ident, Vec<Term>)>,
}

impl DnfFormula {
    pub fn new() -> DnfFormula {
        DnfFormula {
            eqs: Vec::new(),
            prims: Vec::new(),
            succ_preds: Vec::new(),
            fail_preds: Vec::new(),
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

        for (_prim, args) in &self.succ_preds {
            args.iter().for_each(|arg| arg.free_vars(vars));
        }

        for (_prim, args) in &self.fail_preds {
            args.iter().for_each(|arg| arg.free_vars(vars));
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DnfPredicate {
    pub name: Ident,
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
pub fn dnf_pred_dict(dict: &HashMap<Ident, Predicate>) -> HashMap<Ident, DnfPredicate> {
    let mut preds = HashMap::new();
    for (name, pred) in dict {
        let pred = dnf_trans_predicate(pred);
        preds.insert(*name, pred);
    }
    preds
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

fn dnf_trans_formula(body: &mut Vec<DnfFormula>, form: &Formula) {
    match form {
        Formula::Const(true) => {}
        Formula::Const(false) => {
            body.pop().unwrap();
        }
        Formula::Eq(term1, term2) => {
            body.last_mut()
                .unwrap()
                .eqs
                .push((term1.clone(), term2.clone()));
        }
        Formula::And(forms) => {
            for form in forms {
                dnf_trans_formula(body, form);
            }
        }
        Formula::Or(forms) => {
            let root_form = body.pop().unwrap();
            for form in forms {
                body.push(root_form.clone());
                dnf_trans_formula(body, form);
            }
        }
        Formula::Prim(prim, args) => {
            body.last_mut().unwrap().prims.push((*prim, args.clone()));
        }
        Formula::PredSucc(name, args) => {
            body.last_mut()
                .unwrap()
                .succ_preds
                .push((*name, args.clone()));
        }
        Formula::PredFail(name, args) => {
            body.last_mut()
                .unwrap()
                .fail_preds
                .push((*name, args.clone()));
        }
    }
}

pub fn prog_to_triple(
    prog: &Program,
) -> (
    HashMap<Ident, Predicate>,
    HashMap<Ident, Predicate>,
    HashMap<Ident, Predicate>,
) {
    let mut succ_preds = HashMap::new();
    let mut fail_preds = HashMap::new();
    let mut check_preds = HashMap::new();

    for func in &prog.funcs {
        let (succ_pred, fail_pred) = func_to_predicate(func);
        succ_preds.insert(func.name, succ_pred);
        fail_preds.insert(func.name, fail_pred);
    }
    for pred in &prog.preds {
        let pred = compile_pred(pred);
        check_preds.insert(pred.name, pred);
    }
    (succ_preds, fail_preds, check_preds)
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

    let (succ_preds, fail_preds, check_preds) = prog_to_triple(&prog);
    println!("{:#?}", succ_preds);
    println!("{:#?}", fail_preds);
    println!("{:#?}", check_preds);
    let succ_preds = dnf_pred_dict(&succ_preds);
    println!("{:#?}", succ_preds);
}
