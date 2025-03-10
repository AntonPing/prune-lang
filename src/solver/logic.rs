use crate::syntax::ast::*;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Formula {
    Const(bool),
    Eq(Term, Term),
    And(Vec<Formula>),
    Or(Vec<Formula>),
    Prim(Prim, Vec<Term>),
    Pred(Ident, Vec<Term>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    name: Ident,
    pars: Vec<Ident>,
    body: Formula,
}

fn formula_flatten(form: Formula) -> Formula {
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

pub fn prog_to_pred_dict(prog: &Program) -> HashMap<Ident, Predicate> {
    let mut preds = HashMap::new();
    for func in &prog.funcs {
        let pred = func_to_predicate(func);
        preds.insert(func.name, pred);
    }
    preds
}

fn func_to_predicate(func: &FuncDecl) -> Predicate {
    let name = func.name;
    let mut pars: Vec<Ident> = func.pars.iter().map(|(id, _typ)| *id).collect();
    let (term, form) = expr_to_formula(&func.body);

    if let Term::Var(x) = term {
        pars.push(x);
        let body = formula_flatten(form);
        Predicate { name, pars, body }
    } else {
        let x = Ident::fresh(&"x");
        pars.push(x);
        let body = formula_flatten(Formula::And(vec![Formula::Eq(Term::Var(x), term), form]));
        Predicate { name, pars, body }
    }
}

fn expr_to_formula(expr: &Expr) -> (Term, Formula) {
    match expr {
        Expr::Lit { lit } => (Term::Lit(*lit), Formula::Const(true)),
        Expr::Var { var } => (Term::Var(*var), Formula::Const(true)),
        Expr::Prim { prim, args } => {
            let x = Ident::fresh(&"x");
            let (mut terms, mut forms): (Vec<Term>, Vec<Formula>) =
                args.iter().map(|expr| expr_to_formula(expr)).unzip();
            terms.push(Term::Var(x));
            forms.push(Formula::Prim(*prim, terms));
            (Term::Var(x), Formula::And(forms))
        }
        Expr::Cons { name, flds } => {
            let (terms, forms): (Vec<Term>, Vec<Formula>) =
                flds.iter().map(|expr| expr_to_formula(expr)).unzip();
            (Term::Cons(*name, terms), Formula::And(forms))
        }
        Expr::Match { expr, brchs } => {
            let x = Ident::fresh(&"x");
            let (term, form) = expr_to_formula(expr);
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
                    let (term2, form2) = expr_to_formula(expr);
                    let form3 = Formula::Eq(Term::Var(x), term2);
                    Formula::And(vec![form1, form2, form3])
                })
                .collect();
            (Term::Var(x), Formula::And(vec![form, Formula::Or(forms)]))
        }
        Expr::Let { bind, expr, cont } => {
            let (term1, form1) = expr_to_formula(expr);
            let (term2, form2) = expr_to_formula(cont);
            let form = Formula::And(vec![form1, Formula::Eq(Term::Var(*bind), term1), form2]);
            (term2, form)
        }
        Expr::App { func, args } => {
            let x = Ident::fresh(&"x");
            let (mut terms, mut forms): (Vec<Term>, Vec<Formula>) =
                args.iter().map(|expr| expr_to_formula(expr)).unzip();
            terms.push(Term::Var(x));
            forms.push(Formula::Pred(*func, terms));
            (Term::Var(x), Formula::And(forms))
        }
        Expr::Assert { expr, cont } => {
            todo!()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DnfFormula {
    pub eqs: Vec<(Term, Term)>,
    pub prims: Vec<(Prim, Vec<Term>)>,
    pub preds: Vec<(Ident, Vec<Term>)>,
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
    pub name: Ident,
    pub pars: Vec<Ident>,
    pub body: Vec<DnfFormula>,
}

impl DnfPredicate {
    pub fn free_vars(&self, vars: &mut Vec<Ident>) {
        assert!(vars.is_empty());
        for par in &self.pars {
            vars.push(*par);
        }

        for form in &self.body {
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
    let mut body = Vec::new();
    body.push(DnfFormula::new());
    dnf_trans_formula(&mut body, &pred.body);
    DnfPredicate {
        name: pred.name,
        pars: pred.pars.clone(),
        body,
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
        Formula::Pred(name, args) => {
            body.last_mut().unwrap().preds.push((*name, args.clone()));
        }
    }
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
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end
"#;
    let prog = crate::syntax::parser::parser::ProgramParser::new()
        .parse(p1)
        .unwrap();

    let pred = prog_to_pred_dict(&prog);
    println!("{:#?}", pred);
    let pred2 = dnf_pred_dict(&pred);
    println!("{:#?}", pred2);
}
