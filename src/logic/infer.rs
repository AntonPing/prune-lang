use super::term::Term;
use super::trans::{Formula, PredIdent, Predicate};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum InferCell {
    Var(Ident),
    Lit(LitType),
    Cons(Ident),
}

struct TypeInfer {
    map: HashMap<Ident, InferCell>,
}

impl TypeInfer {
    fn new() -> TypeInfer {
        TypeInfer {
            map: HashMap::new(),
        }
    }

    fn walk(&self, var: Ident) -> InferCell {
        self.walk_safe(var, 0)
    }

    fn walk_safe(&self, var: Ident, iter: usize) -> InferCell {
        assert!(iter < 1000);
        if let Some(cell) = self.map.get(&var) {
            match cell {
                InferCell::Var(var2) => self.walk_safe(*var2, iter + 1),
                InferCell::Lit(lit) => InferCell::Lit(*lit),
                InferCell::Cons(cons) => InferCell::Cons(*cons),
            }
        } else {
            InferCell::Var(var)
        }
    }

    fn bind(&mut self, lhs: Ident, rhs: InferCell) {
        let lhs = self.walk(lhs);
        match (lhs, rhs) {
            (InferCell::Var(var1), InferCell::Var(var2)) if var1 == var2 => {}
            (InferCell::Var(var), cell) | (cell, InferCell::Var(var)) => {
                self.map.insert(var, cell);
            }
            (InferCell::Lit(lit1), InferCell::Lit(lit2)) => {
                assert_eq!(lit1, lit2);
            }
            (InferCell::Cons(_cons1), InferCell::Cons(_cons)) => {}
            (_, _) => {
                panic!("failed to infer type!");
            }
        }
    }

    fn infer_form(&mut self, dict: &HashMap<PredIdent, Predicate>, form: &Formula) {
        match form {
            Formula::Const(_) => {}
            Formula::Eq(var, term) => match term {
                Term::Var(var2) => {
                    self.bind(*var, InferCell::Var(*var2));
                }
                Term::Lit(lit) => {
                    self.bind(*var, InferCell::Lit(lit.get_typ()));
                }
                Term::Cons(cons, _flds) => {
                    self.bind(*var, InferCell::Cons(*cons));
                }
            },
            Formula::And(forms) => {
                forms.iter().for_each(|form| self.infer_form(dict, form));
            }
            Formula::Or(forms) => {
                forms.iter().for_each(|form| self.infer_form(dict, form));
            }
            Formula::Prim(prim, terms) => {
                let typs = prim.get_typ();
                assert_eq!(terms.len(), typs.len());
                terms
                    .iter()
                    .zip(typs.iter())
                    .for_each(|(term, typ)| match term {
                        Term::Var(var) => self.bind(*var, InferCell::Lit(*typ)),
                        Term::Lit(lit) => assert_eq!(lit.get_typ(), *typ),
                        Term::Cons(_cons, _flds) => {
                            panic!("failed to infer type!");
                        }
                    });
            }
            Formula::PredCall(pred, args) => {
                let pars = dict[pred].pars.clone();
                assert_eq!(pars.len(), args.len());
                pars.iter()
                    .zip(args.iter())
                    .for_each(|(par, arg)| match arg {
                        Term::Var(var2) => {
                            self.bind(*par, InferCell::Var(*var2));
                        }
                        Term::Lit(lit) => {
                            self.bind(*par, InferCell::Lit(lit.get_typ()));
                        }
                        Term::Cons(cons, _flds) => {
                            self.bind(*par, InferCell::Cons(*cons));
                        }
                    });
            }
        }
    }

    pub fn infer_dict(&mut self, dict: &HashMap<PredIdent, Predicate>) {
        for (_, pred) in dict.iter() {
            self.infer_form(dict, &pred.form);
        }
    }
}

pub fn infer_type_map(dict: &HashMap<PredIdent, Predicate>) -> HashMap<Ident, LitType> {
    let mut tyinf = TypeInfer::new();
    tyinf.infer_dict(dict);

    let mut res = HashMap::new();
    for var in tyinf.map.keys() {
        let cell = tyinf.walk(*var);
        if let InferCell::Lit(lit) = cell {
            res.insert(*var, lit);
        }
    }

    res
}

#[test]
fn infer_type_map_test() {
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

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if @icmpeq(head, x) then true else is_elem(tail, x) 
    | Nil => false
    end
end

predicate is_elem_after_append(xs: IntList, x: Int)
begin
    is_elem(append(xs, x), x) = false
end
"#;
    let prog = crate::syntax::parser::parser::ProgramParser::new()
        .parse(p1)
        .unwrap();

    let dict = super::trans::prog_to_dict(&prog);
    let ty_map = infer_type_map(&dict);
    assert!(!ty_map.is_empty())
}
