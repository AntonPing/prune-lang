use std::collections::HashSet;

use super::term::Term;
use super::trans::{Goal, PredIdent, Predicate};

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InferCell {
    Var(Ident),
    Lit(LitType),
    Cons(Ident),
}

impl InferCell {
    fn get_infer_cell(term: &Term<Ident>) -> InferCell {
        match term {
            Term::Var(var) => InferCell::Var(*var),
            Term::Lit(lit) => InferCell::Lit(lit.get_typ()),
            Term::Cons(cons, _flds) => InferCell::Cons(*cons),
        }
    }
}

struct TypeInfer {
    map: HashMap<Ident, InferCell>,
    set: HashSet<Ident>,
}

impl TypeInfer {
    fn new() -> TypeInfer {
        TypeInfer {
            map: HashMap::new(),
            set: HashSet::new(),
        }
    }

    fn touch(&mut self, term: &Term<Ident>) {
        match term {
            Term::Var(var) => {
                self.set.insert(*var);
            }
            Term::Lit(_lit) => {}
            Term::Cons(_cons, flds) => {
                flds.iter().for_each(|fld| self.touch(fld));
            }
        }
    }

    fn walk(&self, cell: InferCell) -> InferCell {
        self.walk_safe(cell, 0)
    }

    fn walk_safe(&self, cell: InferCell, iter: usize) -> InferCell {
        assert!(iter < 1000);
        if let InferCell::Var(var) = cell {
            if let Some(cell) = self.map.get(&var) {
                self.walk_safe(*cell, iter + 1)
            } else {
                cell
            }
        } else {
            cell
        }
    }

    fn unify(&mut self, lhs: InferCell, rhs: InferCell) {
        let lhs = self.walk(lhs);
        let rhs = self.walk(rhs);
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

    fn infer_goal(&mut self, dict: &HashMap<PredIdent, Predicate>, goal: &Goal) {
        match goal {
            Goal::Const(_) => {}
            Goal::Eq(var, term) => {
                self.set.insert(*var);
                self.touch(term);
                self.unify(InferCell::Var(*var), InferCell::get_infer_cell(term));
            }
            Goal::And(goals) => {
                goals.iter().for_each(|goal| self.infer_goal(dict, goal));
            }
            Goal::Or(goals) => {
                goals.iter().for_each(|goal| self.infer_goal(dict, goal));
            }
            Goal::Prim(prim, args) => {
                let typs = prim.get_typ();
                assert_eq!(args.len(), typs.len());
                args.iter().zip(typs.iter()).for_each(|(arg, typ)| {
                    self.touch(arg);
                    self.unify(InferCell::get_infer_cell(arg), InferCell::Lit(*typ));
                });
            }
            Goal::PredCall(pred, args) => {
                let pars = dict[pred].pars.clone();
                assert_eq!(pars.len(), args.len());
                pars.iter().zip(args.iter()).for_each(|(par, arg)| {
                    self.set.insert(*par);
                    self.touch(arg);
                    self.unify(InferCell::Var(*par), InferCell::get_infer_cell(arg));
                });
            }
        }
    }

    pub fn infer_dict(&mut self, dict: &HashMap<PredIdent, Predicate>) {
        for (_, pred) in dict.iter() {
            self.infer_goal(dict, &pred.goal);
        }
    }
}

pub fn infer_type_map(dict: &HashMap<PredIdent, Predicate>) -> HashMap<Ident, LitType> {
    let mut tyinf = TypeInfer::new();
    tyinf.infer_dict(dict);

    let mut res = HashMap::new();
    for var in tyinf.set.clone().into_iter() {
        let cell = tyinf.walk(InferCell::Var(var));
        match cell {
            InferCell::Var(_) => {
                // println!("guess type: {} : Int!", var);
                res.insert(var, LitType::TyInt);
            }
            InferCell::Lit(lit) => {
                res.insert(var, lit);
            }
            InferCell::Cons(_) => {}
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
