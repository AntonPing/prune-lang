use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(Ident),
    Lit(LitVal),
    Cons(Ident, Vec<Term>),
}

impl Term {
    pub fn subst(self, env: &HashMap<Ident, Term>) -> Self {
        match self {
            Term::Var(var) => env.get(&var).cloned().unwrap_or(self),
            Term::Lit(_) => self,
            Term::Cons(cons, terms) => {
                let terms = terms.into_iter().map(|term| term.subst(env)).collect();
                Term::Cons(cons, terms)
            }
        }
    }

    pub fn instantiate(&self, map: &Vec<Ident>) -> UnifyTerm {
        match self {
            Term::Var(var) => UnifyTerm::Hole(map.iter().position(|x| *x == *var).unwrap()),
            Term::Lit(lit) => UnifyTerm::Lit(*lit),
            Term::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| fld.instantiate(map)).collect();
                UnifyTerm::Cons(*cons, flds)
            }
        }
    }

    pub fn free_vars(&self, vars: &mut Vec<Ident>) {
        match self {
            Term::Var(var) => {
                if !vars.contains(var) {
                    vars.push(*var);
                }
            }
            Term::Lit(_lit) => {}
            Term::Cons(_ident, flds) => {
                flds.iter().for_each(|fld| fld.free_vars(vars));
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnifyTerm {
    Hole(usize),
    Lit(LitVal),
    Cons(Ident, Vec<UnifyTerm>),
}

impl UnifyTerm {
    pub fn rename_map(&self, map: &Vec<usize>) -> UnifyTerm {
        match self {
            UnifyTerm::Hole(hole) => UnifyTerm::Hole(map[*hole]),
            UnifyTerm::Lit(lit) => UnifyTerm::Lit(*lit),
            UnifyTerm::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| fld.rename_map(map)).collect();
                UnifyTerm::Cons(*cons, flds)
            }
        }
    }

    pub fn rename_shift(&self, offset: usize) -> UnifyTerm {
        match self {
            UnifyTerm::Hole(hole) => UnifyTerm::Hole(*hole + offset),
            UnifyTerm::Lit(lit) => UnifyTerm::Lit(*lit),
            UnifyTerm::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| fld.rename_shift(offset)).collect();
                UnifyTerm::Cons(*cons, flds)
            }
        }
    }

    pub fn occur_check(&self, x: usize) -> bool {
        match self {
            UnifyTerm::Hole(hole) => x == *hole,
            UnifyTerm::Lit(_lit) => false,
            UnifyTerm::Cons(_cons, flds) => flds.iter().any(|fld| fld.occur_check(x)),
        }
    }
}
