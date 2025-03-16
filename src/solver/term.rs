use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(Ident),
    Lit(LitVal),
    Cons(Ident, Vec<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Var(var) => Display::fmt(&var, f),
            Term::Lit(lit) => Display::fmt(&lit, f),
            Term::Cons(cons, flds) => {
                if flds.is_empty() {
                    Display::fmt(&cons, f)
                } else {
                    let flds = flds.iter().format(&", ");
                    write!(f, "{cons}({flds})")
                }
            }
        }
    }
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

impl Display for UnifyTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnifyTerm::Hole(hole) => write!(f, "?{hole}"),
            UnifyTerm::Lit(lit) => Display::fmt(&lit, f),
            UnifyTerm::Cons(cons, flds) => {
                if flds.is_empty() {
                    Display::fmt(&cons, f)
                } else {
                    let flds = flds.iter().format(&", ");
                    write!(f, "{cons}({flds})")
                }
            }
        }
    }
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
