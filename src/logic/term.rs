use super::*;
use std::hash::Hash;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Term<V> {
    Var(V),
    Lit(LitVal),
    Cons(Ident, Vec<Term<V>>),
}

impl<V: Display> Display for Term<V> {
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

impl<V> Term<V> {
    pub fn var_rename<F: Fn(&V) -> V>(&self, f: &F) -> Self {
        match self {
            Term::Var(var) => Term::Var(f(var)),
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(cons, terms) => {
                let terms = terms.into_iter().map(|term| term.var_rename(f)).collect();
                Term::Cons(*cons, terms)
            }
        }
    }

    pub fn var_rename_inplace<F: Fn(&mut V)>(&mut self, f: &F) {
        match self {
            Term::Var(var) => {
                f(var);
            }
            Term::Lit(_lit) => {}
            Term::Cons(_cons, terms) => {
                terms
                    .into_iter()
                    .for_each(|term| term.var_rename_inplace(f));
            }
        }
    }
}

impl Term<usize> {
    pub fn var_offset(&self, offset: usize) -> Self {
        match self {
            Term::Var(var) => Term::Var(*var + offset),
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(cons, terms) => {
                let terms = terms
                    .into_iter()
                    .map(|term| term.var_offset(offset))
                    .collect();
                Term::Cons(*cons, terms)
            }
        }
    }
}

impl<V1: Eq + Hash> Term<V1> {
    pub fn var_map<V2: Clone>(&self, map: &HashMap<V1, V2>) -> Term<V2> {
        match self {
            Term::Var(var) => Term::Var(map[var].clone()),
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(cons, terms) => {
                let terms = terms.into_iter().map(|term| term.var_map(map)).collect();
                Term::Cons(*cons, terms)
            }
        }
    }
}

impl<V: Eq> Term<V> {
    pub fn occur_check(&self, x: &V) -> bool {
        match self {
            Term::Var(hole) => *x == *hole,
            Term::Lit(_lit) => false,
            Term::Cons(_cons, flds) => flds.iter().any(|fld| fld.occur_check(x)),
        }
    }
}

impl<V: Eq + Copy> Term<V> {
    pub fn free_vars(&self, vars: &mut Vec<V>) {
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

// impl<V: Eq + Clone + std::hash::Hash> Term<V> {
//     pub fn subst(self, env: &HashMap<V, Term<V>>) -> Self {
//         match self {
//             Term::Var(var) => env.get(&var).cloned().unwrap_or(self),
//             Term::Lit(_) => self,
//             Term::Cons(cons, terms) => {
//                 let terms = terms.into_iter().map(|term| term.subst(env)).collect();
//                 Term::Cons(cons, terms)
//             }
//         }
//     }
// }
