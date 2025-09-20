use itertools::Itertools;

use super::ident::{Ident, IdentCtx};
use super::lit::{LitType, LitVal};

use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Term<V, L, C> {
    Var(V),
    Lit(L),
    Cons(C, Ident, Vec<Term<V, L, C>>),
}

impl<V: fmt::Display, L: fmt::Display, C> fmt::Display for Term<V, L, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Var(var) => fmt::Display::fmt(&var, f),
            Term::Lit(lit) => fmt::Display::fmt(&lit, f),
            Term::Cons(_, cons, flds) => {
                if flds.is_empty() {
                    fmt::Display::fmt(&cons, f)
                } else {
                    let flds = flds.iter().format(&", ");
                    write!(f, "{cons}({flds})")
                }
            }
        }
    }
}

pub type TermId = Term<Ident, LitVal, ()>;
pub type TermCtx = Term<IdentCtx, LitVal, ()>;
pub type AtomId = Term<Ident, LitVal, Infallible>;
pub type AtomCtx = Term<IdentCtx, LitVal, Infallible>;

pub type TypeId = Term<Ident, LitType, ()>;

impl<V, L, C> Term<V, L, C> {
    pub fn is_var(&self) -> bool {
        matches!(self, Term::Var(_))
    }

    pub fn is_lit(&self) -> bool {
        matches!(self, Term::Lit(_))
    }

    pub fn is_cons(&self) -> bool {
        matches!(self, Term::Cons(_, _, _))
    }
}

impl<L: Copy, C: Copy> Term<Ident, L, C> {
    pub fn tag_ctx(&self, ctx: usize) -> Term<IdentCtx, L, C> {
        match self {
            Term::Var(var) => Term::Var(var.tag_ctx(ctx)),
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(c, cons, flds) => {
                let flds = flds.into_iter().map(|fld| fld.tag_ctx(ctx)).collect();
                Term::Cons(*c, *cons, flds)
            }
        }
    }
}

impl<V: Copy, L: Copy> Term<V, L, ()> {
    pub fn to_atom(&self) -> Option<Term<V, L, Infallible>> {
        match self {
            Term::Var(var) => Some(Term::Var(*var)),
            Term::Lit(lit) => Some(Term::Lit(*lit)),
            Term::Cons(_c, _cons, _flds) => None,
        }
    }
}

impl<V: Copy, L: Copy> Term<V, L, Infallible> {
    pub fn to_term(&self) -> Term<V, L, ()> {
        match self {
            Term::Var(var) => Term::Var(*var),
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(_c, _cons, _flds) => unreachable!(),
        }
    }
}

impl<V: Copy + Eq, L, C> Term<V, L, C> {
    pub fn occurs(&self, x: &V) -> bool {
        match self {
            Term::Var(y) => x == y,
            Term::Lit(_) => false,
            Term::Cons(_, _cons, flds) => flds.iter().any(|fld| fld.occurs(x)),
        }
    }

    pub fn free_vars(&self) -> Vec<V> {
        let mut vec = Vec::new();
        self.free_vars_help(&mut vec);
        vec
    }

    fn free_vars_help(&self, vec: &mut Vec<V>) {
        match self {
            Term::Var(var) => {
                if !vec.contains(var) {
                    vec.push(*var);
                }
            }
            Term::Lit(_lit) => {}
            Term::Cons(_, _cons, flds) => {
                flds.iter().for_each(|fld| fld.free_vars_help(vec));
            }
        }
    }
}

impl<V: Copy + Eq + std::hash::Hash, L: Copy, C: Copy> Term<V, L, C> {
    pub fn substitute(&self, map: &HashMap<V, Term<V, L, C>>) -> Term<V, L, C> {
        match self {
            Term::Var(var) => {
                if let Some(term) = map.get(var) {
                    term.clone()
                } else {
                    Term::Var(*var)
                }
            }
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(c, cons, flds) => {
                let flds = flds.into_iter().map(|fld| fld.substitute(map)).collect();
                Term::Cons(*c, *cons, flds)
            }
        }
    }
}
