use itertools::Itertools;

use super::ident::{Ident, IdentCtx};
use super::lit::LitVal;

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

impl<V: Copy, L: Copy, C> Term<V, L, C> {
    pub fn to_atom(&self) -> Option<Term<V, L, Infallible>> {
        match self {
            Term::Var(var) => Some(Term::Var(*var)),
            Term::Lit(lit) => Some(Term::Lit(*lit)),
            Term::Cons(_c, _cons, _flds) => None,
        }
    }
}
