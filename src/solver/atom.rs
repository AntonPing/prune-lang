use std::fmt::Display;

use super::*;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Atom {
    Var(IdentCtx),
    Lit(LitVal),
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Var(var) => Display::fmt(&var, f),
            Atom::Lit(lit) => Display::fmt(&lit, f),
        }
    }
}

impl From<&Term<IdentCtx>> for Option<Atom> {
    fn from(value: &Term<IdentCtx>) -> Self {
        match value {
            Term::Var(var) => Some(Atom::Var(*var)),
            Term::Lit(lit) => Some(Atom::Lit(*lit)),
            Term::Cons(_, _) => None,
        }
    }
}
