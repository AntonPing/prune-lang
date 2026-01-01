use super::term::*;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub enum UnifyError<V, L, C> {
    UnifyFailed(Term<V, L, C>, Term<V, L, C>),
    OccurCheckFailed(V, Term<V, L, C>),
    UnifyVecDiffLen(Vec<Term<V, L, C>>, Vec<Term<V, L, C>>),
}

use crate::cli::diagnostic::Diagnostic;
impl<V: fmt::Display, L: fmt::Display, C: fmt::Display> From<UnifyError<V, L, Option<C>>>
    for Diagnostic
{
    // todo: better error message
    fn from(val: UnifyError<V, L, Option<C>>) -> Self {
        match val {
            UnifyError::UnifyFailed(lhs, rhs) => {
                Diagnostic::error(format!("Can not unify types: {} and {}!", lhs, rhs))
            }
            UnifyError::OccurCheckFailed(x, typ) => {
                Diagnostic::error(format!("Occur check failed at variable: {} in {}!", x, typ))
            }
            UnifyError::UnifyVecDiffLen(vec1, vec2) => {
                let vec1 = vec1.iter().format(", ");
                let vec2 = vec2.iter().format(", ");
                Diagnostic::error(format!(
                    "Unify vectors of different length: [{}] and [{}]!",
                    vec1, vec2
                ))
            }
        }
    }
}

#[derive(Debug)]
pub struct Unifier<V, L, C> {
    map: HashMap<V, Term<V, L, C>>,
}

impl<V: Eq + Hash + Clone, L, C> Unifier<V, L, C> {
    pub fn new() -> Unifier<V, L, C> {
        Unifier {
            map: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn reset(&mut self) {
        self.map.clear();
    }
}

impl<V: Eq + Hash + Clone, L: Eq + Clone, C: Eq + Clone> Unifier<V, L, C> {
    pub fn deref<'a>(&'a self, term: &'a Term<V, L, C>) -> &'a Term<V, L, C> {
        let mut term = term;
        loop {
            if let Term::Var(var) = term {
                if let Some(term2) = self.map.get(var) {
                    term = term2;
                    continue;
                } else {
                    return term;
                }
            } else {
                return term;
            }
        }
    }

    pub fn merge(&self, term: &Term<V, L, C>) -> Term<V, L, C> {
        match term {
            Term::Var(var) => {
                if let Some(term) = self.map.get(var) {
                    self.merge(term)
                } else {
                    Term::Var(var.clone())
                }
            }
            Term::Lit(lit) => Term::Lit(lit.clone()),
            Term::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| self.merge(fld)).collect();
                Term::Cons(cons.clone(), flds)
            }
        }
    }

    fn occur_check(&self, x: &V, term: &Term<V, L, C>) -> bool {
        let term = self.deref(term);
        match term {
            Term::Var(y) => x == y,
            Term::Lit(_) => false,
            Term::Cons(_cons, flds) => flds.iter().any(|fld| self.occur_check(x, fld)),
        }
    }

    pub fn unify(
        &mut self,
        lhs: &Term<V, L, C>,
        rhs: &Term<V, L, C>,
    ) -> Result<(), UnifyError<V, L, C>> {
        let lhs = self.deref(lhs).clone();
        let rhs = self.deref(rhs).clone();
        match (&lhs, &rhs) {
            (Term::Var(x1), Term::Var(x2)) if x1 == x2 => Ok(()),
            (Term::Var(x), term) | (term, Term::Var(x)) => {
                if self.occur_check(x, term) {
                    return Err(UnifyError::OccurCheckFailed(x.clone(), term.clone()));
                }
                self.map.insert(x.clone(), term.clone());
                Ok(())
            }
            (Term::Lit(lit1), Term::Lit(lit2)) => {
                if lit1 == lit2 {
                    Ok(())
                } else {
                    Err(UnifyError::UnifyFailed(lhs.clone(), rhs.clone()))
                }
            }
            (Term::Cons(cons1, flds1), Term::Cons(cons2, flds2)) => {
                if cons1 == cons2 {
                    self.unify_many(flds1, flds2)
                } else {
                    Err(UnifyError::UnifyFailed(lhs.clone(), rhs.clone()))
                }
            }
            (lhs, rhs) => Err(UnifyError::UnifyFailed(lhs.clone(), rhs.clone())),
        }
    }

    pub fn unify_many(
        &mut self,
        lhss: &[Term<V, L, C>],
        rhss: &[Term<V, L, C>],
    ) -> Result<(), UnifyError<V, L, C>> {
        if lhss.len() == rhss.len() {
            for (lhs, rhs) in lhss.iter().zip(rhss.iter()) {
                self.unify(lhs, rhs)?;
            }
            Ok(())
        } else {
            Err(UnifyError::UnifyVecDiffLen(
                lhss.iter().cloned().collect(),
                rhss.iter().cloned().collect(),
            ))
        }
    }
}
