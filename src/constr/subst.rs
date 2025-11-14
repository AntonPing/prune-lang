use crate::utils::env_map::EnvMap;

use super::*;

#[derive(Debug)]
pub struct Subst {
    map: EnvMap<IdentCtx, TermCtx>,
}

impl std::fmt::Display for Subst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (x, term) in self.map.iter() {
            writeln!(f, "{x} = {term}")?;
        }
        Ok(())
    }
}

impl Default for Subst {
    fn default() -> Self {
        Self::new()
    }
}

impl Subst {
    pub fn new() -> Subst {
        Subst { map: EnvMap::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_scope_empty()
    }

    pub fn reset(&mut self) {
        self.map.clear();
    }

    pub fn savepoint(&mut self) {
        self.map.enter_scope();
    }

    pub fn backtrack(&mut self) {
        self.map.leave_scope();
    }
}

impl Subst {
    pub fn deref<'a>(&'a self, term: &'a TermCtx) -> &'a TermCtx {
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

    pub fn merge(&self, term: &TermCtx) -> TermCtx {
        match term {
            Term::Var(var) => {
                if let Some(term) = self.map.get(var) {
                    self.merge(term)
                } else {
                    Term::Var(*var)
                }
            }
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| self.merge(fld)).collect();
                Term::Cons(*cons, flds)
            }
        }
    }

    fn occur_check(&self, x: &IdentCtx, term: &TermCtx) -> bool {
        let term = self.deref(term);
        match term {
            Term::Var(y) => x == y,
            Term::Lit(_) => false,
            Term::Cons(_cons, flds) => flds.iter().any(|fld| self.occur_check(x, fld)),
        }
    }

    pub fn bind(&mut self, x: IdentCtx, term: TermCtx) -> Option<Vec<(IdentCtx, AtomCtx)>> {
        let mut subst = Vec::new();
        self.unify_help(&mut subst, Term::Var(x), term)?;
        Some(subst)
    }

    pub fn unify(&mut self, lhs: TermCtx, rhs: TermCtx) -> Option<Vec<(IdentCtx, AtomCtx)>> {
        let mut subst = Vec::new();
        self.unify_help(&mut subst, lhs, rhs)?;
        Some(subst)
    }

    fn unify_help(
        &mut self,
        subst: &mut Vec<(IdentCtx, AtomCtx)>,
        lhs: TermCtx,
        rhs: TermCtx,
    ) -> Option<()> {
        let lhs = self.deref(&lhs).clone();
        let rhs = self.deref(&rhs).clone();
        match (lhs, rhs) {
            (Term::Var(x1), Term::Var(x2)) if x1 == x2 => Some(()),
            (Term::Var(x), term) | (term, Term::Var(x)) => {
                if self.occur_check(&x, &term) {
                    None
                } else {
                    match term {
                        Term::Var(var) => {
                            subst.push((x, Term::Var(var)));
                        }
                        Term::Lit(lit) => {
                            subst.push((x, Term::Lit(lit)));
                        }
                        Term::Cons(_, _) => {}
                    }
                    self.map.insert(x, term.clone());
                    Some(())
                }
            }
            (Term::Lit(lit1), Term::Lit(lit2)) => {
                if lit1 == lit2 {
                    Some(())
                } else {
                    None
                }
            }
            (Term::Cons(cons1, flds1), Term::Cons(cons2, flds2)) => {
                if cons1 == cons2 {
                    assert_eq!(flds1.len(), flds2.len());
                    for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                        self.unify_help(subst, fld1, fld2)?;
                    }
                    Some(())
                } else {
                    None
                }
            }
            (_, _) => panic!("unify simple and complex type!"),
        }
    }
}
