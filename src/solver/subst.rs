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
    pub fn walk(&self, var: &IdentCtx) -> TermCtx {
        self.walk_safe(var, 0)
    }
    pub fn walk_safe(&self, var: &IdentCtx, iter: usize) -> TermCtx {
        assert!(iter < 1000);
        for (k, v) in self.map.iter() {
            if *k == *var {
                if let Term::Var(var2) = v {
                    return self.walk_safe(var2, iter + 1);
                } else {
                    return v.clone();
                }
            }
        }
        Term::Var(*var)
    }

    pub fn bind(&mut self, x: IdentCtx, term: TermCtx) -> Result<Vec<(IdentCtx, AtomCtx)>, ()> {
        let mut subst = Vec::new();
        self.unify_help(&mut subst, Term::Var(x), term)?;
        Ok(subst)
    }

    pub fn unify(&mut self, lhs: TermCtx, rhs: TermCtx) -> Result<Vec<(IdentCtx, AtomCtx)>, ()> {
        let mut subst = Vec::new();
        self.unify_help(&mut subst, lhs, rhs)?;
        Ok(subst)
    }

    fn unify_help(
        &mut self,
        subst: &mut Vec<(IdentCtx, AtomCtx)>,
        lhs: TermCtx,
        rhs: TermCtx,
    ) -> Result<(), ()> {
        let lhs = if let Term::Var(var) = lhs {
            self.walk(&var)
        } else {
            lhs
        };
        let rhs = if let Term::Var(var) = rhs {
            self.walk(&var)
        } else {
            rhs
        };
        match (lhs, rhs) {
            (Term::Var(x1), Term::Var(x2)) if x1 == x2 => Ok(()),
            (Term::Var(x), term) | (term, Term::Var(x)) => {
                match term {
                    Term::Var(var) => {
                        subst.push((x, Term::Var(var)));
                    }
                    Term::Lit(lit) => {
                        subst.push((x, Term::Lit(lit)));
                    }
                    Term::Cons(_, _, _) => {}
                }
                self.map.insert(x, term);
                Ok(())
            }
            (Term::Lit(lit1), Term::Lit(lit2)) => {
                if lit1 == lit2 {
                    Ok(())
                } else {
                    Err(())
                }
            }
            (Term::Cons(_, cons1, flds1), Term::Cons(_, cons2, flds2)) => {
                if cons1 == cons2 {
                    assert_eq!(flds1.len(), flds2.len());
                    for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                        self.unify_help(subst, fld1, fld2)?;
                    }
                    Ok(())
                } else {
                    Err(())
                }
            }
            (_, _) => panic!("unify simple and complex type!"),
        }
    }
}
