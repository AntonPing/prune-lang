use crate::utils::env_map::EnvMap;

use super::*;

#[derive(Debug)]
pub struct Subst {
    map: EnvMap<IdentCtx, Term<IdentCtx>>,
    pub bridge: Vec<(IdentCtx, Term<IdentCtx>)>,
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
        Subst {
            map: EnvMap::new(),
            bridge: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        assert!(self.bridge.is_empty());
        self.map.is_scope_empty()
    }

    pub fn reset(&mut self) {
        self.map.clear();
        self.bridge.clear();
    }

    pub fn savepoint(&mut self) {
        assert!(self.bridge.is_empty());
        self.map.enter_scope();
    }

    pub fn backtrack(&mut self) {
        assert!(self.bridge.is_empty());
        self.map.leave_scope();
    }
}

impl Subst {
    pub fn walk(&self, var: &IdentCtx) -> Term<IdentCtx> {
        self.walk_safe(var, 0)
    }
    pub fn walk_safe(&self, var: &IdentCtx, iter: usize) -> Term<IdentCtx> {
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

    pub fn bind(&mut self, x: IdentCtx, term: Term<IdentCtx>) {
        match term {
            Term::Var(_) | Term::Lit(_) => {
                self.bridge.push((x, term.clone()));
            }
            Term::Cons(_, _) => {}
        }
        self.map.insert(x, term);
    }

    pub fn unify(&mut self, lhs: Term<IdentCtx>, rhs: Term<IdentCtx>) -> Result<(), ()> {
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
                self.bind(x, term);
                Ok(())
            }
            (Term::Lit(lit1), Term::Lit(lit2)) => {
                if lit1 == lit2 {
                    Ok(())
                } else {
                    Err(())
                }
            }
            (Term::Cons(cons1, flds1), Term::Cons(cons2, flds2)) => {
                if cons1 == cons2 {
                    assert_eq!(flds1.len(), flds2.len());
                    for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                        self.unify(fld1, fld2)?;
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
