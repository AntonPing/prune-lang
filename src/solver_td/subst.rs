use super::*;

#[derive(Clone, Debug)]
pub struct Subst {
    map: Vec<(IdentCtx, TermCtx)>,
    save: Vec<usize>,
    pub bridge: Vec<(IdentCtx, TermCtx)>,
}

impl std::fmt::Display for Subst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (x, term) in self.map.iter() {
            writeln!(f, "{x} = {term}")?;
        }
        let save = self.save.iter().format(&", ");
        writeln!(f, "subst save:[{save:?}]")?;
        Ok(())
    }
}

impl Subst {
    pub fn new() -> Subst {
        Subst {
            map: Vec::new(),
            save: Vec::new(),
            bridge: Vec::new(),
        }
    }

    pub fn walk(&self, var: &IdentCtx) -> TermCtx {
        for (k, v) in self.map.iter().rev() {
            if *k == *var {
                if let TermCtx::Var(var2) = v {
                    return self.walk(var2);
                } else {
                    return v.clone();
                }
            }
        }
        TermCtx::Var(*var)
    }

    pub fn bind(&mut self, x: IdentCtx, term: TermCtx) {
        match term {
            Term::Var(_) | Term::Lit(_) => {
                self.bridge.push((x, term.clone()));
            }
            Term::Cons(_, _) => {}
        }
        self.map.push((x, term));
    }

    pub fn unify(&mut self, lhs: TermCtx, rhs: TermCtx) -> Result<(), ()> {
        let lhs = if let TermCtx::Var(var) = lhs {
            self.walk(&var)
        } else {
            lhs
        };
        let rhs = if let TermCtx::Var(var) = rhs {
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
                if cons1 == cons2 && flds1.len() == flds2.len() {
                    for (fld1, fld2) in flds1.into_iter().zip(flds2.into_iter()) {
                        self.unify(fld1, fld2)?;
                    }
                    Ok(())
                } else {
                    Err(())
                }
            }
            (_, _) => Err(()),
        }
    }

    pub fn savepoint(&mut self) {
        self.save.push(self.map.len())
    }

    pub fn backtrack(&mut self) {
        let subst_len = self.save.pop().unwrap();
        for _ in 0..(self.map.len() - subst_len) {
            self.map.pop().unwrap();
        }
    }
}
