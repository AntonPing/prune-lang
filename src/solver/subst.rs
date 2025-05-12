use super::*;

#[derive(Debug)]
pub struct Subst<V> {
    map: Vec<(V, Term<V>)>,
    save: Vec<usize>,
    pub bridge: Vec<(V, Term<V>)>,
}

impl<V: fmt::Display> std::fmt::Display for Subst<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (x, term) in self.map.iter() {
            writeln!(f, "{x} = {term}")?;
        }
        let save = self.save.iter().format(&", ");
        writeln!(f, "subst save:[{save:?}]")?;
        Ok(())
    }
}

impl<V> Subst<V> {
    pub fn new() -> Subst<V> {
        Subst {
            map: Vec::new(),
            save: Vec::new(),
            bridge: Vec::new(),
        }
    }
}

impl<V: Eq + Copy> Subst<V> {
    pub fn walk(&self, var: &V) -> Term<V> {
        for (k, v) in self.map.iter().rev() {
            if *k == *var {
                if let Term::Var(var2) = v {
                    return self.walk(var2);
                } else {
                    return v.clone();
                }
            }
        }
        Term::Var(*var)
    }

    pub fn bind(&mut self, x: V, term: Term<V>) {
        match term {
            Term::Var(_) | Term::Lit(_) => {
                self.bridge.push((x, term.clone()));
            }
            Term::Cons(_, _) => {}
        }
        self.map.push((x, term));
    }

    pub fn unify(&mut self, lhs: Term<V>, rhs: Term<V>) -> Result<(), ()> {
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
        let len = self.save.pop().unwrap();
        for _ in 0..(self.map.len() - len) {
            self.map.pop().unwrap();
        }
    }
}
