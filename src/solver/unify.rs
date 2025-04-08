use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct UnifyArena {
    pub arena: Vec<Option<Term<usize>>>,
}

impl UnifyArena {
    pub fn new(len: usize) -> UnifyArena {
        let mut arena = Vec::new();
        for _ in 0..len {
            arena.push(None);
        }
        UnifyArena { arena }
    }

    pub fn len(&self) -> usize {
        self.arena.len()
    }

    pub fn alloc(&mut self, len: usize) -> usize {
        let offset = self.arena.len();
        for _ in 0..len {
            self.arena.push(None);
        }
        offset
    }

    pub fn merge(&self, term: &Term<usize>) -> Term<usize> {
        match term {
            Term::Var(hole) => {
                if let Some(term2) = &self.arena[*hole] {
                    self.merge(term2)
                } else {
                    Term::Var(*hole)
                }
            }
            Term::Lit(lit) => Term::Lit(*lit),
            Term::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| self.merge(fld)).collect();
                Term::Cons(*cons, flds)
            }
        }
    }

    pub fn merge_update_all(&mut self) {
        let new_arena = self
            .arena
            .iter()
            .map(|term| {
                if let Some(term) = term {
                    Some(self.merge(&term))
                } else {
                    None
                }
            })
            .collect();
        self.arena = new_arena;
    }

    // pub fn merge_name(&self, term: &Term<usize>, vars: &Vec<Ident>) -> Term {
    //     match term {
    //         Term::Hole(hole) => {
    //             if let Some(term2) = &self.arena[*hole] {
    //                 self.merge_name(term2, vars)
    //             } else {
    //                 if let Some(var) = vars.get(*hole) {
    //                     Term::Var(*var)
    //                 } else {
    //                     Term::Var(Ident::hole(*hole))
    //                 }
    //             }
    //         }
    //         Term::Lit(lit) => Term::Lit(*lit),
    //         Term::Cons(cons, flds) => {
    //             let flds = flds.iter().map(|fld| self.merge_name(fld, vars)).collect();
    //             Term::Cons(*cons, flds)
    //         }
    //     }
    // }

    // pub fn merge_name_all(&self, vars: &Vec<Ident>) -> Vec<(Ident, Term)> {
    //     let mut vec = Vec::new();
    //     for (hole, var) in vars.iter().enumerate() {
    //         let term = self.merge_name(&Term::Hole(hole), vars);
    //         vec.push((*var, term));
    //     }
    //     vec
    // }

    pub fn assign(&mut self, hole: usize, term: &Term<usize>) -> Result<(), ()> {
        if term.occur_check(&hole) {
            return Err(());
        }
        if let Some(t) = &self.arena[hole] {
            // actually there is no need for cloning
            // todo: hack the lifetime
            self.unify(term, &t.clone())?;
        } else {
            self.arena[hole] = Some(term.clone());
        }
        Ok(())
    }

    pub fn unify(&mut self, t1: &Term<usize>, t2: &Term<usize>) -> Result<(), ()> {
        match (t1, t2) {
            (Term::Var(hole1), Term::Var(hole2)) if hole1 == hole2 => Ok(()),
            (Term::Var(hole), term) | (term, Term::Var(hole)) => self.assign(*hole, term),
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
}

impl Display for UnifyArena {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (hole, term) in self.arena.iter().enumerate() {
            if let Some(term) = term {
                writeln!(f, "{hole}: {term}")?;
            } else {
                writeln!(f, "{hole}: ?{hole}")?;
            }
        }
        Ok(())
    }
}
