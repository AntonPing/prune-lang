use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct UnifyArena {
    pub arena: Vec<Option<UnifyTerm>>,
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

    pub fn merge(&self, term: &UnifyTerm) -> UnifyTerm {
        match term {
            UnifyTerm::Hole(hole) => {
                if let Some(term2) = &self.arena[*hole] {
                    self.merge(term2)
                } else {
                    UnifyTerm::Hole(*hole)
                }
            }
            UnifyTerm::Lit(lit) => UnifyTerm::Lit(*lit),
            UnifyTerm::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| self.merge(fld)).collect();
                UnifyTerm::Cons(*cons, flds)
            }
        }
    }

    pub fn merge_name(&self, term: &UnifyTerm, vars: &Vec<Ident>) -> Term {
        match term {
            UnifyTerm::Hole(hole) => {
                if let Some(term2) = &self.arena[*hole] {
                    self.merge_name(term2, vars)
                } else {
                    if let Some(var) = vars.get(*hole) {
                        Term::Var(*var)
                    } else {
                        Term::Var(Ident::hole(*hole))
                    }
                }
            }
            UnifyTerm::Lit(lit) => Term::Lit(*lit),
            UnifyTerm::Cons(cons, flds) => {
                let flds = flds.iter().map(|fld| self.merge_name(fld, vars)).collect();
                Term::Cons(*cons, flds)
            }
        }
    }

    pub fn merge_name_all(&self, vars: &Vec<Ident>) -> Vec<(Ident, Term)> {
        let mut vec = Vec::new();
        for (hole, var) in vars.iter().enumerate() {
            let term = self.merge_name(&UnifyTerm::Hole(hole), vars);
            vec.push((*var, term));
        }
        vec
    }

    pub fn assign(&mut self, hole: usize, term: &UnifyTerm) -> Result<(), ()> {
        if term.occur_check(hole) {
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

    pub fn unify(&mut self, t1: &UnifyTerm, t2: &UnifyTerm) -> Result<(), ()> {
        match (t1, t2) {
            (UnifyTerm::Hole(hole1), UnifyTerm::Hole(hole2)) if hole1 == hole2 => Ok(()),
            (UnifyTerm::Hole(hole), term) | (term, UnifyTerm::Hole(hole)) => {
                self.assign(*hole, term)
            }
            (UnifyTerm::Lit(lit1), UnifyTerm::Lit(lit2)) => {
                if lit1 == lit2 {
                    Ok(())
                } else {
                    Err(())
                }
            }
            (UnifyTerm::Cons(cons1, flds1), UnifyTerm::Cons(cons2, flds2)) => {
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

#[derive(Clone, Debug, PartialEq)]
pub struct Solution {
    pub vars: UnifyArena,
    pub cons: Vec<(Prim, Vec<UnifyTerm>)>,
}

impl Solution {
    pub fn from_base(
        len: usize,
        eqs: &Vec<(UnifyTerm, UnifyTerm)>,
        prims: Vec<(Prim, Vec<UnifyTerm>)>,
    ) -> Result<Solution, ()> {
        let mut sol = Solution {
            vars: UnifyArena::new(len),
            cons: prims,
        };
        for (lhs, rhs) in eqs {
            sol.vars.unify(lhs, rhs)?;
        }
        Ok(sol)
    }

    pub fn concat(&mut self, other: &Solution) {
        let len = self.vars.len();
        for term in other.vars.arena.iter() {
            let term = term.as_ref().map(|term| term.rename_shift(len));
            self.vars.arena.push(term);
        }
        self.cons.append(&mut other.cons.clone());
    }

    pub fn concat_join(&mut self, other: &Solution, args: &Vec<UnifyTerm>) -> Result<(), ()> {
        let len = self.vars.len();
        for term in other.vars.arena.iter() {
            let term = term.as_ref().map(|term| term.rename_shift(len));
            self.vars.arena.push(term);
        }
        self.cons.append(&mut other.cons.clone());

        for (hole, arg) in (len..(len + args.len())).zip(args.iter()) {
            if self.vars.assign(hole, arg).is_err() {
                return Err(());
            }
        }

        Ok(())
    }
}

pub fn concat_sol_set(
    sols1: &Vec<Solution>,
    sols2: &Vec<Solution>,
    args: &Vec<UnifyTerm>,
) -> Vec<Solution> {
    let mut vec = Vec::new();
    for sol1 in sols1 {
        for sol2 in sols2 {
            let mut res = sol1.clone();
            if res.concat_join(sol2, args).is_ok() {
                vec.push(res);
            }
        }
    }
    vec
}
