use super::constr::Constr;
use super::unify::UnifyArena;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Solution {
    pub vars: UnifyArena,
    pub cons: Constr,
}

impl Display for Solution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.vars)?;
        writeln!(f, "{}", self.cons)?;
        Ok(())
    }
}

impl Solution {
    pub fn from_base(
        len: usize,
        eqs: &Vec<(UnifyTerm, UnifyTerm)>,
        prims: Vec<(Prim, Vec<UnifyTerm>)>,
    ) -> Result<Solution, ()> {
        let mut sol = Solution {
            vars: UnifyArena::new(len),
            cons: Constr { prims },
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
        self.cons.prims.append(&mut other.cons.prims.clone());
    }

    pub fn concat_join(&mut self, other: &Solution, args: &Vec<UnifyTerm>) -> Result<(), ()> {
        let len = self.vars.len();
        for term in other.vars.arena.iter() {
            let term = term.as_ref().map(|term| term.rename_shift(len));
            self.vars.arena.push(term);
        }
        for (prim, args) in other.cons.prims.iter().cloned() {
            let args = args.into_iter().map(|arg| arg.rename_shift(len)).collect();
            self.cons.prims.push((prim, args));
        }

        for (hole, arg) in (len..(len + args.len())).zip(args.iter()) {
            if self.vars.assign(hole, arg).is_err() {
                return Err(());
            }
        }

        Ok(())
    }

    pub fn merge_print(&self, name: Ident, pars: &Vec<Ident>) {
        println!("{}({}):", name, pars.iter().format(&", "));
        for (hole, (name, term)) in pars.iter().zip(self.vars.arena.iter()).enumerate() {
            if let Some(term) = term {
                let term = self.vars.merge(term);
                println!("  {name}: {term}");
            } else {
                println!("  {name}: ?{hole}");
            }
        }
        for (prim, args) in self.cons.prims.iter() {
            let args = args.iter().map(|arg| self.vars.merge(arg)).format(&", ");
            println!("  {prim:?}({args}):");
        }
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
