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
        eqs: &Vec<(Term<usize>, Term<usize>)>,
        prims: Vec<(Prim, Vec<Term<usize>>)>,
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

    fn concat_join(&self, other: &Solution, args: &Vec<Term<usize>>) -> Result<Solution, ()> {
        let len = self.vars.len();
        let mut vars = self.vars.clone();
        let mut cons = self.cons.clone();

        for term in other.vars.arena.iter() {
            if let Some(term) = term {
                let term = term.var_offset(len);
                vars.arena.push(Some(term));
            } else {
                vars.arena.push(None);
            }
        }

        for (prim, args) in other.cons.prims.iter().cloned() {
            let args = args.into_iter().map(|arg| arg.var_offset(len)).collect();
            cons.prims.push((prim, args));
        }

        for (hole, arg) in (len..(len + args.len())).zip(args.iter()) {
            if vars.assign(hole, arg).is_err() {
                return Err(());
            }
        }

        Ok(Solution { vars, cons })
    }

    pub fn merge_cons(&mut self) {
        for (_prim, args) in self.cons.prims.iter_mut() {
            for arg in args {
                *arg = self.vars.merge(arg);
            }
        }
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
    args: &Vec<Term<usize>>,
) -> Vec<Solution> {
    let mut vec = Vec::new();
    for sol1 in sols1 {
        for sol2 in sols2 {
            if let Ok(res) = sol1.concat_join(sol2, args) {
                vec.push(res);
            }
        }
    }
    vec
}
