use super::*;

use super::smt_z3::*;
use super::subst::*;

#[derive(Debug)]
pub struct Solver<V> {
    subst: Subst<V>,
    constr: Constr<V>,
    unify_vec: Vec<(Term<V>, Term<V>)>,
    solve_vec: Vec<(Prim, Vec<Term<V>>)>,
    saves: Vec<(usize, usize)>,
}

impl<V: fmt::Display> fmt::Display for Solver<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let unify_vec = self
            .unify_vec
            .iter()
            .map(|(lhs, rhs)| format!("{} = {}", lhs, rhs))
            .format(&", ");
        writeln!(f, "unify: [{}]", unify_vec)?;

        let solve_vec = self
            .solve_vec
            .iter()
            .map(|(prim, args)| format!("{:?}({})", prim, args.iter().format(&", ")))
            .format(&",");
        writeln!(f, "solve: [{}]", solve_vec)?;
        Ok(())
    }
}

impl<V> Solver<V> {
    pub fn new() -> Solver<V> {
        let subst = Subst::new();
        let constr = Constr::new();
        Solver {
            subst,
            constr,
            unify_vec: Vec::new(),
            solve_vec: Vec::new(),
            saves: Vec::new(),
        }
    }
}

impl<V: Eq + Copy + fmt::Display> Solver<V> {
    pub fn unify(&mut self, lhs: Term<V>, rhs: Term<V>) -> Result<(), ()> {
        self.unify_vec.push((lhs.clone(), rhs.clone()));

        if self.subst.unify(lhs, rhs).is_err() {
            return Err(());
        }
        for (x, term) in self.subst.bridge.drain(..) {
            self.constr.push_eq(x, term)
        }
        if !self.constr.solve() {
            return Err(());
        }
        Ok(())
    }

    pub fn solve(&mut self, prim: Prim, args: Vec<Term<V>>) -> Result<(), ()> {
        self.solve_vec.push((prim.clone(), args.clone()));

        self.constr.push_cons(prim, args);
        if !self.constr.solve() {
            return Err(());
        }
        Ok(())
    }

    pub fn savepoint(&mut self) {
        self.subst.savepoint();
        self.constr.savepoint();
        self.saves
            .push((self.unify_vec.len(), self.solve_vec.len()));
    }

    pub fn backtrack(&mut self) {
        assert!(!self.saves.is_empty());
        self.subst.backtrack();
        self.constr.backtrack();

        let (len1, len2) = self.saves.pop().unwrap();
        for _ in 0..(self.unify_vec.len() - len1) {
            self.unify_vec.pop().unwrap();
        }
        for _ in 0..(self.solve_vec.len() - len2) {
            self.solve_vec.pop().unwrap();
        }
    }
}

#[test]

fn test_solver() {
    use crate::utils::ident::Ident;

    let mut sol: Solver<Ident> = Solver::new();

    sol.solve(
        Prim::ICmp(Compare::Lt),
        vec![
            Term::Var(Ident::dummy(&"x")),
            Term::Var(Ident::dummy(&"y")),
            Term::Lit(LitVal::Bool(true)),
        ],
    )
    .unwrap();

    sol.savepoint();

    sol.unify(
        Term::Cons(
            Ident::dummy(&"Cons"),
            vec![Term::Var(Ident::dummy(&"x")), Term::Var(Ident::dummy(&"y"))],
        ),
        Term::Cons(
            Ident::dummy(&"Cons"),
            vec![Term::Var(Ident::dummy(&"y")), Term::Var(Ident::dummy(&"x"))],
        ),
    )
    .unwrap_err();

    sol.backtrack();
    sol.savepoint();

    sol.unify(
        Term::Cons(
            Ident::dummy(&"Cons"),
            vec![Term::Var(Ident::dummy(&"x")), Term::Var(Ident::dummy(&"y"))],
        ),
        Term::Cons(
            Ident::dummy(&"Cons"),
            vec![Term::Lit(LitVal::Int(43)), Term::Lit(LitVal::Int(42))],
        ),
    )
    .unwrap_err();

    sol.backtrack();
}
