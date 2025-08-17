use super::*;

use super::smt_z3::*;
use super::subst::*;

#[derive(Debug)]
pub struct Solver {
    subst: Subst,
    constr: Constr,
    unify_vec: Vec<(TermCtx, TermCtx)>,
    solve_vec: Vec<(Prim, Vec<AtomCtx>)>,
    saves: Vec<(usize, usize)>,
}

impl fmt::Display for Solver {
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

impl Solver {
    pub fn new() -> Solver {
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

    pub fn is_empty(&self) -> bool {
        self.saves.is_empty() && self.subst.is_empty() && self.constr.is_empty()
    }

    pub fn reset(&mut self) {
        self.subst.reset();
        self.constr.reset();
        self.unify_vec.clear();
        self.solve_vec.clear();
        self.saves.clear();
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

impl Solver {
    pub fn declare(&mut self, var: &IdentCtx) {
        self.constr.declare_var(var);
    }

    pub fn unify(&mut self, lhs: TermCtx, rhs: TermCtx) -> Result<(), ()> {
        self.unify_vec.push((lhs.clone(), rhs.clone()));

        let mut subst = self.subst.unify(lhs, rhs)?;
        for (x, term) in subst.drain(..) {
            self.constr.push_eq(x, term)
        }

        if !self.constr.solve() {
            return Err(());
        }
        Ok(())
    }

    pub fn solve(&mut self, prim: Prim, args: Vec<TermCtx>) -> Result<(), ()> {
        let args: Vec<AtomCtx> = args.iter().map(|arg| arg.to_atom().unwrap()).collect();
        self.solve_vec.push((prim.clone(), args.clone()));
        self.constr.push_cons(prim, args);
        if !self.constr.solve() {
            return Err(());
        }
        Ok(())
    }
}

#[test]
fn test_solver() {
    let x = Ident::dummy(&"x");
    let y = Ident::dummy(&"y");

    let mut sol = Solver::new();

    sol.declare(&x.tag_ctx(0));
    sol.declare(&y.tag_ctx(0));

    sol.solve(
        Prim::ICmp(Compare::Lt),
        vec![
            Term::Var(x.tag_ctx(0)),
            Term::Var(y.tag_ctx(0)),
            Term::Lit(LitVal::Bool(true)),
        ],
    )
    .unwrap();

    sol.savepoint();

    sol.unify(Term::Var(x.tag_ctx(0)), Term::Var(y.tag_ctx(0)))
        .unwrap_err();

    sol.backtrack();
    sol.savepoint();

    sol.unify(
        Term::Cons(
            (),
            Ident::dummy(&"Cons"),
            vec![Term::Var(x.tag_ctx(0)), Term::Var(y.tag_ctx(0))],
        ),
        Term::Cons(
            (),
            Ident::dummy(&"Cons"),
            vec![Term::Lit(LitVal::Int(43)), Term::Lit(LitVal::Int(42))],
        ),
    )
    .unwrap_err();

    sol.backtrack();
}
