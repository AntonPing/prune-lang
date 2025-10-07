use super::*;

use super::smt_z3::*;
use super::subst::*;

#[derive(Debug)]
pub struct Solver {
    ty_map: EnvMap<IdentCtx, TypeId>,
    subst: Subst,
    constr: Constr,
    unify_vec: Vec<(IdentCtx, TermCtx)>,
    solve_vec: Vec<(Prim, Vec<AtomCtx>)>,
    saves: Vec<(usize, usize)>,
}

impl fmt::Display for Solver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let unify_vec = self
            .unify_vec
            .iter()
            .map(|(var, term)| format!("{} = {}", var, term))
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
            ty_map: EnvMap::new(),
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
        self.ty_map.clear();
        self.subst.reset();
        self.constr.reset();
        self.unify_vec.clear();
        self.solve_vec.clear();
        self.saves.clear();
    }

    pub fn savepoint(&mut self) {
        self.ty_map.enter_scope();
        self.subst.savepoint();
        self.constr.savepoint();
        self.saves
            .push((self.unify_vec.len(), self.solve_vec.len()));
    }

    pub fn backtrack(&mut self) {
        assert!(!self.saves.is_empty());
        self.ty_map.leave_scope();
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
    pub fn declare(&mut self, var: &IdentCtx, typ: &TypeId) {
        assert!(!self.ty_map.contains_key(var));
        self.ty_map.insert(*var, typ.clone());
        if let Term::Lit(lit) = typ {
            self.constr.declare_var(var, lit);
        }
    }

    pub fn bind(&mut self, var: IdentCtx, term: TermCtx) -> Result<(), ()> {
        self.unify_vec.push((var.clone(), term.clone()));
        let mut subst = self.subst.bind(var, term)?;
        for (x, term) in subst.drain(..) {
            if self.ty_map[&x].is_lit() {
                self.constr.push_eq(x, term);
                if !self.constr.check() {
                    return Err(());
                }
            }
        }
        Ok(())
    }

    pub fn solve(&mut self, prim: Prim, args: Vec<AtomCtx>) -> Result<(), ()> {
        self.solve_vec.push((prim.clone(), args.clone()));
        self.constr.push_cons(prim, args);
        if !self.constr.check() {
            return Err(());
        }
        Ok(())
    }

    pub fn get_value(&mut self, vars: &Vec<IdentCtx>) -> Vec<TermCtx> {
        let terms: Vec<TermCtx> = vars
            .iter()
            .map(|var| self.subst.merge(&Term::Var(*var)))
            .collect();

        let lit_vars = terms
            .iter()
            .map(|term| {
                term.free_vars()
                    .iter()
                    .filter(|var| self.ty_map[var].is_lit())
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect();

        let map = self
            .constr
            .get_value(&lit_vars)
            .into_iter()
            .map(|(k, v)| (k, Term::Lit(v)))
            .collect();

        terms
            .into_iter()
            .map(|term| term.substitute(&map))
            .collect()
    }
}

#[test]
fn test_solver() {
    let x = Ident::dummy(&"x");
    let y = Ident::dummy(&"y");
    let z = Ident::dummy(&"z");
    let cons = Ident::dummy(&"cons");

    let mut sol = Solver::new();

    sol.declare(&x.tag_ctx(0), &TypeId::Lit(LitType::TyInt));
    sol.declare(&y.tag_ctx(0), &TypeId::Lit(LitType::TyInt));

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

    sol.bind(x.tag_ctx(0), Term::Var(y.tag_ctx(0))).unwrap_err();

    sol.backtrack();
    sol.savepoint();

    sol.bind(
        z.tag_ctx(0),
        Term::Cons((), cons, vec![Term::Var(x.tag_ctx(0))]),
    )
    .unwrap();
    sol.bind(
        z.tag_ctx(0),
        Term::Cons((), cons, vec![Term::Var(y.tag_ctx(0))]),
    )
    .unwrap_err();

    sol.backtrack();
}
