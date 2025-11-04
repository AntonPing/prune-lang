use super::*;

pub struct NoSmtSolver {}

impl fmt::Debug for NoSmtSolver {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl NoSmtSolver {
    pub fn new() -> Self {
        NoSmtSolver {}
    }
}

impl SmtSolver for NoSmtSolver {
    fn is_empty(&self) -> bool {
        true
    }

    fn reset(&mut self) {}

    fn savepoint(&mut self) {}

    fn backtrack(&mut self) {}

    fn declare_var(&mut self, _var: &IdentCtx, _typ: &LitType) {}

    fn push_cons(&mut self, _prim: Prim, _args: Vec<AtomCtx>) {}

    fn push_eq(&mut self, _x: IdentCtx, _atom: AtomCtx) {}

    fn check_complete(&mut self) -> bool {
        true
    }

    fn check_sound(&mut self) -> bool {
        true
    }

    fn get_value(&mut self, _vars: &Vec<IdentCtx>) -> HashMap<IdentCtx, LitVal> {
        panic!("use SMT solver api in `NoSmt` mode!");
    }
}
