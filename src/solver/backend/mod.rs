use super::*;
use easy_smt::{Context, ContextBuilder, SExpr};

pub mod incr_smt;
pub mod no_smt;
pub mod non_incr_smt;

pub trait SmtSolver {
    fn is_empty(&self) -> bool;
    fn reset(&mut self);
    fn savepoint(&mut self);
    fn backtrack(&mut self);
    fn declare_var(&mut self, var: &IdentCtx, typ: &LitType);
    fn push_cons(&mut self, prim: Prim, args: Vec<AtomCtx>);
    fn push_eq(&mut self, x: IdentCtx, atom: AtomCtx);
    fn check_complete(&mut self) -> bool;
    fn check_sound(&mut self) -> bool;
    fn get_value(&mut self, vars: &Vec<IdentCtx>) -> HashMap<IdentCtx, LitVal>;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SmtBackend {
    Z3,
    CVC5,
}
