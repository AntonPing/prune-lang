use super::*;

pub struct NoSmtSolver;

impl NoSmtSolver {
    pub fn new() -> Self {
        NoSmtSolver
    }
}

impl Default for NoSmtSolver {
    fn default() -> Self {
        Self::new()
    }
}

impl common::PrimSolver for NoSmtSolver {
    fn check_sat(
        &mut self,
        prims: &[(Prim, Vec<AtomVal<IdentCtx>>)],
    ) -> Option<HashMap<IdentCtx, LitVal>> {
        if prims.is_empty() {
            Some(HashMap::new())
        } else {
            None
        }
    }
}
