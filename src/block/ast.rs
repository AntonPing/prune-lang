use super::*;

use std::fmt;

#[derive(Clone, Debug)]
pub struct Block {
    pub blk_pred: Ident,
    pub blk_idx: usize,
    pub min_depth: usize,
    pub eqs: Vec<(Ident, AtomId)>,
    pub cons: Vec<(Ident, OptCons<Ident>, Vec<AtomId>)>,
    pub prims: Vec<(Prim, Vec<AtomId>)>,
    pub brchss: Vec<Vec<usize>>,
    pub calls: Vec<(Ident, Vec<AtomId>)>,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "block {}.{} {{{}}}:",
            self.blk_pred, self.blk_idx, self.min_depth
        )?;

        for (var, atom) in self.eqs.iter() {
            writeln!(f, "    {} = {}; ", var, atom)?;
        }

        for (var, cons, flds) in self.cons.iter() {
            let flds = flds.iter().format(", ");
            writeln!(f, "    {} = {}({})", var, cons, flds)?;
        }

        for (prim, args) in self.prims.iter() {
            let args = args.iter().format(", ");
            writeln!(f, "    {:?}({})", prim, args)?;
        }

        if !self.brchss.is_empty() {
            writeln!(f, "    brch {:?}", self.brchss)?;
        }

        for (pred, args) in self.calls.iter() {
            let args = args.iter().format(", ");
            writeln!(f, "    call {}({})", pred, args)?;
        }
        Ok(())
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

impl Block {
    pub fn new() -> Block {
        Block {
            blk_pred: Ident::dummy(&"?"),
            blk_idx: 0,
            min_depth: 0,
            eqs: Vec::new(),
            cons: Vec::new(),
            prims: Vec::new(),
            brchss: Vec::new(),
            calls: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PredDef {
    pub name: Ident,
    pub pars: Vec<(Ident, TypeId)>,
    pub vars: Vec<(Ident, TypeId)>,
    pub blks: Vec<Block>,
}
impl std::fmt::Display for PredDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "define {}:", self.name)?;

        for (par, par_ty) in self.pars.iter() {
            writeln!(f, "par {par}: {par_ty}")?;
        }

        for (var, var_ty) in self.vars.iter() {
            writeln!(f, "var {var}: {var_ty}")?;
        }

        for blk in self.blks.iter() {
            write!(f, "{blk}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub ty_map: HashMap<Ident, TypeId>,
    pub preds: HashMap<Ident, PredDef>,
    pub querys: Vec<crate::logic::ast::QueryDecl>,
}
