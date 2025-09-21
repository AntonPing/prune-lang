use super::*;
use crate::logic::ast::*;

use std::fmt;

#[derive(Clone, Debug)]
pub struct Block {
    pub blk_pred: PredIdent,
    pub blk_idx: usize,
    pub eqs: Vec<(Ident, AtomId)>,
    pub cons: Vec<(Ident, Ident, Vec<AtomId>)>,
    pub prims: Vec<(Prim, Vec<AtomId>)>,
    pub brchss: Vec<Vec<usize>>,
    pub calls: Vec<(PredIdent, Vec<AtomId>)>,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "block {} {{{}}}:", self.blk_pred, self.blk_idx)?;

        for (var, atom) in self.eqs.iter() {
            writeln!(f, "    {} = {}; ", var, atom)?;
        }

        for (var, cons, flds) in self.cons.iter() {
            let flds = flds.iter().format(&", ");
            writeln!(f, "    {} = {}({})", var, cons, flds)?;
        }

        for (prim, args) in self.prims.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "    {:?}({})", prim, args)?;
        }

        if !self.brchss.is_empty() {
            writeln!(f, "    brch {:?}", self.brchss)?;
        }

        for (pred, args) in self.calls.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "    call {}({})", pred, args)?;
        }
        Ok(())
    }
}

impl Block {
    pub fn new() -> Block {
        Block {
            blk_pred: PredIdent::Pred(Ident::dummy(&"?")),
            blk_idx: 0,
            eqs: Vec::new(),
            cons: Vec::new(),
            prims: Vec::new(),
            brchss: Vec::new(),
            calls: Vec::new(),
        }
    }
}

pub fn compile_goal(pred: PredIdent, goal: &Goal) -> Vec<Block> {
    let mut blks = Vec::new();
    compile_goal_help(goal, &mut blks);
    for (idx, blk) in blks.iter_mut().enumerate() {
        blk.blk_pred = pred;
        blk.blk_idx = idx;
    }
    blks
}

pub fn compile_goal_help(goal: &Goal, blks: &mut Vec<Block>) -> usize {
    // push a placeholder
    blks.push(Block::new());
    let idx = blks.len() - 1;
    let mut blk = Block::new();
    emit_goal(goal, blks, &mut blk);
    blks[idx] = blk;
    idx
}

pub fn emit_goal(goal: &Goal, blks: &mut Vec<Block>, blk: &mut Block) {
    match goal {
        Goal::Lit(_) => {
            panic!("no literal value after optimize!");
        }
        Goal::Eq(var, atom) => {
            blk.eqs.push((*var, atom.clone()));
        }
        Goal::Cons(var, cons, flds) => {
            blk.cons.push((*var, *cons, flds.clone()));
        }
        Goal::Prim(prim, args) => {
            blk.prims.push((*prim, args.clone()));
        }
        Goal::And(goals) => {
            for goal in goals {
                emit_goal(goal, blks, blk);
            }
        }
        Goal::Or(goals) => {
            let blks: Vec<usize> = goals
                .iter()
                .map(|goal| compile_goal_help(goal, blks))
                .collect();
            blk.brchss.push(blks);
        }
        Goal::Call(pred, args) => {
            blk.calls.push((*pred, args.clone()));
        }
    }
}

#[derive(Clone, Debug)]
pub struct PredDef {
    pub name: PredIdent,
    pub pars: Vec<(Ident, TypeId)>,
    pub vars: Vec<(Ident, TypeId)>,
    pub blks: Vec<Block>,
}
impl std::fmt::Display for PredDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "define {}:", self.name)?;

        for (par, par_ty) in self.pars.iter() {
            writeln!(f, "par {par}: {par_ty:?}")?;
        }

        for (var, var_ty) in self.vars.iter() {
            writeln!(f, "var {var}: {var_ty:?}")?;
        }

        for blk in self.blks.iter() {
            write!(f, "{blk}")?;
        }
        Ok(())
    }
}

pub fn compile_pred(pred: &PredDecl, map: &HashMap<Ident, TypeId>) -> PredDef {
    let pars = pred
        .pars
        .iter()
        .map(|par| (*par, map[par].clone()))
        .collect();
    let vars = pred
        .vars
        .iter()
        .map(|var| (*var, map[var].clone()))
        .collect();

    let blks = compile_goal(pred.name, &pred.goal);

    PredDef {
        name: pred.name,
        pars,
        vars,
        blks,
    }
}

pub fn compile_dict(prog: &Program, map: &HashMap<Ident, TypeId>) -> HashMap<PredIdent, PredDef> {
    prog.preds
        .iter()
        .map(|(name, pred)| (*name, compile_pred(pred, map)))
        .collect()
}

#[derive(Clone, Debug)]
pub struct BlockCtx {
    pub pred: PredIdent,
    pub idx: usize,
    pub ctx: usize,
}

impl BlockCtx {
    pub fn new(pred: PredIdent) -> BlockCtx {
        BlockCtx {
            pred,
            idx: 0,
            ctx: 0,
        }
    }

    pub fn jump(&self, idx: usize) -> BlockCtx {
        let mut res = self.clone();
        res.idx = idx;
        res
    }

    pub fn call(&self, pred: PredIdent, ctx: usize) -> BlockCtx {
        let mut res = self.clone();
        res.pred = pred;
        res.idx = 0;
        res.ctx = ctx;
        res
    }
}

#[test]
fn compile_pred_test() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> IntList
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if @icmpeq(head, x) then true else is_elem(tail, x) 
    | Nil => false
    end
end

predicate is_elem_after_append(xs: IntList, x: Int)
begin
    fresh(ys) (
        and(
            ys = append(xs, x),
            is_elem(ys, x) = false,
        )
    )
end
"#;
    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let errs = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    let errs = crate::tych::check::check_pass(&prog);
    assert!(errs.is_empty());

    let prog = crate::logic::transform::logic_translation(&prog);
    // println!("{:#?}", prog);

    let map = crate::tych::elab::elab_pass(&prog);
    // println!("{:?}", map);

    let dict = compile_dict(&prog, &map);
    for (_pred, blk) in dict.iter() {
        println!("{}", blk);
    }
}
