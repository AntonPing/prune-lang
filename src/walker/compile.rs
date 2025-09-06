use super::*;
use crate::logic::ast::*;

use std::fmt;

#[derive(Clone, Debug)]
pub struct Block {
    pub eqs: Vec<(Ident, AtomId)>,
    pub cons: Vec<(Ident, Ident, Vec<AtomId>)>,
    pub prims: Vec<(Prim, Vec<AtomId>)>,
    pub calls: Vec<(PredIdent, Vec<AtomId>)>,
    pub brchss: Vec<Vec<Block>>,
}

impl Block {
    fn fmt_indented(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        let indent = "    ".repeat(level);

        for (var, atom) in self.eqs.iter() {
            writeln!(f, "{indent}{} = {}; ", var, atom)?;
        }

        for (var, cons, flds) in self.cons.iter() {
            let flds = flds.iter().format(&", ");
            writeln!(f, "{indent}{} = {}({})", var, cons, flds)?;
        }

        for (prim, args) in self.prims.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "{indent}{:?}({})", prim, args)?;
        }

        for (pred, args) in self.calls.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "{indent}{}({})", pred, args)?;
        }

        for brchs in self.brchss.iter() {
            assert!(!brchs.is_empty());
            writeln!(f, "{indent}{{")?;
            brchs[0].fmt_indented(f, level + 1)?;
            for brch in &brchs[1..] {
                writeln!(f, "{indent}|")?;
                brch.fmt_indented(f, level + 1)?;
            }
            writeln!(f, "{indent}}}")?;
        }
        Ok(())
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indented(f, 0)
    }
}

impl Block {
    pub fn new() -> Block {
        Block {
            eqs: Vec::new(),
            cons: Vec::new(),
            prims: Vec::new(),
            calls: Vec::new(),
            brchss: Vec::new(),
        }
    }
    pub fn compile_goal(goal: &Goal) -> Block {
        let mut blk = Block::new();
        blk.emit_goal(goal);
        blk
    }
    pub fn emit_goal(&mut self, goal: &Goal) {
        match goal {
            Goal::Lit(_) => {
                panic!("no literal value after optimize!");
            }
            Goal::Eq(var, atom) => {
                self.eqs.push((*var, atom.clone()));
            }
            Goal::Cons(var, cons, flds) => {
                self.cons.push((*var, *cons, flds.clone()));
            }
            Goal::Prim(prim, args) => {
                self.prims.push((*prim, args.clone()));
            }
            Goal::And(goals) => {
                for goal in goals {
                    self.emit_goal(goal);
                }
            }
            Goal::Or(goals) => {
                let blks: Vec<Block> = goals.iter().map(|goal| Block::compile_goal(goal)).collect();
                self.brchss.push(blks);
            }
            Goal::Call(pred, args) => {
                self.calls.push((*pred, args.clone()));
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct PredBlock {
    pub name: PredIdent,
    pub pars: Vec<(Ident, TypeId)>,
    pub vars: Vec<(Ident, TypeId)>,
    pub blk: Block,
}
impl std::fmt::Display for PredBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pars = self.pars.iter().format(&", ");
        let vars = self.vars.iter().format(&", ");
        writeln!(f, "{}({:?}) fresh: {{{:?}}}", self.name, pars, vars)?;
        writeln!(f, "{}", self.blk)?;
        Ok(())
    }
}

pub fn compile_pred(pred: &PredDecl, map: &HashMap<Ident, TypeId>) -> PredBlock {
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
    let blk = Block::compile_goal(&pred.goal);
    PredBlock {
        name: pred.name,
        pars,
        vars,
        blk,
    }
}

pub fn compile_dict(prog: &Program, map: &HashMap<Ident, TypeId>) -> HashMap<PredIdent, PredBlock> {
    prog.preds
        .iter()
        .map(|(name, pred)| (*name, compile_pred(pred, map)))
        .collect()
}

#[derive(Clone, Copy, Debug)]
pub struct BlockCtx<'blk> {
    pub blk: &'blk Block,
    pub ctx: usize,
}

impl<'blk> fmt::Display for BlockCtx<'blk> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ctx = {}}} {}", self.ctx, self.blk)
    }
}

impl<'blk> Block {
    pub fn tag_ctx(&'blk self, ctx: usize) -> BlockCtx<'blk> {
        BlockCtx { ctx, blk: self }
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
    println!("{:#?}", dict);
}
