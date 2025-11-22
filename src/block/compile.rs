use super::*;
use crate::logic::ast;

pub fn compile_goal(pred: Ident, goal: &ast::Goal) -> Vec<Block> {
    let mut blks = Vec::new();
    compile_goal_help(goal, &mut blks);
    for (idx, blk) in blks.iter_mut().enumerate() {
        blk.blk_pred = pred;
        blk.blk_idx = idx;
    }
    blks
}

pub fn compile_goal_help(goal: &ast::Goal, blks: &mut Vec<Block>) -> usize {
    // push a placeholder
    blks.push(Block::new());
    let idx = blks.len() - 1;
    let mut blk = Block::new();
    emit_goal(goal, blks, &mut blk);
    blks[idx] = blk;
    idx
}

pub fn emit_goal(goal: &ast::Goal, blks: &mut Vec<Block>, blk: &mut Block) {
    match goal {
        ast::Goal::Lit(_) => {
            panic!("no literal value after optimize!");
        }
        ast::Goal::Eq(var, atom) => {
            blk.eqs.push((*var, atom.clone()));
        }
        ast::Goal::Cons(var, cons, flds) => {
            blk.cons.push((*var, *cons, flds.clone()));
        }
        ast::Goal::Prim(prim, args) => {
            blk.prims.push((*prim, args.clone()));
        }
        ast::Goal::And(goals) => {
            for goal in goals {
                emit_goal(goal, blks, blk);
            }
        }
        ast::Goal::Or(goals) => {
            let blks: Vec<usize> = goals
                .iter()
                .map(|goal| compile_goal_help(goal, blks))
                .collect();
            blk.brchss.push(blks);
        }
        ast::Goal::Call(pred, args) => {
            blk.calls.push((*pred, args.clone()));
        }
    }
}

pub fn compile_pred(pred: &ast::PredDecl, map: &HashMap<Ident, TypeId>) -> PredDef {
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

pub fn compile_dict(prog: &ast::Program) -> Program {
    let ty_map = super::elab::elab_pass(prog);
    let preds = prog
        .preds
        .iter()
        .map(|(name, pred)| (*name, compile_pred(pred, &ty_map)))
        .collect();

    let querys = prog.querys.clone();

    Program {
        ty_map,
        preds,
        querys,
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
    | Cons(head, tail) => if head == x then true else is_elem(tail, x) 
    | Nil => false
    end
end

function is_elem_after_append(xs: IntList, x: Int)
begin
    guard !is_elem(append(xs, x), x);
end

query is_elem_after_append(depth_step=5, depth_limit=50, answer_limit=1)
"#;
    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let errs = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    let errs = crate::tych::check::check_pass(&prog);
    assert!(errs.is_empty());

    let prog = crate::logic::transform::logic_translation(&prog);
    // println!("{:#?}", prog);

    let prog = compile_dict(&prog);
    for (_pred, blk) in prog.preds.iter() {
        println!("{}", blk);
    }
}
