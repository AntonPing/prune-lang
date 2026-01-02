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

fn compile_goal_help(goal: &ast::Goal, blks: &mut Vec<Block>) -> usize {
    // push a placeholder
    blks.push(Block::new());
    let idx = blks.len() - 1;
    let mut blk = Block::new();
    emit_goal(goal, blks, &mut blk);
    blks[idx] = blk;
    idx
}

fn emit_goal(goal: &ast::Goal, blks: &mut Vec<Block>, blk: &mut Block) {
    match goal {
        ast::Goal::Lit(_) => {
            panic!("no literal value after optimize!");
        }
        ast::Goal::Eq(lhs, rhs) => {
            blk.eqs.push((lhs.clone(), rhs.clone()));
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

fn get_block_min_depth(preds: &HashMap<Ident, PredDef>, pred: &PredDef, blk: &Block) -> usize {
    let mut calls_depth = 0;
    for (pred, _args) in blk.calls.iter() {
        let blk = &preds[pred].blks[0];
        calls_depth += blk.min_depth;
    }

    let mut brchs_depth = 0;
    for brchs in blk.brchss.iter() {
        let min_depth = brchs
            .iter()
            .map(|brch| pred.blks[*brch].min_depth)
            .min()
            .unwrap_or(0);
        brchs_depth += min_depth;
    }

    calls_depth + brchs_depth + 1
}

fn update_min_depth_iter(preds: &mut HashMap<Ident, PredDef>) -> bool {
    let mut map: HashMap<Ident, Vec<usize>> = HashMap::new();
    for (name, pred) in preds.iter() {
        let mut vec = Vec::new();
        for blk in pred.blks.iter() {
            let min_depth = get_block_min_depth(preds, pred, blk);
            vec.push(min_depth);
        }
        map.insert(*name, vec);
    }

    let mut flag = false;
    for (name, vec) in map.iter() {
        for (blk, depth) in preds.get_mut(name).unwrap().blks.iter_mut().zip(vec.iter()) {
            assert!(blk.min_depth <= *depth);
            if blk.min_depth < *depth {
                blk.min_depth = *depth;
                flag = true;
            }
        }
    }

    flag
}

fn update_min_depth(preds: &mut HashMap<Ident, PredDef>) {
    for _ in 0..10000 {
        let res = update_min_depth_iter(preds);
        if !res {
            return;
        }
    }
    panic!("potential infinite recursive without branching point!");
}

fn compile_pred(pred: &ast::PredDecl, map: &HashMap<Ident, TypeId>) -> PredDef {
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
    let mut preds = prog
        .preds
        .iter()
        .map(|(name, pred)| (*name, compile_pred(pred, &ty_map)))
        .collect();

    update_min_depth(&mut preds);
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
