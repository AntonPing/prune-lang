use super::*;

use crate::utils::lit::LitVal::*;
use crate::utils::term::Term::*;

pub enum PropagateResult {
    Skip,
    Propagate(Vec<(AtomVal<IdentCtx>, AtomVal<IdentCtx>)>),
    Conflit,
}

pub fn propagate_prims(prim: Prim, args: &Vec<AtomVal<IdentCtx>>) -> PropagateResult {
    match (prim, &args[..]) {
        (Prim::IAdd, [arg1, arg2, arg3]) => propagate_iadd(arg1, arg2, arg3),
        (Prim::ISub, [arg1, arg2, arg3]) => propagate_isub(arg1, arg2, arg3),
        (Prim::IMul, [arg1, arg2, arg3]) => propagate_imul(arg1, arg2, arg3),
        (Prim::IDiv, [arg1, arg2, arg3]) => propagate_idiv(arg1, arg2, arg3),
        (Prim::IRem, [arg1, arg2, arg3]) => propagate_irem(arg1, arg2, arg3),
        (Prim::INeg, [arg1, arg2]) => propagate_ineg(arg1, arg2),
        (Prim::ICmp(cmp), [arg1, arg2, arg3]) => propagate_icmp(cmp, arg1, arg2, arg3),
        (Prim::BAnd, [arg1, arg2, arg3]) => propagate_bool_and(arg1, arg2, arg3),
        (Prim::BOr, [arg1, arg2, arg3]) => propagate_bool_or(arg1, arg2, arg3),
        (Prim::BNot, [arg1, arg2]) => propagate_bool_not(arg1, arg2),
        (_, _) => panic!("wrong arity of primitive!"),
    }
}

fn propagate_iadd(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (arg1, Lit(Int(lit2)), Lit(Int(lit3))) => {
            let res = vec![(arg1.clone(), Lit(Int(*lit3 - *lit2)))];
            PropagateResult::Propagate(res)
        }
        (Lit(Int(lit1)), arg2, Lit(Int(lit3))) => {
            let res = vec![(arg2.clone(), Lit(Int(*lit3 - *lit1)))];
            PropagateResult::Propagate(res)
        }
        (Lit(Int(lit1)), Lit(Int(lit2)), arg3) => {
            let res = vec![(arg3.clone(), Lit(Int(*lit1 + *lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_isub(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (arg1, Lit(Int(lit2)), Lit(Int(lit3))) => {
            let res = vec![(arg1.clone(), Lit(Int(*lit3 + *lit2)))];
            PropagateResult::Propagate(res)
        }
        (Lit(Int(lit1)), arg2, Lit(Int(lit3))) => {
            let res = vec![(arg2.clone(), Lit(Int(*lit1 - *lit3)))];
            PropagateResult::Propagate(res)
        }
        (Lit(Int(lit1)), Lit(Int(lit2)), arg3) => {
            let res = vec![(arg3.clone(), Lit(Int(*lit1 - *lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_imul(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (arg1, Lit(Int(lit2)), Lit(Int(lit3))) => {
            if *lit3 % *lit2 == 0 {
                let res = vec![(arg1.clone(), Lit(Int(*lit3 / *lit2)))];
                PropagateResult::Propagate(res)
            } else {
                PropagateResult::Conflit
            }
        }
        (Lit(Int(lit1)), arg2, Lit(Int(lit3))) => {
            if *lit3 % *lit1 == 0 {
                let res = vec![(arg2.clone(), Lit(Int(*lit3 / *lit1)))];
                PropagateResult::Propagate(res)
            } else {
                PropagateResult::Conflit
            }
        }
        (Lit(Int(lit1)), Lit(Int(lit2)), arg3) => {
            let res = vec![(arg3.clone(), Lit(Int(*lit1 * *lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_idiv(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (Lit(Int(lit1)), Lit(Int(lit2)), arg3) => {
            let res = vec![(arg3.clone(), Lit(Int(*lit1 / *lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_irem(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (Lit(Int(lit1)), Lit(Int(lit2)), arg3) => {
            let res = vec![(arg3.clone(), Lit(Int(*lit1 % *lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_ineg(arg1: &AtomVal<IdentCtx>, arg2: &AtomVal<IdentCtx>) -> PropagateResult {
    match (arg1, arg2) {
        (Lit(Int(lit1)), arg2) => {
            let res = vec![(arg2.clone(), Lit(Int(-*lit1)))];
            PropagateResult::Propagate(res)
        }
        (arg1, Lit(Int(lit2))) => {
            let res = vec![(arg1.clone(), Lit(Int(-*lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _) => PropagateResult::Skip,
    }
}

fn propagate_icmp(
    cmp: Compare,
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (arg1, arg2, Lit(Bool(true))) if cmp == Compare::Eq => {
            let res = vec![(arg1.clone(), arg2.clone())];
            PropagateResult::Propagate(res)
        }
        (arg1, arg2, Lit(Bool(false))) if cmp == Compare::Ne => {
            let res = vec![(arg1.clone(), arg2.clone())];
            PropagateResult::Propagate(res)
        }
        (Lit(Int(lit1)), Lit(Int(lit2)), arg3) => {
            let cmp_res = match cmp {
                Compare::Lt => *lit1 < *lit2,
                Compare::Le => *lit1 <= *lit2,
                Compare::Eq => *lit1 == *lit2,
                Compare::Ge => *lit1 >= *lit2,
                Compare::Gt => *lit1 > *lit2,
                Compare::Ne => *lit1 != *lit2,
            };
            let res = vec![(arg3.clone(), Lit(Bool(cmp_res)))];
            PropagateResult::Propagate(res)
        }
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_bool_and(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (Lit(Bool(true)), arg2, arg3) => {
            let res = vec![(arg2.clone(), arg3.clone())];
            PropagateResult::Propagate(res)
        }
        (arg1, Lit(Bool(true)), arg3) => {
            let res = vec![(arg1.clone(), arg3.clone())];
            PropagateResult::Propagate(res)
        }
        (arg1, arg2, Lit(Bool(true))) => {
            let res = vec![
                (arg1.clone(), Lit(Bool(true))),
                (arg2.clone(), Lit(Bool(true))),
            ];
            PropagateResult::Propagate(res)
        }
        (Lit(Bool(false)), _, _) | (_, Lit(Bool(false)), _) => PropagateResult::Conflit,
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_bool_or(
    arg1: &AtomVal<IdentCtx>,
    arg2: &AtomVal<IdentCtx>,
    arg3: &AtomVal<IdentCtx>,
) -> PropagateResult {
    match (arg1, arg2, arg3) {
        (Lit(Bool(false)), arg2, arg3) => {
            let res = vec![(arg2.clone(), arg3.clone())];
            PropagateResult::Propagate(res)
        }
        (arg1, Lit(Bool(false)), arg3) => {
            let res = vec![(arg1.clone(), arg3.clone())];
            PropagateResult::Propagate(res)
        }
        (arg1, arg2, Lit(Bool(false))) => {
            let res = vec![
                (arg1.clone(), Lit(Bool(false))),
                (arg2.clone(), Lit(Bool(false))),
            ];
            PropagateResult::Propagate(res)
        }
        (Lit(Bool(true)), _, _) | (_, Lit(Bool(true)), _) => PropagateResult::Conflit,
        (_, _, _) => PropagateResult::Skip,
    }
}

fn propagate_bool_not(arg1: &AtomVal<IdentCtx>, arg2: &AtomVal<IdentCtx>) -> PropagateResult {
    match (arg1, arg2) {
        (Lit(Bool(lit1)), arg2) => {
            let res = vec![(arg2.clone(), Lit(Bool(!*lit1)))];
            PropagateResult::Propagate(res)
        }
        (arg1, Lit(Bool(lit2))) => {
            let res = vec![(arg1.clone(), Lit(Bool(!*lit2)))];
            PropagateResult::Propagate(res)
        }
        (_, _) => PropagateResult::Skip,
    }
}
