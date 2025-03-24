use smtlib::terms::StaticSorted;
use smtlib::*;
use std::ops::*;

use crate::utils::intern::InternStr;
use crate::utils::lit::LitVal;
use crate::utils::prim::*;

use super::constr::Constr;
use super::term::*;

pub fn solve_cons_sat<'st, B: Backend>(
    st: &'st Storage,
    solver: &mut Solver<'st, B>,
    cons: &Constr,
) -> bool {
    // println!("{:?}", cons);
    solver
        .scope(|solver| {
            for (prim, args) in &cons.prims {
                match (prim, &args[..]) {
                    (
                        Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem,
                        [arg1, arg2, arg3],
                    ) => {
                        let arg1 = get_int(&st, arg1).unwrap();
                        let arg2 = get_int(&st, arg2).unwrap();
                        let arg3 = get_int(&st, arg3).unwrap();
                        let res = match prim {
                            Prim::IAdd => arg1 + arg2,
                            Prim::ISub => arg1 - arg2,
                            Prim::IMul => arg1 * arg2,
                            Prim::IDiv => arg1 / arg2,
                            Prim::IRem => arg1 - (arg1 / arg2) * arg2,
                            _ => unreachable!(),
                        };
                        solver.assert(res._eq(arg3)).unwrap();
                    }
                    (Prim::INeg, [arg1, arg2]) => {
                        let arg1 = get_int(&st, arg1).unwrap();
                        let arg2 = get_int(&st, arg2).unwrap();
                        let res = 0 - arg1;
                        solver.assert(res._eq(arg2)).unwrap();
                    }
                    (Prim::ICmp(cmp), [arg1, arg2, arg3]) => {
                        let arg1 = get_int(&st, arg1).unwrap();
                        let arg2 = get_int(&st, arg2).unwrap();
                        let arg3 = get_bool(&st, arg3).unwrap();
                        let res = match cmp {
                            Compare::Lt => arg1.lt(arg2),
                            Compare::Le => arg1.le(arg2),
                            Compare::Eq => arg1._eq(arg2),
                            Compare::Ge => arg1.ge(arg2),
                            Compare::Gt => arg1.gt(arg2),
                            Compare::Ne => arg1._neq(arg2),
                        };
                        solver.assert(res._eq(arg3)).unwrap();
                    }
                    (Prim::BAnd | Prim::BOr, [arg1, arg2, arg3]) => {
                        let arg1 = get_bool(&st, arg1).unwrap();
                        let arg2 = get_bool(&st, arg2).unwrap();
                        let arg3 = get_bool(&st, arg3).unwrap();
                        let res = match prim {
                            Prim::BAnd => and(st, [arg1, arg2]),
                            Prim::BOr => or(st, [arg1, arg2]),
                            _ => unreachable!(),
                        };
                        solver.assert(res._eq(arg3)).unwrap();
                    }
                    (Prim::BNot, [arg1, arg2]) => {
                        let arg1 = get_bool(&st, arg1).unwrap();
                        let arg2 = get_bool(&st, arg2).unwrap();
                        let res = arg1.not();
                        solver.assert(res._eq(arg2)).unwrap();
                    }
                    _ => {
                        panic!("wrong arity of primitives!");
                    }
                }
            }
            if let Ok(res) = solver.check_sat_with_model() {
                match res {
                    SatResultWithModel::Sat(_model) => {
                        // println!("{:#?}", model);
                        Ok(true)
                    }
                    SatResultWithModel::Unsat => Ok(false),
                    SatResultWithModel::Unknown => Ok(true),
                }
            } else {
                Err(smtlib::Error::Smt(
                    "check_sat() failed!".to_string(),
                    "".to_string(),
                ))
            }
        })
        .unwrap()
}

pub fn get_int<'st>(st: &'st Storage, term: &UnifyTerm) -> Option<Int<'st>> {
    match term {
        UnifyTerm::Hole(hole) => {
            Some(Int::new_const(&st, InternStr::indexd(*hole).as_str()).into())
        }
        UnifyTerm::Lit(LitVal::Int(x)) => Some(Int::new(st, *x)),
        _ => None,
    }
}

pub fn get_bool<'st>(st: &'st Storage, term: &UnifyTerm) -> Option<Bool<'st>> {
    match term {
        UnifyTerm::Hole(hole) => {
            Some(Bool::new_const(&st, InternStr::indexd(*hole).as_str()).into())
        }
        UnifyTerm::Lit(LitVal::Bool(x)) => Some(Bool::new(st, *x)),
        _ => None,
    }
}
