use std::collections::HashMap;

use easy_smt::{Context, Response, SExpr};

use crate::utils::intern::InternStr;
use crate::utils::lit::LitVal;
use crate::utils::prim::*;

use super::constr::Constr;
use super::*;

pub fn solve_cons_sat(ctx: &mut Context, cons: &Constr) -> bool {
    // println!("{:?}", cons);
    // let mut ctx = ContextBuilder::new()
    //     .solver("z3")
    //     .solver_args(["-smt2", "-in"])
    //     .build()
    //     .expect("failed to build smt solver context!");

    ctx.push().unwrap();

    let mut map: HashMap<usize, SExpr> = HashMap::new();

    for (prim, args) in &cons.prims {
        match (prim, &args[..]) {
            (
                Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem,
                [arg1, arg2, arg3],
            ) => {
                let arg1 = get_int(ctx, &mut map, arg1).unwrap();
                let arg2 = get_int(ctx, &mut map, arg2).unwrap();
                let arg3 = get_int(ctx, &mut map, arg3).unwrap();
                let res = match prim {
                    Prim::IAdd => ctx.plus(arg1, arg2),
                    Prim::ISub => ctx.sub(arg1, arg2),
                    Prim::IMul => ctx.times(arg1, arg2),
                    Prim::IDiv => ctx.div(arg1, arg2),
                    Prim::IRem => ctx.rem(arg1, arg2),
                    _ => unreachable!(),
                };
                ctx.assert(ctx.eq(res, arg3)).unwrap();
            }
            (Prim::INeg, [arg1, arg2]) => {
                let arg1 = get_int(ctx, &mut map, arg1).unwrap();
                let arg2 = get_int(ctx, &mut map, arg2).unwrap();
                let res = ctx.negate(arg1);
                ctx.assert(ctx.eq(res, arg2)).unwrap();
            }
            (Prim::ICmp(cmp), [arg1, arg2, arg3]) => {
                let arg1 = get_int(ctx, &mut map, arg1).unwrap();
                let arg2 = get_int(ctx, &mut map, arg2).unwrap();
                let arg3 = get_bool(ctx, &mut map, arg3).unwrap();
                let res = match cmp {
                    Compare::Lt => ctx.lt(arg1, arg2),
                    Compare::Le => ctx.lte(arg1, arg2),
                    Compare::Eq => ctx.eq(arg1, arg2),
                    Compare::Ge => ctx.gte(arg1, arg2),
                    Compare::Gt => ctx.gt(arg1, arg2),
                    Compare::Ne => ctx.not(ctx.eq(arg1, arg2)),
                };
                ctx.assert(ctx.eq(res, arg3)).unwrap();
            }
            (Prim::BAnd | Prim::BOr, [arg1, arg2, arg3]) => {
                let arg1 = get_bool(ctx, &mut map, arg1).unwrap();
                let arg2 = get_bool(ctx, &mut map, arg2).unwrap();
                let arg3 = get_bool(ctx, &mut map, arg3).unwrap();
                let res = match prim {
                    Prim::BAnd => ctx.and(arg1, arg2),
                    Prim::BOr => ctx.or(arg1, arg2),
                    _ => unreachable!(),
                };
                ctx.assert(ctx.eq(res, arg3)).unwrap();
            }
            (Prim::BNot, [arg1, arg2]) => {
                let arg1 = get_bool(ctx, &mut map, arg1).unwrap();
                let arg2 = get_bool(ctx, &mut map, arg2).unwrap();
                let res = ctx.not(arg1);
                ctx.assert(ctx.eq(res, arg2)).unwrap();
            }
            _ => {
                panic!("wrong arity of primitives!");
            }
        }
    }
    let result = ctx.check().unwrap();
    ctx.pop().unwrap();
    match result {
        Response::Sat => true,
        Response::Unsat => false,
        Response::Unknown => true,
    }
}

pub fn get_int(
    ctx: &mut Context,
    map: &mut HashMap<usize, SExpr>,
    term: &Term<usize>,
) -> Option<SExpr> {
    match term {
        Term::Var(hole) => {
            if let Some(sexp) = map.get(hole) {
                Some(*sexp)
            } else {
                let sexp = ctx
                    .declare_const(InternStr::indexd(*hole).as_str(), ctx.int_sort())
                    .unwrap();
                map.insert(*hole, sexp);
                Some(sexp)
            }
        }
        Term::Lit(LitVal::Int(x)) => Some(ctx.numeral(*x)),
        _ => None,
    }
}

pub fn get_bool(
    ctx: &mut Context,
    map: &mut HashMap<usize, SExpr>,
    term: &Term<usize>,
) -> Option<SExpr> {
    match term {
        Term::Var(hole) => {
            if let Some(sexp) = map.get(hole) {
                Some(*sexp)
            } else {
                let sexp = ctx
                    .declare_const(InternStr::indexd(*hole).as_str(), ctx.bool_sort())
                    .unwrap();
                map.insert(*hole, sexp);
                Some(sexp)
            }
        }
        Term::Lit(LitVal::Bool(true)) => Some(ctx.true_()),
        Term::Lit(LitVal::Bool(false)) => Some(ctx.false_()),
        _ => None,
    }
}

// #[test]
// fn test_smt_z3() -> io::Result<()> {
//     let mut ctx = ContextBuilder::new()
//         .solver("z3")
//         .solver_args(["-smt2", "-in"])
//         .build()?;

//     // Declare `x` and `y` variables that are bitvectors of width 32.
//     let bv32 = ctx.bit_vec_sort(ctx.numeral(32));
//     let x = ctx.declare_const("x", bv32)?;
//     let y = ctx.declare_const("y", bv32)?;

//     // Assert that `x * y = 18`.
//     ctx.assert(ctx.eq(ctx.bvmul(x, y), ctx.binary(32, 18)))?;

//     // And assert that neither `x` nor `y` is 1.
//     ctx.assert(ctx.not(ctx.eq(x, ctx.binary(32, 1))))?;
//     ctx.assert(ctx.not(ctx.eq(y, ctx.binary(32, 1))))?;

//     // Check whether the assertions are satisfiable. They should be in this example.
//     assert_eq!(ctx.check()?, Response::Sat);

//     // Print the solution!
//     let solution = ctx.get_value(vec![x, y])?;
//     for (variable, value) in solution {
//         println!("{} = {}", ctx.display(variable), ctx.display(value));
//     }
//     // There are many solutions, but the one I get from Z3 is:
//     //
//     //     x = #x10000012
//     //     y = #x38000001
//     //
//     // Solvers are great at finding edge cases and surprising-to-humans results! In
//     // this case, I would have naively expected something like `x = 2, y = 9` or
//     // `x = 3, y = 6`, but the solver found a solution where the multiplication
//     // wraps around. Neat!
//     Ok(())
// }
