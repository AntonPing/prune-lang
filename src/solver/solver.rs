use std::collections::HashMap;

use crate::utils::ident::Ident;
use crate::utils::prim::Prim;

use super::logic::DnfPredicate;
use super::term::*;
use super::unify::*;

#[derive(Clone, Debug, PartialEq)]
pub struct InductivePath {
    base_sol: Solution,
    rec_preds: Vec<(Ident, Vec<UnifyTerm>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredSolver {
    name: Ident,
    pars: Vec<Ident>,
    paths: Vec<InductivePath>,
    sols: Vec<Solution>,
}

impl PredSolver {
    pub fn new(pred: &DnfPredicate) -> PredSolver {
        let name = pred.name;
        let pars = pred.pars.clone();

        let mut map = Vec::new();
        pred.free_vars(&mut map);

        let mut paths = Vec::new();
        for form in &pred.body {
            let eqs: Vec<(UnifyTerm, UnifyTerm)> = form
                .eqs
                .iter()
                .map(|(t1, t2)| (t1.instantiate(&map), t2.instantiate(&map)))
                .collect();

            let prims: Vec<(Prim, Vec<UnifyTerm>)> = form
                .prims
                .iter()
                .map(|(prim, args)| {
                    let args = args.iter().map(|arg| arg.instantiate(&map)).collect();
                    (*prim, args)
                })
                .collect();

            let base_sol = Solution::from_base(map.len(), &eqs, prims);

            if let Ok(base_sol) = base_sol {
                let rec_preds = form
                    .preds
                    .iter()
                    .map(|(name, args)| {
                        let args = args.iter().map(|arg| arg.instantiate(&map)).collect();
                        (*name, args)
                    })
                    .collect();

                let path = InductivePath {
                    base_sol,
                    rec_preds,
                };

                paths.push(path);
            }
        }

        PredSolver {
            name,
            pars,
            paths,
            sols: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Solver {
    preds: HashMap<Ident, PredSolver>,
}

impl Solver {
    pub fn new(preds: &HashMap<Ident, DnfPredicate>) -> Solver {
        let preds = preds
            .iter()
            .map(|(name, pred)| (*name, PredSolver::new(pred)))
            .collect();
        Solver { preds }
    }

    pub fn solve_step(&mut self) {
        let mut new_sol_map: HashMap<Ident, Vec<Solution>> = HashMap::new();

        for pred in self.preds.values() {
            let mut new_sol: Vec<Solution> = Vec::new();

            for path in &pred.paths {
                let mut path_sols = vec![path.base_sol.clone()];
                for (name, args) in &path.rec_preds {
                    let ind_sols = &self.preds[name].sols;
                    path_sols = concat_sol_set(&path_sols, ind_sols, args)
                }
                new_sol.append(&mut path_sols);
            }

            new_sol_map.insert(pred.name, new_sol);
        }

        for (k, v) in new_sol_map.into_iter() {
            self.preds.get_mut(&k).unwrap().sols = v;
        }
    }
}

#[test]
fn prog_to_solver_test() {
    let p1: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end
"#;
    let prog = crate::syntax::parser::parser::ProgramParser::new()
        .parse(p1)
        .unwrap();

    let dict = super::logic::prog_to_pred_dict(&prog);
    println!("{:#?}", dict);
    let dict2 = super::logic::dnf_pred_dict(&dict);
    println!("{:#?}", dict2);
    let mut solver = Solver::new(&dict2);
    println!("{:#?}", solver);

    for iter in 0..3 {
        solver.solve_step();
        println!("iter={}", iter);
        for pred in solver.preds.values() {
            for sol in pred.sols.iter() {
                let sol = sol.vars.merge_name_all(&pred.pars);
                println!("{:#?}", sol);
            }
        }
    }
}
