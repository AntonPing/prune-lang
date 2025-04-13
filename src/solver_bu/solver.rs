use easy_smt::Context;
use rand::seq::SliceRandom;
use std::collections::HashMap;

use super::smt_z3::solve_cons_sat;
use super::solution::*;
use crate::logic::trans::{DnfFormula, DnfPredicate, PredIdent};
use crate::utils::ident::Ident;
use crate::utils::prim::Prim;

use super::*;

use rand;

#[derive(Clone, Debug, PartialEq)]
pub struct InductivePath {
    base_sol: Solution,
    ind_preds: Vec<(PredIdent, Vec<Term<usize>>)>,
}

impl InductivePath {
    pub fn new(form: &DnfFormula, map: &HashMap<Ident, usize>) -> Option<InductivePath> {
        let eqs: Vec<(Term<usize>, Term<usize>)> = form
            .eqs
            .iter()
            .map(|(t1, t2)| (t1.var_map(&map), t2.var_map(&map)))
            .collect();

        let prims: Vec<(Prim, Vec<Term<usize>>)> = form
            .prims
            .iter()
            .map(|(prim, args)| {
                let args = args.iter().map(|arg| arg.var_map(&map)).collect();
                (*prim, args)
            })
            .collect();

        let base_sol = Solution::from_base(map.len(), &eqs, prims);

        if let Ok(base_sol) = base_sol {
            let ind_preds = form
                .preds
                .iter()
                .map(|(name, args)| {
                    let args = args.iter().map(|arg| arg.var_map(&map)).collect();
                    (*name, args)
                })
                .collect();

            Some(InductivePath {
                base_sol,
                ind_preds,
            })
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredPaths {
    name: PredIdent,
    pars: Vec<Ident>,
    paths: Vec<InductivePath>,
}

impl PredPaths {
    pub fn new(pred: &DnfPredicate) -> PredPaths {
        let name = pred.name;

        let mut vec = Vec::new();
        pred.free_vars(&mut vec);
        let map = vec.into_iter().enumerate().map(|(i, x)| (x, i)).collect();

        let paths: Vec<InductivePath> = pred
            .forms
            .iter()
            .filter_map(|form| InductivePath::new(form, &map))
            .collect();

        PredPaths {
            name,
            pars: pred.pars.clone(),
            paths,
        }
    }
}

#[derive(Debug)]
pub struct Checker {
    preds_map: HashMap<PredIdent, PredPaths>,
    sols_map: HashMap<PredIdent, Vec<Solution>>,
}

impl Checker {
    pub fn new(dict: &HashMap<PredIdent, DnfPredicate>) -> Checker {
        let preds_map = dict
            .iter()
            .map(|(name, pred)| (*name, PredPaths::new(pred)))
            .collect();

        let sols_map = dict.keys().map(|k| (*k, Vec::new())).collect();

        Checker {
            preds_map,
            sols_map,
        }
    }

    pub fn check_counter_example(&self) -> bool {
        self.sols_map
            .iter()
            .any(|(name, sols)| name.is_check() && !sols.is_empty())
    }

    pub fn solve_pred(&self, ctx: &mut Context, pred: &PredPaths) -> Vec<Solution> {
        let mut new_sol: Vec<Solution> = Vec::new();
        for path in &pred.paths {
            let mut path_sols = vec![path.base_sol.clone()];
            for (name, args) in &path.ind_preds {
                let ind_sols = &self.sols_map[name];
                path_sols = concat_sol_set(&path_sols, ind_sols, args);
            }
            for sol in path_sols.into_iter() {
                let mut sol = sol;
                sol.merge_cons();
                if solve_cons_sat(ctx, &sol.cons) {
                    new_sol.push(sol);
                }
            }
        }
        new_sol
    }

    pub fn solve_step(&mut self, ctx: &mut Context) {
        let mut new_sols_map: HashMap<PredIdent, Vec<Solution>> = HashMap::new();
        for (name, pred) in self.preds_map.iter() {
            let new_sol = self.solve_pred(ctx, pred);
            new_sols_map.insert(*name, new_sol);
        }
        self.sols_map = new_sols_map;
    }

    pub fn drop_sols(&mut self, capacity: usize) {
        let mut rng = rand::rng();
        for sols in self.sols_map.values_mut() {
            if sols.len() > capacity {
                sols.shuffle(&mut rng);
                sols.resize_with(capacity, || unreachable!());
            }
        }
    }

    pub fn print_stat(&self) {
        println!("------------------------------");
        for (name, sols) in self.sols_map.iter() {
            println!("{}: {}", name, sols.len())
        }
        println!("------------------------------");
    }

    pub fn merge_print(&self) {
        println!("------------------------------");
        for (name, pred) in self.preds_map.iter() {
            for sol in &self.sols_map[name] {
                sol.merge_print(*name, &pred.pars);
            }
        }
        println!("------------------------------");
    }
}
