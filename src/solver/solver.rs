use easy_smt::Context;
use rand::seq::SliceRandom;
use std::collections::HashMap;

use super::logic::DnfFormula;
use super::logic::DnfPredicate;
use super::smt_z3::solve_cons_sat;
use super::solution::*;
use super::term::*;
use crate::utils::ident::Ident;
use crate::utils::prim::Prim;

use rand;

#[derive(Clone, Debug, PartialEq)]
pub struct InductivePath {
    base_sol: Solution,
    succ_preds: Vec<(Ident, Vec<UnifyTerm>)>,
    fail_preds: Vec<(Ident, Vec<UnifyTerm>)>,
}

impl InductivePath {
    pub fn new(form: &DnfFormula, map: &Vec<Ident>) -> Option<InductivePath> {
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
            let succ_preds = form
                .succ_preds
                .iter()
                .map(|(name, args)| {
                    let args = args.iter().map(|arg| arg.instantiate(&map)).collect();
                    (*name, args)
                })
                .collect();

            let fail_preds = form
                .fail_preds
                .iter()
                .map(|(name, args)| {
                    let args = args.iter().map(|arg| arg.instantiate(&map)).collect();
                    (*name, args)
                })
                .collect();

            Some(InductivePath {
                base_sol,
                succ_preds,
                fail_preds,
            })
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredPaths {
    name: Ident,
    pars: Vec<Ident>,
    paths: Vec<InductivePath>,
}

impl PredPaths {
    pub fn new(pred: &DnfPredicate) -> PredPaths {
        let name = pred.name;
        let mut map = Vec::new();
        pred.free_vars(&mut map);

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
    succ_preds: HashMap<Ident, PredPaths>,
    fail_preds: HashMap<Ident, PredPaths>,
    check_preds: HashMap<Ident, PredPaths>,
    succ_sols: HashMap<Ident, Vec<Solution>>,
    fail_sols: HashMap<Ident, Vec<Solution>>,
    check_sols: HashMap<Ident, Vec<Solution>>,
}

impl Checker {
    pub fn new(
        succ_preds: &HashMap<Ident, DnfPredicate>,
        fail_preds: &HashMap<Ident, DnfPredicate>,
        check_preds: &HashMap<Ident, DnfPredicate>,
    ) -> Checker {
        let succ_preds: HashMap<Ident, PredPaths> = succ_preds
            .iter()
            .map(|(name, pred)| (*name, PredPaths::new(pred)))
            .collect();

        let fail_preds: HashMap<Ident, PredPaths> = fail_preds
            .iter()
            .map(|(name, pred)| (*name, PredPaths::new(pred)))
            .collect();

        let check_preds: HashMap<Ident, PredPaths> = check_preds
            .iter()
            .map(|(name, pred)| (*name, PredPaths::new(pred)))
            .collect();

        let succ_sols = succ_preds.keys().map(|k| (*k, Vec::new())).collect();
        let fail_sols = fail_preds.keys().map(|k| (*k, Vec::new())).collect();
        let check_sols = check_preds.keys().map(|k| (*k, Vec::new())).collect();

        Checker {
            succ_preds,
            fail_preds,
            check_preds,
            succ_sols,
            fail_sols,
            check_sols,
        }
    }

    pub fn check_counter_example(&self) -> bool {
        self.check_sols.values().any(|sols| !sols.is_empty())
    }

    pub fn solve_pred(&self, ctx: &mut Context, pred: &PredPaths) -> Vec<Solution> {
        let mut new_sol: Vec<Solution> = Vec::new();
        for path in &pred.paths {
            let mut path_sols = vec![path.base_sol.clone()];
            for (name, args) in &path.succ_preds {
                let ind_sols = &self.succ_sols[name];
                path_sols = concat_sol_set(&path_sols, ind_sols, args);
            }
            for (name, args) in &path.fail_preds {
                let ind_sols = &self.fail_sols[name];
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
        let mut new_succ_sols: HashMap<Ident, Vec<Solution>> = HashMap::new();
        let mut new_fail_sols: HashMap<Ident, Vec<Solution>> = HashMap::new();
        let mut new_check_sols: HashMap<Ident, Vec<Solution>> = HashMap::new();
        for pred in self.succ_preds.values() {
            let new_sol = self.solve_pred(ctx, pred);
            new_succ_sols.insert(pred.name, new_sol);
        }
        for pred in self.fail_preds.values() {
            let new_sol = self.solve_pred(ctx, pred);
            new_fail_sols.insert(pred.name, new_sol);
        }
        for pred in self.check_preds.values() {
            let new_sol = self.solve_pred(ctx, pred);
            new_check_sols.insert(pred.name, new_sol);
        }
        for (k, v) in new_succ_sols.into_iter() {
            self.succ_sols.insert(k, v);
        }
        for (k, v) in new_fail_sols.into_iter() {
            self.fail_sols.insert(k, v);
        }
        for (k, v) in new_check_sols.into_iter() {
            self.check_sols.insert(k, v);
        }
    }

    pub fn drop_sols(&mut self, capacity: usize) {
        let mut rng = rand::rng();
        for sols in self.succ_sols.values_mut() {
            if sols.len() > capacity {
                sols.shuffle(&mut rng);
                sols.resize_with(capacity, || unreachable!());
            }
        }
        for sols in self.fail_sols.values_mut() {
            if sols.len() > capacity {
                sols.shuffle(&mut rng);
                sols.resize_with(capacity, || unreachable!());
            }
        }
        for sols in self.check_sols.values_mut() {
            if sols.len() > capacity {
                sols.shuffle(&mut rng);
                sols.resize_with(capacity, || unreachable!());
            }
        }
    }

    pub fn print_stat(&self) {
        println!("------------------------------");
        for pred in self.succ_preds.values() {
            println!("{}(succ): {}", pred.name, self.succ_sols[&pred.name].len())
        }
        for pred in self.fail_preds.values() {
            println!("{}(fail): {}", pred.name, self.fail_sols[&pred.name].len())
        }
        for pred in self.check_preds.values() {
            println!(
                "{}(check): {}",
                pred.name,
                self.check_sols[&pred.name].len()
            )
        }
        println!("------------------------------");
    }

    pub fn merge_print(&self) {
        println!("----------successed solution----------");
        for pred in self.succ_preds.values() {
            for sol in &self.succ_sols[&pred.name] {
                sol.merge_print(pred.name, &pred.pars);
            }
        }
        println!("----------failed solution----------");
        for pred in self.fail_preds.values() {
            for sol in &self.fail_sols[&pred.name] {
                sol.merge_print(pred.name, &pred.pars);
            }
        }
        println!("----------counter examples----------");
        for pred in self.check_preds.values() {
            for sol in &self.check_sols[&pred.name] {
                sol.merge_print(pred.name, &pred.pars);
            }
        }
        println!("------------------------------");
    }
}
