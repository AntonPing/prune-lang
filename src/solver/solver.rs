use std::collections::HashMap;

use super::logic::DnfFormula;
use super::logic::DnfPredicate;
use super::solution::*;
use super::term::*;
use crate::utils::ident::Ident;
use crate::utils::prim::Prim;

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
pub struct PredSolver {
    name: Ident,
    pars: Vec<Ident>,
    paths: Vec<InductivePath>,
    sols: Vec<Solution>,
}

impl PredSolver {
    pub fn new(pred: &DnfPredicate) -> (PredSolver, PredSolver) {
        let name = pred.name;
        let mut pars = pred.pars.clone();

        let mut map = Vec::new();
        pred.free_vars(&mut map);

        let succ_paths: Vec<InductivePath> = pred
            .succ_forms
            .iter()
            .filter_map(|form| InductivePath::new(form, &map))
            .collect();

        let fail_paths: Vec<InductivePath> = pred
            .fail_forms
            .iter()
            .filter_map(|form| InductivePath::new(form, &map))
            .collect();

        let succ_pars = pars.clone();
        pars.pop().unwrap();

        (
            PredSolver {
                name,
                pars: succ_pars,
                paths: succ_paths,
                sols: Vec::new(),
            },
            PredSolver {
                name,
                pars,
                paths: fail_paths,
                sols: Vec::new(),
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Solver {
    succ_preds: HashMap<Ident, PredSolver>,
    fail_preds: HashMap<Ident, PredSolver>,
}

impl Solver {
    pub fn new(preds: &HashMap<Ident, DnfPredicate>) -> Solver {
        let (succ_preds, fail_preds) = preds
            .iter()
            .map(|(name, pred)| {
                let (pred1, pred2) = PredSolver::new(pred);
                ((*name, pred1), (*name, pred2))
            })
            .unzip();

        Solver {
            succ_preds,
            fail_preds,
        }
    }

    pub fn solve_pred(&self, pred: &PredSolver) -> Vec<Solution> {
        let mut new_sol: Vec<Solution> = Vec::new();
        for path in &pred.paths {
            let mut path_sols = vec![path.base_sol.clone()];
            for (name, args) in &path.succ_preds {
                let ind_sols = &self.succ_preds[name].sols;
                path_sols = concat_sol_set(&path_sols, ind_sols, args);
            }
            for (name, args) in &path.fail_preds {
                let ind_sols = &self.fail_preds[name].sols;
                path_sols = concat_sol_set(&path_sols, ind_sols, args);
            }
            new_sol.append(&mut path_sols);
        }
        new_sol
    }

    pub fn solve_step(&mut self) {
        let mut new_succ_sols: HashMap<Ident, Vec<Solution>> = HashMap::new();
        let mut new_fail_sols: HashMap<Ident, Vec<Solution>> = HashMap::new();
        for pred in self.succ_preds.values() {
            let new_sol = self.solve_pred(pred);
            new_succ_sols.insert(pred.name, new_sol);
        }
        for pred in self.fail_preds.values() {
            let new_sol = self.solve_pred(pred);
            new_fail_sols.insert(pred.name, new_sol);
        }
        for (k, v) in new_succ_sols.into_iter() {
            self.succ_preds.get_mut(&k).unwrap().sols = v;
        }
        for (k, v) in new_fail_sols.into_iter() {
            self.fail_preds.get_mut(&k).unwrap().sols = v;
        }
    }

    pub fn merge_print(&self) {
        println!("----------successed solution----------");
        for pred in self.succ_preds.values() {
            for sol in pred.sols.iter() {
                sol.merge_print(pred.name, &pred.pars);
            }
        }
        println!("----------failed solution----------");
        for pred in self.fail_preds.values() {
            for sol in pred.sols.iter() {
                sol.merge_print(pred.name, &pred.pars);
            }
        }
        println!("------------------------------");
    }
}

#[test]
fn prog_to_solver_test_1() {
    let p1: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => assert x; Cons(x, Nil)
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
        println!("iter={}", iter);
        solver.merge_print();
        solver.solve_step();
    }
}

#[test]
fn prog_to_solver_test_2() {
    let p1: &'static str = r#"
datatype AVLTree where
| Node(AVLTree, Int, AVLTree)
| Empty
end

function insert(tree: AVLTree, x: Int) -> AVLTree
begin
    match tree with
    | Node(left, y, right) => 
        if @icmplt(x, y) then
            Node(insert(left, x), y, right)
        else if @icmpgt(x, y) then
            Node(left, y, insert(right, x))
        else tree
    | Empty => Node(Empty, x, Empty)
    end
end

function abs(x: Int) -> Int
begin
    if @icmplt(x, 0) then @ineg(x) else x
end

function max(x: Int, y: Int) -> Int
begin
    if @icmplt(x, y) then y else x
end

function tree_depth(tree: AVLTree) -> Int
begin
    match tree with
    | Node(left, y, right) => 
        let left_depth = tree_depth(left);
        let right_depth = tree_depth(right);
        assert @icmple(abs(@isub(left_depth, right_depth)), 1);
        @iadd(max(left_depth, right_depth), 1)
    | Empty => 0
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
        println!("iter={}", iter);
        solver.merge_print();
        solver.solve_step();
    }
}
