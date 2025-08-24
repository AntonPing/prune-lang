use super::*;

use std::io;

use super::compile::LinearCode;
use crate::solver::solver::Solver;
use crate::walker::vsids::{Point, Priority};

#[derive(Clone, Debug)]
struct State {
    cost: usize,
    stack: Vec<Point>,
}

impl State {
    fn new() -> State {
        State {
            cost: 0,
            stack: Vec::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}

#[derive(Clone, Debug)]
struct Saves {
    stack: Vec<State>,
}

impl Saves {
    fn new() -> Saves {
        Saves { stack: Vec::new() }
    }

    fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    fn push(&mut self, sol: &mut Solver, state: State) {
        sol.savepoint();
        self.stack.push(state);
    }

    fn pop(&mut self, sol: &mut Solver) -> State {
        sol.backtrack();
        self.stack.pop().unwrap()
    }
}

#[derive(Debug)]
pub struct Walker<'log, Log: io::Write> {
    codes: Vec<LinearCode>,
    idx_cnt: usize,
    tmsp_cnt: usize,
    total_step: usize,
    sol: Solver,
    log: &'log mut Log,
}

impl<'sol, 'log, Log: io::Write> Walker<'log, Log> {
    pub fn new(codes: Vec<LinearCode>, log: &'log mut Log) -> Walker<'log, Log> {
        Walker {
            codes,
            sol: Solver::new(),
            idx_cnt: 0,
            tmsp_cnt: 0,
            total_step: 0,
            log,
        }
    }

    fn reset(&mut self) {
        self.idx_cnt = 0;
        self.tmsp_cnt = 0;
        self.sol.reset();
    }

    pub fn write_code_window(&mut self, addr: usize) -> io::Result<()> {
        let start = std::cmp::max(addr, 2) - 2;
        let end = std::cmp::min(addr, self.codes.len() - 3) + 3;

        for (i, code) in self.codes[start..end].iter().enumerate() {
            let i = i + start;

            if i == addr {
                writeln!(self.log, "{:03}: >>> {}", &i, &code)?;
            } else {
                writeln!(self.log, "{:03}: {}", &i, &code)?;
            }
        }
        Ok(())
    }

    pub fn write_stack(&mut self, stack: &Vec<Point>) -> io::Result<()> {
        let stack = stack.iter().format(&", ");
        writeln!(self.log, "stack = [{}]", stack)?;
        Ok(())
    }

    pub fn write_saves(&mut self, saves: &Vec<Vec<Point>>) -> io::Result<()> {
        for (i, stack) in saves.iter().rev().enumerate() {
            let stack = stack.iter().format(&", ");
            writeln!(self.log, "save{} = [{}]", i, stack)?;
        }
        Ok(())
    }

    pub fn write_solver(&mut self) -> io::Result<()> {
        writeln!(self.log, "{}", self.sol)
    }

    fn new_point(&self, addr: usize, idx: usize, pred: Option<&Point>) -> Point {
        Point::new(
            addr,
            idx,
            Priority::new(0, self.tmsp_cnt),
            pred.map(|x| x.clone()),
        )
    }

    fn run_saves(&mut self, depth: usize, saves: &mut Saves) -> Option<usize> {
        let mut steps: usize = 0;
        while !saves.is_empty() {
            let mut state = saves.pop(&mut self.sol);
            let res = self.run_stack(depth, &mut state, saves)?;
            steps += res;
        }
        Some(steps)
    }

    fn run_stack(&mut self, depth: usize, state: &mut State, saves: &mut Saves) -> Option<usize> {
        // self.write_stack(stack).unwrap();
        // self.write_saves(saves).unwrap();

        let mut steps: usize = 0;
        while !state.is_empty() {
            if state.cost >= depth {
                return Some(steps);
            }
            let idx = self.select_point(depth, state)?;
            let curr_pnt = state.stack.remove(idx);

            steps += 1;
            if !self.run_point(&curr_pnt, state, saves) {
                curr_pnt.update_bump_upward(self.tmsp_cnt);
                self.tmsp_cnt += 1;
                return Some(steps);
            }
        }
        return None;
    }

    fn select_point(&mut self, _depth: usize, state: &mut State) -> Option<usize> {
        let (idx, _tag, _prior) = state
            .stack
            .iter_mut()
            .enumerate()
            .map(|(idx, pnt)| {
                let tag = self.codes[pnt.get_addr_idx().0].tag();
                pnt.update_decay(self.tmsp_cnt);
                let prior = pnt.get_prior();
                (idx, tag, prior)
            })
            .max_by(
                |(_, tag1, prior1), (_, tag2, prior2)| match tag1.cmp(tag2) {
                    core::cmp::Ordering::Equal => prior1.cmp(prior2),
                    ord => ord,
                },
            )
            .unwrap();

        return Some(idx);

        // Look-ahead simulation

        // let mut res_vec: Vec<usize> = Vec::new();
        // for i in 0..state.len() {
        //     let mut sim_state: State = state.clone();
        //     let mut sim_saves: Saves = Saves::new();

        //     let new_depth = std::cmp::min(sim_state.cost + 1, depth);

        //     self.sol.savepoint();

        //     let curr_pnt = sim_state.stack.remove(i);
        //     if self.run_point(&curr_pnt, &mut sim_state, &mut sim_saves) {
        //         sim_saves.push(&mut self.sol, sim_state);
        //     }
        //     let res = self.run_saves(new_depth, &mut sim_saves)?;
        //     res_vec.push(res);

        //     self.sol.backtrack();
        // }

        // let idx = res_vec
        //     .iter()
        //     .enumerate()
        //     .min_by(|(_, a), (_, b)| a.partial_cmp(b).expect("REASON"))
        //     .map(|(idx, _)| idx)
        //     .unwrap();

        // Some(idx)
    }

    fn run_point(&mut self, curr_pnt: &Point, state: &mut State, saves: &mut Saves) -> bool {
        self.total_step += 1;
        state.cost += 1;

        let (curr_addr, curr_idx) = curr_pnt.get_addr_idx();
        let code = &self.codes[curr_addr];
        match code {
            LinearCode::Lit(_) => {
                panic!("literal should be optimized!")
            }
            LinearCode::Eq(var, atom) => {
                let var = var.tag_ctx(curr_idx);
                let atom = atom.tag_ctx(curr_idx);
                self.sol.bind(var, atom.to_term()).is_ok()
            }
            LinearCode::Cons(var, cons, flds) => {
                let var = var.tag_ctx(curr_idx);
                let flds = flds
                    .iter()
                    .map(|fld| fld.tag_ctx(curr_idx).to_term())
                    .collect();
                self.sol.bind(var, Term::Cons((), *cons, flds)).is_ok()
            }
            LinearCode::Prim(prim, args) => {
                let args = args.iter().map(|arg| arg.tag_ctx(curr_idx)).collect();
                self.sol.solve(*prim, args).is_ok()
            }
            LinearCode::Call(pred1, args, addr) => {
                if let LinearCode::Label(pred2, pars, vars) = &self.codes[*addr] {
                    assert_eq!(pred1, pred2);
                    self.idx_cnt += 1;
                    assert_eq!(pars.len(), args.len());
                    for (par, arg) in pars.iter().zip(args.iter()) {
                        self.sol.declare(&par.tag_ctx(self.idx_cnt));
                        let par = par.tag_ctx(self.idx_cnt);
                        let arg = arg.tag_ctx(curr_idx);
                        self.sol.bind(par, arg.to_term()).unwrap(); // unify with a fresh variable cannot fail
                    }
                    for var in vars {
                        self.sol.declare(&var.tag_ctx(self.idx_cnt));
                    }
                    let pnt = self.new_point(addr + 1, self.idx_cnt, Some(curr_pnt));
                    state.stack.push(pnt);
                    true
                } else {
                    panic!("addr of call not reference to a label!");
                }
            }
            LinearCode::And(addrs) => {
                assert!(addrs.len() >= 2);
                for addr in addrs.iter().rev() {
                    let pnt = self.new_point(*addr, curr_idx, Some(curr_pnt));
                    state.stack.push(pnt);
                }
                true
            }
            LinearCode::Or(addrs) => {
                assert!(addrs.len() >= 2);
                for addr in addrs[1..].iter().rev() {
                    let pnt = self.new_point(*addr, curr_idx, Some(curr_pnt));
                    state.stack.push(pnt);
                    saves.push(&mut self.sol, state.clone());
                    state.stack.pop();
                }
                let pnt = self.new_point(addrs[0], curr_idx, Some(curr_pnt));
                state.stack.push(pnt);
                true
            }
            LinearCode::Label(_pred, args, vars) => {
                assert!(state.stack.is_empty());
                assert_eq!(self.idx_cnt, 0);
                for arg in args {
                    self.sol.declare(&arg.tag_ctx(self.idx_cnt));
                }
                for var in vars {
                    self.sol.declare(&var.tag_ctx(self.idx_cnt));
                }
                let pnt = self.new_point(curr_addr + 1, self.idx_cnt, Some(curr_pnt));
                state.stack.push(pnt);
                true
            }
        }
    }

    pub fn run_loop(&mut self, entry: usize, start: usize, end: usize, step: usize) -> bool {
        // use std::io::Read;
        // let mut stdin = io::stdin();
        // self.write_code().unwrap();

        for depth in (start..end).into_iter().step_by(step) {
            write!(self.log, "try depth = {}...", depth).unwrap();

            self.reset();

            let mut state = State::new();

            let pnt = self.new_point(entry, self.idx_cnt, None);
            state.stack.push(pnt);
            let mut saves = Saves::new();
            saves.push(&mut self.sol, state);

            let res = self.run_saves(depth, &mut saves);

            if let Some(steps) = res {
                writeln!(
                    self.log,
                    "fail. depth = {}, step = {}, total = {}",
                    depth, steps, self.total_step
                )
                .unwrap();
            } else {
                writeln!(self.log, "success! depth = {}", depth).unwrap();
                return true;
            }
        }

        false
    }
}

#[test]
fn test_walker() {
    use crate::logic::ast::*;
    use crate::utils::ident::Ident;

    let src: &'static str = r#"
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

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if @icmpeq(head, x) then true else is_elem(tail, x)
    | Nil => false
    end
end

predicate is_elem_after_append(xs: IntList, x: Int)
begin
    is_elem(append(xs, x), x) = false
end
    "#;

    let (prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let dict = crate::logic::transform::prog_to_dict(&prog);
    // println!("{:#?}", dict);
    let (codes, map) = super::compile::compile_dict(&dict);
    // println!("{:?}", map);

    let mut log = std::io::empty();
    let mut wlk = Walker::new(codes, &mut log);
    let entry = map[&PredIdent::Pos(Ident::dummy(&"is_elem_after_append"))];
    assert!(!wlk.run_loop(entry, 10, 100, 10))
}
