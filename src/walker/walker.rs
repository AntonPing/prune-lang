use super::*;
use std::{fmt, io};

use super::compile::LinearCode;
use crate::solver::solver::Solver;

use super::vsids::*;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum StateResult {
    Running,
    Succ,
    Fail,
}

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
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stack = self.stack.iter().format(&", ");
        writeln!(f, "cost = {}, stack = [{}]", self.cost, stack)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Walker<'log, Log: io::Write> {
    codes: Vec<LinearCode>,
    fuel: usize,
    step: usize,
    idx_cnt: usize,
    tmsp_cnt: usize,
    state: State,
    saves: Vec<State>,
    sol: Solver,
    log: &'log mut Log,
}

impl<'log, Log: io::Write> Walker<'log, Log> {
    pub fn write_code(&mut self) -> io::Result<()> {
        for (i, code) in self.codes.iter().enumerate() {
            writeln!(self.log, "{:03}: {}", &i, &code)?;
        }
        Ok(())
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

    pub fn write_state(&mut self) -> io::Result<()> {
        writeln!(self.log, "{}", self.state)
    }

    pub fn write_saves(&mut self) -> io::Result<()> {
        for state in self.saves.iter().rev() {
            writeln!(self.log, "{}", state)?;
        }

        Ok(())
    }

    pub fn write_solver(&mut self) -> io::Result<()> {
        writeln!(self.log, "{}", self.sol)
    }
}

impl<'log, Log: io::Write> Walker<'log, Log> {
    pub fn new(codes: Vec<LinearCode>, log: &'log mut Log) -> Walker<'log, Log> {
        Walker {
            codes,
            fuel: 0,
            step: 0,
            idx_cnt: 0,
            tmsp_cnt: 0,
            state: State::new(),
            saves: Vec::new(),
            sol: Solver::new(),
            log,
        }
    }

    fn is_empty(&self) -> bool {
        self.saves.is_empty() && self.sol.is_empty()
    }

    fn reset(&mut self, entry: usize, fuel: usize) {
        self.fuel = fuel;
        self.step = 0;
        self.idx_cnt = 0;
        self.tmsp_cnt = 0;
        self.state.stack.drain(..);
        let pnt = self.new_point(entry, 0, None);
        self.state.stack.push(pnt);
        self.state.cost = 0;
        self.sol.reset();
    }

    fn savepoint(&mut self) {
        self.saves.push(self.state.clone());
        self.sol.savepoint();
    }

    fn backtrack(&mut self) -> StateResult {
        if self.saves.is_empty() {
            return StateResult::Fail;
        }
        self.state = self.saves.pop().unwrap();
        self.state
            .stack
            .iter()
            .for_each(|pnt| pnt.update_decay(self.tmsp_cnt));
        self.state.stack.sort_by_key(|pnt| pnt.get_prior());
        self.sol.backtrack();
        StateResult::Running
    }

    fn update_backtrack(&mut self, pnt: Point) -> StateResult {
        pnt.update_bump_upward(self.tmsp_cnt);
        self.tmsp_cnt += 1;
        self.backtrack()
    }

    fn new_point(&self, addr: usize, idx: usize, pred: Option<Point>) -> Point {
        let tag = match &self.codes[addr] {
            LinearCode::Lit(_) => 4,
            LinearCode::Eq(_, _) => 4,
            LinearCode::Cons(_, _, _) => 4,
            LinearCode::Prim(_, _) => 4,
            LinearCode::Call(_, _, _) => 2,
            LinearCode::And(_) => 3,
            LinearCode::Or(_) => 1,
            LinearCode::Label(_, _, _) => 5,
        };
        Point::new(addr, idx, Priority::new(tag, self.tmsp_cnt), pred)
    }

    fn push_point(&mut self, pnt: Point) {
        let pos = self
            .state
            .stack
            .binary_search_by_key(&pnt.get_prior(), |pnt| pnt.get_prior())
            .unwrap_or_else(|e| e);
        self.state.stack.insert(pos, pnt);
    }

    fn pop_point(&mut self) -> Point {
        self.state.stack.pop().unwrap()
    }

    pub fn run_step(&mut self) -> StateResult {
        if self.state.stack.is_empty() {
            return StateResult::Succ;
        }

        self.state.cost += 1;
        if self.state.cost > self.fuel {
            return self.backtrack();
        }
        self.step += 1;

        let curr_pnt = self.pop_point();
        let (addr, idx) = curr_pnt.get_addr_idx();
        let code = &self.codes[addr];
        match code {
            LinearCode::Lit(true) => {}
            LinearCode::Lit(false) => {
                return self.update_backtrack(curr_pnt);
            }
            LinearCode::Eq(var, atom) => {
                let var = var.tag_ctx(idx);
                let atom = atom.tag_ctx(idx);
                if self.sol.unify(Term::Var(var), atom.to_term()).is_err() {
                    return self.update_backtrack(curr_pnt);
                }
            }
            LinearCode::Cons(var, cons, flds) => {
                let var = var.tag_ctx(idx);
                let flds = flds.iter().map(|fld| fld.tag_ctx(idx).to_term()).collect();
                if self
                    .sol
                    .unify(Term::Var(var), Term::Cons((), *cons, flds))
                    .is_err()
                {
                    return self.update_backtrack(curr_pnt);
                }
            }
            LinearCode::Prim(prim, args) => {
                let args = args.iter().map(|arg| arg.tag_ctx(idx).to_term()).collect();
                if self.sol.solve(*prim, args).is_err() {
                    return self.update_backtrack(curr_pnt);
                }
            }
            LinearCode::Call(_pred, args, addr) => {
                if let LinearCode::Label(_pred, pars, vars) = &self.codes[*addr] {
                    self.idx_cnt += 1;
                    assert_eq!(pars.len(), args.len());
                    for (par, arg) in pars.iter().zip(args.iter()) {
                        self.sol.declare(&par.tag_ctx(self.idx_cnt));
                        let par = Term::Var(par.tag_ctx(self.idx_cnt));
                        let arg = arg.tag_ctx(idx);
                        self.sol.unify(par, arg.to_term()).unwrap(); // unify with a fresh variable cannot fail
                    }
                    for var in vars {
                        self.sol.declare(&var.tag_ctx(self.idx_cnt));
                    }
                    let pnt = self.new_point(addr + 1, self.idx_cnt, Some(curr_pnt));
                    self.push_point(pnt);
                } else {
                    panic!("addr of call not reference to a label!");
                }
            }
            LinearCode::And(addrs) => {
                for addr in addrs.clone().into_iter().rev() {
                    let pnt = self.new_point(addr, idx, Some(curr_pnt.clone()));
                    self.push_point(pnt);
                }
            }
            LinearCode::Or(addrs) => {
                for addr in addrs.clone().into_iter().rev() {
                    let pnt = self.new_point(addr, idx, Some(curr_pnt.clone()));
                    self.state.stack.push(pnt);
                    self.savepoint();
                    self.state.stack.pop();
                }
                return self.backtrack();
            }
            LinearCode::Label(_pred, args, vars) => {
                assert!(self.state.stack.is_empty());
                assert_eq!(self.idx_cnt, 0);
                for arg in args {
                    self.sol.declare(&arg.tag_ctx(self.idx_cnt));
                }
                for var in vars {
                    self.sol.declare(&var.tag_ctx(self.idx_cnt));
                }
                let pnt = self.new_point(addr + 1, self.idx_cnt, Some(curr_pnt));
                self.push_point(pnt);
            }
        }
        StateResult::Running
    }

    pub fn run_loop(&mut self, entry: usize, start: usize, end: usize, step: usize) -> bool {
        // use std::io::Read;
        // let mut stdin = io::stdin();
        // self.write_code().unwrap();

        for fuel in (start..end).into_iter().step_by(step) {
            assert!(self.is_empty());
            self.reset(entry, fuel);

            loop {
                match self.run_step() {
                    StateResult::Running => {
                        // self.write_state().unwrap();
                        // self.write_saves().unwrap();
                        // self.write_solver().unwrap();
                        // write!(self.log, "Press any key to continue...\n\n").unwrap();
                        // self.log.flush().unwrap();
                        // let _ = stdin.read(&mut [0u8]).unwrap();
                    }
                    StateResult::Succ => {
                        writeln!(self.log, "success! fuel = {}, step = {}", fuel, self.step)
                            .unwrap();
                        return true;
                    }
                    StateResult::Fail => {
                        writeln!(self.log, "fail. fuel = {}, step = {}", fuel, self.step).unwrap();
                        break;
                    }
                }
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
