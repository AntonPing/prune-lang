use super::*;
use std::{fmt, io};

use super::compile::LinearCode;
use crate::solver::solver::Solver;
use crate::utils::lit::LitType;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum StateResult {
    Running,
    Succ,
    Fail,
}

#[derive(Clone, Debug)]
pub struct State {
    fuel: usize,
    stack: Vec<(usize, usize)>,
}

impl State {
    fn new() -> State {
        State {
            fuel: 0,
            stack: Vec::new(),
        }
    }

    fn reset(&mut self, entry: usize, fuel: usize) {
        self.stack.drain(..);
        self.stack.push((entry + 1, 0));
        self.fuel = fuel;
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stack = self.stack.iter().format(&",");
        writeln!(f, "fuel = {}, stack = [{:?}]", self.fuel, stack)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Walker {
    codes: Vec<LinearCode>,
    counter: usize,
    state: State,
    saves: Vec<State>,
    sol: Solver,
}

impl Walker {
    pub fn new(codes: Vec<LinearCode>, map: HashMap<Ident, LitType>) -> Walker {
        Walker {
            codes,
            counter: 0,
            state: State::new(),
            saves: Vec::new(),
            sol: Solver::new(map),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.saves.is_empty() && self.sol.is_empty()
    }

    pub fn reset(&mut self, entry: usize, fuel: usize) {
        self.counter = 0;
        self.state.reset(entry, fuel);
        self.sol.reset();
    }

    pub fn savepoint(&mut self) {
        self.saves.push(self.state.clone());
        self.sol.savepoint();
    }

    pub fn backtrack(&mut self) -> StateResult {
        if self.saves.is_empty() {
            return StateResult::Fail;
        }
        self.state = self.saves.pop().unwrap();
        self.sol.backtrack();
        StateResult::Running
    }
}

impl Walker {
    pub fn write_code<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        for (i, code) in self.codes.iter().enumerate() {
            writeln!(f, "{:03}: {}", &i, &code)?;
        }
        Ok(())
    }

    pub fn write_code_window<W: io::Write>(&self, f: &mut W, addr: usize) -> io::Result<()> {
        let start = std::cmp::max(addr, 2) - 2;
        let end = std::cmp::min(addr, self.codes.len() - 3) + 3;

        for (i, code) in self.codes[start..end].iter().enumerate() {
            let i = i + start;

            if i == addr {
                writeln!(f, "{:03}: >>> {}", &i, &code)?;
            } else {
                writeln!(f, "{:03}: {}", &i, &code)?;
            }
        }
        Ok(())
    }

    pub fn write_state<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        writeln!(f, "{}", self.state)
    }

    pub fn write_saves<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        for state in self.saves.iter().rev() {
            writeln!(f, "{}", state)?;
        }

        Ok(())
    }

    pub fn write_solver<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        writeln!(f, "{}", self.sol)
    }
}

impl Walker {
    pub fn run_step(&mut self) -> StateResult {
        if self.state.stack.is_empty() {
            return StateResult::Succ;
        }

        let (addr, idx) = self.state.stack.pop().unwrap();
        let code = &self.codes[addr];
        match code {
            LinearCode::Const(true) => {}
            LinearCode::Const(false) => {
                return self.backtrack();
            }
            LinearCode::Eq(lhs, rhs) => {
                let lhs = lhs.tag_ctx(idx);
                let rhs = rhs.var_map_func(&|x| x.tag_ctx(idx));
                if self.sol.unify(Term::Var(lhs), rhs).is_err() {
                    return self.backtrack();
                }
            }
            LinearCode::Prim(prim, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.var_map_func(&|x| x.tag_ctx(idx)))
                    .collect();
                if self.sol.solve(*prim, args).is_err() {
                    return self.backtrack();
                }
            }
            LinearCode::Conj(addrs) => {
                for addr in addrs.clone().into_iter().rev() {
                    self.state.stack.push((addr, idx));
                }
            }
            LinearCode::Disj(addrs) => {
                for addr in addrs.clone().into_iter().rev() {
                    self.state.stack.push((addr, idx));
                    self.savepoint();
                    self.state.stack.pop();
                }
                return self.backtrack();
            }
            LinearCode::Label(_pred, _args) => {
                panic!("the interpreter can not execute a label!");
            }
            LinearCode::Call(_pred, args, addr) => {
                if let LinearCode::Label(_pred, pars) = &self.codes[*addr] {
                    let cost = self.state.stack.len();
                    if self.state.fuel >= cost {
                        self.state.fuel -= cost;
                        self.counter += 1;
                        assert_eq!(pars.len(), args.len());
                        for (par, arg) in pars.iter().zip(args.iter()) {
                            let lhs = Term::Var(par.tag_ctx(self.counter));
                            let rhs = arg.var_map_func(&|x| x.tag_ctx(idx));
                            self.sol.unify(lhs, rhs).unwrap(); // unify with a fresh variable cannot fail
                        }
                        self.state.stack.push((addr + 1, self.counter));
                        return StateResult::Running;
                    } else {
                        return self.backtrack();
                    }
                } else {
                    panic!("addr of call not reference to a label!");
                }
            }
        }
        StateResult::Running
    }

    pub fn run_loop(&mut self, entry: usize, start: usize, end: usize, step: usize) -> bool {
        // use std::io::{self, Read, Write};
        // let mut stdin = io::stdin();
        // let mut stdout = io::stdout();
        // self.write_code(&mut stdout).unwrap();

        for fuel in (start..end).into_iter().step_by(step) {
            // println!("try fuel = {fuel}");
            assert!(self.is_empty());
            self.reset(entry, fuel);

            loop {
                // println!("{}", self);
                match self.run_step() {
                    StateResult::Running => {
                        // self.write_code_window(&mut stdout).unwrap();
                        // self.write_state(&mut stdout).unwrap();
                        // self.write_saves(&mut stdout).unwrap();
                        // self.write_solver(&mut stdout).unwrap();
                        // write!(stdout, "Press any key to continue...\n\n").unwrap();
                        // stdout.flush().unwrap();
                        // let _ = stdin.read(&mut [0u8]).unwrap();
                    }
                    StateResult::Succ => {
                        // println!("successed to find solution!");
                        // println!("{}", self);
                        return true;
                    }
                    StateResult::Fail => {
                        // println!("failed to find any solution!");
                        // println!("{}", self);
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
    use crate::logic::trans::PredIdent;
    use crate::utils::ident::Ident;

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

    let prog = crate::syntax::parser::parser::ProgramParser::new()
        .parse(&p1)
        .unwrap();
    let dict = crate::logic::trans::prog_to_dict(&prog);
    let (codes, map) = super::compile::compile_dict(&dict);
    let ty_map = crate::logic::infer::infer_type_map(&dict);
    println!("{:?}", map);
    println!("{:?}", ty_map);

    let mut stdout = io::stdout();

    let mut wlk = Walker::new(codes, ty_map);
    wlk.write_code(&mut stdout).unwrap();

    let entry = map[&PredIdent::Check(Ident::dummy(&"is_elem_after_append"))];
    assert!(!wlk.run_loop(entry, 3, 10, 1))
}
