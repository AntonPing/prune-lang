use super::*;
use std::collections::HashMap;
use std::{fmt, io};

use super::compile::ByteCode;
use crate::solver::solver::Solver;
use crate::utils::lit::LitType;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum StateResult {
    Running,
    Succ,
    Fail,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PathOp {
    Push(usize),
    Pop(usize),
}

#[derive(Clone, Debug)]
pub struct State {
    code: usize,
    idx: usize,
    fuel: usize,
    stack: Vec<(usize, usize)>,
    path: Vec<PathOp>,
}

impl State {
    fn new() -> State {
        State {
            code: 0,
            idx: 0,
            stack: Vec::new(),
            path: Vec::new(),
            fuel: 0,
        }
    }

    fn reset(&mut self, entry: usize, fuel: usize) {
        self.code = entry;
        self.idx = 0;
        self.stack.drain(..);
        self.path.drain(..);
        self.fuel = fuel;
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stack = self.stack.iter().format(&",");
        let path = self.path.iter().format(&",");
        writeln!(
            f,
            "code = {}, fuel = {}, stack = [{:?}]",
            self.code, self.fuel, stack
        )?;
        writeln!(f, "path = [{:?}]", path)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Walker {
    codes: Vec<ByteCode>,
    counter: usize,
    state: State,
    saves: Vec<State>,
    sol: Solver,
}

impl Walker {
    pub fn new(codes: Vec<ByteCode>, map: HashMap<Ident, LitType>) -> Walker {
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

    pub fn write_code_window<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        let start = std::cmp::max(self.state.code, 2) - 2;
        let end = std::cmp::min(self.state.code, self.codes.len() - 3) + 3;

        for (i, code) in self.codes[start..end].iter().enumerate() {
            let i = i + start;

            if i == self.state.code {
                writeln!(f, "{:03}: >>> {}", &i, &code)?;
            } else {
                writeln!(f, "{:03}: {}", &i, &code)?;
            }
        }
        Ok(())
    }

    pub fn write_state<W: io::Write>(&self, state: &State, f: &mut W) -> io::Result<()> {
        let stack = state
            .stack
            .iter()
            .map(|cp| match self.codes[cp.0 - 1] {
                ByteCode::Call(func, _) => func,
                _ => panic!("code pointer in stack should refer to a call instruction!"),
            })
            .format(&", ");

        let path = state
            .path
            .iter()
            .map(|op| match op {
                PathOp::Push(cp) => match self.codes[*cp - 1] {
                    ByteCode::Call(func, _) => format!("Push({})", func),
                    _ => panic!("code pointer in stack should refer to a call instruction!"),
                },
                PathOp::Pop(cp) => match self.codes[*cp - 1] {
                    ByteCode::Call(func, _) => format!("Pop({})", func),
                    _ => panic!("code pointer in stack should refer to a call instruction!"),
                },
            })
            .format(&", ");

        writeln!(
            f,
            "code = {}, fuel = {}, stack = [{}]",
            state.code, state.fuel, stack
        )?;

        writeln!(f, "path = [{}]", path)?;

        Ok(())
    }

    pub fn write_solver_state<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        self.write_state(&self.state, f)
    }

    pub fn write_solver_saves<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        for state in self.saves.iter().rev() {
            self.write_state(state, f)?;
        }

        Ok(())
    }

    pub fn write_solver<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        writeln!(f, "{}", self.sol)
    }
}

impl Walker {
    pub fn run_step(&mut self) -> StateResult {
        let code = &self.codes[self.state.code];
        match code {
            ByteCode::Unify(lhs, rhs) => {
                let lhs = lhs.tag_ctx(self.state.idx);
                let rhs = rhs.var_map_func(&|x| x.tag_ctx(self.state.idx));
                if self.sol.unify(Term::Var(lhs), rhs).is_err() {
                    return self.backtrack();
                }
            }
            ByteCode::Solve(prim, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.var_map_func(&|x| x.tag_ctx(self.state.idx)))
                    .collect();
                if self.sol.solve(*prim, args).is_err() {
                    return self.backtrack();
                }
            }
            ByteCode::CondStart => {}
            ByteCode::BranchSave(cp) => {
                let old_cp = self.state.code;
                self.state.code = *cp;
                self.savepoint();
                self.state.code = old_cp;
            }
            ByteCode::BranchJump(cp) => {
                self.state.code = *cp;
                return StateResult::Running;
            }
            ByteCode::CondEnd => {}
            ByteCode::BranchFail => {
                return self.backtrack();
            }
            ByteCode::SetArg(x, term) => {
                let lhs = Term::Var(x.tag_ctx(self.counter + 1));
                let rhs = term.var_map_func(&|x| x.tag_ctx(self.state.idx));
                self.sol.unify(lhs, rhs).unwrap(); // SetArg cannot fail
            }
            ByteCode::Label(_label) => {}
            ByteCode::Call(_func, cp) => {
                let push_code = self.state.code + 1;
                if self.state.fuel >= 1 {
                    self.state.fuel -= 1;
                    self.state.stack.push((push_code, self.state.idx));
                    self.counter += 1;
                    self.state.idx = self.counter;
                    self.state.path.push(PathOp::Push(push_code));
                    self.state.code = *cp;
                    return StateResult::Running;
                } else {
                    return self.backtrack();
                }
            }
            ByteCode::Ret => {
                if self.state.stack.is_empty() {
                    return StateResult::Succ;
                } else {
                    let (pop_code, idx) = self.state.stack.pop().unwrap();
                    self.state.code = pop_code;
                    self.state.idx = idx;
                    self.state.path.push(PathOp::Pop(pop_code));
                    return StateResult::Running;
                }
            }
        }
        self.state.code += 1;
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
                        // self.write_solver_state(&mut stdout).unwrap();
                        // self.write_solver_saves(&mut stdout).unwrap();
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
    let (codes, entrys) = super::compile::compile_dict(&dict);
    let map = crate::logic::infer::infer_type_map(&dict);
    println!("{:?}", map);
    let mut wlk = Walker::new(codes, map);
    let entry = entrys[&PredIdent::Check(Ident::dummy(&"is_elem_after_append"))];
    assert!(!wlk.run_loop(entry, 3, 10, 1))
}
