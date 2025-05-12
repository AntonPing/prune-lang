use super::*;
use std::{fmt, io};

use super::compile::ByteCode;
use super::indexer::Indexer;
use crate::solver::solver::Solver;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum StateResult {
    Running,
    Succ,
    Fail,
}

#[derive(Clone, Debug)]
pub struct State {
    pub code: usize,
    pub stack: Vec<usize>,
    pub fuel: usize,
}

impl State {
    fn new() -> State {
        State {
            code: 0,
            stack: Vec::new(),
            fuel: 0,
        }
    }

    fn reset(&mut self, entry: usize, fuel: usize) {
        self.code = entry;
        self.stack.drain(..);
        self.fuel = fuel;
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stack = self.stack.iter().format(&",");
        writeln!(f, "code = {}, [{}], fuel = {}", self.code, stack, self.fuel)
    }
}

#[derive(Debug)]
pub struct Walker {
    codes: Vec<ByteCode>,
    state: State,
    saves: Vec<State>,
    ctx: Indexer,
    sol: Solver<IdentIdx>,
}

impl Walker {
    pub fn new(codes: Vec<ByteCode>) -> Walker {
        Walker {
            codes,
            state: State::new(),
            saves: Vec::new(),
            ctx: Indexer::new(),
            sol: Solver::new(),
        }
    }

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

    pub fn write_state<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        let stack = self
            .state
            .stack
            .iter()
            .map(|cp| match self.codes[*cp - 1] {
                ByteCode::Call(func, _) => func,
                _ => panic!("code pointer in stack should refer to a call instruction!"),
            })
            .format(&", ");
        writeln!(
            f,
            "code = {}, [{}], fuel = {}",
            self.state.code, stack, self.state.fuel,
        )
    }

    pub fn write_saves<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        for state in self.saves.iter().rev() {
            let stack = state
                .stack
                .iter()
                .map(|cp| match self.codes[*cp - 1] {
                    ByteCode::Call(func, _) => func,
                    _ => panic!("code pointer in stack should refer to a call instruction!"),
                })
                .format(&", ");
            writeln!(
                f,
                "code = {}, [{}], fuel = {}",
                state.code, stack, state.fuel,
            )?;
        }

        Ok(())
    }

    pub fn write_solver<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        writeln!(f, "{}", self.sol)
    }
}

impl Walker {
    pub fn reset(&mut self, entry: usize, fuel: usize) {
        self.state.reset(entry, fuel);
        assert!(self.saves.is_empty());
        // todo: reset for CtxAlloc and Solver
        self.ctx = Indexer::new();
        self.sol = Solver::new();
    }

    pub fn run_step(&mut self) -> StateResult {
        let code = &self.codes[self.state.code];
        match code {
            ByteCode::Unify(lhs, rhs) => {
                let lhs = lhs.var_map_func(&|x| self.ctx.add_ctx(x));
                let rhs = rhs.var_map_func(&|x| self.ctx.add_ctx(x));
                if self.sol.unify(lhs, rhs).is_err() {
                    return self.backtrack();
                }
            }
            ByteCode::Solve(prim, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.var_map_func(&|x| self.ctx.add_ctx(x)))
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
                let lhs = Term::Var(self.ctx.add_next_ctx(&x));
                let rhs = term.var_map_func(&|x| self.ctx.add_ctx(x));
                self.sol.unify(lhs, rhs).unwrap(); // SetArg cannot fail
            }
            ByteCode::Label(_label) => {}
            ByteCode::Call(_func, cp) => {
                if self.state.fuel > 0 {
                    self.state.fuel -= 1;
                    self.ctx.push();
                    self.state.stack.push(self.state.code + 1);
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
                    self.ctx.pop();
                    self.state.code = self.state.stack.pop().unwrap();
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

    pub fn savepoint(&mut self) {
        self.saves.push(self.state.clone());
        self.ctx.savepoint();
        self.sol.savepoint();
    }

    pub fn backtrack(&mut self) -> StateResult {
        if self.saves.is_empty() {
            return StateResult::Fail;
        }
        self.state = self.saves.pop().unwrap();
        self.ctx.backtrack();
        self.sol.backtrack();
        StateResult::Running
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
    let mut wlk = Walker::new(codes);
    let entry = map[&PredIdent::Check(Ident::dummy(&"is_elem_after_append"))];
    assert!(!wlk.run_loop(entry, 3, 10, 1))
}
