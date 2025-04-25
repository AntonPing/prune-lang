use super::smt_z3::Constr;
use super::subst::Subst;
use super::term::CtxAlloc;
use crate::solver_td::codes::*;

#[derive(Debug)]
pub struct Solver {
    saves: usize,
    codes: CodeState,
    ctx: CtxAlloc,
    subst: Subst,
    constr: Constr,
}

impl std::fmt::Display for Solver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "saves = {}", self.saves)?;
        writeln!(f, "{}", self.ctx)?;
        writeln!(f, "{}", self.codes)?;
        writeln!(f, "{}", self.subst)?;
        writeln!(f, "{}", self.constr)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum StateResult {
    Running,
    Succ,
    Fail,
}

impl Solver {
    pub fn new(codes: Vec<ByteCode>) -> Solver {
        Solver {
            saves: 0,
            codes: CodeState::new(codes),
            ctx: CtxAlloc::new(),
            subst: Subst::new(),
            constr: Constr::new(),
        }
    }

    pub fn savepoint(&mut self) {
        self.saves += 1;
        self.codes.savepoint();
        self.ctx.savepoint();
        self.subst.savepoint();
        self.constr.savepoint();
    }

    pub fn backtrack(&mut self) -> StateResult {
        // println!("backtrack!");
        // println!("{}", self);
        if self.saves == 0 {
            StateResult::Fail
        } else {
            self.saves -= 1;
            self.codes.backtrack();
            self.ctx.backtrack();
            self.subst.backtrack();
            self.constr.backtrack();
            StateResult::Running
        }
    }

    pub fn run_step(&mut self) -> StateResult {
        let code = self.codes.next().clone();
        match code {
            ByteCode::Unify(lhs, rhs) => {
                let lhs = lhs.var_map_func(&|x| self.ctx.add_ctx(x));
                let rhs = rhs.var_map_func(&|x| self.ctx.add_ctx(x));
                if self.subst.unify(lhs, rhs).is_err() {
                    return self.backtrack();
                }
                for (x, term) in self.subst.bridge.drain(..) {
                    // println!("{} := {}", x, term);
                    self.constr.push_eq(x, term)
                }
                if !self.constr.solve() {
                    return self.backtrack();
                }
            }
            ByteCode::Solve(prim, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.var_map_func(&|x| self.ctx.add_ctx(x)))
                    .collect();
                self.constr.push_cons(prim, args);
                if !self.constr.solve() {
                    return self.backtrack();
                }
            }
            ByteCode::CondStart => {}
            ByteCode::BranchSave(cp) => {
                let old_cp = self.codes.get_cp();
                self.codes.jump(cp);
                self.savepoint();
                self.codes.jump(old_cp);
            }
            ByteCode::BranchJump(cp) => {
                self.codes.jump(cp);
            }
            ByteCode::CondEnd => {}
            ByteCode::BranchFail => {
                return self.backtrack();
            }
            ByteCode::SetArg(x, term) => {
                let x = self.ctx.add_next_ctx(&x);
                let term = term.var_map_func(&|x| self.ctx.add_ctx(x));
                self.subst.bind(x, term);
            }
            ByteCode::Label(_label) => {}
            ByteCode::Call(_func, cp) => {
                if self.codes.get_fuel() > 0 {
                    self.ctx.push();
                    self.codes.call(cp);
                } else {
                    return self.backtrack();
                }
            }
            ByteCode::Ret => {
                if self.codes.stack_empty() {
                    return StateResult::Succ;
                } else {
                    // println!("return!");
                    // println!("{}", self);
                    self.ctx.pop();
                    self.codes.ret();
                }
            }
        }
        StateResult::Running
    }

    pub fn run_loop(&mut self, entry: usize, start: usize, end: usize, step: usize) -> bool {
        // use std::io::{self, Read, Write};
        // let mut stdin = io::stdin();
        // let mut stdout = io::stdout();

        // self.codes.print_code();

        for fuel in (start..end).into_iter().step_by(step) {
            // println!("try fuel = {fuel}");
            assert_eq!(self.saves, 0);
            self.codes.reset(entry, fuel);
            self.ctx = CtxAlloc::new();
            self.subst = Subst::new();
            self.constr = Constr::new();
            
            loop {
                // println!("{}", self);
                match self.run_step() {
                    StateResult::Running => {
                        // write!(stdout, "Press any key to continue...\n").unwrap();
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
fn state_run_test() {
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
    let mut state = Solver::new(codes);
    let entry = map[&PredIdent::Check(Ident::dummy(&"is_elem_after_append"))];
    state.run_loop(entry, 5, 10, 1);
}

#[test]
fn state_run_test2() {
    use crate::logic::trans::PredIdent;
    use crate::utils::ident::Ident;

    let p1: &'static str = r#"
datatype IntTree where
| Node(IntTree, Int, IntTree)
| Empty
end

function insert(tree: IntTree, x: Int) -> IntTree
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

datatype CheckSort where
| Left(Int)
| Right(Int)
| Root
end

function is_sorted(tree: AVLTree) -> Bool
begin
    is_sorted_help(tree, Root)
end

function is_sorted_help(tree: AVLTree, chk: CheckSort) -> Bool
begin
    match tree with
    | Node(left, y, right) =>
        let p = match chk with
            | Left(x1) => @icmplt(y, x1)
            | Right(x2) => @icmplt(x2, y)
            | Root => true
            end;
        if p then @band(is_sorted_help(left, Left(y)), is_sorted_help(right, Right(y)))
        else false
    | Empty => true
    end
end

predicate always_sorted(xs: IntList, x: Int)
begin
    and(
        is_sorted(xs) = true,
        is_sorted(insert(xs, x)) = false,
    )
end
    "#;

    let prog = crate::syntax::parser::parser::ProgramParser::new()
        .parse(&p1)
        .unwrap();
    let dict = crate::logic::trans::prog_to_dict(&prog);
    let (codes, map) = super::compile::compile_dict(&dict);
    let mut state = Solver::new(codes);
    let entry = map[&PredIdent::Check(Ident::dummy(&"always_sorted"))];
    state.run_loop(entry, 5, 10, 1);
}
