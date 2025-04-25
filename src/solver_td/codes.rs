use super::*;

#[derive(Clone, Debug)]
pub enum ByteCode {
    Unify(Term<Ident>, Term<Ident>),
    Solve(Prim, Vec<Term<Ident>>),
    CondStart,
    BranchSave(usize),
    BranchJump(usize),
    CondEnd,
    BranchFail,
    Label(PredIdent),
    SetArg(Ident, Term<Ident>),
    Call(PredIdent, usize),
    Ret,
}

impl std::fmt::Display for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteCode::Unify(lhs, rhs) => write!(f, "Unify({}, {})", lhs, rhs),
            ByteCode::Solve(prim, args) => {
                let args = args.iter().format(&", ");
                write!(f, "Solve({:?}, {})", prim, args)
            }
            ByteCode::CondStart => write!(f, "CondStart"),
            ByteCode::BranchSave(cp) => write!(f, "BranchSave({})", cp),
            ByteCode::BranchJump(cp) => write!(f, "BranchJump({})", cp),
            ByteCode::CondEnd => write!(f, "CondEnd"),
            ByteCode::BranchFail => write!(f, "Fail"),
            ByteCode::Label(label) => write!(f, "Label({})", label),
            ByteCode::SetArg(x, term) => write!(f, "SetArg({}, {})", x, term),
            ByteCode::Call(label, cp) => write!(f, "Call({}, {})", label, cp),
            ByteCode::Ret => write!(f, "Ret"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CodeState {
    codes: Vec<ByteCode>,
    code_ptr: usize,
    fuel: usize,
    stack: Vec<usize>,
    save: Vec<(Vec<usize>, usize)>,
}

impl std::fmt::Display for CodeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "code_ptr: {}", self.code_ptr)?;
        writeln!(f, "stack: {:?}", self.stack)?;
        writeln!(f, "program save: {:?}", self.save)?;
        Ok(())
    }
}

impl<'src> CodeState {
    pub fn new(codes: Vec<ByteCode>) -> CodeState {
        CodeState {
            codes,
            code_ptr: 0,
            fuel: 0,
            stack: Vec::new(),
            save: Vec::new(),
        }
    }

    pub fn print_code(&self) {
        for (i, code) in self.codes.iter().enumerate() {
            println!("{:03}: {}", &i, &code);
        }
    }

    pub fn get_cp(&self) -> usize {
        self.code_ptr
    }

    pub fn get_fuel(&self) -> usize {
        self.fuel
    }

    pub fn reset(&mut self, entry: usize, fuel: usize) {
        self.code_ptr = entry;
        self.fuel = fuel;
        self.stack.drain(..);
        assert!(self.save.is_empty());
    }

    pub fn next(&'src mut self) -> &'src ByteCode {
        let code = &self.codes[self.code_ptr];
        self.code_ptr += 1;
        code
    }

    pub fn jump(&mut self, cp: usize) {
        self.code_ptr = cp;
    }

    pub fn stack_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn call(&mut self, cp: usize) {
        assert!(self.fuel > 0);
        self.stack.push(self.code_ptr);
        self.code_ptr = cp;
        self.fuel -= 1;
    }

    pub fn ret(&mut self) {
        self.code_ptr = self.stack.pop().unwrap();
    }

    pub fn savepoint(&mut self) {
        self.save.push((self.stack.clone(), self.fuel));
        self.save.last_mut().unwrap().0.push(self.code_ptr);
    }

    pub fn backtrack(&mut self) {
        let res = self.save.pop().unwrap();
        self.stack = res.0;
        self.code_ptr = self.stack.pop().unwrap();
        self.fuel = res.1;
    }
}
