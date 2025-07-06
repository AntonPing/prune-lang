use super::*;
use crate::logic::ast::*;

#[derive(Clone, Debug)]
pub enum LinearCode {
    Const(bool),
    Eq(Ident, Term<Ident>),
    Prim(Prim, Vec<Term<Ident>>),
    Conj(Vec<usize>),
    Disj(Vec<usize>),
    Label(PredIdent, Vec<Ident>),
    Call(PredIdent, Vec<Term<Ident>>, usize),
}

impl std::fmt::Display for LinearCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinearCode::Const(p) => write!(f, "Const({})", p),
            LinearCode::Eq(lhs, rhs) => write!(f, "Eq({}, {})", lhs, rhs),
            LinearCode::Prim(prim, args) => {
                let args = args.iter().format(&", ");
                write!(f, "Prim({:?}, {})", prim, args)
            }
            LinearCode::Conj(addrs) => write!(f, "Conj([{}])", addrs.iter().format(&",")),
            LinearCode::Disj(addrs) => write!(f, "Disj([{}])", addrs.iter().format(&",")),
            LinearCode::Label(label, pars) => {
                write!(f, "Label({}, [{}])", label, pars.iter().format(&","),)
            }
            LinearCode::Call(label, args, addr) => {
                write!(
                    f,
                    "Call({}, [{}], {})",
                    label,
                    args.iter().format(&","),
                    addr
                )
            }
        }
    }
}

struct CompileState {
    dict: HashMap<PredIdent, usize>,
    map: HashMap<usize, usize>,
    counter: usize,
    codes: Vec<LinearCode>,
}

impl CompileState {
    fn new() -> CompileState {
        CompileState {
            dict: HashMap::new(),
            map: HashMap::new(),
            counter: 1,
            codes: Vec::new(),
        }
    }

    fn compile_pred(&mut self, pred: &Predicate) {
        self.dict.insert(pred.name, self.codes.len());
        self.codes
            .push(LinearCode::Label(pred.name, pred.pars.clone()));
        self.compile_goal(&pred.goal);
    }

    fn compile_goal(&mut self, goal: &Goal) {
        match goal {
            Goal::Const(p) => {
                self.codes.push(LinearCode::Const(*p));
            }
            Goal::Eq(lhs, rhs) => {
                self.codes.push(LinearCode::Eq(lhs.clone(), rhs.clone()));
            }
            Goal::And(goals) => {
                if goals.is_empty() {
                    self.codes.push(LinearCode::Const(true));
                } else {
                    let base: usize = self.counter;
                    self.counter += goals.len();
                    self.codes
                        .push(LinearCode::Conj((base..self.counter).collect()));
                    for (offset, goal) in goals.iter().enumerate() {
                        self.map.insert(base + offset, self.codes.len());
                        // self.codes.push(LinearCode::Label(base + offset));
                        self.compile_goal(goal);
                    }
                }
            }
            Goal::Or(goals) => {
                if goals.is_empty() {
                    self.codes.push(LinearCode::Const(false));
                } else {
                    let base = self.counter;
                    self.counter += goals.len();
                    self.codes
                        .push(LinearCode::Disj((base..self.counter).collect()));
                    for (offset, goal) in goals.iter().enumerate() {
                        self.map.insert(base + offset, self.codes.len());
                        // self.codes.push(LinearCode::Label(base + offset));
                        self.compile_goal(goal);
                    }
                }
            }
            Goal::Prim(prim, args) => {
                self.codes.push(LinearCode::Prim(*prim, args.clone()));
            }
            Goal::PredCall(pred, args) => {
                self.codes.push(LinearCode::Call(*pred, args.clone(), 0));
            }
        }
    }

    fn remap_addr(&mut self) {
        for code in self.codes.iter_mut() {
            match code {
                LinearCode::Conj(addrs) => {
                    addrs.iter_mut().for_each(|addr| *addr = self.map[addr]);
                }
                LinearCode::Disj(addrs) => {
                    addrs.iter_mut().for_each(|addr| *addr = self.map[addr]);
                }
                LinearCode::Call(pred, _args, addr) => {
                    *addr = self.dict[pred];
                }
                _ => {}
            }
        }
    }
}

pub fn compile_dict(
    dict: &HashMap<PredIdent, Predicate>,
) -> (Vec<LinearCode>, HashMap<PredIdent, usize>) {
    let mut st = CompileState::new();
    for pred in dict.values() {
        st.compile_pred(pred);
    }
    st.remap_addr();
    (st.codes, st.dict)
}

#[test]
fn compile_pred_test() {
    use crate::syntax;
    let p1: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Nil
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
    let prog = syntax::parser::parser::ProgramParser::new()
        .parse(&p1)
        .unwrap();
    let dict = crate::logic::transform::prog_to_dict(&prog);
    let (codes, map) = compile_dict(&dict);
    for (i, code) in codes.iter().enumerate() {
        println!("{:03}: {}", &i, &code);
    }
    println!("{:?}", map);
}
