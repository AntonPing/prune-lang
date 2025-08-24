use super::*;
use crate::logic::ast::*;

#[derive(Clone, Debug)]
pub enum LinearCode {
    Lit(bool),
    Eq(Ident, AtomId),
    Cons(Ident, Ident, Vec<AtomId>),
    Prim(Prim, Vec<AtomId>),
    And(Vec<usize>),
    Or(Vec<usize>),
    Call(PredIdent, Vec<AtomId>, usize),
    Label(PredIdent, Vec<Ident>, Vec<Ident>),
}

impl LinearCode {
    pub fn tag(&self) -> i32 {
        match self {
            LinearCode::Lit(_) => 4,
            LinearCode::Eq(_, _) => 4,
            LinearCode::Cons(_, _, _) => 4,
            LinearCode::Prim(_, _) => 4,
            LinearCode::Call(_, _, _) => 2,
            LinearCode::And(_) => 3,
            LinearCode::Or(_) => 1,
            LinearCode::Label(_, _, _) => 5,
        }
    }
}

impl std::fmt::Display for LinearCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinearCode::Lit(p) => write!(f, "Lit({})", p),
            LinearCode::Eq(var, atom) => write!(f, "Eq({}, {})", var, atom),
            LinearCode::Cons(var, cons, flds) => {
                let flds = flds.iter().format(&", ");
                write!(f, "Cons({}, {}, {})", var, cons, flds)
            }
            LinearCode::Prim(prim, args) => {
                let args = args.iter().format(&", ");
                write!(f, "Prim({:?}, {})", prim, args)
            }
            LinearCode::And(addrs) => write!(f, "And([{}])", addrs.iter().format(&", ")),
            LinearCode::Or(addrs) => write!(f, "Or([{}])", addrs.iter().format(&", ")),
            LinearCode::Call(label, args, addr) => {
                write!(
                    f,
                    "Call({}, [{}], {})",
                    label,
                    args.iter().format(&", "),
                    addr
                )
            }
            LinearCode::Label(label, pars, vars) => {
                write!(
                    f,
                    "Label({}, [{}], [{}])",
                    label,
                    pars.iter().format(&", "),
                    vars.iter().format(&", ")
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
        self.codes.push(LinearCode::Label(
            pred.name,
            pred.pars.clone(),
            pred.vars.clone(),
        ));
        self.compile_goal(&pred.goal);
    }

    fn compile_goal(&mut self, goal: &Goal) {
        match goal {
            Goal::Lit(p) => {
                self.codes.push(LinearCode::Lit(*p));
            }
            Goal::Eq(var, atom) => {
                self.codes.push(LinearCode::Eq(*var, atom.clone()));
            }
            Goal::Cons(var, cons, flds) => {
                self.codes.push(LinearCode::Cons(*var, *cons, flds.clone()));
            }
            Goal::Prim(prim, args) => {
                self.codes.push(LinearCode::Prim(*prim, args.clone()));
            }
            Goal::And(goals) => {
                if goals.is_empty() {
                    self.codes.push(LinearCode::Lit(true));
                } else {
                    let base: usize = self.counter;
                    self.counter += goals.len();
                    self.codes
                        .push(LinearCode::And((base..self.counter).collect()));
                    for (offset, goal) in goals.iter().enumerate() {
                        self.map.insert(base + offset, self.codes.len());
                        // self.codes.push(LinearCode::Label(base + offset));
                        self.compile_goal(goal);
                    }
                }
            }
            Goal::Or(goals) => {
                if goals.is_empty() {
                    self.codes.push(LinearCode::Lit(false));
                } else {
                    let base = self.counter;
                    self.counter += goals.len();
                    self.codes
                        .push(LinearCode::Or((base..self.counter).collect()));
                    for (offset, goal) in goals.iter().enumerate() {
                        self.map.insert(base + offset, self.codes.len());
                        // self.codes.push(LinearCode::Label(base + offset));
                        self.compile_goal(goal);
                    }
                }
            }
            Goal::Call(pred, args) => {
                self.codes.push(LinearCode::Call(*pred, args.clone(), 0));
            }
        }
    }

    fn remap_addr(&mut self) {
        for code in self.codes.iter_mut() {
            match code {
                LinearCode::And(addrs) => {
                    addrs.iter_mut().for_each(|addr| *addr = self.map[addr]);
                }
                LinearCode::Or(addrs) => {
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
    let src: &'static str = r#"
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
    let (prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let dict = crate::logic::transform::prog_to_dict(&prog);
    let (codes, map) = compile_dict(&dict);
    for (i, code) in codes.iter().enumerate() {
        println!("{:03}: {}", &i, &code);
    }
    println!("{:?}", map);
}
