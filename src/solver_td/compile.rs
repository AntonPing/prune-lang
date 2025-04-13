use std::collections::HashMap;

use super::codes::ByteCode;
use crate::logic::trans::*;

use super::*;

pub fn compile_dict(
    dict: &HashMap<PredIdent, Predicate>,
) -> (Vec<ByteCode>, HashMap<PredIdent, usize>) {
    let mut codes = Vec::new();
    for pred in dict.values() {
        compile_pred(pred, &mut codes);
    }
    link_branch_addr(&mut codes);
    let map = link_call_addr(&mut codes);
    link_args(&mut codes, dict);
    (codes, map)
}

pub fn compile_pred(pred: &Predicate, codes: &mut Vec<ByteCode>) {
    codes.push(ByteCode::Label(pred.name));
    compile_form(&pred.form, codes);
    codes.push(ByteCode::Ret);
}

pub fn compile_form(form: &Formula, codes: &mut Vec<ByteCode>) {
    match form {
        Formula::Const(true) => {}
        Formula::Const(false) => {
            codes.push(ByteCode::BranchFail);
        }
        Formula::Eq(lhs, rhs) => {
            codes.push(ByteCode::Unify(lhs.clone(), rhs.clone()));
        }
        Formula::And(forms) => {
            for form in forms {
                compile_form(form, codes);
            }
        }
        Formula::Or(forms) => {
            if forms.is_empty() {
                codes.push(ByteCode::BranchFail);
            } else {
                codes.push(ByteCode::CondStart);
                for form in forms {
                    codes.push(ByteCode::BranchSave(0));
                    compile_form(form, codes);
                    codes.push(ByteCode::BranchJump(0));
                }
                codes.push(ByteCode::BranchFail);
                codes.push(ByteCode::CondEnd);
            }
        }
        Formula::Prim(prim, args) => {
            codes.push(ByteCode::Solve(*prim, args.clone()));
        }
        Formula::PredCall(func, args) => {
            for arg in args {
                codes.push(ByteCode::SetArg(Ident::dummy(&"arg"), arg.clone()));
            }
            codes.push(ByteCode::Call(*func, 0));
        }
    }
}

pub fn link_branch_addr(codes: &mut Vec<ByteCode>) {
    let mut last_branch = Vec::new();
    let mut end_points = Vec::new();
    for (i, code) in codes.iter_mut().enumerate().rev() {
        match code {
            ByteCode::CondStart => {
                end_points.pop().unwrap();
                last_branch.pop().unwrap();
            }
            ByteCode::BranchSave(cp) => {
                *cp = *last_branch.last().unwrap();
                *last_branch.last_mut().unwrap() = i;
            }
            ByteCode::BranchJump(cp) => {
                *cp = *end_points.last().unwrap();
            }
            ByteCode::BranchFail => {
                last_branch.push(i);
            }
            ByteCode::CondEnd => {
                end_points.push(i);
            }
            _ => {}
        }
    }
}

pub fn link_call_addr(codes: &mut Vec<ByteCode>) -> HashMap<PredIdent, usize> {
    let mut label_map = HashMap::new();
    for (i, code) in codes.iter().enumerate() {
        if let ByteCode::Label(label) = code {
            label_map.insert(*label, i);
        }
    }
    for code in codes.iter_mut() {
        if let ByteCode::Call(label, cp) = code {
            *cp = label_map[label];
        }
    }
    label_map
}

pub fn link_args(codes: &mut Vec<ByteCode>, dict: &HashMap<PredIdent, Predicate>) {
    let mut pars_vec = Vec::new();
    for code in codes.iter_mut().rev() {
        match code {
            ByteCode::SetArg(x, _term) => {
                *x = pars_vec.pop().unwrap();
            }
            ByteCode::Call(label, _cp) => {
                assert!(pars_vec.is_empty());
                pars_vec = dict[label].pars.clone();
            }
            _ => {}
        }
    }
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
    let dict = prog_to_dict(&prog);
    let (codes, _map) = compile_dict(&dict);
    for (i, code) in codes.iter().enumerate() {
        println!("{:03}: {}", &i, &code);
    }
}
