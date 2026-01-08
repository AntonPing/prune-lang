use crate::utils::unify::Unifier;

use super::*;

#[derive(Clone, Debug)]
struct RuleWithEqs {
    rule: Rule,
    eqs: Vec<(TermId, TermId)>,
}

fn normalize_goal(goal: &Goal, stack: &mut Vec<RuleWithEqs>) {
    match goal {
        Goal::Lit(_) => panic!("no literal goal after optimization!"),
        Goal::Eq(lhs, rhs) => {
            if let Some(brch) = stack.last_mut() {
                brch.eqs.push((lhs.clone(), rhs.clone()));
            }
        }
        Goal::Prim(prim, args) => {
            if let Some(brch) = stack.last_mut() {
                brch.rule.prims.push((*prim, args.clone()));
            }
        }
        Goal::And(goals) => {
            if !stack.is_empty() {
                for goal in goals {
                    normalize_goal(goal, stack);
                }
            }
        }
        Goal::Or(goals) => {
            if let Some(brch) = stack.pop() {
                for goal in goals {
                    stack.push(brch.clone());
                    normalize_goal(goal, stack);
                }
            }
        }
        Goal::Call(pred, polys, args) => {
            if let Some(brch) = stack.last_mut() {
                brch.rule.calls.push((*pred, polys.clone(), args.clone()));
            }
        }
    }
}

fn solve_branch(brch: RuleWithEqs) -> Option<Rule> {
    let mut unifier: Unifier<Ident, LitVal, OptCons<Ident>> = Unifier::new();

    for (lhs, rhs) in brch.eqs.iter() {
        if unifier.unify(&lhs, &rhs).is_err() {
            return None; // unsat branch!
        }
    }

    let mut rule = brch.rule;

    for term in rule.head.iter_mut() {
        *term = unifier.merge(&term);
    }

    for (_prim, args) in rule.prims.iter_mut() {
        for arg in args {
            *arg = unifier.merge(&arg.to_term()).to_atom().unwrap();
        }
    }

    for (_pred, _polys, args) in rule.calls.iter_mut() {
        for arg in args {
            *arg = unifier.merge(&arg);
        }
    }

    Some(rule)
}

fn normalize_pred(pred: &mut PredDecl) {
    let head = pred
        .pars
        .iter()
        .map(|(par, _typ)| Term::Var(*par))
        .collect();

    let mut stack = vec![RuleWithEqs {
        rule: Rule {
            head,
            prims: Vec::new(),
            calls: Vec::new(),
        },
        eqs: Vec::new(),
    }];
    normalize_goal(&pred.goal, &mut stack);

    let rules = stack
        .into_iter()
        .flat_map(|brch| solve_branch(brch))
        .collect();

    pred.rules = rules;
}

pub fn normalize_pass(prog: &mut Program) {
    for (_, pred) in prog.preds.iter_mut() {
        normalize_pred(pred);
    }
}

#[test]
// #[ignore = "just to see result"]
fn normalize_pass_test() {
    let src: &'static str = r#"
datatype List[a] where
| Cons(a, List[a])
| Nil
end

function id[a](x: a) -> a
begin
    x
end

function append(xs: List[Int], x: Int) -> List[Int]
begin
    match xs with
    | Cons(head, tail) =>
        Cons(head, append(tail, id(x)))
    | Nil => Cons(x, Nil)
    end
end
"#;
    let (prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let mut prog = super::transform::logic_translation(&prog);
    println!("{:#?}", prog);

    crate::logic::elab::elab_pass(&mut prog);
    println!("{:#?}", prog);

    normalize_pass(&mut prog);
    println!("{:#?}", prog);
}
