use crate::utils::unify::Unifier;

use super::*;

#[derive(Clone, Debug)]
struct RuleWithEqs {
    rule: Rule,
    eqs: Vec<(TermVal, TermVal)>,
}

fn normalize_goal(goal: &Goal, mut brch: RuleWithEqs) -> Vec<RuleWithEqs> {
    match goal {
        Goal::Lit(_) => panic!("no literal goal after optimization!"),
        Goal::Eq(lhs, rhs) => {
            brch.eqs.push((lhs.clone(), rhs.clone()));
            vec![brch]
        }
        Goal::Prim(prim, args) => {
            brch.rule.prims.push((*prim, args.clone()));
            vec![brch]
        }
        Goal::And(goals) => {
            let mut brchs = vec![brch];
            for goal in goals {
                let mut new_brchs = Vec::new();
                for brch in brchs.into_iter() {
                    new_brchs.push(normalize_goal(goal, brch));
                }
                brchs = new_brchs.into_iter().flatten().collect();
            }
            brchs
        }
        Goal::Or(goals) => {
            let mut brchs = Vec::new();
            for goal in goals {
                brchs.push(normalize_goal(goal, brch.clone()));
            }
            brchs.into_iter().flatten().collect()
        }
        Goal::Call(pred, polys, args) => {
            brch.rule.calls.push((*pred, polys.clone(), args.clone()));
            vec![brch]
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

fn occurs_in_body<V: Copy + Eq>(rule: &Rule<V>, var: V) -> bool {
    for term in rule.head.iter() {
        if term.occurs(&var) {
            return true;
        }
    }

    for (_prim, args) in rule.prims.iter() {
        for arg in args.iter() {
            if arg.occurs(&var) {
                return true;
            }
        }
    }

    for (_pred, _polys, args) in rule.calls.iter() {
        for arg in args {
            if arg.occurs(&var) {
                return true;
            }
        }
    }

    false
}

fn normalize_pred(pred: &mut PredDecl) {
    let head = pred
        .pars
        .iter()
        .map(|(par, _typ)| Term::Var(*par))
        .collect();

    let init_brch = RuleWithEqs {
        rule: Rule {
            vars: pred.vars.clone(),
            head,
            prims: Vec::new(),
            calls: Vec::new(),
        },
        eqs: Vec::new(),
    };
    let brchs = normalize_goal(&pred.goal, init_brch);

    let mut rules: Vec<Rule> = brchs
        .into_iter()
        .flat_map(|brch| solve_branch(brch))
        .collect();

    for rule in rules.iter_mut() {
        rule.vars = rule
            .vars
            .iter()
            .filter(|(var, _typ)| occurs_in_body(&rule, *var))
            .cloned()
            .collect();
    }

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
