use super::*;

pub fn goal_flatten(goal: Goal) -> Goal {
    match goal {
        Goal::And(goals) => {
            let mut vec = Vec::new();
            for goal in goals {
                let goal = goal_flatten(goal);
                match goal {
                    Goal::Lit(true) => {}
                    Goal::Lit(false) => return Goal::Lit(false),
                    Goal::And(mut goals) => vec.append(&mut goals),
                    goal => vec.push(goal),
                }
            }
            match vec.len() {
                0 => Goal::Lit(true),
                1 => vec.into_iter().next().unwrap(),
                _ => Goal::And(vec),
            }
        }
        Goal::Or(goals) => {
            let mut vec = Vec::new();
            for goal in goals {
                let goal = goal_flatten(goal);
                match goal {
                    Goal::Lit(false) => {}
                    Goal::Lit(true) => return Goal::Lit(true),
                    Goal::Or(mut goals) => vec.append(&mut goals),
                    goal => vec.push(goal),
                }
            }
            match vec.len() {
                0 => Goal::Lit(false),
                1 => vec.into_iter().next().unwrap(),
                _ => Goal::Or(vec),
            }
        }
        goal => goal,
    }
}

pub fn goal_reorder(goal: Goal) -> Goal {
    goal_reorder_help(goal).0
}

fn goal_reorder_help(goal: Goal) -> (Goal, usize) {
    match goal {
        Goal::Lit(_) => (goal, 0),
        Goal::Eq(_, _) => (goal, 100),
        Goal::Cons(_, _, _) => (goal, 200),
        Goal::Prim(_, _) => (goal, 500),
        Goal::And(goals) => {
            let (goals, priors): (Vec<Goal>, Vec<usize>) = goals
                .into_iter()
                .map(|goal| goal_reorder_help(goal))
                .sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
                .unzip();

            (Goal::And(goals), priors.iter().sum())
        }
        Goal::Or(goals) => {
            let (goals, priors): (Vec<Goal>, Vec<usize>) = goals
                .into_iter()
                .map(|goal| goal_reorder_help(goal))
                .sorted_by(|x, y| Ord::cmp(&x.1, &y.1))
                .unzip();

            (Goal::Or(goals), priors.iter().max().unwrap_or(&0) + 1000)
        }
        Goal::Call(_, _) => (goal, 10000),
    }
}

pub fn goal_optimize(goal: Goal) -> Goal {
    let goal = goal_flatten(goal);
    let goal = goal_reorder(goal);
    goal
}
