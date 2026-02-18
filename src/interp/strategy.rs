use super::*;

#[derive(Clone, Debug)]
pub struct Branch {
    pub depth: usize,
    pub answers: Vec<(Ident, TermVal<IdentCtx>)>,
    pub prims: Vec<(Prim, Vec<AtomVal<IdentCtx>>)>,
    pub calls: Vec<PredCall>,
}

impl Branch {
    pub fn new(pred: Ident, pars: Vec<Ident>, ctx_cnt: usize) -> Branch {
        let call = PredCall {
            pred,
            polys: Vec::new(),
            args: pars
                .iter()
                .map(|par| Term::Var(par.tag_ctx(ctx_cnt)))
                .collect(),
            history: History::new(),
        };

        Branch {
            depth: 0,
            answers: pars
                .iter()
                .map(|par| (*par, Term::Var(par.tag_ctx(ctx_cnt))))
                .collect(),
            prims: Vec::new(),
            calls: vec![call],
        }
    }

    pub fn clear_history(&mut self) {
        for call in self.calls.iter_mut() {
            call.history.clear();
        }
    }

    pub fn merge(&mut self, unifier: Unifier<IdentCtx, LitVal, OptCons<Ident>>) {
        for call in self.calls.iter_mut() {
            for arg in call.args.iter_mut() {
                *arg = unifier.merge(arg);
            }
        }

        for (_par, val) in self.answers.iter_mut() {
            *val = unifier.merge(val);
        }
    }

    pub fn insert(&mut self, call_idx: usize, call: PredCall) {
        self.calls.insert(call_idx, call);
    }

    pub fn remove(&mut self, call_idx: usize) -> PredCall {
        self.calls.remove(call_idx)
    }

    #[allow(unused)]
    pub fn random_strategy(&mut self) -> usize {
        assert!(!self.calls.is_empty());
        rand::random::<u32>().rem_euclid(self.calls.len() as u32) as usize
    }

    #[allow(unused)]
    pub fn left_biased_strategy(&mut self) -> usize {
        assert!(!self.calls.is_empty());
        0
    }

    #[allow(unused)]
    pub fn naive_strategy(&mut self, n: usize) -> usize {
        assert!(!self.calls.is_empty());

        let idx = self
            .calls
            .iter()
            .position(|call| call.history.naive_strategy_pred(n));

        if let Some(idx) = idx {
            idx
        } else {
            self.clear_history();
            self.naive_strategy(n)
        }
    }

    #[allow(unused)]
    pub fn struct_recur_strategy(&mut self) -> usize {
        assert!(!self.calls.is_empty());

        let idx = self.calls.iter().position(|call| {
            call.history
                .struct_recur_strategy_pred(call.pred, &call.args)
        });

        if let Some(idx) = idx {
            idx
        } else {
            self.clear_history();
            self.struct_recur_strategy()
        }
    }
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct PredCall {
    pub pred: Ident,
    pub polys: Vec<TermType>,
    pub args: Vec<TermVal<IdentCtx>>,
    pub history: History,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct HistoryNode {
    pred: Ident,
    args_size: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct History(Vec<HistoryNode>);

impl History {
    pub fn new() -> History {
        History(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push(&mut self, pred: Ident, args_size: Vec<usize>) {
        self.0.push(HistoryNode { pred, args_size });
    }

    pub fn left_biased_strategy_pred(&self) -> bool {
        true
    }

    pub fn naive_strategy_pred(&self, n: usize) -> bool {
        self.0.len() < n
    }

    pub fn struct_recur_strategy_pred(&self, pred: Ident, args: &[TermVal<IdentCtx>]) -> bool {
        let args_size: Vec<usize> = args.iter().map(|arg| arg.height()).collect();

        for node in self.0.iter() {
            if node.pred == pred
                && node
                    .args_size
                    .iter()
                    .zip(args_size.iter())
                    .all(|(arg0, arg)| arg0 <= arg)
            {
                return false;
            }
        }

        true
    }
}

impl Default for History {
    fn default() -> Self {
        Self::new()
    }
}
