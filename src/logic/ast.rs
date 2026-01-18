use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub datas: HashMap<Ident, DataDecl>,
    pub preds: HashMap<Ident, PredDecl>,
    pub querys: Vec<QueryDecl>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Goal {
    Lit(bool),
    Eq(TermVal, TermVal),
    Prim(Prim, Vec<AtomVal>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    Call(Ident, Vec<TermType>, Vec<TermVal>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rule<V = Ident> {
    pub vars: Vec<(V, TermType)>,
    pub head: Vec<TermVal<V>>,
    pub prims: Vec<(Prim, Vec<AtomVal<V>>)>,
    pub calls: Vec<(Ident, Vec<TermType>, Vec<TermVal<V>>)>,
}

impl Rule<Ident> {
    pub fn tag_ctx(&self, ctx: usize) -> Rule<IdentCtx> {
        let vars: Vec<(IdentCtx, TermType)> = self
            .vars
            .iter()
            .map(|(par, typ)| (par.tag_ctx(ctx), typ.clone()))
            .collect();

        let head: Vec<TermVal<IdentCtx>> = self.head.iter().map(|par| par.tag_ctx(ctx)).collect();

        let prims: Vec<(Prim, Vec<AtomVal<IdentCtx>>)> = self
            .prims
            .iter()
            .map(|(prim, args)| (*prim, args.iter().map(|arg| arg.tag_ctx(ctx)).collect()))
            .collect();

        let calls: Vec<(Ident, Vec<TermType>, Vec<TermVal<IdentCtx>>)> = self
            .calls
            .iter()
            .map(|(pred, poly, args)| {
                (
                    *pred,
                    poly.clone(),
                    args.iter().map(|arg| arg.tag_ctx(ctx)).collect(),
                )
            })
            .collect();

        Rule {
            vars,
            head,
            prims,
            calls,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataDecl {
    pub name: Ident,
    pub polys: Vec<Ident>,
    pub cons: Vec<Constructor>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor {
    pub name: Ident,
    pub flds: Vec<TermType>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredDecl {
    pub name: Ident,
    pub polys: Vec<Ident>,
    pub pars: Vec<(Ident, TermType)>,
    pub vars: Vec<(Ident, TermType)>,
    pub goal: Goal,
    pub rules: Vec<Rule>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QueryDecl {
    pub entry: Ident,
    pub params: Vec<QueryParam>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum QueryParam {
    DepthStep(usize),
    DepthLimit(usize),
    AnswerLimit(usize),
    AnswerPause(bool),
}
