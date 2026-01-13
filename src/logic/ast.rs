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
pub struct Rule {
    pub head: Vec<TermVal>,
    pub prims: Vec<(Prim, Vec<AtomVal>)>,
    pub calls: Vec<(Ident, Vec<TermType>, Vec<TermVal>)>,
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
