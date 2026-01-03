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
    Eq(TermId, TermId),
    Prim(Prim, Vec<AtomId>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    Call(Ident, Vec<TypeId>, Vec<TermId>),
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
    pub flds: Vec<TypeId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredDecl {
    pub name: Ident,
    pub polys: Vec<Ident>,
    pub pars: Vec<(Ident, TypeId)>,
    pub vars: Vec<(Ident, TypeId)>,
    pub goal: Goal,
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
