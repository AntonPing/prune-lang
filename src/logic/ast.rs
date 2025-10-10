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
    Eq(Ident, AtomId),
    Cons(Ident, Ident, Vec<AtomId>),
    Prim(Prim, Vec<AtomId>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    Call(Ident, Vec<AtomId>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataDecl {
    pub name: Ident,
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
    pub pars: Vec<Ident>,
    pub vars: Vec<Ident>,
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
