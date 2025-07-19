use super::*;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PredIdent {
    Succ(Ident),
    Fail(Ident),
    Check(Ident),
}

impl PredIdent {
    pub fn is_succ(&self) -> bool {
        matches!(self, PredIdent::Succ(_))
    }

    pub fn is_fail(&self) -> bool {
        matches!(self, PredIdent::Fail(_))
    }

    pub fn is_check(&self) -> bool {
        matches!(self, PredIdent::Check(_))
    }
}

impl std::fmt::Display for PredIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredIdent::Succ(ident) => write!(f, "(succ){}", ident),
            PredIdent::Fail(ident) => write!(f, "(fail){}", ident),
            PredIdent::Check(ident) => write!(f, "(check){}", ident),
        }
    }
}

// #[derive(Clone, Debug, PartialEq, PartialOrd)]
// pub struct Program {
//     pub datas: Vec<DataDecl>,
//     pub funcs: Vec<FuncDecl>,
//     pub preds: Vec<PredDecl>,
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Goal {
    Const(bool),
    Eq(Ident, Term<Ident>),
    Prim(Prim, Vec<Term<Ident>>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    PredCall(PredIdent, Vec<Term<Ident>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    pub name: PredIdent,
    pub pars: Vec<Ident>,
    pub vars: Vec<Ident>,
    pub goal: Goal,
}
