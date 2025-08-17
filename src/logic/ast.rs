use super::*;

// This enum is for supporting non-monotonic logic in the future
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PredIdent {
    Pos(Ident),
    Neg(Ident),
}

impl PredIdent {
    pub fn is_pos(&self) -> bool {
        matches!(self, PredIdent::Pos(_))
    }

    pub fn is_neg(&self) -> bool {
        matches!(self, PredIdent::Neg(_))
    }
}

impl std::fmt::Display for PredIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredIdent::Pos(ident) => write!(f, "(+){}", ident),
            PredIdent::Neg(ident) => write!(f, "(-){}", ident),
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
    Eq(Ident, AtomId),
    Cons(Ident, Ident, Vec<AtomId>),
    Prim(Prim, Vec<AtomId>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    PredCall(PredIdent, Vec<AtomId>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Predicate {
    pub name: PredIdent,
    pub pars: Vec<Ident>,
    pub vars: Vec<Ident>,
    pub goal: Goal,
}
