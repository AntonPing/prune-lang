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

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub datas: HashMap<Ident, DataDecl>,
    pub preds: HashMap<PredIdent, PredDecl>,
    pub entrys: Vec<EntryDecl>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Goal {
    Lit(bool),
    Eq(Ident, AtomId),
    Cons(Ident, Ident, Vec<AtomId>),
    Prim(Prim, Vec<AtomId>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    Call(PredIdent, Vec<AtomId>),
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
    pub name: PredIdent,
    pub pars: Vec<Ident>,
    pub vars: Vec<Ident>,
    pub goal: Goal,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EntryDecl {
    pub entry: PredIdent,
    pub iter_start: usize,
    pub iter_end: usize,
    pub iter_step: usize,
}
