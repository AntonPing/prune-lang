use super::lit::LitType;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum Compare {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
    Ne,
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum Prim {
    /// integer arithmetics
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    INeg,

    /// float-point arithmetics
    // FAdd,
    // FSub,
    // FMul,
    // FDiv,
    // FNeg,

    /// comparision
    ICmp(Compare),

    /// boolean operation
    BAnd,
    BOr,
    BNot,
}

impl Prim {
    pub fn get_typ(&self) -> Vec<LitType> {
        match self {
            Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem => {
                vec![LitType::TyInt, LitType::TyInt, LitType::TyInt]
            }
            Prim::INeg => {
                vec![LitType::TyInt, LitType::TyInt]
            }
            Prim::ICmp(_) => {
                vec![LitType::TyInt, LitType::TyInt, LitType::TyBool]
            }
            Prim::BAnd | Prim::BOr => {
                vec![LitType::TyBool, LitType::TyBool, LitType::TyBool]
            }
            Prim::BNot => {
                vec![LitType::TyBool, LitType::TyBool]
            }
        }
    }

    pub fn get_prior(&self) -> u8 {
        match self {
            Prim::IAdd => 3,
            Prim::ISub => 3,
            Prim::IMul => 4,
            Prim::IDiv => 4,
            Prim::IRem => 4,
            Prim::INeg => 0,
            Prim::BAnd => 1,
            Prim::BOr => 1,
            Prim::BNot => 0,
            Prim::ICmp(_) => 2,
        }
    }
}
