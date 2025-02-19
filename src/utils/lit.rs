#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum LitType {
    TyInt,
    TyFloat,
    TyBool,
    TyChar,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum LitVal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
}

impl LitVal {
    pub fn get_typ(&self) -> LitType {
        match self {
            LitVal::Int(_) => LitType::TyInt,
            LitVal::Float(_) => LitType::TyFloat,
            LitVal::Bool(_) => LitType::TyBool,
            LitVal::Char(_) => LitType::TyChar,
        }
    }
}
