use std::fmt::Display;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum LitType {
    TyInt,
    TyFloat,
    TyBool,
    TyChar,
}

impl Display for LitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitType::TyInt => "Int".fmt(f),
            LitType::TyFloat => "Float".fmt(f),
            LitType::TyBool => "Bool".fmt(f),
            LitType::TyChar => "Char".fmt(f),
        }
    }
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

impl Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitVal::Int(x) => x.fmt(f),
            LitVal::Float(x) => x.fmt(f),
            LitVal::Bool(x) => x.fmt(f),
            LitVal::Char(x) => x.fmt(f),
        }
    }
}
