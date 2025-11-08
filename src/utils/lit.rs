use std::fmt;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum LitType {
    TyInt,
    TyFloat,
    TyBool,
    TyChar,
}

impl fmt::Display for LitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitType::TyInt => "Int".fmt(f),
            LitType::TyFloat => "Float".fmt(f),
            LitType::TyBool => "Bool".fmt(f),
            LitType::TyChar => "Char".fmt(f),
        }
    }
}

impl FromStr for LitType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Int" => Ok(LitType::TyInt),
            "Float" => Ok(LitType::TyFloat),
            "Bool" => Ok(LitType::TyBool),
            "Char" => Ok(LitType::TyChar),
            _ => Err(()),
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

impl fmt::Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitVal::Int(x) => x.fmt(f),
            LitVal::Float(x) => x.fmt(f),
            LitVal::Bool(x) => x.fmt(f),
            LitVal::Char(x) => x.fmt(f),
        }
    }
}

impl FromStr for LitVal {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(x) = s.parse::<i64>() {
            return Ok(LitVal::Int(x));
        }

        if let Ok(x) = s.parse::<f64>() {
            return Ok(LitVal::Float(x));
        }

        match s {
            "true" => {
                return Ok(LitVal::Bool(true));
            }
            "false" => {
                return Ok(LitVal::Bool(false));
            }
            _ => {}
        }

        // todo: other basic datatypes
        Err(())
    }
}
